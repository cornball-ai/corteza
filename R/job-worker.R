# The job worker: one persistent R process per conversation session,
# running that session's delegated jobs one at a time.
#
# Persistent rather than per job because warm R state -- loaded
# packages, objects built up across jobs, the worker's own conversation
# history -- is the reason to use a callr worker at all. A per-job
# worker would throw that away without buying recoverability, since a
# crash loses in-memory state either way.
#
# What survives a crash is a checkpoint taken at each job boundary: the
# worker's global environment (in the run_r worker's
# corteza_workspace_v1 format) and its conversation history. A new
# worker for the same session restores the last checkpoint and says so.
# It is never mid-job state: work done by a job that did not finish is
# lost from memory, and whatever it did to files stays done. That job is
# settled indeterminate by the ledger (R/jobs.R), not re-run.
#
# Restoring has limits the user is told about rather than hidden from:
# objects holding external pointers (connections, torch tensors, DB
# handles, processes) come back as dead shells, and attached packages
# are re-attached by name only.
#
# The parent side never blocks on the worker. job_pump() polls with a
# zero timeout and returns events for the caller's surface to present.

JOB_WORKER_DEFAULTS <- list(max_turns = 50L)

# The session's key for its jobs and its worker checkpoint. Surfaces set
# session$job_key (a Matrix room or thread key, a REPL session key); the
# fallbacks keep a bare session usable.
job_worker_key <- function(session) {
    session$job_key %||% session$sessionKey %||% "local"
}

# The process identity that owns this session's jobs (see job_create()).
# Bots set it to their Matrix id; everything else is "local".
job_worker_owner <- function(session) {
    session$job_owner %||% "local"
}

# Is job `j` this session's? Same session key and same owner: two bots
# in one room share the key but not the owner.
job_in_session <- function(j, session) {
    !is.null(j) &&
    identical(j$origin$session_key, job_worker_key(session)) &&
    identical(j$owner %||% "local", job_worker_owner(session))
}

# Whose saved workspace a worker may restore. All four parts count:
#   owner      a bot's Matrix id, or for a REPL its host and conversation
#              (job_checkpoint_owner), so two bots in one room, or two
#              CLIs at once, never share a workspace
#   key        the room, thread, or REPL session
#   workspace  the directory the jobs run in
#   role       a reviewer's worker must not wake up holding the doer's
#              workspace
# Surfaces set job_checkpoint_owner / job_checkpoint_key where these
# differ from the job owner and key (a REPL's job owner is its pid,
# which a restart changes; its checkpoint should survive a restart of
# the same conversation).
job_worker_identity <- function(session, role = "doer") {
    list(owner = session$job_checkpoint_owner %||% job_worker_owner(session),
         key = session$job_checkpoint_key %||% job_worker_key(session),
         workspace = normalizePath(session$cwd %||% getwd(), mustWork = FALSE),
         role = role)
}

# Checkpoint directory for an identity. Named by a digest, since keys
# carry characters that do not belong in a path; the identity itself is
# recorded in checkpoint.json and checked again on restore.
job_worker_state_dir <- function(identity) {
    file.path(bot_signal_dir(), "workers",
              substr(digest::digest(job_identity_text(identity), algo = "sha256",
                                    serialize = FALSE),
                     1L, 24L))
}

job_identity_text <- function(identity) {
    paste(c(identity$owner, identity$key, identity$workspace, identity$role),
          collapse = "\n")
}

# What a worker for this session and role is started with. Tools come
# from the role; provider and model from the session unless the config
# names a worker model. `session$job_worker_spec` overrides fields, and
# is also how tests replace the child's init and run functions with ones
# that need no provider.
job_worker_spec <- function(session, role = "doer") {
    cfg <- session$config$jobs %||% list()
    tools <- switch(role, doer = SUBAGENT_PRESETS$work,
                    stop("unknown job role: ", role, call. = FALSE))
    spec <- list(
                 role = role,
                 # A talker session keeps its configured model for the doer
                 # (talker_enable() records it as doer_model).
                 provider = cfg$provider %||% session$doer_provider %||%
                 session$provider %||% "anthropic",
                 model = cfg$model %||% session$doer_model %||%
                 session$model_map$cloud,
                 tools = tools,
                 max_turns = as.integer(cfg$max_turns %||%
                                        JOB_WORKER_DEFAULTS$max_turns),
                 system = job_worker_system(role),
                 plan_mode = isTRUE(session$plan_mode),
                 # The originating session's channel, so policy judges the
                 # worker's calls as it would the session's own.
                 channel = session$channel %||% "console",
                 approval_timeout = as.numeric(cfg$approval_timeout_sec %||% 600),
                 identity = job_worker_identity(session, role),
                 init_fn = NULL,
                 run_fn = NULL
    )
    utils::modifyList(spec, session$job_worker_spec %||% list())
}

job_worker_system <- function(role) {
    paste0("You are the ", role, " for jobs a talker agent delegates ",
           "to you. Each message is one job. Work it in the current ",
           "directory. When you finish, say what you changed, how you ",
           "checked it, and anything left undone. Your R global ",
           "environment and this conversation persist between jobs.")
}

# ---- child side --------------------------------------------------------

# Child-process state: the job being run and the approval timeout. Each
# callr worker loads its own namespace, so this never reaches the host.
.job_worker_state <- new.env(parent = emptyenv())

# Start the child's turn session and restore the last checkpoint, if
# any. Returns what was restored so the parent can report it.
.job_worker_child_init <- function(cwd, spec, state_dir) {
    worker_init(cwd)
    init <- spec$init_fn %||% function(spec) {
        subagent_turn_init(provider = spec$provider, model = spec$model,
                           tools_filter = spec$tools, system = spec$system,
                           max_turns = spec$max_turns,
                           plan_mode = spec$plan_mode, channel = spec$channel)
    }
    init(spec)
    .job_worker_state$approval_timeout <- spec$approval_timeout %||% 600
    # Replace the subagent default (deny everything) with the bridge.
    if (!is.null(.subagent_state$session)) {
        .subagent_state$session$approval_cb <- .job_worker_child_ask
    }
    .job_worker_child_restore(state_dir, spec$identity)
}

# The worker's approval callback: ask through the job's approval files
# and wait. Outside a job there is nobody to ask, so it declines.
.job_worker_child_ask <- function(call, decision) {
    job_id <- .job_worker_state$job_id
    if (is.null(job_id)) {
        return(FALSE)
    }
    req <- job_approval_request(job_id, call, decision)
    job_approval_wait(job_id, req,
                      timeout = .job_worker_state$approval_timeout %||% 600)
}

.job_worker_child_restore <- function(state_dir, identity) {
    marker <- file.path(state_dir, "checkpoint.json")
    if (!file.exists(marker)) {
        return(list(restored = FALSE))
    }
    info <- jsonlite::fromJSON(marker, simplifyVector = TRUE)
    # The directory name is only a digest. A checkpoint is restored only
    # when the identity it recorded is this worker's, field for field.
    if (!identical(job_identity_text(info$identity),
                   job_identity_text(identity))) {
        return(list(restored = FALSE, mismatch = TRUE))
    }
    env <- new.env(parent = emptyenv())
    load(file.path(state_dir, "workspace.RData"), envir = env)
    values <- .workspace_checkpoint_values(
        get(.run_r_worker_checkpoint_key, envir = env), globalenv())
    for (name in names(values)) {
        assign(name, values[[name]], envir = globalenv())
    }
    for (pkg in info$packages %||% character()) {
        suppressPackageStartupMessages(
                                       try(library(pkg, character.only = TRUE), silent = TRUE))
    }
    history_path <- file.path(state_dir, "history.rds")
    if (!is.null(.subagent_state$session) && file.exists(history_path)) {
        .subagent_state$session$history <- readRDS(history_path)
    }
    list(restored = TRUE, job = info$job, at = info$at, objects = names(values))
}

# Checkpoint at a job boundary. The marker is written last: a restore
# reads it first, so a crash partway through leaves the previous marker
# (and the previous workspace it names) or none, never a marker pointing
# at a half-written save.
.job_worker_child_checkpoint <- function(state_dir, identity, job_id) {
    dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
    objects <- .workspace_checkpoint_write(globalenv(),
        file.path(state_dir, "workspace.RData"))
    history <- .subagent_state$session$history
    if (!is.null(history)) {
        tmp <- tempfile("history.", tmpdir = state_dir)
        saveRDS(history, tmp)
        if (!isTRUE(file.rename(tmp, file.path(state_dir, "history.rds")))) {
            unlink(tmp)
            stop("could not write worker history checkpoint", call. = FALSE)
        }
    }
    attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
    job_write_file(file.path(state_dir, "checkpoint.json"),
                   list(identity = identity, job = job_id, at = job_now(),
                        objects = objects, packages = attached))
    objects
}

# Run one job in the child. Errors from the turn are the job's outcome,
# not the worker's: they come back as data, and the checkpoint is still
# taken, so the next job starts from what this one left.
.job_worker_child_run <- function(job_id, task, spec, state_dir) {
    .job_worker_state$job_id <- job_id
    on.exit(.job_worker_state$job_id <- NULL, add = TRUE)
    run <- spec$run_fn %||% function(task) subagent_turn_prompt(task)
    res <- tryCatch(run(task), error = function(e) {
        list(error = conditionMessage(e))
    })
    checkpoint <- tryCatch(
                           list(ok = TRUE,
                                objects = .job_worker_child_checkpoint(state_dir, spec$identity,
                job_id)),
                           error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    list(reply = res$reply, usage = res$usage, error = res$error,
         checkpoint = checkpoint)
}

# ---- parent side -------------------------------------------------------

job_worker_alive <- function(session) {
    w <- session$.job_worker
    !is.null(w) && isTRUE(tryCatch(w$is_alive(), error = function(e) FALSE))
}

# Start the session's worker, restoring its last checkpoint. Blocks for
# the child's startup (a second or two) -- the one synchronous step, and
# it happens only when a job is dispatched to a session with no live
# worker. Returns the restore info.
job_worker_start <- function(session, role = "doer") {
    job_worker_close(session)
    spec <- job_worker_spec(session, role)
    worker <- callr::r_session$new(
                                   options = .run_r_worker_session_options(session$config %||% list(),
            "job_worker_options"),
                                   wait = TRUE)
    restore <- tryCatch(
                        worker$run(function(cwd, spec, state_dir) {
        library(corteza)
        get(".job_worker_child_init", envir = asNamespace("corteza"),
            inherits = FALSE)(cwd, spec, state_dir)
    }, list(cwd = session$cwd %||% getwd(), spec = spec,
                state_dir = job_worker_state_dir(spec$identity))),
                        error = function(e) {
        tryCatch(worker$close(), error = function(e2) NULL)
        stop("Failed to start job worker: ", conditionMessage(e), call. = FALSE)
    })
    session$.job_worker <- worker
    session$.job_worker_role <- role
    restore
}

job_worker_close <- function(session) {
    w <- session$.job_worker
    if (!is.null(w)) {
        tryCatch(w$close(), error = function(e) NULL)
    }
    session$.job_worker <- NULL
    session$.job_current <- NULL
    invisible(TRUE)
}

# Queue a job for this session and try to start it. Returns the id.
job_submit <- function(session, task, role = "doer", requester = "local",
                       origin = list(), parent = NULL, limits = list()) {
    origin$session_key <- job_worker_key(session)
    id <- job_create(task, role = role,
                     workspace = session$cwd %||% getwd(),
                     requester = requester, origin = origin,
                     parent = parent, limits = limits,
                     permissions = list(tools = job_worker_spec(session, role)$tools),
                     owner = job_worker_owner(session))
    # job_pump() drains any events already held, so what it returns is
    # the whole backlog; the next pump from the surface delivers it.
    session$.job_events <- job_pump(session)
    id
}

# Cancel a job. A queued job ends at once. A running one has its worker
# stopped by the next pump; the request is recorded either way, so a
# restart between the two still sees it.
job_cancel <- function(session, id, by = "local") {
    j <- job_read(id)
    if (!job_in_session(j, session)) {
        stop("no job ", id, " in this session", call. = FALSE)
    }
    if (!job_request_cancel(id, by = by)) {
        return(FALSE)
    }
    if (identical(j$status, "queued")) {
        job_settle(id, "cancelled",
                   reason = paste("cancelled by", by, "before it started"))
    }
    TRUE
}

# Advance this session's jobs without blocking. Returns a list of
# events, each list(type = ..., job = <record>) with type one of
# "settled", "started", or "restored" (the last carrying `restore`).
# Call it from the surface's loop; also drains events that job_submit()
# collected.
job_pump <- function(session) {
    events <- session$.job_events %||% list()
    session$.job_events <- NULL
    current <- session$.job_current
    if (!is.null(current)) {
        ev <- job_pump_current(session, current)
        if (!is.null(ev)) {
            events[[length(events) + 1L]] <- ev
        }
    }
    if (is.null(session$.job_current)) {
        events <- c(events, job_dispatch_next(session))
    }
    current <- session$.job_current
    if (!is.null(current)) {
        events <- c(events, job_pump_approvals(session, current))
    }
    events
}

# New approval requests from the running job, each reported once as an
# "approval" event carrying the request. The surface asks, then answers
# with job_answer(); until then the worker waits and the loop does not.
job_pump_approvals <- function(session, id) {
    asked <- session$.job_asked %||% character()
    fresh <- Filter(function(r) !r$id %in% asked, job_approval_pending(id))
    if (!length(fresh)) {
        return(list())
    }
    session$.job_asked <- c(asked, vapply(fresh, function(r) r$id, ""))
    j <- job_read(id)
    lapply(fresh, function(r) list(type = "approval", job = j, request = r))
}

# Answer one of this session's approval requests. FALSE when the answer
# no longer counts (see job_approval_answer()).
job_answer <- function(session, id, req, approved, by = "local") {
    j <- job_read(id)
    if (!job_in_session(j, session)) {
        stop("no job ", id, " in this session", call. = FALSE)
    }
    job_approval_answer(id, req, approved, by = by)
}

job_pump_current <- function(session, id) {
    worker <- session$.job_worker
    j <- job_read(id)
    # Every ending settles the job, frees the session, and gives back the
    # checkout lock (a no-op for a role that never took one).
    finish <- function(status, ...) {
        job_settle(id, status, ...)
        session$.job_current <- NULL
        if (job_role_writes(j$role)) {
            job_lock_release(job_checkout(j$workspace), id)
        }
        list(type = "settled", job = job_read(id))
    }
    if (isTRUE(j$cancel_requested)) {
        # Stopping the worker is the only way to stop a turn in flight.
        # Its memory goes with it; the next job restores the checkpoint
        # from before this one.
        job_worker_close(session)
        return(finish("cancelled",
                      reason = paste("stopped mid-job; changes it made",
                                     "before stopping are kept")))
    }
    wall <- j$limits$wall_seconds
    if (!is.null(wall) && !is.null(j$dispatch)) {
        started <- as.POSIXct(j$dispatch$dispatched_at,
                              format = "%Y-%m-%dT%H:%M:%OS%z")
        if (as.numeric(difftime(Sys.time(), started, units = "secs")) > wall) {
            job_worker_close(session)
            return(finish("failed", reason = sprintf(
                        "exceeded its %s s wall-clock limit", format(wall))))
        }
    }
    if (!job_worker_alive(session)) {
        session$.job_worker <- NULL
        return(finish("indeterminate",
                      reason = paste("the worker process exited mid-job;",
                                     "it may have acted before it stopped")))
    }
    if (!identical(worker$poll_process(0L), "ready")) {
        return(NULL)
    }
    msg <- worker$read()
    if (!is.null(msg$error)) {
        return(finish("failed", error = conditionMessage(msg$error)))
    }
    res <- msg$result
    if (!is.null(res$error)) {
        return(finish("failed", error = res$error, usage = res$usage))
    }
    finish("done", result = res$reply %||% "", usage = res$usage,
           reason = if (!isTRUE(res$checkpoint$ok)) {
            paste("workspace checkpoint failed:", res$checkpoint$error)
        })
}

# A job waiting on another writer's checkout lock. Reported once per
# holder, so the surface can say who it is waiting for without
# repeating itself on every pump.
job_blocked_event <- function(session, j, holder) {
    blocked <- session$.job_blocked %||% list()
    if (identical(blocked[[j$id]], holder$job)) {
        return(list())
    }
    blocked[[j$id]] <- holder$job %||% "unknown"
    session$.job_blocked <- blocked
    list(list(type = "blocked", job = j, holder = holder))
}

# Start the oldest queued job for this session, if any.
job_dispatch_next <- function(session) {
    key <- job_worker_key(session)
    queued <- job_list(status = "queued", origin_key = key,
                       owner = job_worker_owner(session))
    if (!length(queued)) {
        return(list())
    }
    j <- queued[[1L]]
    events <- list()
    checkout <- NULL
    if (job_role_writes(j$role)) {
        checkout <- job_checkout(j$workspace)
        lock <- job_lock_acquire(checkout, j$id, job_worker_owner(session))
        if (!isTRUE(lock$ok)) {
            return(job_blocked_event(session, j, lock$holder))
        }
    }
    if (!job_worker_alive(session) ||
        !identical(session$.job_worker_role, j$role)) {
        restore <- tryCatch(job_worker_start(session, j$role),
                            error = function(e) e)
        if (inherits(restore, "error")) {
            # Nothing reached a worker, so the job had no effect: it
            # fails cleanly rather than going indeterminate.
            job_settle(j$id, "failed", error = conditionMessage(restore))
            if (!is.null(checkout)) {
                job_lock_release(checkout, j$id)
            }
            return(list(list(type = "settled", job = job_read(j$id))))
        }
        if (isTRUE(restore$restored)) {
            events[[length(events) + 1L]] <- list(type = "restored",
                job = j, restore = restore)
        }
    }
    spec <- job_worker_spec(session, j$role)
    job_mark_dispatched(j$id, worker = list(
            pid = session$.job_worker$get_pid()))
    session$.job_worker$call(function(job_id, task, spec, state_dir) {
        get(".job_worker_child_run", envir = asNamespace("corteza"),
            inherits = FALSE)(job_id, task, spec, state_dir)
    }, list(job_id = j$id, task = j$task, spec = spec,
            state_dir = job_worker_state_dir(spec$identity)))
    session$.job_current <- j$id
    events[[length(events) + 1L]] <- list(type = "started",
        job = job_read(j$id))
    events
}
