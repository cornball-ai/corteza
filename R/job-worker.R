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

# Did this session hand job `j` to another session's worker? A job
# handed across rooms (bot_job_handoff()) belongs to the room that runs
# it, and records the session that asked under origin$from. Same owner
# required: only a bot's own sessions hand work to each other.
job_requested_by <- function(j, session) {
    !is.null(j) && !is.null(j$origin$from$session_key) &&
    identical(j$origin$from$session_key, job_worker_key(session)) &&
    identical(j$owner %||% "local", job_worker_owner(session))
}

# May this session ask about job `j` or cancel it? The session that runs
# it and the session that asked for it both may.
job_visible_to <- function(j, session) {
    job_in_session(j, session) || job_requested_by(j, session)
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
                    reviewer = JOB_REVIEWER_TOOLS,
                    stop("unknown job role: ", role, call. = FALSE))
    # A talker session keeps its configured model for the doer
    # (talker_enable() records it as doer_model).
    provider <- cfg$provider %||% session$doer_provider %||%
    session$provider %||% "anthropic"
    configured <- session$doer_model %||% session$model_map$cloud
    model <- cfg$model %||% configured
    # Reasoning settings follow the model they were chosen for. For a
    # talker session those are the ones talker_enable() set aside for
    # the doer. A worker on some other model (`jobs$model`, or a
    # reviewer's own below) gets the provider's defaults unless its
    # config names a setting: effort tuned for one model can be an
    # error on another.
    effort <- if (isTRUE(session$talker)) {
        session$doer_reasoning_effort
    } else {
        .session_reasoning_effort(session)
    }
    budget <- if (isTRUE(session$talker)) {
        session$doer_thinking_budget
    } else {
        .session_thinking_budget(session)
    }
    thinking <- if (isTRUE(session$talker)) {
        session$doer_thinking
    } else {
        .session_thinking(session)
    }
    if (!identical(model, configured)) {
        effort <- budget <- thinking <- NULL
    }
    effort <- cfg$reasoning_effort %||% effort
    budget <- cfg$thinking_budget_tokens %||% budget
    # `[[`: `$thinking` on a list also matches `thinking_budget_tokens`.
    thinking <- cfg[["thinking"]] %||% thinking
    reviewer <- identical(role, "reviewer")
    if (reviewer) {
        # The reviewer may run on another provider or model than the
        # doer (`jobs$reviewer`); a second opinion from the same model
        # shares the first one's blind spots. A model named without a
        # provider keeps the doer's provider.
        rc <- cfg$reviewer %||% list()
        doer <- list(provider = provider, model = model)
        if (!is.null(rc$provider) && is.null(rc$model) &&
            !identical(rc$provider, provider)) {
            model <- NULL
        }
        provider <- rc$provider %||% provider
        model <- rc$model %||% model
        if (!identical(list(provider = provider, model = model), doer)) {
            effort <- budget <- thinking <- NULL
        }
        effort <- rc$reasoning_effort %||% effort
        budget <- rc$thinking_budget_tokens %||% budget
        thinking <- rc[["thinking"]] %||% thinking
    }
    spec <- list(
                 role = role,
                 provider = provider,
                 model = model,
                 reasoning_effort = effort,
                 thinking_budget_tokens = budget,
                 thinking = thinking,
                 tools = tools,
                 # Read-only has to mean no network and no files outside
                 # the checkout: a tool list alone grants neither limit
                 # (see PRESET_WEB_SEARCH and PRESET_CONFINED).
                 web_search = if (reviewer) FALSE,
                 allowed_paths = if (reviewer) {
            job_checkout(session$cwd %||% getwd())
        },
                 max_turns = as.integer(cfg$max_turns %||%
                                        JOB_WORKER_DEFAULTS$max_turns),
                 system = job_worker_system(role),
                 # A doer works on the project, so it gets the project's
                 # context (job_worker_context()). A reviewer judges one
                 # change against the task it was given and keeps its
                 # own short prompt.
                 project_context = !reviewer,
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
    if (identical(role, "reviewer")) {
        return(JOB_REVIEWER_SYSTEM)
    }
    paste0("You are the ", role, " for jobs a talker agent delegates ",
           "to you. Each message is one job. Work it in the current ",
           "directory. When you finish, say what you changed, how you ",
           "checked it, and anything left undone. Your R global ",
           "environment and this conversation persist between jobs.")
}

# The system prompt for a worker that works on the project in `cwd`: its
# role first, then the context a session opened in that directory gets
# (load_context_bundle(): the shared instructions file, the project's
# AGENTS.md or CLAUDE.md, the briefing, configured context files).
#
# The task text is the only thing a talker hands over, and a talker
# cannot be relied on to restate a project's rules in every task. A doer
# without them edits a repository knowing nothing of how its owner works
# in it: which tools, which git habits, what never to do.
#
# Built in the worker's own process, for the directory it runs in, and
# not copied from the talker: the talker's prompt also carries its
# surface (the Matrix room, talker guidance), which is not the doer's.
# The instruction catalog is left out because subagent_turn_init() adds
# it for the tools the worker actually has. An error here stops the
# worker from starting, which the job reports; a doer that quietly
# starts without the rules is the failure this exists to prevent.
job_worker_context <- function(system, cwd = getwd()) {
    role <- context_text_source("job_worker_role", "runtime", system, -400,
                                "corteza::job_worker_system", "session")
    bundle <- load_context_bundle(cwd,
                                  prefix_sources = Filter(Negate(is.null), list(role)),
                                  include_instruction_catalog = FALSE)
    bundle$system %||% system
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
        system <- spec$system
        if (isTRUE(spec$project_context)) {
            system <- job_worker_context(system)
        }
        subagent_turn_init(provider = spec$provider, model = spec$model,
                           tools_filter = spec$tools, system = system,
                           max_turns = spec$max_turns,
                           plan_mode = spec$plan_mode, channel = spec$channel,
                           web_search = spec$web_search,
                           allowed_paths = spec$allowed_paths,
                           reasoning_effort = spec$reasoning_effort,
                           thinking_budget_tokens = spec$thinking_budget_tokens,
                           thinking = spec[["thinking"]])
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

# The directory this session's jobs run in, in the form job records and
# worker notes compare by.
job_worker_dir <- function(session) {
    normalizePath(session$cwd %||% getwd(), mustWork = FALSE)
}

# What the session knows about its worker for `role`: the directory the
# process was started in, and when it last had work. The first says
# whether a live worker may be reused (job_worker_activate()), the
# second when an idle one may be retired (job_workers_retire()).
job_worker_note <- function(session, role, dir = NULL, used = Sys.time()) {
    notes <- session$.job_worker_notes %||% list()
    note <- notes[[role]] %||% list()
    if (!is.null(dir)) {
        note$dir <- dir
    }
    note$used <- used
    notes[[role]] <- note
    session$.job_worker_notes <- notes
    invisible(note)
}

# Is the live worker for `role` in the directory the session works in
# now? A worker is a process with a working directory of its own; one
# started before the session's directory changed would run the next job
# somewhere else than its record says.
job_worker_in_place <- function(session, role) {
    identical(session$.job_worker_notes[[role]]$dir, job_worker_dir(session))
}

# Start the session's worker, restoring its last checkpoint. Blocks for
# the child's startup (a second or two) -- the one synchronous step, and
# it happens only when a job is dispatched to a session with no live
# worker. Returns the restore info.
job_worker_start <- function(session, role = "doer") {
    job_worker_close(session)
    job_worker_drop_parked(session, role)
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
    job_worker_note(session, role, dir = job_worker_dir(session))
    restore
}

# Close the active worker: the one running, or last to run, a job. This
# is how a job in flight is stopped. Workers parked for other roles are
# left alone.
job_worker_close <- function(session) {
    w <- session$.job_worker
    if (!is.null(w)) {
        tryCatch(w$close(), error = function(e) NULL)
    }
    session$.job_worker <- NULL
    session$.job_worker_role <- NULL
    session$.job_current <- NULL
    invisible(TRUE)
}

# A session runs one job at a time but keeps a worker per role, so a
# review does not cost the doer its loaded packages and objects. The
# active worker is session$.job_worker; the others wait in
# session$.job_workers_parked, by role.

# Make the worker for `role` the active one, starting it if there is
# none alive. Returns the restore info when a worker was started, NULL
# when a live one was reused.
#
# A live worker is reused only while it is in the directory the session
# works in (job_worker_in_place()). One that is not is replaced: started
# fresh in the right directory, where it restores that directory's own
# checkpoint if there is one.
job_worker_activate <- function(session, role) {
    if (job_worker_alive(session) &&
        identical(session$.job_worker_role, role)) {
        if (job_worker_in_place(session, role)) {
            return(NULL)
        }
        return(job_worker_start(session, role))
    }
    parked <- session$.job_workers_parked %||% list()
    if (job_worker_alive(session)) {
        parked[[session$.job_worker_role]] <- session$.job_worker
    }
    session$.job_worker <- NULL
    session$.job_worker_role <- NULL
    w <- parked[[role]]
    parked[[role]] <- NULL
    session$.job_workers_parked <- parked
    if (!is.null(w) &&
        isTRUE(tryCatch(w$is_alive(), error = function(e) FALSE))) {
        if (job_worker_in_place(session, role)) {
            session$.job_worker <- w
            session$.job_worker_role <- role
            return(NULL)
        }
        tryCatch(w$close(), error = function(e) NULL)
    }
    job_worker_start(session, role)
}

job_worker_drop_parked <- function(session, role) {
    parked <- session$.job_workers_parked %||% list()
    w <- parked[[role]]
    if (!is.null(w)) {
        tryCatch(w$close(), error = function(e) NULL)
        parked[[role]] <- NULL
        session$.job_workers_parked <- parked
    }
    invisible(TRUE)
}

# Close every worker the session has, active and parked. For shutdown.
job_worker_close_all <- function(session) {
    job_worker_close(session)
    for (w in session$.job_workers_parked %||% list()) {
        tryCatch(w$close(), error = function(e) NULL)
    }
    session$.job_workers_parked <- NULL
    invisible(TRUE)
}

# Everything a session holds about its jobs and workers.
JOB_SESSION_STATE <- c(".job_worker", ".job_worker_role",
                       ".job_workers_parked", ".job_worker_notes",
                       ".job_current", ".job_events", ".job_asked",
                       ".job_blocked", ".job_prompts")

# Move a session's job state to the session that replaces it under the
# same key (a /clear, a /model switch). The ledger finds a room's jobs
# by key, and the pump runs them through whichever session holds that
# key, so the worker handles and the job in flight have to be there.
#
# The replacement may work in another directory (the room's topic was
# edited). The job in flight finishes where it started; after that
# job_worker_activate() sees the worker is not in the session's
# directory and starts one that is.
job_state_move <- function(from, to) {
    for (field in JOB_SESSION_STATE) {
        if (exists(field, envir = from, inherits = FALSE)) {
            assign(field, get(field, envir = from), envir = to)
            rm(list = field, envir = from)
        }
    }
    invisible(to)
}

# Queue a job for this session and try to start it. Returns the id.
#
# `review = TRUE` has a reviewer check the work when the job ends `done`
# (R/job-review.R). Only a job that writes can be reviewed.
job_submit <- function(session, task, role = "doer", requester = "local",
                       origin = list(), parent = NULL, limits = list(),
                       review = FALSE) {
    origin$session_key <- job_worker_key(session)
    id <- job_create(task, role = role,
                     workspace = session$cwd %||% getwd(),
                     requester = requester, origin = origin,
                     parent = parent, limits = limits,
                     permissions = list(tools = job_worker_spec(session, role)$tools),
                     owner = job_worker_owner(session),
                     review = isTRUE(review) && job_role_writes(role))
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
    # The room that asked for a handed-off job may cancel it too. The
    # request is a file the running room's pump reads, so nothing here
    # needs that room's worker.
    if (!job_visible_to(j, session)) {
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
        # Idle from now, as far as retiring the worker goes.
        job_worker_note(session, j$role)
        if (job_role_locks(j$role)) {
            job_lock_release(job_checkout(j$workspace), id)
        }
        list(type = "settled", job = job_read(id))
    }
    # A job that ends `done`. A doer's job submitted for review gets its
    # review queued, and the checkout lock handed to it, before the job
    # is settled: settling first would free the lock for any queued
    # writer. A review's verdict is read off its reply.
    done <- function(result, usage, reason) {
        extra <- list()
        if (isTRUE(j$review) && job_role_writes(j$role)) {
            review <- tryCatch(job_queue_review(session, j, result),
                               error = function(e) e)
            if (inherits(review, "error")) {
                extra$review_error <- conditionMessage(review)
            } else {
                extra$review_job <- review$id
                extra$review_locked <- review$locked
            }
        }
        if (identical(j$role, "reviewer")) {
            verdict <- job_review_verdict(result)
            extra["verdict"] <- list(if (is.na(verdict)) NULL else verdict)
        }
        finish("done", result = result, usage = usage, reason = reason,
               extra = extra)
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
    done(res$reply %||% "", res$usage, if (!isTRUE(res$checkpoint$ok)) {
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

# Start the oldest queued job for this session that can run now.
#
# Oldest first, but a job waiting on its checkout lock is passed over
# rather than holding up the queue. That matters for reviews: a review
# is queued when its doer job ends and already holds the lock, so an
# older queued writer is blocked by it. Stopping at the first blocked
# job would leave the review waiting behind the job it blocks.
job_dispatch_next <- function(session) {
    key <- job_worker_key(session)
    queued <- job_list(status = "queued", origin_key = key,
                       owner = job_worker_owner(session))
    events <- list()
    checkouts <- list()
    for (j in queued) {
        checkout <- NULL
        if (job_role_locks(j$role)) {
            # One git lookup per workspace per pass, not per job.
            if (is.null(checkouts[[j$workspace]])) {
                checkouts[[j$workspace]] <- job_checkout(j$workspace)
            }
            checkout <- checkouts[[j$workspace]]
            lock <- job_lock_acquire(checkout, j$id, job_worker_owner(session))
            if (!isTRUE(lock$ok)) {
                events <- c(events, job_blocked_event(session, j, lock$holder))
                next
            }
            # A review whose checkout another job held since the doer
            # would inspect that job's changes too. It is cancelled, not
            # run; nothing has reached a worker yet.
            if (!is.null(j$review_of)) {
                held <- job_review_unbroken(checkout, j$review_of, j$id)
                if (!isTRUE(held$ok)) {
                    job_settle(j$id, "cancelled", reason = sprintf(
                            paste0("the checkout was not held continuously ",
                                   "since job %s ended%s, so it may hold ",
                                   "other changes. Ask for the review again ",
                                   "if it is still wanted."),
                            j$review_of,
                            if (length(held$by)) {
                                sprintf(" (job %s held it in between)",
                                        paste(held$by, collapse = ", "))
                            } else {
                                ""
                            }))
                    job_lock_release(checkout, j$id)
                    events <- c(events, list(list(type = "settled",
                                job = job_read(j$id))))
                    next
                }
            }
        }
        return(c(events, job_dispatch(session, j, checkout)))
    }
    events
}

# Hand job `j` to its role's worker. `checkout` is the lock it holds, or
# NULL. Every way out either leaves the job running as the session's
# current job, or ends it and gives the lock back.
job_dispatch <- function(session, j, checkout) {
    events <- list()
    # Nothing has reached a worker yet, so a job stopped here had no
    # effect: it fails cleanly rather than going indeterminate.
    refuse <- function(why) {
        job_settle(j$id, "failed", error = why)
        if (!is.null(checkout)) {
            job_lock_release(checkout, j$id)
        }
        list(list(type = "settled", job = job_read(j$id)))
    }
    # The job's record, the session, and the worker have to name one
    # directory. The record was written when the job was queued; a
    # session that works somewhere else by now would run it in a place
    # its record, its lock, and whoever asked for it know nothing of.
    wanted <- normalizePath(j$workspace, mustWork = FALSE)
    if (!identical(wanted, job_worker_dir(session))) {
        return(refuse(sprintf(paste0("this job was queued to run in %s, but ",
                                     "its session now works in %s; nothing ",
                                     "was run. Ask again."),
                              wanted, job_worker_dir(session))))
    }
    # Always through job_worker_activate(): it reuses a live worker only
    # when that worker is in the session's directory.
    restore <- tryCatch(job_worker_activate(session, j$role),
                        error = function(e) e)
    if (inherits(restore, "error")) {
        return(refuse(conditionMessage(restore)))
    }
    if (isTRUE(restore$restored)) {
        events[[length(events) + 1L]] <- list(type = "restored",
            job = j, restore = restore)
    }
    job_worker_note(session, j$role)
    spec <- job_worker_spec(session, j$role)
    # Every way out of the hand-off ends the job and gives back the lock.
    # Without this, a failure here left the job marked running and
    # holding its checkout, with no current job for later pumps to find.
    abandon <- function(status, ...) {
        job_settle(j$id, status, ...)
        if (!is.null(checkout)) {
            job_lock_release(checkout, j$id)
        }
        c(events, list(list(type = "settled", job = job_read(j$id))))
    }
    marked <- tryCatch({
        job_mark_dispatched(j$id, worker = list(
                pid = session$.job_worker$get_pid()),
                            # What the checkout looked like as the job began, so its
                            # review can show exactly what it changed. Only taken for a
                            # job that will be reviewed: nothing else reads it.
                            base = if (isTRUE(j$review) && !is.null(checkout)) {
                job_git_snapshot(checkout, job_snapshot_max_bytes(session))
            })
        TRUE
    }, error = function(e) e)
    if (inherits(marked, "error")) {
        # No dispatch record, so no worker was given the job.
        return(abandon("failed", error = conditionMessage(marked)))
    }
    sent <- tryCatch({
        session$.job_worker$call(function(job_id, task, spec, state_dir) {
            get(".job_worker_child_run", envir = asNamespace("corteza"),
                inherits = FALSE)(job_id, task, spec, state_dir)
        }, list(job_id = j$id, task = j$task, spec = spec,
                state_dir = job_worker_state_dir(spec$identity)))
        TRUE
    }, error = function(e) e)
    if (inherits(sent, "error")) {
        # The call failed partway; whether the worker received the job
        # cannot be known from here. Stop the worker so it cannot run it
        # later, and record the job as possibly started.
        job_worker_close(session)
        return(abandon("indeterminate", error = conditionMessage(sent),
                       reason = paste("the hand-off to the worker failed;",
                                      "it may have started before stopping")))
    }
    session$.job_current <- j$id
    events[[length(events) + 1L]] <- list(type = "started",
        job = job_read(j$id))
    events
}
