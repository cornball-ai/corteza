# Durable job records for delegated work.
#
# A job is one piece of work a talker hands to a worker. Its record lives
# on disk, owned by the process that created it, so a crash of either
# side leaves an account of what was asked and how far it got.
#
# The model is viento's claim ledger (R/claim.R there): intent is written
# before anything acts, and the outcome is written beside the intent,
# never over it. Each job directory holds up to four files:
#
#   intent.json    what was asked, by whom, under which permissions.
#                  Written once, at creation.
#   dispatch.json  written just before the job is handed to a worker.
#   outcome.json   written once, when the job ends. First writer wins.
#   cancel.json    a cancellation request. The owner reads it; it does
#                  not end the job by itself.
#
# Status is derived from which files exist rather than kept in a field
# that is rewritten, so no update can leave the record half-changed.
#
# What recovery does with an unfinished job follows from the same files.
# No dispatch record means no worker ever saw it, so it is safe to run.
# A dispatch record with no outcome means a worker may have acted --
# edited files, posted, spent tokens -- and the result is gone. That job
# is settled `indeterminate`, reported, and never re-run: running it
# again is a new job.

JOB_STATUSES_OPEN <- c("queued", "running")
JOB_STATUSES_FINAL <- c("done", "failed", "cancelled", "indeterminate")

JOB_DEFAULT_LIMITS <- list(hops = 3L)

# Root of the job ledger. Under the same state directory as the bot's
# signal files, so CORTEZA_STATE_DIR relocates both for tests and for
# hosts that keep state somewhere unusual. Not created here: writers
# create what they write.
job_root <- function() {
    file.path(bot_signal_dir(), "jobs")
}

job_dir <- function(id) {
    file.path(job_root(), id)
}

# Time-ordered so a directory listing is creation order, with a random
# suffix so two jobs created in the same second do not collide.
job_new_id <- function() {
    paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "-",
           paste(sample(c(0:9, letters[1:6]), 8L, replace = TRUE), collapse = ""))
}

# Ids reach file paths, so anything but the shape job_new_id() makes is
# refused before it can name a path outside the ledger.
job_check_id <- function(id) {
    if (!is.character(id) || length(id) != 1L || is.na(id) ||
        !grepl("^[0-9]{8}T[0-9]{6}-[0-9a-f]{8}$", id)) {
        stop("invalid job id: ", paste(format(id), collapse = " "),
             call. = FALSE)
    }
    id
}

# Write one record file atomically: a temp file in the same directory,
# then a rename. A reader sees the old state or the new one, never a
# partial file. file.rename() returns FALSE rather than erroring, so its
# result is checked; reporting success on a failed rename would lose the
# record silently.
job_write_file <- function(path, x) {
    tmp <- tempfile(paste0(basename(path), "."), tmpdir = dirname(path))
    on.exit(unlink(tmp), add = TRUE)
    writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null",
                                pretty = TRUE, digits = NA), tmp)
    if (!isTRUE(file.rename(tmp, path))) {
        stop("could not write job record ", path, call. = FALSE)
    }
    invisible(path)
}

job_read_file <- function(path) {
    if (!file.exists(path)) {
        return(NULL)
    }
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE)
}

job_now <- function() {
    format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
}

# Create a job and write its intent. Returns the id.
#
# `parent` is the id of the job this one was delegated from, if any. The
# hop count is the parent's plus one, and it is checked here, in code,
# against the parent's hop limit: a limit the model is only told about
# does not stop two agents delegating to each other forever. A child
# inherits its parent's limits and cannot raise them.
#
# `permissions` is recorded as given. Nothing reads it to widen a
# worker's access; it is the grant the worker is started with.
job_create <- function(task, role = "doer", workspace = getwd(),
                       requester = "local", origin = list(), parent = NULL,
                       backend = "subagent", permissions = list(),
                       limits = list()) {
    if (!is.character(task) || length(task) != 1L || is.na(task) ||
        !nzchar(trimws(task))) {
        stop("job task must be one non-empty string", call. = FALSE)
    }
    limits <- utils::modifyList(JOB_DEFAULT_LIMITS, limits)
    hop <- 0L
    if (!is.null(parent)) {
        up <- job_read(parent)
        if (is.null(up)) {
            stop("parent job not found: ", parent, call. = FALSE)
        }
        # The parent's limits bind the child: it may ask for fewer hops,
        # never more, and any other limit the parent set is kept.
        hops <- min(as.integer(limits$hops),
                    as.integer(up$limits$hops %||% limits$hops))
        limits <- utils::modifyList(limits, up$limits)
        limits$hops <- hops
        hop <- as.integer(up$hop) + 1L
        if (hop > as.integer(limits$hops)) {
            stop(sprintf("job %s is at its hop limit (%d); refusing to delegate further",
                         parent, as.integer(limits$hops)), call. = FALSE)
        }
    }
    id <- job_new_id()
    dir <- job_dir(id)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    job_write_file(file.path(dir, "intent.json"), list(
            id = id,
            created_at = job_now(),
            task = task,
            task_digest = digest::digest(task, algo = "sha256",
                serialize = FALSE),
            role = role,
            workspace = normalizePath(workspace, mustWork = FALSE),
            requester = requester,
            origin = origin,
            parent = parent,
            hop = hop,
            backend = backend,
            permissions = permissions,
            limits = limits
        ))
    id
}

# Record that the job is about to reach a worker. Written before the
# hand-off, not after: if the process dies between the two, recovery
# must assume the worker may have started, which is the safe mistake.
job_mark_dispatched <- function(id, worker = list()) {
    job_check_id(id)
    dir <- job_dir(id)
    if (!file.exists(file.path(dir, "intent.json"))) {
        stop("job not found: ", id, call. = FALSE)
    }
    if (file.exists(file.path(dir, "outcome.json"))) {
        stop("job ", id, " has already ended", call. = FALSE)
    }
    if (file.exists(file.path(dir, "dispatch.json"))) {
        stop("job ", id, " was already dispatched", call. = FALSE)
    }
    job_write_file(file.path(dir, "dispatch.json"),
                   list(dispatched_at = job_now(), worker = worker))
    invisible(id)
}

# End a job. First writer wins: returns TRUE when this call recorded the
# outcome, FALSE when the job had already ended. A late result arriving
# after a cancellation or a recovery verdict therefore cannot overwrite
# what was decided first.
job_settle <- function(id, status, result = NULL, error = NULL, usage = NULL,
                       reason = NULL) {
    job_check_id(id)
    if (!is.character(status) || length(status) != 1L ||
        !status %in% JOB_STATUSES_FINAL) {
        stop("job status must be one of: ",
             paste(JOB_STATUSES_FINAL, collapse = ", "), call. = FALSE)
    }
    dir <- job_dir(id)
    if (!file.exists(file.path(dir, "intent.json"))) {
        stop("job not found: ", id, call. = FALSE)
    }
    path <- file.path(dir, "outcome.json")
    if (file.exists(path)) {
        return(FALSE)
    }
    job_write_file(path, list(status = status, finished_at = job_now(),
                              result = result, error = error,
                              usage = usage, reason = reason))
    TRUE
}

# Ask for a job to stop. The owner acts on it (kills the worker, settles
# the job `cancelled`); this only records the request and who made it.
# Returns FALSE when the job has already ended.
job_request_cancel <- function(id, by = "local") {
    job_check_id(id)
    dir <- job_dir(id)
    if (!file.exists(file.path(dir, "intent.json"))) {
        stop("job not found: ", id, call. = FALSE)
    }
    if (file.exists(file.path(dir, "outcome.json"))) {
        return(FALSE)
    }
    job_write_file(file.path(dir, "cancel.json"),
                   list(requested_at = job_now(), by = by))
    TRUE
}

# One job's record: the intent fields plus `status`, `dispatch`,
# `outcome`, and `cancel_requested`. NULL when there is no such job.
job_read <- function(id) {
    job_check_id(id)
    dir <- job_dir(id)
    intent <- job_read_file(file.path(dir, "intent.json"))
    if (is.null(intent)) {
        return(NULL)
    }
    dispatch <- job_read_file(file.path(dir, "dispatch.json"))
    outcome <- job_read_file(file.path(dir, "outcome.json"))
    cancel <- job_read_file(file.path(dir, "cancel.json"))
    intent$status <- if (!is.null(outcome)) {
        outcome$status
    } else if (!is.null(dispatch)) {
        "running"
    } else {
        "queued"
    }
    intent$dispatch <- dispatch
    intent$outcome <- outcome
    intent$cancel_requested <- !is.null(cancel)
    intent
}

# All jobs, oldest first, optionally narrowed by status and by the
# originating session key.
job_list <- function(status = NULL, origin_key = NULL) {
    root <- job_root()
    if (!dir.exists(root)) {
        return(list())
    }
    ids <- sort(list.files(root))
    ids <- ids[grepl("^[0-9]{8}T[0-9]{6}-[0-9a-f]{8}$", ids)]
    jobs <- lapply(ids, job_read)
    jobs <- Filter(Negate(is.null), jobs)
    if (!is.null(status)) {
        jobs <- Filter(function(j) j$status %in% status, jobs)
    }
    if (!is.null(origin_key)) {
        jobs <- Filter(function(j) identical(j$origin$session_key, origin_key),
                       jobs)
    }
    jobs
}

# Classify every unfinished job after a restart or a worker death.
#
# `live` is the ids of jobs a worker is known to still be running. They
# are left alone. Of the rest:
#   queued  -> "retryable": no worker ever saw it. Left queued.
#   running -> "indeterminate": dispatched, no outcome. Settled so, with
#              the reason, and never re-run automatically.
#
# Returns a data frame of id, verdict, and task, so the caller can tell
# the user what was found. The verdicts are written before this returns;
# a second call finds nothing new to classify.
job_recover <- function(live = character()) {
    open <- job_list(status = JOB_STATUSES_OPEN)
    open <- Filter(function(j) !j$id %in% live, open)
    verdicts <- vapply(open, function(j) {
        if (identical(j$status, "queued")) {
            return("retryable")
        }
        job_settle(j$id, "indeterminate",
                   reason = paste("dispatched with no recorded outcome;",
                                  "the worker may have acted before it", "stopped"))
        "indeterminate"
    }, character(1))
    data.frame(id = vapply(open, function(j) j$id, character(1)),
               verdict = unname(verdicts),
               task = vapply(open, function(j) j$task, character(1)),
               stringsAsFactors = FALSE)
}

# ---- Approval bridge ---------------------------------------------------
#
# A worker has no channel to the user. When policy says "ask", the
# worker writes a request into its job's approvals/ directory and waits
# for an answer file; the owning loop sees the request, asks through its
# surface, and writes the answer. The loop never blocks on the user and
# the worker never talks to the surface.
#
# Per request, up to three files:
#   <req>.request.json  what the worker wants to do (tool, args, reason)
#   <req>.answer.json   the surface's verdict and who gave it
#   <req>.closed.json   written by the worker when it stops waiting:
#                       approved, denied, timeout, or cancelled
#
# An answer authorizes exactly that request of that job. It is refused
# once the request is closed or the job has ended, so an approval that
# arrives after a cancellation, an expiry, or a restart cannot resume
# anything: the worker that asked is gone or has moved on.

job_new_request_id <- function() {
    paste0("r", format(Sys.time(), "%H%M%S"), "-",
           paste(sample(c(0:9, letters[1:6]), 6L, replace = TRUE), collapse = ""))
}

job_check_request_id <- function(req) {
    if (!is.character(req) || length(req) != 1L || is.na(req) ||
        !grepl("^r[0-9]{6}-[0-9a-f]{6}$", req)) {
        stop("invalid approval request id: ",
             paste(format(req), collapse = " "), call. = FALSE)
    }
    req
}

job_approval_path <- function(id, req, kind) {
    file.path(job_dir(id), "approvals", paste0(req, ".", kind, ".json"))
}

# Bound what goes into the record: args are model-written and can be a
# whole file's contents. The surface renders from this, and a prompt
# does not need more than the start of each value.
job_approval_args <- function(args, max_chars = 2000L) {
    lapply(args %||% list(), function(v) {
        v <- paste(format(v), collapse = " ")
        if (nchar(v) > max_chars) {
            paste0(substr(v, 1L, max_chars), "...")
        } else {
            v
        }
    })
}

# Worker side: record a request. Returns the request id.
job_approval_request <- function(id, call, decision) {
    job_check_id(id)
    req <- job_new_request_id()
    dir.create(file.path(job_dir(id), "approvals"), recursive = TRUE,
               showWarnings = FALSE)
    job_write_file(job_approval_path(id, req, "request"), list(
            id = req,
            job = id,
            requested_at = job_now(),
            tool = call$tool %||% call$name %||% "",
            args = job_approval_args(call$args),
            reason = decision$reason %||% "ask"
        ))
    req
}

# Worker side: wait for the answer. TRUE only for an explicit approval
# of this request. Stops waiting, and closes the request, on a denial,
# on timeout, or when the job is cancelled or has ended. `interval` and
# `timeout` are seconds.
job_approval_wait <- function(id, req, timeout = 600, interval = 0.5) {
    job_check_id(id)
    job_check_request_id(req)
    close_as <- function(verdict) {
        job_write_file(job_approval_path(id, req, "closed"),
                       list(closed_at = job_now(), verdict = verdict))
        identical(verdict, "approved")
    }
    dir <- job_dir(id)
    deadline <- Sys.time() + timeout
    repeat {
        answer <- job_read_file(job_approval_path(id, req, "answer"))
        if (!is.null(answer)) {
            return(close_as(if (isTRUE(answer$approved)) "approved" else
                            "denied"))
        }
        if (file.exists(file.path(dir, "cancel.json")) ||
            file.exists(file.path(dir, "outcome.json"))) {
            return(close_as("cancelled"))
        }
        if (Sys.time() >= deadline) {
            return(close_as("timeout"))
        }
        Sys.sleep(interval)
    }
}

# Owner side: open requests for a job, oldest first. A request is open
# while it has neither an answer nor a closed record.
job_approval_pending <- function(id) {
    job_check_id(id)
    dir <- file.path(job_dir(id), "approvals")
    if (!dir.exists(dir)) {
        return(list())
    }
    files <- sort(list.files(dir, pattern = "\\.request\\.json$"))
    reqs <- lapply(file.path(dir, files), job_read_file)
    Filter(function(r) {
        !file.exists(job_approval_path(id, r$id, "answer")) &&
        !file.exists(job_approval_path(id, r$id, "closed"))
    }, reqs)
}

# Owner side: answer a request. Returns TRUE when the answer was
# recorded, FALSE when it can no longer count -- the request was already
# answered or closed, or the job has ended. Who is allowed to answer is
# the surface's decision; `by` records who did.
job_approval_answer <- function(id, req, approved, by = "local") {
    job_check_id(id)
    job_check_request_id(req)
    if (!file.exists(job_approval_path(id, req, "request"))) {
        stop("no approval request ", req, " for job ", id, call. = FALSE)
    }
    if (file.exists(file.path(job_dir(id), "outcome.json")) ||
        file.exists(job_approval_path(id, req, "answer")) ||
        file.exists(job_approval_path(id, req, "closed"))) {
        return(FALSE)
    }
    job_write_file(job_approval_path(id, req, "answer"),
                   list(approved = isTRUE(approved), by = by, answered_at = job_now()))
    TRUE
}
