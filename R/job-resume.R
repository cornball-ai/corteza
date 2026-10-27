# One explicit continuation per stopped attempt. Further continuation targets
# the new attempt, keeping the chain and its spending visible in the ledger.
job_resume <- function(session, id, by, operators = character(), event = NULL,
                       source = session) {
    j <- job_read(id)
    if ((!job_in_session(j, source) && !job_requested_by(j, source)) ||
        !job_in_session(j, session) || !identical(job_worker_dir(session), j$workspace)) {
        stop("No resumable job in this conversation and workspace", call. = FALSE)
    }
    if (!isTRUE(by == j$requester || by %in% operators)) {
        stop("Only the original requester or a bot operator may resume this job", call. = FALSE)
    }
    if (isTRUE(source$plan_mode) || isTRUE(session$plan_mode)) stop("Resume is unavailable in plan mode", call. = FALSE)
    if (!identical(j$status, "incomplete") || !identical(j$backend, "subagent") ||
        !identical(j$role, "doer")) stop("Only an incomplete local doer job can be resumed", call. = FALSE)
    slot <- file.path(job_dir(id), "continuation")
    claim <- job_read_file(file.path(slot, "request.json"))
    if (!is.null(claim) && !is.null(job_read(claim$id))) return(claim$id)
    if (!isTRUE(j$outcome$resume$ready)) stop("This attempt has no usable resume snapshot", call. = FALSE)
    job_resume_snapshot(id, job_worker_identity(session, j$role))
    if (is.null(claim)) {
        stage <- tempfile(".continuation-", tmpdir = job_dir(id))
        dir.create(stage, mode = "0700")
        on.exit(unlink(stage, recursive = TRUE), add = TRUE)
        job_write_file(file.path(stage, "request.json"), list(id = job_new_id(),
            by = by, event = event, at = job_now()))
        # A nonempty destination cannot be replaced. Concurrent requests all
        # converge on its one reserved ID, including after a coordinator crash.
        suppressWarnings(file.rename(stage, slot))
        claim <- job_read_file(file.path(slot, "request.json"))
        if (is.null(claim)) stop("Could not reserve a resume attempt", call. = FALSE)
    }
    task <- paste(j$task, paste("Continuation of job", id,
        ". Earlier attempts may have changed files before this attempt's review baseline.",
        "A review must distinguish this attempt's diff from the task's overall completion."), sep = "\n\n")
    new_id <- job_create(task, role = j$role, workspace = j$workspace,
        requester = j$requester, origin = j$origin, parent = j$parent,
        backend = j$backend, permissions = j$permissions, limits = j$limits,
        owner = j$owner, review = isTRUE(j$review), resume_of = id,
        id = claim$id, resume_request = claim)
    session$.job_events <- job_pump(session)
    new_id
}

job_resume_usage <- function(job, usage = list()) {
    restore_na <- function(x) {
        if (is.list(x)) return(lapply(x, restore_na))
        if (identical(x, "NA")) NA_real_ else x
    }
    seen <- character()
    id <- job$resume_of
    while (!is.null(id)) {
        if (id %in% seen) stop("Invalid resume chain", call. = FALSE)
        seen <- c(seen, id)
        prior <- job_read(id)
        if (is.null(prior) || !identical(prior$owner, job$owner)) stop("Invalid resume chain", call. = FALSE)
        usage <- .turn_add_usage(usage, restore_na(prior$outcome$usage))
        id <- prior$resume_of
    }
    usage
}

#' Resume an incomplete delegated job after an explicit user request.
#' @param id (character) Incomplete job id.
#' @param ctx Server-side context; supplies requester authorization.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_job_resume <- function(id, ctx = list()) {
    s <- ctx$session
    if (!is.environment(s)) return(err("job_resume needs a live session"))
    tryCatch({
        next_id <- if (is.function(s$job_resume)) s$job_resume(id) else {
            job_resume(s, id, by = s$job_requester %||% "local")
        }
        ok(sprintf("Continuation attempt %s for job %s. Follow that attempt; no other continuation was started.", next_id, id))
    }, error = function(e) err(conditionMessage(e)))
}

bot_job_resume <- function(sessions, session, cfg, sender, event, id) {
    j <- job_read(id)
    if (!job_in_session(j, session) && !job_requested_by(j, session)) {
        stop("No resumable job in this conversation", call. = FALSE)
    }
    target <- if (job_in_session(j, session)) session else {
        get0(j$origin$session_key, envir = sessions, inherits = FALSE)
    }
    if (!is.environment(target)) stop("The job's worker conversation is not loaded; open it first", call. = FALSE)
    job_resume(target, id, by = sender, operators = bot_operators(cfg), event = event, source = session)
}

bot_parse_job_resume <- function(body) {
    cmd <- bot_command_text(body)
    if (!grepl("^/job(?:\\s|$)", cmd, perl = TRUE)) return(NULL)
    parts <- strsplit(trimws(cmd), "\\s+")[[1L]]
    if (length(parts) != 3L || parts[2L] != "resume") return(list(error = "Usage: /job resume <id>"))
    list(id = parts[3L])
}
