# Explicit human authorization, scoped to one job. The model cannot mint a
# grant: only the authenticated room command handler calls this writer.
job_approval_grant <- function(session, job, by, enabled = TRUE) {
    if (!job_in_session(job, session) || !job$status %in% JOB_STATUSES_OPEN) {
        stop("That job is not active in this session.", call. = FALSE)
    }
    grant <- list(job = job$id, owner = job_worker_owner(session),
                  session_key = job_worker_key(session), by = by,
                  enabled = isTRUE(enabled), at = job_now())
    job_write_file(file.path(job_dir(job$id), "approval-mode.json"), grant)
    invisible(grant)
}

bot_job_auto_grant <- function(chat, cfg, s, job) {
    current <- job_read(job$id)
    if (is.null(current) || !current$status %in% JOB_STATUSES_OPEN ||
        !job_in_session(current, s)) {
        return(NULL)
    }
    g <- job_read_file(file.path(job_dir(job$id), "approval-mode.json"))
    if (!isTRUE(g$enabled) || !identical(g$job, job$id) ||
        !identical(g$owner, job_worker_owner(s)) ||
        !identical(g$session_key, job_worker_key(s)) ||
        !g$by %in% bot_operators(cfg)) {
        return(NULL)
    }
    members <- tryCatch(chat.api::chat_members(chat,
            job$origin$room %||% s$room_id),
                        error = function(e) character())
    if (!g$by %in% members) {
        return(NULL)
    }
    g
}

bot_job_auto_answer <- function(chat, cfg, s, job, req) {
    if (!is.null(req$local_approval) || !is.null(.approval_remote_problem(
                list(tool = req$tool, args = req$args), list(reason = req$reason), .secret_values(cfg)))) {
        return(FALSE)
    }
    call <- list(tool = req$tool, args = req$args)
    check <- tryCatch(supervisor_route(call, list(approval = "ask",
                reason = req$reason),
                                       cwd = job$workspace %||% getwd(), mode = "human"),
                      error = function(e) list(flags = "credential check failed"))
    if (any(grepl("credential|secret|hard safety", c(check$flags, req$reason), ignore.case = TRUE))) {
        return(FALSE)
    }
    g <- bot_job_auto_grant(chat, cfg, s, job)
    if (is.null(g)) {
        return(FALSE)
    }
    isTRUE(job_answer(s, job$id, req$id, TRUE, by = g$by,
                      reason = "Operator approved the remaining requests for this job."))
}

bot_parse_approval_mode <- function(body) {
    cmd <- bot_command_text(body)
    m <- regmatches(cmd, regexec(
                                 "^/(approve-all)(?:\\s+(on|off|status))?(?:\\s+([0-9]{8}T[0-9]{6}-[0-9a-f]{8}))?\\s*$",
                                 cmd, perl = TRUE, ignore.case = TRUE))[[1L]]
    if (!length(m)) {
        return(NULL)
    }
    list(mode = if (nzchar(m[3L])) tolower(m[3L]) else "on",
         job = if (nzchar(m[4L])) m[4L] else NULL)
}

bot_approval_mode_command <- function(chat, cfg, session, command, sender) {
    if (!sender %in% bot_operators(cfg)) {
        return("Only a configured operator can change job approval mode.")
    }
    jobs <- job_list(status = JOB_STATUSES_OPEN,
                     origin_key = job_worker_key(session),
                     owner = job_worker_owner(session))
    if (!is.null(command$job)) jobs <- Filter(function(j) identical(j$id, command$job), jobs)
    if (!length(jobs)) {
        return("No active job here. Start a job, then use /approve-all.")
    }
    if (length(jobs) != 1L) {
        return(paste("Choose a job: /approve-all <job-id>. Active jobs:",
                     paste(vapply(jobs, function(j) j$id, ""), collapse = ", ")))
    }
    job <- jobs[[1L]]
    if (identical(command$mode, "status")) {
        active <- !is.null(bot_job_auto_grant(chat, cfg, session, job))
        return(sprintf("Job %s: approval of remaining requests is %s.",
                       job$id, if (active) "on" else "off"))
    }
    enabled <- identical(command$mode, "on")
    members <- tryCatch(chat.api::chat_members(chat, job$origin$room %||% session$room_id),
                        error = function(e) character())
    if (enabled && !sender %in% members) {
        return("Could not verify your membership in the job's room; approval mode is unchanged.")
    }
    job_approval_grant(session, job, sender, enabled)
    if (enabled) {
        answered <- character()
        for (req in job_approval_pending(job$id)) {
            if (bot_job_auto_answer(chat, cfg, session, job, req)) {
                answered <- c(answered, req$id)
            }
        }
        prompts <- session$.job_prompts %||% list()
        session$.job_prompts <- Filter(function(p) !(identical(p$job, job$id) && p$req %in% answered), prompts)
    }
    log_event("job_approval_mode", job = job$id, by = sender, enabled = enabled)
    if (!enabled) {
        return(sprintf("Job %s: per-call approvals restored.", job$id))
    }
    sprintf(paste0("Job %s: remaining approval requests authorized by %s until this job ends. ",
                   "Policy denials and per-call credential approval still apply. Use /approve-all off %s to revoke."), job$id, sender, job$id)
}
