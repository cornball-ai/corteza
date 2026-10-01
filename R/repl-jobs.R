# Delegated jobs in the interactive REPL (corteza CLI and chat()).
#
# Same job system as the Matrix rooms (R/jobs.R, R/job-worker.R); only
# input and presentation differ. The REPL cannot print while it waits
# for a line of input -- the CLI reads through a blocking `read -e`, and
# chat() through readline() -- so jobs are pumped just before each
# prompt and after each command. A finished job's result appears then,
# and an approval request is asked right there. The cost is that a job
# waiting on approval waits until the next Enter; /jobs pumps on demand.

# Wire jobs into a REPL session: the job key and owner, talker mode when
# the config asks for it, and recovery of jobs left by local corteza
# processes that have exited.
#
# Only talker sessions can delegate, so a session not in talker mode
# gets none of this: no job key, no ledger reads, no recovery. Existing
# REPL behavior is untouched unless the config turns talker mode on.
.repl_jobs_setup <- function(ctx) {
    session <- ctx$session
    if (!is.environment(session) || isTRUE(session$.repl_jobs_ready)) {
        return(invisible(FALSE))
    }
    talker <- talker_config(ctx$config %||% session$config)
    if (is.null(talker) && !isTRUE(session$talker)) {
        return(invisible(FALSE))
    }
    # One key for the whole process: a /clear starts a new conversation,
    # not a new set of jobs, and results still come back to it.
    session$job_key <- "repl"
    session$job_owner <- job_local_owner()
    if (!isTRUE(session$talker)) {
        talker_enable(session, talker)
    }
    orphans <- tryCatch(job_recover_local_orphans(), error = function(e) NULL)
    if (!is.null(orphans)) {
        here <- orphans[orphans$workspace == normalizePath(
                session$cwd %||% ctx$cwd %||% getwd(), mustWork = FALSE),]
        for (i in seq_len(nrow(here))) {
            cat(sprintf("%sJob %s from an exited corteza process: %s (%s)%s\n",
                        ctx$palette$dim, here$id[[i]], here$verdict[[i]],
                        .sanitize_inline(here$task[[i]], max_chars = 60L),
                        ctx$palette$reset))
        }
    }
    session$.repl_jobs_ready <- TRUE
    invisible(TRUE)
}

# Advance this session's jobs and present what happened. Approval
# requests are asked inline through ctx$read_input().
.repl_pump_jobs <- function(ctx) {
    session <- ctx$session
    if (!isTRUE(session$.repl_jobs_ready)) {
        return(invisible(0L))
    }
    if (is.null(session$.job_current) && !length(session$.job_events) &&
        !length(job_list(status = "queued",
                         origin_key = job_worker_key(session),
                         owner = job_worker_owner(session)))) {
        return(invisible(0L))
    }
    events <- tryCatch(job_pump(session), error = function(e) {
        cat(sprintf("%sjobs: %s%s\n", ctx$palette$dim,
                    conditionMessage(e), ctx$palette$reset))
        list()
    })
    for (ev in events) {
        .repl_present_job_event(ctx, ev)
    }
    invisible(length(events))
}

.repl_present_job_event <- function(ctx, ev) {
    p <- ctx$palette
    switch(ev$type,
           settled = {
        text <- .repl_job_result_text(ev$job)
        cat(sprintf("\n%s%s%s\n\n", p$cyan %||% "", text, p$reset %||% ""))
        # Into the talker's history, so it knows the job ended.
        ctx$session$history <- c(ctx$session$history %||% list(),
                                 list(list(role = "assistant", content = text)))
    },
           restored = cat(sprintf("%s%s%s\n", p$dim,
                                  bot_job_restored_text(ev$restore), p$reset)),
           blocked = cat(sprintf("%s%s%s\n", p$dim, job_blocked_text(ev),
                                 p$reset)),
           approval = .repl_job_approval(ctx, ev),
           # "started": the talker already said so.
           NULL)
    invisible(NULL)
}

.repl_job_result_text <- function(job) {
    o <- job$outcome
    body <- switch(job$status, done = o$result %||% "",
                   failed = o$error %||% o$reason %||% "", o$reason %||% "")
    head <- sprintf("Job %s %s: %s", job$id, job$status,
                    .sanitize_inline(job$task, max_chars = 100L))
    if (nzchar(body)) {
        paste0(head, "\n", body)
    } else {
        head
    }
}

# Ask about one approval request at the prompt. Only an explicit y/yes
# approves; anything else, including EOF, declines.
.repl_job_approval <- function(ctx, ev) {
    req <- ev$request
    call <- list(tool = req$tool, args = req$args)
    cat(sprintf("\n%sJob %s asks: %s%s\n", ctx$palette$yellow %||% "",
                ev$job$id,
                bot_approval_prompt(call, list(reason = req$reason),
                                    timeout_sec = as.integer(
                    ctx$session$config$jobs$approval_timeout_sec %||%
                    600)),
                ctx$palette$reset %||% ""))
    answer <- ctx$read_input("Approve? [y/N] ")
    approved <- length(answer) == 1L &&
    tolower(trimws(answer)) %in% c("y", "yes")
    recorded <- tryCatch(job_answer(ctx$session, ev$job$id, req$id,
                                    approved, by = "local"),
                         error = function(e) FALSE)
    if (!isTRUE(recorded)) {
        cat(sprintf("%sToo late: that request had already closed.%s\n",
                    ctx$palette$dim, ctx$palette$reset))
    }
    invisible(recorded)
}

# On /quit: stop this process's jobs and say so. Settling them here
# records a known outcome -- cancelled because corteza exited -- where
# leaving them would make the next start-up report them indeterminate.
.repl_jobs_shutdown <- function(ctx) {
    session <- ctx$session
    if (!isTRUE(session$.repl_jobs_ready)) {
        return(invisible(0L))
    }
    open <- job_list(status = JOB_STATUSES_OPEN,
                     origin_key = job_worker_key(session),
                     owner = job_worker_owner(session))
    if (!length(open)) {
        return(invisible(0L))
    }
    for (j in open) {
        tryCatch(job_cancel(session, j$id, by = "exit"),
                 error = function(e) NULL)
    }
    tryCatch(job_pump(session), error = function(e) NULL)
    job_worker_close(session)
    cat(sprintf("%sStopped %d job%s on exit.%s\n", ctx$palette$dim,
                length(open), if (length(open) == 1L) "" else "s",
                ctx$palette$reset))
    invisible(length(open))
}

# /jobs [id]: pump, then list this session's jobs or show one.
.repl_cmd_jobs <- function(ctx, parts) {
    if (!isTRUE(ctx$session$.repl_jobs_ready)) {
        cat(.repl_jobs_off_text(ctx))
        return(invisible(NULL))
    }
    .repl_pump_jobs(ctx)
    res <- tool_job_status(id = if (length(parts) >= 2L) parts[2] else NULL,
                           ctx = list(session = ctx$session))
    cat(res$content[[1L]]$text, "\n", sep = "")
    invisible(NULL)
}

.repl_jobs_off_text <- function(ctx) {
    sprintf(paste0("%sNo jobs: talker mode is off. Set ",
                   "\"talker\": {\"enabled\": true} in the corteza config.%s\n"),
            ctx$palette$dim %||% "", ctx$palette$reset %||% "")
}

# /cancel <id>
.repl_cmd_cancel <- function(ctx, parts) {
    if (!isTRUE(ctx$session$.repl_jobs_ready)) {
        cat(.repl_jobs_off_text(ctx))
        return(invisible(NULL))
    }
    if (length(parts) < 2L) {
        cat(sprintf("%sUsage:%s /cancel <job-id>\n", ctx$palette$dim,
                    ctx$palette$reset))
        return(invisible(NULL))
    }
    res <- tool_job_cancel(parts[2], ctx = list(session = ctx$session))
    cat(res$content[[1L]]$text, "\n", sep = "")
    .repl_pump_jobs(ctx)
    invisible(NULL)
}
