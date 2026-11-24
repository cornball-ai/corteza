# Delegated jobs in the interactive REPL (corteza CLI and chat()).
#
# Same job system as the Matrix rooms (R/jobs.R, R/job-worker.R); only
# input and presentation differ. The REPL cannot print while it waits
# for a line of input -- the CLI reads through a blocking `read -e`, and
# chat() through readline() -- so jobs are pumped just before each
# prompt and after each command. A finished job's result appears then,
# and an approval request is asked right there. The cost is that a job
# waiting on approval waits until the next Enter; /jobs pumps on demand.

# Whether a session here is a talker when its config does not say.
# The CLI and chat() both default to direct execution. A surface can
# explicitly request a talker default. A session whose tools are restricted
# (--tools, the config's `tools`) is no talker by default, as in a room,
# since a doer gets the work tools whatever that list says.
.repl_talker_default <- function(ctx) {
    isTRUE(ctx$talker_default) && !length(ctx$session$tools_filter)
}

# Wire jobs into a REPL session: the job key and owner, talker mode
# where the config or the surface's default asks for it, and recovery of
# jobs left by local corteza processes that have exited.
#
# Only talker sessions can delegate, so a session not in talker mode
# gets none of this: no job key, no ledger reads, no recovery.
.repl_jobs_setup <- function(ctx) {
    session <- ctx$session
    if (!is.environment(session) || isTRUE(session$.repl_jobs_ready)) {
        return(invisible(FALSE))
    }
    cfg <- ctx$config %||% session$config
    talker <- talker_config(cfg, default = .repl_talker_default(ctx))
    if (is.null(talker) && !isTRUE(session$talker)) {
        return(invisible(FALSE))
    }
    by_default <- is.null(cfg$talker[["enabled"]])
    # One key for the whole process: a /clear starts a new conversation,
    # not a new set of jobs, and results still come back to it.
    session$job_key <- "repl"
    session$job_owner <- job_local_owner()
    # The worker's saved workspace belongs to this conversation, not this
    # process: resuming the conversation in a new process restores it,
    # and two CLIs open at once (different conversations) never share
    # one. A session with no conversation id falls back to the process.
    session$job_checkpoint_owner <- sprintf("local:%s",
        Sys.info()[["nodename"]])
    session$job_checkpoint_key <- paste0("repl:",
        ctx$disk_session$sessionId %||% session$sessionId %||%
        job_local_owner())
    if (!isTRUE(session$talker)) {
        talker_enable(session, talker)
        if (by_default) {
            cat(sprintf(paste0("%sTalker mode: this session answers, and work goes to a ",
                               "doer of its own (/jobs).\n\"talker\": {\"enabled\": ",
                               "false} in the corteza config turns it off.%s\n"),
                        ctx$palette$dim %||% "", ctx$palette$reset %||% ""))
        }
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

# Is a job of this session's open: running in its worker, or waiting
# to start?
.repl_jobs_open <- function(ctx) {
    s <- ctx$session
    if (!is.null(s$.job_current)) {
        return(TRUE)
    }
    open <- job_list(status = JOB_STATUSES_OPEN,
                     origin_key = job_worker_key(s),
                     owner = job_worker_owner(s))
    length(open) > 0L
}

# Stay with the session's jobs while one is open (WP20c): pump on a short
# sleep and print what comes, a step's progress, an approval question
# and the result, as a room posts them. Otherwise they would wait for
# the next Enter. An interrupt returns the prompt and leaves the job
# running, and /jobs stays with it again. A terminal driven from a room
# (remote control) does not stay: the room's next message is read as
# before, and the job's events show between messages.
.repl_attach_jobs <- function(ctx, interval = 0.5) {
    if (!isTRUE(ctx$session$.repl_jobs_ready) || !is.null(ctx$remote_dir) ||
        !.repl_jobs_open(ctx)) {
        return(invisible(FALSE))
    }
    p <- ctx$palette
    cat(sprintf("%sStaying with the job; Ctrl+C returns the prompt and leaves it running.%s\n",
                p$dim %||% "", p$reset %||% ""))
    done <- .repl_interruptible({
        repeat {
            .repl_pump_jobs(ctx)
            if (!.repl_jobs_open(ctx)) {
                break
            }
            Sys.sleep(interval)
        }
        TRUE
    }, p)
    if (inherits(done, "repl_interrupted")) {
        cat(sprintf("%sThe job goes on; /jobs shows it and stays with it again.%s\n",
                    p$dim %||% "", p$reset %||% ""))
    }
    invisible(TRUE)
}

# One line for each step a job's worker reported: what it said, then
# each tool call, as a room's activity post has them
# (rooms_activity_record()).
.repl_job_progress_lines <- function(ev) {
    id <- ev$job$id
    lines <- character()
    for (r in ev$records) {
        said <- .sanitize_inline(r$narration %||% "", max_chars = 160L)
        if (nzchar(said)) {
            lines <- c(lines, sprintf("job %s: %s", id, said))
        }
        t <- r$tool
        if (is.list(t)) {
            what <- trimws(paste(t$tool %||% "", t$command %||% ""))
            what <- .sanitize_inline(what, max_chars = 160L)
            how <- if (isTRUE(t$refused)) {
                " (refused)"
            } else if (!isTRUE(t$success)) {
                " (failed)"
            } else {
                ""
            }
            lines <- c(lines, sprintf("job %s: - %s%s", id, what, how))
        }
    }
    lines
}

.repl_job_progress <- function(ctx, ev) {
    p <- ctx$palette
    for (line in .repl_job_progress_lines(ev)) {
        cat(sprintf("%s%s%s\n", p$dim %||% "", line, p$reset %||% ""))
    }
    invisible(NULL)
}

.repl_present_job_event <- function(ctx, ev) {
    p <- ctx$palette
    switch(ev$type,
           settled = {
        text <- .repl_job_result_text(ev$job)
        usage <- ev$job$outcome$usage
        asked <- ev$job$origin$conversation
        current <- session_conversation(ctx$session)
        if (!is.null(asked) && !identical(asked, current)) {
            # Asked before a /clear: the result and what the job spent go
            # to the conversation that asked for it (/spent), and the new
            # one's talker does not take it.
            spend_accumulate_for(ctx$session, usage, asked)
            cat(sprintf("\n%s%s%s\n%s(asked in session %s, before /clear)%s\n\n",
                        p$cyan %||% "", text, p$reset %||% "", p$dim %||% "",
                        asked, p$reset %||% ""))
            record <- list(sessionId = asked, cwd = ctx$session$cwd)
            tryCatch(transcript_append(record, "assistant", text,
                                       job = ev$job$id),
                     error = function(e) NULL)
            return(invisible(NULL))
        }
        # What the job spent is this conversation's (/spent).
        session_accumulate_spend(ctx$session, usage)
        cat(sprintf("\n%s%s%s\n\n", p$cyan %||% "", text, p$reset %||% ""))
        # Into the talker's history, so it knows the job ended, and into
        # the transcript, which --resume seeds the history from and the
        # reviewer reads (review_conversation()).
        ctx$session$history <- c(ctx$session$history %||% list(),
                                 list(list(role = "assistant", content = text)))
        if (!is.null(ctx$disk_session)) {
            tryCatch(transcript_append(ctx$disk_session$session, "assistant",
                                       text, job = ev$job$id),
                     error = function(e) NULL)
        }
    },
           restored = cat(sprintf("%s%s%s\n", p$dim,
                                  bot_job_restored_text(ev$restore), p$reset)),
           blocked = cat(sprintf("%s%s%s\n", p$dim, job_blocked_text(ev),
                                 p$reset)),
           approval = .repl_job_approval(ctx, ev),
           progress = .repl_job_progress(ctx, ev),
           # "started": the talker already said so.
           NULL)
    invisible(NULL)
}

.repl_job_result_text <- function(job) {
    body <- job_result_body(job)
    head <- sprintf("Job %s %s: %s", job$id, job$status, job_title(job))
    paste(c(head, if (nzchar(body)) body, job_outcome_notes(job)),
          collapse = "\n")
}

# Ask about one approval request at the prompt. Only an explicit y/yes
# approves, or "a" where "always" is offered; anything else, including
# EOF, declines. A tool the person allowed always in this directory is
# allowed for a job's call as for the session's own, before the reviewer
# and without a prompt. Under review (R/review.R), the reviewer rules
# first on a read, write or exec call the doer's rules left open
# (`route` "monitor"), as a room's monitor does (bot_job_approval());
# what it passes on, or cannot judge, comes to the person with its
# reason.
.repl_job_approval <- function(ctx, ev) {
    req <- ev$request
    call <- job_request_call(req)
    p <- ctx$palette
    cb <- ctx$session$approval_cb
    if (.repl_job_answered(cb, req, call)) {
        recorded <- tryCatch(job_answer(ctx$session, ev$job$id, req$id,
                                        TRUE, by = "always"),
                             error = function(e) FALSE)
        if (isTRUE(recorded)) {
            cat(sprintf("%sJob %s: %s allowed, as you allowed it always here.%s\n",
                        p$dim %||% "", ev$job$id, call$tool, p$reset %||% ""))
        }
        return(invisible(recorded))
    }
    if (identical(req$route, "monitor") && review_mode(ctx$session) &&
        call_class(call) %in% c("read", "write", "exec")) {
        ruled <- .repl_job_review(ctx, ev$job, req)
        if (isTRUE(ruled$answered)) {
            return(invisible(ruled$recorded))
        }
        req$reason <- ruled$reason
    }
    cat(sprintf("\n%sJob %s asks: %s%s\n", ctx$palette$yellow %||% "",
                ev$job$id,
                bot_approval_prompt(call, list(reason = req$reason),
                                    timeout_sec = as.integer(
                    ctx$session$config$jobs$approval_timeout_sec %||%
                    600)),
                ctx$palette$reset %||% ""))
    if (!is.null(ctx$remote_dir)) {
        approved <- .remote_approve(ctx, call, list(reason = req$reason))
    } else {
        always <- .repl_job_always(cb, req)
        question <- if (is.function(always)) {
            "Approve? [y/a(lways)/N] "
        } else {
            "Approve? [y/N] "
        }
        # A Ctrl+C at the question is a no, as EOF is. Each request is
        # asked once (job_pump_approvals()), and one left open would hold
        # the worker until its wait ran out.
        answer <- tryCatch(ctx$read_input(question),
                           interrupt = function(e) character())
        answer <- if (length(answer) == 1L) {
            tolower(trimws(answer))
        } else {
            ""
        }
        kept <- is.function(always) && answer %in% c("a", "always")
        approved <- kept || answer %in% c("y", "yes")
        if (kept) {
            said <- tryCatch(always(call), error = function(e) {
                paste("It was not kept:", conditionMessage(e))
            })
            cat(sprintf("%s%s%s\n", p$dim %||% "", said, p$reset %||% ""))
        }
    }
    recorded <- tryCatch(job_answer(ctx$session, ev$job$id, req$id,
                                    approved, by = "local"),
                         error = function(e) FALSE)
    if (!isTRUE(recorded)) {
        cat(sprintf("%sToo late: that request had already closed.%s\n",
                    ctx$palette$dim, ctx$palette$reset))
    }
    invisible(recorded)
}

# Did the person answer a job's call for good: its tool allowed always in
# this directory, saved or held for the session? The session's approval
# callback says, by the rule it keeps for its own calls (its `answered`
# attribute, which the reviewer reads too), given the rule the worker's
# policy gave the call. That never covers the credential rule's ask, and
# a request that names no rule, from before 0.8.1, is asked.
.repl_job_answered <- function(cb, req, call) {
    answered <- attr(cb, "answered")
    rule <- req$rule
    if (!is.function(answered) || !is.character(rule) || length(rule) != 1L) {
        return(FALSE)
    }
    decision <- list(approval = "ask", reason = req$reason)
    if (nzchar(rule)) {
        decision$rule <- rule
    }
    isTRUE(tryCatch(answered(call, decision), error = function(e) FALSE))
}

# How a job's prompt keeps "always": the callback's own (its `always`
# attribute), which keeps it as its own prompt does. NULL where it has
# none, and for a rule a person answers each time (.person_rule()).
.repl_job_always <- function(cb, req) {
    always <- attr(cb, "always")
    rule <- req$rule
    if (!is.function(always) || !is.character(rule) || length(rule) != 1L ||
        .person_rule(list(rule = rule))) {
        return(NULL)
    }
    always
}

# The reviewer's ruling on a doer's request: the conversation that asked
# for the job, read from its transcript (review_conversation()), the
# task the talker wrote, and the call. An approval or a refusal answers
# the request (list(answered, recorded)); anything else returns the
# reason the person is told (list(reason)). A Ctrl+C while the reviewer
# rules hands the call to the person.
.repl_job_review <- function(ctx, job, req) {
    session <- ctx$session
    p <- ctx$palette
    conv <- review_conversation(job$origin$conversation %||% session$sessionId,
                                .review_record_budget(session, paste0("job:", job$id)))
    if (!is.null(conv$why)) {
        return(list(reason = paste("The reviewer was not asked:", conv$why)))
    }
    q <- bot_monitor_question(job, req, conv$lines)
    q$room <- FALSE
    r <- tryCatch(review_ask(session, q),
                  interrupt = function(e) {
        list(verdict = "escalate", reason = "you stopped the reviewer")
    },
                  error = function(e) {
        list(verdict = "escalate",
             reason = paste("the reviewer failed:", conditionMessage(e)))
    })
    why <- .sanitize_inline(r$reason %||% "", max_chars = 300L)
    if (!nzchar(why)) {
        why <- "no reason given"
    }
    if (isTRUE(r$verdict %in% c("approve", "refuse"))) {
        approved <- identical(r$verdict, "approve")
        recorded <- tryCatch(job_answer(session, job$id, req$id, approved,
                                        by = "reviewer", reason = why),
                             error = function(e) FALSE)
        if (approved) {
            verb <- "approved"
        } else {
            verb <- "refused"
        }
        tool <- .sanitize_inline(req$tool %||% "call", max_chars = 60L)
        cat(sprintf("%sThe reviewer %s job %s's %s: %s%s\n", p$dim %||% "",
                    verb, job$id, tool, why, p$reset %||% ""))
        return(list(answered = TRUE, recorded = recorded))
    }
    list(reason = paste("The reviewer passed this to you:", why))
}

# On /quit: stop this process's jobs and say so. Settling them here
# records a known outcome -- cancelled because corteza exited -- where
# leaving them would make the next start-up report them indeterminate.
.repl_jobs_shutdown <- function(ctx) {
    session <- ctx$session
    if (!isTRUE(session$.repl_jobs_ready)) {
        return(invisible(0L))
    }
    # The worker is closed on every exit, idle or not, and even if
    # cancelling fails. Idempotent: run_repl_loop() also registers this
    # with on.exit() for exits by error or interrupt.
    on.exit(job_worker_close_all(session), add = TRUE)
    open <- tryCatch(job_list(status = JOB_STATUSES_OPEN,
                              origin_key = job_worker_key(session),
                              owner = job_worker_owner(session)),
                     error = function(e) list())
    if (!length(open)) {
        return(invisible(0L))
    }
    for (j in open) {
        tryCatch(job_cancel(session, j$id, by = "exit"),
                 error = function(e) NULL)
    }
    tryCatch(job_pump(session), error = function(e) NULL)
    cat(sprintf("%sStopped %d job%s on exit.%s\n", ctx$palette$dim,
                length(open), if (length(open) == 1L) "" else "s",
                ctx$palette$reset))
    invisible(length(open))
}

.repl_cmd_job_resume <- function(ctx, parts) {
    if (length(parts) != 3L || parts[2L] != "resume") {
        cat("Usage: /job resume <id>\n")
        return(invisible(NULL))
    }
    .repl_jobs_setup(ctx)
    result <- tool_job_resume(parts[3L], ctx = list(session = ctx$session))
    cat(paste(vapply(result$content, function(x) x$text %||% "", ""),
              collapse = "\n"),
        "\n")
    invisible(NULL)
}

# /jobs [id]: pump, then list this session's jobs or show one, and stay
# with them while one is open (.repl_attach_jobs()).
.repl_cmd_jobs <- function(ctx, parts) {
    if (!isTRUE(ctx$session$.repl_jobs_ready)) {
        cat(.repl_jobs_off_text(ctx))
        return(invisible(NULL))
    }
    .repl_pump_jobs(ctx)
    res <- tool_job_status(id = if (length(parts) >= 2L) parts[2] else NULL,
                           ctx = list(session = ctx$session))
    cat(res$content[[1L]]$text, "\n", sep = "")
    .repl_attach_jobs(ctx)
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
