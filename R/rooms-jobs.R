# Delegated jobs in Matrix rooms: presenting job events and collecting
# approvals without blocking the poll loop.
#
# bot_run_step() pumps every room session's jobs after each poll. A
# finished job is posted back to the room (or thread) it came from and
# recorded in that session's history, so the talker that delegated it
# knows the result. A job that needs approval gets a prompt; the answer
# arrives as a reaction in a later poll, handled by
# bot_handle_job_reactions(), and nothing waits for it in between. That
# is the difference from bot_reaction_approval(), which holds the whole
# loop until someone taps.

# Poll interval while any room has work in flight. Short enough that a
# finished job or an approval prompt shows up promptly, long enough not
# to spin.
BOT_JOB_POLL_MS <- 2000L

bot_sessions_list <- function(sessions) {
    if (is.null(sessions)) {
        return(list())
    }
    mget(ls(sessions), envir = sessions)
}

# Room sessions with something to pump: a running job, events held from
# a submit, or a queued job under their key. The ledger is read once for
# all of them rather than once per room.
bot_job_sessions <- function(sessions) {
    all <- bot_sessions_list(sessions)
    if (!length(all)) {
        return(list())
    }
    # Owner and key together: another bot's queued job in the same room
    # is not this bot's to start.
    queued <- vapply(job_list(status = "queued"), function(j) {
        paste(j$owner %||% "local", j$origin$session_key %||% "", sep = "\n")
    }, "")
    Filter(function(s) {
        !is.null(s$.job_current) || length(s$.job_events) ||
        paste(job_worker_owner(s), job_worker_key(s), sep = "\n") %in% queued
    }, all)
}

# TRUE when any room has work in flight or an approval prompt waiting on
# a reaction.
bot_jobs_active <- function(sessions) {
    if (length(bot_job_sessions(sessions))) {
        return(TRUE)
    }
    any(vapply(bot_sessions_list(sessions),
               function(s) length(s$.job_prompts) > 0L, logical(1)))
}

# The ledger owner for this bot's jobs: its Matrix id, read from the
# config because it is needed before any client exists.
bot_job_owner <- function(cfg) {
    cfg$user_id %||% paste0("bot:", cfg$user %||% "unknown")
}

# Send into a job's originating room or thread and record the message as
# the bot's own, the same way every other bot reply is recorded.
bot_job_send <- function(chat, s, job, text) {
    room <- job$origin$room %||% s$room_id
    sent <- tryCatch(bot_reply_send(chat, room, text, markdown = TRUE,
                                    thread = job$origin$thread),
                     error = function(e) NULL)
    if (!is.null(sent)) {
        s$seen_event_ids <- bot_remember_event(s$seen_event_ids, sent)
        bot_transcript_add(s, sent, "assistant", text)
    }
    sent
}

bot_pump_jobs <- function(sessions, chat, cfg) {
    for (s in bot_job_sessions(sessions)) {
        events <- tryCatch(job_pump(s), error = function(e) {
            message("bot_pump_jobs: ", conditionMessage(e))
            list()
        })
        for (ev in events) {
            bot_present_job_event(chat, cfg, s, ev)
        }
    }
    invisible(TRUE)
}

bot_present_job_event <- function(chat, cfg, s, ev) {
    switch(ev$type,
           settled = bot_job_settled(chat, s, ev$job),
           restored = bot_job_send(chat, s, ev$job,
                                   bot_job_restored_text(ev$restore)),
           approval = bot_job_approval(chat, cfg, s, ev),
           # "started" needs no post: the talker already said so.
           NULL)
}

bot_job_settled <- function(chat, s, job) {
    text <- bot_job_result_text(job)
    sent <- bot_job_send(chat, s, job, text)
    # Into the talker's history either way. The post can fail; the
    # talker still has to know the job ended, or it will keep telling the
    # user the work is in progress.
    s$history <- c(s$history %||% list(),
                   list(list(role = "assistant", content = text)))
    invisible(sent)
}

bot_job_result_text <- function(job) {
    head <- sprintf("**Job %s %s**: %s", job$id, job$status,
                    .sanitize_inline(job$task, max_chars = 120L))
    o <- job$outcome
    body <- switch(job$status,
                   done = o$result %||% "",
                   failed = o$error %||% o$reason %||% "",
                   o$reason %||% "")
    note <- if (identical(job$status, "done") && !is.null(o$reason)) {
        paste0("\n\n_", o$reason, "_")
    } else {
        ""
    }
    paste0(head, if (nzchar(body)) paste0("\n\n", body) else "", note)
}

bot_job_restored_text <- function(restore) {
    sprintf(paste("The job worker restarted and restored its workspace from",
                  "the end of job %s (%d objects). Anything an unfinished",
                  "job held in memory is gone; objects holding connections",
                  "or external pointers do not survive a restore."),
            restore$job %||% "?", length(restore$objects))
}

# An approval request from a running job. auto_approve_asks keeps its
# meaning here: the room's own turns never prompt under it, and neither
# do its jobs. Otherwise the approvers are worked out as for a turn's
# approval (bot_approvers()), a prompt is posted with both reactions
# seeded, and the prompt is remembered on the session until a reaction
# answers it.
bot_job_approval <- function(chat, cfg, s, ev) {
    job <- ev$job
    req <- ev$request
    if (isTRUE(cfg$auto_approve_asks)) {
        job_answer(s, job$id, req$id, TRUE, by = "auto_approve_asks")
        return(invisible(NULL))
    }
    room <- job$origin$room %||% s$room_id
    self_id <- tryCatch(chat.api::chat_whoami(chat)$id,
                        error = function(e) cfg$user_id)
    members <- tryCatch(chat.api::chat_members(chat, room),
                        error = function(e) NULL)
    approvers <- bot_approvers(cfg, members, bot_known_bots(cfg, self_id))
    call <- list(tool = req$tool, args = req$args)
    if (!length(approvers)) {
        job_answer(s, job$id, req$id, FALSE, by = "no approver")
        bot_job_send(chat, s, job, bot_no_approver_notice(call))
        return(invisible(NULL))
    }
    text <- paste0(sprintf("Job %s: ", job$id),
                   bot_approval_prompt(call, list(reason = req$reason),
                                       timeout_sec = as.integer(
                s$config$jobs$approval_timeout_sec %||% 600)))
    eid <- bot_job_send(chat, s, job, text)
    if (is.null(eid)) {
        # Nobody can see a prompt that was never posted.
        job_answer(s, job$id, req$id, FALSE, by = "prompt not delivered")
        return(invisible(NULL))
    }
    for (k in c(intToUtf8(0x1F44D), intToUtf8(0x1F44E))) {
        tryCatch(chat.api::chat_react(chat, room, eid, k),
                 error = function(e) NULL)
    }
    prompts <- s$.job_prompts %||% list()
    prompts[[eid]] <- list(job = job$id, req = req$id, room = room,
                           approvers = approvers)
    s$.job_prompts <- prompts
    invisible(eid)
}

# Answer job approval prompts from this poll's reactions. Uses the same
# verdict reader as a turn's approval, so the same rules hold: only an
# approver counts, the bot's own seeds never do, first verdict wins.
bot_handle_job_reactions <- function(reactions, sessions, chat, cfg) {
    if (!length(reactions)) {
        return(invisible(0L))
    }
    answered <- 0L
    for (s in bot_sessions_list(sessions)) {
        prompts <- s$.job_prompts
        for (eid in names(prompts)) {
            p <- prompts[[eid]]
            verdict <- bot_reaction_verdict(reactions, p$room, eid,
                bot_approve_keys(cfg), bot_deny_keys(cfg), p$approvers)
            if (is.null(verdict)) {
                next
            }
            who <- bot_job_reactor(reactions, p, eid, cfg)
            recorded <- tryCatch(job_answer(s, p$job, p$req, verdict, by = who),
                                 error = function(e) FALSE)
            prompts[[eid]] <- NULL
            answered <- answered + 1L
            if (!isTRUE(recorded)) {
                job <- job_read(p$job)
                bot_job_send(chat, s, job, sprintf(
                        "Job %s: that answer came too late; the request had already closed.",
                        p$job))
            }
        }
        s$.job_prompts <- prompts
    }
    invisible(answered)
}

# Who gave the verdict, for the record: the first approver whose
# reaction on this prompt carries a verdict key.
bot_job_reactor <- function(reactions, p, eid, cfg) {
    keys <- c(bot_approve_keys(cfg), bot_deny_keys(cfg))
    for (r in reactions) {
        if (!isTRUE(r$self) && identical(r$target, eid) &&
            isTRUE(r$sender %in% p$approvers) && r$key %in% keys) {
            return(r$sender)
        }
    }
    "unknown"
}

# On startup: classify jobs a previous process left unfinished and tell
# each originating room what was found. Queued jobs never reached a
# worker, so they stay queued and run once their room's session is
# pumped (startup backfill creates it for joined rooms). Dispatched ones
# are indeterminate and are not re-run.
bot_recover_jobs <- function(chat, owner) {
    v <- tryCatch(job_recover(owner = owner), error = function(e) {
        message("bot_run: job recovery failed: ", conditionMessage(e))
        NULL
    })
    if (is.null(v) || !nrow(v)) {
        return(invisible(v))
    }
    for (i in seq_len(nrow(v))) {
        job <- job_read(v$id[[i]])
        room <- job$origin$room
        if (is.null(room) || is.null(chat)) {
            next
        }
        text <- if (identical(v$verdict[[i]], "indeterminate")) {
            sprintf(paste("Job %s was interrupted by a restart after it",
                          "started (%s). It may have partly run, so it",
                          "will not be re-run; ask again if you want it."),
                    job$id, .sanitize_inline(job$task, max_chars = 80L))
        } else {
            sprintf(paste("Job %s had not started before a restart (%s).",
                          "It is still queued and will run."),
                    job$id, .sanitize_inline(job$task, max_chars = 80L))
        }
        tryCatch(bot_reply_send(chat, room, text, thread = job$origin$thread),
                 error = function(e) NULL)
    }
    message(sprintf("bot_run: recovered %d unfinished job(s)", nrow(v)))
    invisible(v)
}
