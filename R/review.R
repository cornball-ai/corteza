# The reviewer on every surface: `approval_mode` "review".
#
# A Matrix room with `auto_approve_asks` has had this since the
# supervisor (R/supervisor.R). "review" gives the terminal and chat()
# the same 3 steps for each call that would prompt:
#
#   1. The rules in code (supervisor_route()). What they flag goes to
#      the person; no model is asked.
#   2. The reviewer: the room's monitor, a model in a process of its own
#      with read-only tools confined to the checkout (R/job-monitor.R).
#      It rules on read, write and exec calls of a known class. It
#      approves; refuses, and the model is told why and the turn goes
#      on; or passes the call on. A reviewer that fails or does not
#      answer in time passes it on.
#   3. The person at the prompt.
#
# The setting says what an "ask" becomes, and nothing more: a call
# policy() allows runs, and one it refuses is refused, as under "ask".
# Never the reviewer's: the credential rule's ask, a tool of class
# unknown, whatever the rules flag, a call the person already allowed
# always, which their own answer settles, and any call while the
# conversation cannot be read whole (review_conversation()).
#
# A person turns it on for a session with `/permissions review`, or for
# every session with "approval_mode": "review" in the user's config. A
# project's config cannot set it (load_config()). Rooms keep
# `auto_approve_asks`, and an /auto run its hall monitor (R/monitor.R),
# since nobody is at that prompt.
#
# A talker's doer is covered too. Its worker reads the mode from its
# job's authority record before each call (.authority_apply()) and,
# under review, holds the same rules (.job_worker_child_review_gate());
# what they leave open comes to the job pump with `route` "monitor",
# and the reviewer rules on it with the conversation that asked for the
# job and the task the talker wrote (.repl_job_review()).

# CLM can insert this fixed header before its first working note.
.review_inputs_intact <- function(original, inputs) {
    if (length(inputs) == length(original) + 1L &&
        identical(inputs[[1L]]$content, "Context workspace: task facts and next actions.")) {
        inputs <- inputs[-1L]
    }
    identical(inputs, original)
}

# Is the session's approval mode "review"?
review_mode <- function(session) {
    identical(session$config$approval_mode, "review")
}

# Give the session the reviewer's gate, or take it away, as its approval
# mode says. turn() consults the gate for every call (session$auto_gate).
# A terminal or chat() session calls this when it starts and after
# /permissions; taking the gate away closes the reviewer's process.
review_apply <- function(session) {
    gate <- session$.review_gate
    installed <- is.function(gate) && identical(session$auto_gate, gate)
    if (review_mode(session)) {
        # Another gate holds the session while it runs: an /auto run's
        # hall monitor, whose loop calls this as it starts. Its rule
        # stands until it gives the session back.
        if (!installed && is.null(session$auto_gate)) {
            # The reviewer's process, and the journal of what it was
            # asked, belong to this conversation: two terminals open in
            # one directory never share them.
            if (is.null(session$job_checkpoint_key)) {
                session$job_checkpoint_key <- paste0("review:",
                    session$sessionId %||% job_local_owner())
            }
            session$.review_gate <- review_gate(session)
            session$auto_gate <- session$.review_gate
        }
    } else if (installed) {
        session$auto_gate <- NULL
        session$.review_gate <- NULL
        review_cancel(session)
    }
    invisible(review_mode(session))
}

# Close the reviewer's process, when the session that owns it ends.
review_close <- function(session) {
    if (is.environment(session) && is.function(session$.review_gate)) {
        review_cancel(session)
    }
    invisible(TRUE)
}

# The gate of a session in review mode. `reviewer(session, q)` returns
# list(verdict, reason), verdict one of "approve", "refuse" and
# "escalate"; the default asks the session's monitor process and waits.
# Returns a function(call, decision) -> list(action, reason), as
# supervisor_gate() does.
review_gate <- function(session, reviewer = review_ask) {
    force(reviewer)
    # One git lookup per directory, not per call.
    roots <- list()
    root_of <- function(dir) {
        if (is.null(roots[[dir]])) {
            roots[[dir]] <<- job_checkout(dir)
        }
        roots[[dir]]
    }
    function(call, decision) {
        approval <- decision$approval %||% "ask"
        if (identical(approval, "deny")) {
            return(list(action = "deny", reason = decision$reason %||% ""))
        }
        if (!identical(approval, "ask")) {
            return(list(action = "proceed", reason = ""))
        }
        v <- tryCatch({
            dir <- normalizePath(session$cwd %||% getwd(), mustWork = FALSE)
            supervisor_route(call, decision, dir, mode = "monitor",
                             write_roots = supervisor_config()$write_roots,
                             root = root_of(dir))
        },
                      # Rules that failed ruled on nothing.
                      error = function(e) {
            list(route = "human", notes = character(),
                 flags = paste0("could not be checked (", conditionMessage(e), ")"))
        })
        reviewable <- identical(v$route, "monitor") &&
        call_class(call) %in% c("read", "write", "exec")
        if (reviewable) {
            answered <- attr(session$approval_cb, "answered")
            if (is.function(answered) &&
                isTRUE(tryCatch(answered(call, decision),
                                error = function(e) FALSE))) {
                return(list(action = "proceed", reason = ""))
            }
            # A reviewer that cannot read the whole conversation does not
            # judge the call: the person does.
            conv <- review_conversation(session$sessionId, .review_record_budget(session, session$sessionId))
            if (is.null(conv$why)) {
                q <- review_question(session, call, decision, v$notes,
                                     conv$lines)
                r <- tryCatch(reviewer(session, q), error = function(e) {
                    list(verdict = "escalate",
                         reason = paste("the reviewer failed:", conditionMessage(e)))
                })
                verdict <- r$verdict %||% "escalate"
                why <- .sanitize_inline(r$reason %||% "", max_chars = 300L)
                if (!nzchar(why)) {
                    why <- "no reason given"
                }
                if (verdict %in% c("approve", "refuse")) {
                    review_record(session, call, verdict, why)
                    if (identical(verdict, "approve")) {
                        return(list(action = "proceed", reason = why))
                    }
                    return(list(action = "refuse", reason = why))
                }
                decision$gate_reason <- paste("The reviewer passed this to you:",
                    why)
            } else {
                decision$gate_reason <- paste("The reviewer was not asked:",
                    conv$why)
            }
        } else if (length(v$flags)) {
            decision$gate_reason <- paste("The rules flagged it:",
                paste(v$flags, collapse = "; "))
        }
        # A person's no ends the turn by a condition that is not an
        # error (user_deny_condition()), and it goes on up from here.
        approved <- isTRUE(tryCatch(session$approval_cb(call, decision),
                                    error = function(e) FALSE))
        if (reviewable) {
            review_record(session, call,
                if (approved) "passed, approved" else "passed, declined",
                          decision$gate_reason)
        }
        if (approved) {
            list(action = "proceed", reason = "")
        } else {
            list(action = "declined", reason = decision$reason %||% "")
        }
    }
}

# Ask the session's monitor process, and wait for its ruling. A wait
# that ends early, by an interrupt or an error, cancels the request
# (review_cancel()), so the next call finds the reviewer free.
review_ask <- function(session, q) {
    settled <- FALSE
    on.exit(if (!settled) review_cancel(session), add = TRUE)
    v <- job_monitor_ask_wait(session, q)
    settled <- TRUE
    v
}

# Stop what the reviewer was asked, and forget it. Its process is closed,
# the one way to stop a model mid-answer, as job_monitor_poll() closes one
# out of time; the next call starts another. A room's monitor keeps its
# own lifecycle: the room's loop answers or escalates what it holds
# (bot_monitor_answer()).
review_cancel <- function(session) {
    job_monitor_close(session)
    session$.job_monitor_pending <- NULL
    invisible(TRUE)
}

# The request the reviewer is sent (job_monitor_question() renders it):
# the conversation (review_conversation()), what the worker said with
# this call, and the call. Each ruling gets a request id. The monitor
# verifies its retained record before accepting additional lines.
review_question <- function(session, call, decision, notes, conversation) {
    session$.review_seq <- (session$.review_seq %||% 0L) + 1L
    id <- sprintf("r%06d", session$.review_seq)
    list(scope = paste0("review:", id), record = session$sessionId, request_id = id,
         conversation = conversation,
         narration = call$model_context$assistant_text, tool = call$tool,
         args = job_approval_args(call$args), reason = decision$reason,
         notes = notes, earlier = review_earlier(session))
}

# The conversation as the reviewer reads it, from the session's
# transcript (R/session.R), a line each, oldest first: every prompt of
# the person's, whole, as "Person: ...", and before each, whole, what
# the agent said since the prompt before it, as "Worker: ...": its reply
# and a job's result that came after. So a "yes" comes with what it
# agreed to.
#
# The transcript is appended to and never rewritten. A compaction or a
# context edit rewrites the history the model sees, and leaves a summary
# there as a user message; read from there, the model's words could pass
# for the person's, and the person's own could be gone. Here a summary
# is not shown. A prompt is shown as the person typed it, and the
# transcript says how each was sent (`staged`, `typed`): one sent with
# staged R or shell output shows what was typed and says output came
# with it. An /auto run shows once what the person typed to start it, as
# "/auto <goal>" or a bare "/auto", and then its replies; not the
# driver's prompts, nor a goal corteza wrote for a bare /auto. A task
# list is the model's own, and is not shown.
#
# Returns list(lines, why). `why` says why the reviewer cannot judge the
# call, and is NULL when it can. It cannot when the conversation cannot
# be read whole: no transcript, a line or message it cannot read, a
# prompt that does not say how it was sent (every one written before
# 0.8.1), a prompt with no reply before the next (a write that failed,
# or a process that stopped mid-turn), or more than `budget` characters.
# Nothing is left out to make it fit.
review_conversation <- function(session_id, budget = 100000L) {
    unread <- function(why) list(lines = character(), why = why)
    record <- NULL
    if (is.character(session_id) && length(session_id) == 1L &&
        !is.na(session_id) && nzchar(session_id)) {
        record <- tryCatch(transcript_load_strict(session_id),
                           error = function(e) conditionMessage(e))
    }
    if (is.null(record)) {
        return(unread("there is no transcript of the conversation to judge it by"))
    }
    if (is.character(record)) {
        return(unread(paste("the transcript could not be read:", record)))
    }
    if (record$bad > 0L) {
        return(unread(sprintf("%d %s of the transcript could not be read",
                              record$bad,
                    if (record$bad == 1L) "line" else "lines")))
    }
    lines <- character()
    # What the agent said since the last prompt, and whether it has
    # answered that prompt: a job's result (`job`) and a compaction's
    # marker are not the reply.
    since <- character()
    open <- FALSE
    runs <- character()
    for (e in record$entries) {
        said <- review_message_text(e)
        if (is.na(said)) {
            return(unread("a message in the transcript could not be read"))
        }
        if (identical(e$role, "assistant")) {
            if (!identical(e$model, "compaction")) {
                since <- c(since, said)
                if (is.null(e$job)) {
                    open <- FALSE
                }
            }
            next
        }
        if (!identical(e$role, "user")) {
            return(unread("the transcript holds a message from neither you nor the agent"))
        }
        # Every prompt is answered before the next, an error or a stop
        # included, so a prompt with no answer before the next one is a
        # reply the transcript lost: the person may have answered what
        # the reviewer would not see. Until the conversation ends, the
        # person decides. The last prompt is the one being answered.
        if (open) {
            return(unread(paste("the agent's reply to an earlier message is",
                                "missing from the transcript")))
        }
        open <- TRUE
        reply <- since
        since <- character()
        typed <- e$typed
        typed_ok <- isTRUE(e$staged) && is.character(typed) &&
        length(typed) == 1L && !is.na(typed)
        if (!is.null(e$auto_run_id)) {
            # The driver's later prompts restate the goal.
            if (as.character(e$auto_run_id)[1L] %in% runs) {
                next
            }
            runs <- c(runs, as.character(e$auto_run_id)[1L])
            if (!typed_ok) {
                return(unread("an /auto run's prompt does not say what you asked it"))
            }
            said <- paste("/auto", typed)
        } else if (typed_ok) {
            said <- paste(typed, "[sent with R or shell output, not shown]")
        } else if (!identical(e$staged, FALSE)) {
            return(unread(paste("the transcript has messages from before corteza",
                                "0.8.1, which do not mark what you typed")))
        }
        said <- .sanitize_inline(said, max_chars = budget)
        if (!nzchar(said)) {
            next
        }
        for (r in reply[nzchar(trimws(reply))]) {
            lines <- c(lines, paste("Worker:",
                                    .sanitize_inline(r, max_chars = budget)))
        }
        lines <- c(lines, paste("Person:", said))
    }
    if (!length(lines)) {
        return(unread("the transcript holds no message of yours"))
    }
    if (sum(nchar(lines)) > budget) {
        return(unread(.review_record_limit_reason(budget)))
    }
    list(lines = lines, why = NULL)
}

# The text of a transcript message: its content when that is a string,
# otherwise its text blocks, joined. NA when it holds anything else.
review_message_text <- function(m) {
    content <- m$content
    if (is.character(content) && length(content) == 1L && !is.na(content)) {
        return(content)
    }
    if (!is.list(content) || !length(content)) {
        return(NA_character_)
    }
    parts <- vapply(content, function(b) {
        if (is.list(b) && (is.null(b$type) || identical(b$type, "text")) &&
                    is.character(b$text) && length(b$text) == 1L && !is.na(b$text)) {
            b$text
        } else {
            NA_character_
        }
    }, character(1))
    if (anyNA(parts)) {
        return(NA_character_)
    }
    paste(parts, collapse = "\n")
}

# Keep what came of a reviewed call, for the reviewer's next request and
# for the person: session$on_review(record), where a surface sets it.
review_record <- function(session, call, outcome, reason) {
    rec <- list(tool = call$tool %||% "", brief = supervisor_brief(call),
                outcome = outcome, reason = reason %||% "")
    session$.review_log <- utils::tail(c(session$.review_log, list(rec)), 15L)
    if (is.function(session$on_review)) {
        tryCatch(session$on_review(rec), error = function(e) NULL)
    }
    invisible(rec)
}

# The session's earlier reviewed calls, one line each, newest last.
review_earlier <- function(session) {
    vapply(session$.review_log %||% list(), function(r) {
        sprintf("%s: %s -> %s", r$tool, r$brief,
                switch(r$outcome, approve = "approved by you",
                       refuse = "refused by you",
                       "passed, approved" = "passed on, and the person approved",
                       "passed, declined" = "passed on, and the person declined",
                       r$outcome))
    }, character(1))
}

# A line for the person about a call the reviewer settled. NULL for one
# it passed on: the prompt says that.
review_line <- function(rec) {
    verb <- switch(rec$outcome, approve = "approved", refuse = "refused", NULL)
    if (is.null(verb)) {
        return(NULL)
    }
    sprintf("Reviewer %s %s: %s", verb,
            .sanitize_inline(paste(rec$tool, rec$brief), max_chars = 160L),
            .sanitize_inline(rec$reason, max_chars = 200L))
}
