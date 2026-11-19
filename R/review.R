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
        classify_op(call$tool %||% "") %in% c("read", "write", "exec")
        if (reviewable) {
            answered <- attr(session$approval_cb, "answered")
            if (is.function(answered) &&
                isTRUE(tryCatch(answered(call, decision),
                                error = function(e) FALSE))) {
                return(list(action = "proceed", reason = ""))
            }
            # A reviewer that cannot read the whole conversation does not
            # judge the call: the person does.
            conv <- review_conversation(session$sessionId)
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
# this call, and the call. Each request is a scope of its own, so the
# reviewer starts each ruling from the request alone: the conversation
# comes whole with every call, and kept, it would pile up a copy per
# call. `earlier` says how the session's earlier calls were settled.
review_question <- function(session, call, decision, notes, conversation) {
    session$.review_seq <- (session$.review_seq %||% 0L) + 1L
    id <- sprintf("r%06d", session$.review_seq)
    list(scope = paste0("review:", id), request_id = id,
         conversation = conversation,
         narration = call$model_context$assistant_text, tool = call$tool,
         args = job_approval_args(call$args), reason = decision$reason,
         notes = notes, earlier = review_earlier(session))
}

# The conversation as the reviewer reads it, from the session's
# transcript (R/session.R), a line each, oldest first: every prompt of
# the person's, whole, as "Person: ...", and before each the end of the
# reply it follows, as "Worker: ...", so that a "yes" comes with its
# question.
#
# The transcript is appended to and never rewritten. A compaction or a
# context edit rewrites the history the model sees, and leaves a summary
# there as a user message; read from there, the model's words could pass
# for the person's, and the person's own could be gone. Here a summary
# is not shown, nor an /auto run's prompts, which its driver wrote. A
# prompt sent with staged R or shell output shows what was typed, and
# says output came with it. A task list is the model's own, and is not
# shown.
#
# Returns list(lines, why). `why` says why the reviewer cannot judge the
# call, and is NULL when it can. Without a transcript it cannot, nor when
# the conversation runs past `budget` characters: nothing is left out to
# make it fit.
review_conversation <- function(session_id, tail_chars = 800L,
                                budget = 100000L) {
    path <- NULL
    if (is.character(session_id) && length(session_id) == 1L &&
        !is.na(session_id) && nzchar(session_id)) {
        path <- session_transcript_path(session_id)
    }
    if (is.null(path) || !file.exists(path)) {
        return(list(lines = character(),
                    why = "there is no transcript of the conversation to judge it by"))
    }
    entries <- tryCatch(transcript_load(session_id), error = function(e) NULL)
    if (is.null(entries)) {
        return(list(lines = character(),
                    why = "the transcript of the conversation could not be read"))
    }
    lines <- character()
    reply <- ""
    for (e in entries) {
        if (!is.null(e$auto_run_id)) {
            next
        }
        if (identical(e$role, "assistant")) {
            if (!identical(e$model, "compaction")) {
                reply <- review_message_text(e)
            }
            next
        }
        if (!identical(e$role, "user")) {
            next
        }
        staged <- is.character(e$typed) && length(e$typed) == 1L
        if (staged) {
            said <- e$typed
        } else {
            said <- review_message_text(e)
        }
        said <- .sanitize_inline(said, max_chars = budget)
        if (!nzchar(said)) {
            next
        }
        if (staged) {
            said <- paste(said, "[sent with R or shell output, not shown]")
        }
        if (nzchar(trimws(reply))) {
            lines <- c(lines, paste("Worker:", review_tail(reply, tail_chars)))
        }
        reply <- ""
        lines <- c(lines, paste("Person:", said))
    }
    if (!length(lines)) {
        return(list(lines = character(),
                    why = "the transcript holds no message of the person's"))
    }
    if (sum(nchar(lines)) > budget) {
        return(list(lines = character(),
                    why = sprintf("the conversation runs past the %s characters it reads",
                                  format(budget, big.mark = ","))))
    }
    list(lines = lines, why = NULL)
}

# The last `n` characters of a reply, on one line, marked where cut.
review_tail <- function(text, n) {
    text <- .sanitize_inline(text, max_chars = .Machine$integer.max)
    if (nchar(text) <= n) {
        return(text)
    }
    paste0("...", substr(text, nchar(text) - n + 1L, nchar(text)))
}

# The text of a message: its content when that is a string, otherwise
# its text blocks.
review_message_text <- function(m) {
    content <- m$content
    if (is.character(content)) {
        return(paste(content, collapse = "\n"))
    }
    if (!is.list(content)) {
        return("")
    }
    parts <- vapply(content, function(b) {
        if (is.list(b) && is.character(b$text) &&
                    (is.null(b$type) || b$type %in% c("text", "input_text"))) {
            paste(b$text, collapse = "\n")
        } else {
            ""
        }
    }, character(1))
    paste(parts[nzchar(parts)], collapse = "\n")
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
