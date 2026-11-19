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
# unknown, whatever the rules flag, and a call the person already
# allowed always, which their own answer settles.
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
        job_monitor_close(session)
    }
    invisible(review_mode(session))
}

# Close the reviewer's process, when the session that owns it ends.
review_close <- function(session) {
    if (is.environment(session) && is.function(session$.review_gate)) {
        job_monitor_close(session)
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
            q <- review_question(session, call, decision, v$notes)
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

# Ask the session's monitor process, and wait for its ruling.
review_ask <- function(session, q) {
    job_monitor_ask_wait(session, q)
}

# The request the reviewer is sent (job_monitor_question() renders it):
# the person's first message as the task; the conversation since, the
# person's lines and the worker's, so that a "yes" comes with the
# question it answers; and what the worker said with this call. Only the
# person's words instruct. A task list the worker keeps is not shown: it
# is the worker's own, and grants nothing.
review_question <- function(session, call, decision, notes) {
    conv <- review_conversation(session$history %||% list(),
                                session$current_prompt)
    session$.review_seq <- (session$.review_seq %||% 0L) + 1L
    list(scope = paste0("turn:", session$.audit_turn$id %||% conv$people),
         request_id = sprintf("r%06d", session$.review_seq),
         goal = conv$task, since = conv$since,
         narration = call$model_context$assistant_text,
         tool = call$tool, args = job_approval_args(call$args),
         reason = decision$reason, notes = notes,
         earlier = review_earlier(session))
}

# The conversation as the reviewer reads it. `task` is the person's
# first message. `since` is what followed, oldest first, a line each,
# "Person: ..." or "Worker: ...": every one of the person's last
# `max_person` messages, and anything among the last `max_recent`.
# Text only: no tool calls, tool results or pictures. The turn's own
# prompt is last, though the history llm.api builds holds it only once
# the model has answered. `people` counts the person's messages.
review_conversation <- function(history, prompt = NULL, max_person = 20L,
                                max_recent = 12L, max_chars = 600L) {
    who <- character()
    text <- character()
    for (m in history) {
        speaker <- if (.context_edit_human(m)) {
            "Person"
        } else if (identical(m$role, "assistant")) {
            "Worker"
        } else {
            ""
        }
        if (nzchar(speaker)) {
            said <- review_message_text(m)
        } else {
            said <- ""
        }
        if (nzchar(trimws(said))) {
            who <- c(who, speaker)
            text <- c(text, said)
        }
    }
    if (is.character(prompt) && length(prompt) == 1L && !is.na(prompt) &&
        nzchar(trimws(prompt)) &&
        !identical(utils::tail(text[who == "Person"], 1L), prompt)) {
        who <- c(who, "Person")
        text <- c(text, prompt)
    }
    text <- vapply(text, .sanitize_inline, character(1),
                   max_chars = max_chars, USE.NAMES = FALSE)
    first <- match("Person", who)
    if (is.na(first)) {
        return(list(task = "(no message yet)", since = character(),
                    people = 0L))
    }
    rest <- seq_along(who)[seq_along(who) > first]
    person <- rest[who[rest] == "Person"]
    keep <- rest[rest %in% utils::tail(person, max_person) |
        rest > length(who) - max_recent]
    list(task = text[[first]],
         since = paste0(who[keep], ": ", text[keep]),
         people = sum(who == "Person"))
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
