# A prompt of `corteza acp` (R/acp.R): one turn() in the session's
# directory, its reply streamed to the client as it comes, each tool
# call announced and then finished, and its end answered with ACP's stop
# reason.
#
# The turn's state lives on `st$live` while it runs: its session, whether
# the client cancelled it, whether any reply text went out, and the tool
# call in progress. Tool calls run one at a time, so the call a
# permission request announces is the one the next start or refusal
# belongs to.

.acp_prompt <- function(st, params) {
    sid <- params$sessionId
    session <- .acp_session_get(st, sid)
    text <- .acp_prompt_text(params$prompt)
    live <- new.env(parent = emptyenv())
    live$sid <- sid
    live$session <- session
    live$cancelled <- FALSE
    live$streamed <- FALSE
    live$call <- NULL
    st$live <- live
    on.exit(st$live <- NULL, add = TRUE)
    here <- getwd()
    on.exit(setwd(here), add = TRUE)
    setwd(session$cwd)

    # The client speaks for the person: its prompt is written whole, as
    # what was typed, which is what the reviewer reads (R/review.R).
    disk <- session$disk_session
    tryCatch({
        transcript_append(disk, "user", text, typed = text)
        session$disk_session <- session_title_once(disk, text)
    }, error = function(e) .acp_log("could not write the transcript: ",
                                    conditionMessage(e)))

    before <- length(session$history %||% list())
    result <- tryCatch(turn(text, session),
                       interrupt = function(e) {
        # An interrupt from outside (a signal to the process) ends the
        # turn as Ctrl+C does at the terminal: the history is repaired
        # and marked. A client's cancel ends it inside turn().
        live$cancelled <- TRUE
        apply_exit_marker(session, text, before, user_interrupt_marker())
        NULL
    },
                       error = function(e) e)
    # A plan approved during the turn stays, whatever its end.
    if (isTRUE(session$tasks_dirty)) {
        session$disk_session$tasks <- session$tasks
        session$tasks_dirty <- FALSE
    }
    # A cancel, or an error a cancel caused, is a cancelled turn.
    stopped <- isTRUE(live$cancelled) ||
    identical(result$termination$status, "cancelled")
    .acp_finish_call(st, live, if (stopped) "cancelled" else "it did not finish")
    if (inherits(result, "error") && !stopped) {
        .acp_transcript_end(session, paste("Error:", conditionMessage(result)))
        .acp_refuse(conditionMessage(result), -32603L)
    }
    reply <- if (is.list(result)) result$reply %||% "" else ""
    if (!live$streamed && nzchar(reply)) {
        .acp_update(st, sid, .acp_text_chunk(.redact_text(reply,
                                                          session$secret_values)))
    }
    if (stopped) {
        .acp_transcript_end(session, paste(c(if (nzchar(reply)) reply,
                                             "[The turn was cancelled.]"),
                                           collapse = "\n"))
        return(list(stopReason = "cancelled"))
    }
    .acp_transcript_end(session, reply, result)
    session_accumulate_spend(session, result$usage)
    .acp_compact(session)
    list(stopReason = .acp_stop_reason(result$termination))
}

# The reply into the transcript, and the session record saved.
.acp_transcript_end <- function(session, reply, result = NULL) {
    tryCatch({
        transcript_append(session$disk_session, "assistant", reply,
                          usage = result$usage, audit = result$audit,
                          provider = result$route$provider %||% session$provider,
                          model = result$route$model %||% session$model_map$cloud)
        session_save(session$disk_session)
    }, error = function(e) .acp_log("could not write the transcript: ",
                                    conditionMessage(e)))
    invisible(NULL)
}

# After the turn, as the terminal does it: compact once the context is
# past the session's trigger (R/compact-trigger.R).
.acp_compact <- function(session) {
    parts <- tryCatch(session_request_parts(session),
                      error = function(e) NULL)
    if (is.null(parts)) {
        return(invisible(FALSE))
    }
    tryCatch(maybe_compact_turn_session(session, session$config,
                                        kind = session$kind %||% NULL,
                                        tools = parts$tools,
                                        system = parts$system,
                                        reason = "post_turn"),
             error = function(e) {
        .acp_log("compaction failed: ", conditionMessage(e))
        FALSE
    })
}

# The stop function of a session's turns (R/turn-stop.R): read what has
# come and handle it, then say whether the client cancelled this turn or
# went away.
.acp_stop <- function(st, sid) {
    live <- st$live
    if (is.null(live) || !identical(live$sid, sid)) {
        return(FALSE)
    }
    .acp_service(st, 0L)
    isTRUE(live$cancelled) || st$ch$closed
}

.acp_on_delta <- function(st, sid, text) {
    live <- st$live
    if (!is.null(live) && identical(live$sid, sid) &&
        is.character(text) && length(text) == 1L && nzchar(text)) {
        live$streamed <- TRUE
        .acp_update(st, sid, .acp_text_chunk(text))
    }
    invisible(TRUE)
}

# The id of the next call of a session: unique within it.
.acp_call_id <- function(session) {
    session$.acp_calls <- (session$.acp_calls %||% 0L) + 1L
    sprintf("call-%d", session$.acp_calls)
}

# Announce a call, unless a permission request already has: the id it
# runs under.
.acp_announce <- function(st, live, call, status) {
    if (is.null(live$call)) {
        id <- .acp_call_id(live$session)
        live$call <- id
        .acp_update(st, live$sid, .acp_tool_call(call, id, status,
                                                 live$session$cwd))
        return(id)
    }
    .acp_update(st, live$sid, list(sessionUpdate = "tool_call_update",
                                   toolCallId = live$call, status = status))
    live$call
}

# A tool event of the running turn (R/turn.R's observers), as updates:
# a start announces the call or moves it to in_progress; a run, a
# refusal or a decline finishes it. The event's `display` is the copy
# with known secrets hidden; nothing else is sent.
.acp_on_tool <- function(st, sid, event) {
    live <- st$live
    if (is.null(live) || !identical(live$sid, sid)) {
        return(invisible(NULL))
    }
    shown <- event$display %||% event
    call <- shown$call
    outcome <- event$outcome %||% ""
    if (is.null(call) || !outcome %in% c("start", "ran", "deny", "declined")) {
        return(invisible(NULL))
    }
    if (identical(outcome, "start")) {
        .acp_announce(st, live, call, "in_progress")
        return(invisible(NULL))
    }
    if (is.null(live$call)) {
        .acp_announce(st, live, call, "in_progress")
    }
    .acp_update(st, sid, .acp_tool_done(live$call,
                                        identical(outcome, "ran") && isTRUE(event$success),
                                        shown$result, shown$diff))
    live$call <- NULL
    invisible(NULL)
}

# A call announced and never finished (the turn stopped inside it) gets
# its last update before the prompt is answered.
.acp_finish_call <- function(st, live, why) {
    if (!is.null(live$call)) {
        .acp_update(st, live$sid, .acp_tool_done(live$call, FALSE, why))
        live$call <- NULL
    }
    invisible(NULL)
}
