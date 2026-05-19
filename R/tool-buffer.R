# Session-scoped buffer of recent tool outputs, backing the `/last`
# and `/outputs` slash commands.
#
# State lives in `.tool_buffer_state`, a package-level env keyed by
# `session$sessionId`. Two surfaces use it:
#
#   - The CLI's tool_handler in inst/bin/corteza pushes the tool
#     result text after every execution.
#   - The chat() REPL registers an observer that does the same.
#
# Subagents have their own sessionId, so a parent's buffer can't leak
# into a child's and vice versa. The buffer is in-memory only —
# nothing is persisted to disk.

.tool_buffer_state <- new.env(parent = emptyenv())

#' Internal: get-or-init the per-session record. Returns a list with
#' `outputs` (most-recent-first) and `max` (cap).
#' @noRd
.tool_buffer_record <- function(sid, max_size = 20L) {
    rec <- .tool_buffer_state[[sid]]
    if (is.null(rec)) {
        rec <- list(outputs = list(), max = as.integer(max_size))
    }
    rec
}

#' Push a tool result into the buffer for `session`.
#'
#' Side-effect only; returns invisibly. Drops the oldest entry once
#' the per-session cap (default 20) is exceeded so a long-running
#' session can't grow the buffer unbounded.
#'
#' @param session Persistent session list (must have `sessionId`).
#' @param name Tool name.
#' @param args Args list as passed to the tool.
#' @param result The tool's result text (already flattened).
#' @noRd
tool_buffer_add <- function(session, name, args, result) {
    sid <- session$sessionId
    if (is.null(sid) || !nzchar(sid)) {
        return(invisible(NULL))
    }
    rec <- .tool_buffer_record(sid)
    entry <- list(name = name, args = args, result = result, time = Sys.time())
    rec$outputs <- c(list(entry), rec$outputs)
    if (length(rec$outputs) > rec$max) {
        rec$outputs <- rec$outputs[seq_len(rec$max)]
    }
    .tool_buffer_state[[sid]] <- rec
    invisible(NULL)
}

#' Fetch the Nth most-recent tool output (1 = most recent).
#'
#' Returns NULL when the index is past the end of the buffer.
#' @noRd
tool_buffer_get <- function(session, n = 1L) {
    sid <- session$sessionId
    if (is.null(sid) || !nzchar(sid)) {
        return(NULL)
    }
    rec <- .tool_buffer_state[[sid]]
    if (is.null(rec) || length(rec$outputs) == 0L) {
        return(NULL)
    }
    n <- as.integer(n)
    if (is.na(n) || n < 1L || n > length(rec$outputs)) {
        return(NULL)
    }
    rec$outputs[[n]]
}

#' Full buffer for `session`, newest first.
#' @noRd
tool_buffer_list <- function(session) {
    sid <- session$sessionId
    if (is.null(sid) || !nzchar(sid)) {
        return(list())
    }
    rec <- .tool_buffer_state[[sid]]
    if (is.null(rec)) {
        return(list())
    }
    rec$outputs
}

#' Forget the buffer for a session. Called from `/clear` and similar
#' "start fresh" paths so a cleared session doesn't surface tool
#' outputs from the previous run.
#' @noRd
tool_buffer_reset <- function(session) {
    sid <- session$sessionId
    if (is.null(sid) || !nzchar(sid)) {
        return(invisible(NULL))
    }
    if (exists(sid, envir = .tool_buffer_state, inherits = FALSE)) {
        rm(list = sid, envir = .tool_buffer_state)
    }
    invisible(NULL)
}

#' Build an `add_observer()` callback that pushes successful tool
#' results into `session`'s buffer. Used by chat() so /last and
#' /outputs work the same way they do in the CLI.
#' @noRd
tool_buffer_observer <- function(session) {
    function(event) {
        if (!identical(event$outcome, "ran")) {
            return(invisible())
        }
        if (!isTRUE(event$success)) {
            return(invisible())
        }
        tool_buffer_add(session, name = event$call$tool %||% "",
                        args = event$call$args %||% list(),
                        result = event$result %||% "")
    }
}

