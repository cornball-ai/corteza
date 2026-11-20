# Tool calls left without a result.
#
# A provider refuses a history that holds a tool call with no result:
# Anthropic answers 400, "tool_use ids were found without tool_result
# blocks immediately after", and every later request made from that
# history fails the same way. A turn that ends on an error while a call
# is running leaves one (llm.api mirrors the call into session$history
# before the tool runs), and so does a process stopped in the middle of
# a call whose history comes back from the journal.
#
# turn() closes them twice: when an error leaves .turn_impl(), and at
# the start of every turn, before anything reads the history. Each open
# call gets a placeholder result where the provider looks for it, right
# after the message that made it.

.open_call_placeholder <- "[No result: the turn ended before this call finished]"

# The marker after calls closed because the turn ended on an error.
.turn_error_marker <- function(e) {
    why <- .sanitize_inline(conditionMessage(e))
    if (!nzchar(why)) {
        why <- class(e)[[1L]]
    }
    sprintf("[Stopped by an error before completing: %s]", why)
}

# The marker after calls an earlier turn left open.
.open_calls_marker <- function() {
    "[An earlier turn ended before its tool calls finished; they have no results.]"
}

# The calls `entry` makes: their ids and names, and the shape of the
# results that answer them. NULL for an entry that makes none.
.repair_calls <- function(entry) {
    if (!is.list(entry)) {
        return(NULL)
    }
    text <- function(x) {
        if (is.null(x) || !length(x)) {
            return("")
        }
        as.character(x[[1L]])
    }
    made <- function(shape, items, id, name) {
        list(shape = shape,
             ids = vapply(items, function(x) text(id(x)), character(1)),
             names = vapply(items, function(x) text(name(x)), character(1)))
    }
    if (identical(entry$type, ".openai_codex_output")) {
        items <- Filter(function(x) {
            is.list(x) && identical(x$type, "function_call")
        }, entry$output %||% list())
        return(made("responses", items, function(x) x$call_id %||% x$id,
                    function(x) x$name))
    }
    if (!identical(entry$role, "assistant")) {
        return(NULL)
    }
    if (is.list(entry$content)) {
        blocks <- Filter(function(b) {
            is.list(b) && identical(b$type, "tool_use")
        }, entry$content)
        if (length(blocks)) {
            return(made("anthropic", blocks, function(b) b$id,
                        function(b) b$name))
        }
    }
    if (length(entry$tool_calls)) {
        calls <- Filter(is.list, entry$tool_calls)
        return(made("openai", calls, function(x) x$id,
                    function(x) x$`function`$name))
    }
    NULL
}

# The id an entry answers, for the shapes whose results are entries of
# their own; NULL for any other entry.
.repair_answers <- function(entry, shape) {
    if (!is.list(entry)) {
        return(NULL)
    }
    if (identical(shape, "openai") && identical(entry$role, "tool")) {
        return(as.character(entry$tool_call_id %||% ""))
    }
    if (identical(shape, "responses") &&
        identical(entry$type, "function_call_output")) {
        return(as.character(entry$call_id %||% ""))
    }
    NULL
}

.repair_result <- function(shape, id, name, placeholder) {
    if (identical(shape, "openai")) {
        return(list(role = "tool", tool_call_id = id, name = name,
                    content = placeholder))
    }
    list(type = "function_call_output", call_id = id, output = placeholder)
}

# Give a user message the results it lacks. Anthropic reads tool_result
# blocks only at the front of the message that follows the call, so the
# new ones go after any already there and before the rest.
.repair_user_results <- function(message, ids, placeholder) {
    content <- message$content
    if (is.list(content)) {
        blocks <- content
    } else {
        text <- paste(as.character(content %||% ""), collapse = "\n")
        blocks <- list()
        if (nzchar(text)) {
            blocks <- list(list(type = "text", text = text))
        }
    }
    first <- vapply(blocks, function(b) {
        is.list(b) && identical(b$type, "tool_result")
    }, logical(1))
    added <- lapply(ids, function(id) {
        list(type = "tool_result", tool_use_id = id, content = placeholder)
    })
    message$content <- c(blocks[first], added, blocks[!first])
    message
}

# The ids a user message answers with tool_result blocks.
.repair_user_answers <- function(message) {
    content <- list()
    if (is.list(message$content)) {
        content <- message$content
    }
    results <- Filter(function(b) {
        is.list(b) && identical(b$type, "tool_result")
    }, content)
    vapply(results, function(b) as.character(b$tool_use_id %||% ""),
           character(1))
}

# Does `entry` hold results and nothing else?
.repair_results_only <- function(entry) {
    if (!is.list(entry)) {
        return(FALSE)
    }
    if (identical(entry$role, "tool") ||
        identical(entry$type, "function_call_output")) {
        return(TRUE)
    }
    identical(entry$role, "user") && is.list(entry$content) &&
    length(entry$content) > 0L &&
    all(vapply(entry$content, function(b) {
        is.list(b) && identical(b$type, "tool_result")
    }, logical(1)))
}

# Close every call in `history` that has no result. Returns the history
# with a placeholder result after each, the ids it closed, and whether
# the history now ends with results and nothing after them.
.history_close_open_calls <- function(history,
                                      placeholder = .open_call_placeholder) {
    history <- history %||% list()
    n <- length(history)
    out <- list()
    closed <- character()
    keep <- function(entry) out[[length(out) + 1L]] <<- entry
    i <- 1L
    while (i <= n) {
        entry <- history[[i]]
        keep(entry)
        i <- i + 1L
        made <- .repair_calls(entry)
        wanted <- nzchar(made$ids)
        ids <- made$ids[wanted]
        if (!length(ids)) {
            next
        }
        if (identical(made$shape, "anthropic")) {
            nxt <- if (i <= n) history[[i]]
            if (is.list(nxt) && identical(nxt$role, "user")) {
                missing <- setdiff(ids, .repair_user_answers(nxt))
                if (length(missing)) {
                    nxt <- .repair_user_results(nxt, missing, placeholder)
                    closed <- c(closed, missing)
                }
                keep(nxt)
                i <- i + 1L
            } else {
                msg <- list(role = "user")
                keep(.repair_user_results(msg, ids, placeholder))
                closed <- c(closed, ids)
            }
            next
        }
        # OpenAI and Responses results are entries of their own, after
        # the call; the missing ones go after those already there.
        answered <- character()
        while (i <= n && !is.null(.repair_answers(history[[i]], made$shape))) {
            answered <- c(answered, .repair_answers(history[[i]], made$shape))
            keep(history[[i]])
            i <- i + 1L
        }
        call_names <- made$names[wanted]
        missing <- which(!ids %in% answered)
        for (k in missing) {
            keep(.repair_result(made$shape, ids[[k]], call_names[[k]],
                                placeholder))
        }
        closed <- c(closed, ids[missing])
    }
    list(history = out, closed = closed,
         ends_with_results = length(closed) > 0L &&
         .repair_results_only(out[[length(out)]]))
}

# Close the session's open calls, through history_replace(), so the
# journal keeps the history as it was. Where the closed calls end the
# history, `marker` follows them, as the interrupt marker follows a
# repair (apply_exit_marker()). Never throws: it runs in turn()'s error
# handler, where a second error would hide the first. Returns how many
# calls it closed.
.turn_close_open_calls <- function(session, marker, reason) {
    if (!is.environment(session)) {
        return(invisible(0L))
    }
    tryCatch({
        healed <- .history_close_open_calls(session$history %||% list())
        n <- length(healed$closed)
        if (n > 0L) {
            after <- healed$history
            if (isTRUE(healed$ends_with_results)) {
                said <- list(role = "assistant", content = marker)
                after[[length(after) + 1L]] <- said
            }
            history_replace(session, after, reason = reason)
            log_event("tool_calls_closed", reason = reason, calls = n,
                      level = "warn")
        }
        invisible(n)
    }, error = function(e) {
        log_event("tool_calls_close_failed", reason = reason,
                  error = conditionMessage(e), level = "warn")
        invisible(0L)
    })
}
