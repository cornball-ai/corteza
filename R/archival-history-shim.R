# Shim for llm.api::history_tool_calls.
#
# llm.api 0.1.3 exports `history_tool_calls()` and
# `history_count_tool_calls()` so consumers don't have to handle the
# Anthropic content-block shape and the OpenAI tool_calls-field shape
# separately. Until 0.1.3 lands on CRAN we can't pin
# `Imports: llm.api (>= 0.1.3)` without breaking installs for users
# still on 0.1.2.1.
#
# This shim delegates to the new helper when it's exported and falls
# back to a local equivalent when it isn't. Once corteza pins
# `llm.api (>= 0.1.3)`, archival_history_tool_calls_fallback can be
# deleted and archival_history_tool_calls becomes a one-liner.

#' Walk a history list and return paired tool-call/tool-result records.
#'
#' Delegates to `llm.api::history_tool_calls()` when it's available,
#' falls back to a local walk otherwise. Used by every archival path
#' that needs shape-agnostic tool-call awareness.
#' @param history List of message entries.
#' @return List of records: id, name, arguments, result, completed,
#'   call_message_index, result_message_index, provider_shape.
#' @noRd
archival_history_tool_calls <- function(history) {
    if (.archival_llm_api_has_history_helpers()) {
        return(llm.api::history_tool_calls(history))
    }
    archival_history_tool_calls_fallback(history)
}

#' Lazy / cached check for whether llm.api exports the new helpers.
#' @noRd
.archival_llm_api_has_history_helpers <- function() {
    # Cached on the package-level archival counter env so we don't
    # re-do the existence check on every archival call.
    cached <- .subagent_counter$llm_api_has_history_helpers
    if (!is.null(cached)) {
        return(cached)
    }
    has <- tryCatch(
                    exists("history_tool_calls", envir = asNamespace("llm.api"),
                           inherits = FALSE) &&
                    is.function(get("history_tool_calls",
                                    envir = asNamespace("llm.api"),
                                    inherits = FALSE)),
                    error = function(e) FALSE
    )
    .subagent_counter$llm_api_has_history_helpers <- isTRUE(has)
    isTRUE(has)
}

#' Local fallback for archival_history_tool_calls.
#'
#' Mirrors `llm.api::history_tool_calls()` byte-for-byte. Kept here so
#' corteza works against `llm.api` 0.1.2.1 (no helpers) the same way
#' as 0.1.3+. Delete once `Imports: llm.api (>= 0.1.3)` is pinned.
#' @noRd
archival_history_tool_calls_fallback <- function(history) {
    if (!is.list(history) || length(history) == 0L) {
        return(list())
    }
    calls <- list()
    for (i in seq_along(history)) {
        entry <- history[[i]]
        if (!is.list(entry)) {
            next
        }
        if (!identical(entry$role %||% "", "assistant")) {
            next
        }
        cnt <- entry$content
        if (is.list(cnt)) {
            for (block in cnt) {
                if (identical(block$type %||% "", "tool_use")) {
                    calls[[length(calls) + 1L]] <- list(id = block$id %||% "",
                        name = block$name %||% "",
                        arguments = block$input %||% list(), result = NULL,
                        completed = FALSE, call_message_index = i,
                        result_message_index = NA_integer_,
                        provider_shape = "anthropic")
                }
            }
        }
        if (!is.null(entry$tool_calls)) {
            for (tc in entry$tool_calls) {
                fn <- tc$`function` %||% list()
                args_raw <- fn$arguments %||% list()
                args <- if (is.character(args_raw) && length(args_raw) == 1L) {
                    tryCatch(
                             jsonlite::fromJSON(args_raw, simplifyVector = FALSE),
                             error = function(e) list()
                    )
                } else {
                    args_raw
                }
                calls[[length(calls) + 1L]] <- list(
                    id = tc$id %||% "",
                    name = fn$name %||% "",
                    arguments = args,
                    result = NULL, completed = FALSE,
                    call_message_index = i,
                    result_message_index = NA_integer_,
                    provider_shape = "openai"
                )
            }
        }
    }
    if (length(calls) == 0L) {
        return(list())
    }
    for (i in seq_along(history)) {
        entry <- history[[i]]
        if (!is.list(entry)) {
            next
        }
        role <- entry$role %||% ""
        if (identical(role, "user")) {
            cnt <- entry$content
            if (is.list(cnt)) {
                for (block in cnt) {
                    if (identical(block$type %||% "", "tool_result")) {
                        target_id <- block$tool_use_id %||% block$id %||% ""
                        if (!nzchar(target_id)) {
                            next
                        }
                        result_text <- .archival_block_result_text(block)
                        for (j in seq_along(calls)) {
                            if (!calls[[j]]$completed &&
                                identical(calls[[j]]$id, target_id)) {
                                calls[[j]]$result <- result_text
                                calls[[j]]$completed <- TRUE
                                calls[[j]]$result_message_index <- i
                                break
                            }
                        }
                    }
                }
            }
            next
        }
        if (identical(role, "tool")) {
            target_id <- entry$tool_call_id %||% entry$id %||% ""
            if (!nzchar(target_id)) {
                next
            }
            result_text <- as.character(entry$content %||% "")
            for (j in seq_along(calls)) {
                if (!calls[[j]]$completed &&
                    identical(calls[[j]]$id, target_id)) {
                    calls[[j]]$result <- result_text
                    calls[[j]]$completed <- TRUE
                    calls[[j]]$result_message_index <- i
                    break
                }
            }
        }
    }
    calls
}

# Used by the fallback to render Anthropic's tool_result content
# whether it's a flat string or a list of {type:"text"} blocks.
.archival_block_result_text <- function(block) {
    cnt <- block$content
    if (is.character(cnt)) {
        return(paste(cnt, collapse = "\n"))
    }
    if (is.list(cnt)) {
        parts <- vapply(cnt, function(b) {
            as.character(b$text %||% "")
        }, character(1))
        return(paste(parts, collapse = "\n"))
    }
    as.character(cnt %||% "")
}

