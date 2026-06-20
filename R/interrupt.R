# Interrupt / denial helpers for the turn lifecycle.
#
# When a turn is cut short -- the user hit Ctrl+C / Esc, or denied an
# approval prompt -- history may contain an assistant message with
# tool_use blocks that never got matching tool_results. Anthropic /
# OpenAI both 400 on that on the next API call. These helpers repair
# the history (synthesize result blocks for unfinished calls) and
# append an explanatory marker so the LLM can see what happened.

# Synthesize tool_result entries for every unfinished tool call IN THE
# CURRENT TURN SLICE, then append a final assistant text marker.
#
# Scoping the repair to the current turn (records with
# call_message_index > pre_turn_len) is important: a stale dangling
# tool_use from earlier history can't be safely repaired by appending
# synthetic results at the tail, because the synthetic result would
# land in the wrong position relative to its issuing assistant
# message and still leave the next API call invalid. If prior turns
# left dangling tool_use blocks, that's a separate problem the
# compaction or migration layer needs to handle, not this helper.
#
# Returns the repaired history (still in provider native format, ready
# to feed back into llm.api::agent(history = ...)).
#
# - history: current message list (may end mid-batch)
# - provider: one of "anthropic" | "openai" | "moonshot" | "openai_codex" | "ollama"
# - marker: assistant text message describing why the turn stopped
#   (e.g. user_interrupt_marker() or user_deny_marker())
# - prompt: the user prompt that started this turn. Used only when
#   the turn never made any progress (no history_callback fired
#   before the exit), so the prompt that llm.api appended internally
#   never escaped. Without this we'd append the assistant marker
#   directly after the prior turn's history, dropping the user's
#   attempted exchange.
# - pre_turn_len: length of history before the turn started. Captured
#   by the caller. Used to distinguish "current turn made progress"
#   from "interrupted during the initial API call" and to scope the
#   repair to current-turn records.
# - placeholder: synthetic content for each missing tool_result
#   (defaults to a generic interrupt phrase)
repair_interrupted_tool_history <- function(history, provider, marker,
    prompt, pre_turn_len = 0L,
    placeholder = "[Interrupted before completion]") {
    history <- history %||% list()
    pre_turn_len <- as.integer(pre_turn_len)

    # No history_callback fired -- the turn was interrupted before
    # llm.api's internal messages list ever escaped. Inject the user
    # prompt so the next turn sees the attempted exchange.
    if (length(history) == pre_turn_len) {
        history[[length(history) + 1L]] <- list(role = "user", content = prompt)
    }

    # Scope repair to current-turn slice via call_message_index.
    records <- tryCatch(llm.api::history_tool_calls(history),
                        error = function(e) list())
    unfinished <- Filter(function(r) {
        !isTRUE(r$completed) &&
        isTRUE(as.integer(r$call_message_index) > pre_turn_len)
    }, records)

    if (length(unfinished) > 0L) {
        if (provider %in% c("anthropic", "anthropic_claude")) {
            history <- .append_anthropic_tool_results(history, unfinished,
                placeholder)
        } else {
            history <- .append_openai_tool_results(history, unfinished,
                placeholder)
        }
    }

    history[[length(history) + 1L]] <- list(role = "assistant",
        content = marker)
    history
}

# Anthropic batches all tool_results for one assistant turn into a
# single subsequent user message. If a partial user message already
# exists (some results landed before the interrupt), extend it;
# otherwise append a new one.
#
# The extension test uses compact_entry_is_tool_result_only() so a
# regular user message at the tail is NOT mistaken for a partial
# tool_result batch.
.append_anthropic_tool_results <- function(history, unfinished, placeholder) {
    blocks <- lapply(unfinished, function(r) {
        list(type = "tool_result", tool_use_id = r$id, content = placeholder)
    })
    last <- length(history)
    extend <- last >= 1L &&
    identical(history[[last]]$role, "user") &&
    compact_entry_is_tool_result_only(history[[last]])
    if (extend) {
        history[[last]]$content <- c(history[[last]]$content, blocks)
    } else {
        history[[length(history) + 1L]] <- list(role = "user", content = blocks)
    }
    history
}

# OpenAI / Moonshot / Ollama each render one tool result per message
# (role = "tool"). Append one synthetic message per missing call.
.append_openai_tool_results <- function(history, unfinished, placeholder) {
    for (r in unfinished) {
        history[[length(history) + 1L]] <- list(role = "tool",
            tool_call_id = r$id, name = r$name %||% "", content = placeholder)
    }
    history
}

# Summarize completed tool calls from the current-turn slice of
# turn_session$history as plain text, append the summary to the
# persistent disk session, and return the updated session.
#
# Why this exists: the CLI rebuilds api_history each turn from
# session$messages, which is stored as flat text -- structured
# tool_use / tool_result blocks never round-trip through disk. If
# the user hits Ctrl+C after some tools have completed, the next
# CLI turn would see nothing of that work; from the LLM's
# perspective the prompt ran, then was interrupted, and the four
# completed tool calls in between never happened. This helper
# dumps a text summary of those completed calls before the
# interrupt / deny marker so the next turn's LLM can pick up
# where the interrupted turn left off.
#
# Designed for the CLI exit handlers; chat() preserves the same
# context naturally because turn_session lives in memory across
# chat() turns.
#
# - turn_session: per-turn env mirrored by history_callback during
#   the interrupted turn
# - session: persistent disk session (list returned by session_load)
# - pre_turn_len: history length before the turn started
# - max_result_chars: per-call result truncation cap for the summary
#
# Returns the (possibly updated) session list. Idempotent on empty
# slices and on slices with no completed tool calls.
dump_completed_tools_summary <- function(turn_session, session, pre_turn_len,
    max_result_chars = 500L) {
    history <- turn_session$history %||% list()
    pre_turn_len <- as.integer(pre_turn_len)
    if (length(history) <= pre_turn_len) {
        return(session)
    }
    slice <- history[(pre_turn_len + 1L):length(history)]

    records <- tryCatch(llm.api::history_tool_calls(slice),
                        error = function(e) list())
    completed <- Filter(function(r) isTRUE(r$completed), records)
    if (length(completed) == 0L) {
        return(session)
    }

    summary_lines <- vapply(completed, function(r) {
        args_str <- if (length(r$arguments %||% list()) > 0L) {
            tryCatch(
                     jsonlite::toJSON(r$arguments, auto_unbox = TRUE),
                     error = function(e) "{...}"
            )
        } else {
            "{}"
        }
        result_str <- as.character(r$result %||% "")
        if (nchar(result_str) > max_result_chars) {
            result_str <- paste0(substr(result_str, 1L, max_result_chars),
                                 "...")
        }
        sprintf("[ran %s(%s) -> %s]", r$name %||% "?", args_str, result_str)
    }, character(1))

    summary <- paste(c("[Completed tool calls before exit:]", summary_lines),
                     collapse = "\n")

    session <- session_add_message(session, "assistant", summary)
    transcript_append(session, "assistant", summary)
    session
}

# One-stop shop used by every exit-marker site (chat() and CLI
# interrupt handlers, chat() and CLI user-deny handlers). Mutates
# session$history in place via repair_interrupted_tool_history().
#
# session is an environment (see new_session()), so the mutation is
# visible to the caller even when this is invoked from a condition
# handler that runs in a sibling scope.
#
# Returns the session invisibly for chained-style call sites and
# tests.
apply_exit_marker <- function(session, prompt, pre_turn_len, marker,
                              placeholder = "[Interrupted before completion]") {
    history <- session$history %||% list()
    provider <- session$provider %||% "anthropic"
    session$history <- repair_interrupted_tool_history(history = history,
        provider = provider, marker = marker, prompt = prompt,
        pre_turn_len = pre_turn_len, placeholder = placeholder)
    invisible(session)
}

