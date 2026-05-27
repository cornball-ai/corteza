# Context-budget helpers.
#
# Estimating live model-context size in tokens is needed in two
# places: the CLI loop (decide when to auto-compact the parent
# session) and subagent_turn_prompt() (decide when to compact the
# child's in-memory history). Both should ask the same question and
# get the same answer, so the math lives here in package code and
# both call sites use it.
#
# The estimates here are deliberately char-count / 4 with a small
# per-message and per-tool overhead — close enough to drive
# threshold decisions without depending on a real tokenizer. When
# the provider returns real usage counts, use those instead.

#' Model context limits in tokens.
#'
#' Table of context window sizes for known models. Used by
#' [context_limit_for_model()]. Add new entries here as providers
#' ship them.
#' @keywords internal
MODEL_CONTEXT_LIMITS <- list(
                             # Anthropic — short-form IDs (claude-<family>-<minor>)
                             "claude-opus-4-7" = 200000L,
                             "claude-sonnet-4-6" = 200000L,
                             "claude-haiku-4-5" = 200000L,
                             # Anthropic — date-stamped IDs
                             "claude-sonnet-4-20250514" = 200000L,
                             "claude-opus-4-20250514" = 200000L,
                             "claude-3-5-sonnet-20241022" = 200000L,
                             "claude-3-opus-20240229" = 200000L,
                             "claude-3-haiku-20240307" = 200000L,
                             # OpenAI
                             "gpt-4o" = 128000L,
                             "gpt-4o-mini" = 128000L,
                             "gpt-4-turbo" = 128000L,
                             "gpt-4" = 8192L,
                             "gpt-3.5-turbo" = 16385L,
                             # Ollama (varies by quantization)
                             "llama3.2" = 128000L,
                             "llama3.1" = 128000L,
                             "mistral" = 32000L,
                             "mixtral" = 32000L,
                             "qwen2.5" = 32000L
)

#' Provider-specific default model name.
#'
#' Resolves the actual model a subagent (or chat session) will run
#' with when no explicit \code{model} is set, so /agents, compaction,
#' and the CLI all show the same model identity. Delegates to
#' \code{llm.api::provider_default_model()} -- the canonical table --
#' rather than keeping a parallel one that drifts. Returns NULL for an
#' unknown or empty provider (llm.api errors there; we map it to NULL).
#' @param provider Provider name.
#' @return Model name (character) or NULL.
#' @keywords internal
#' @export
default_provider_model <- function(provider) {
    tryCatch(llm.api::provider_default_model(provider %||% ""),
             error = function(e) NULL)
}

#' Look up the context window for a given model.
#'
#' Tries exact match, then prefix match either direction (so
#' `"claude-3-5-sonnet"` resolves to the dated entry, and a longer
#' model id with a known prefix also resolves).
#' @param model Model name (character).
#' @return Context limit in tokens (integer). Returns 128000L when
#'   no entry matches.
#' @keywords internal
#' @export
context_limit_for_model <- function(model) {
    # No model named (NULL, length-0, NA, or empty) -> fall through to
    # the default rather than indexing MODEL_CONTEXT_LIMITS[[model]],
    # which errors on a zero-length or NA subscript. A function with an
    # "unknown model" fallback must not crash on "no model".
    if (length(model) != 1L || is.na(model) || !nzchar(model)) {
        return(128000L)
    }
    if (!is.null(MODEL_CONTEXT_LIMITS[[model]])) {
        return(MODEL_CONTEXT_LIMITS[[model]])
    }
    for (name in names(MODEL_CONTEXT_LIMITS)) {
        if (startsWith(model, name) || startsWith(name, model)) {
            return(MODEL_CONTEXT_LIMITS[[name]])
        }
    }
    128000L
}

#' Format a token count for display (K / M suffixes).
#' @param n Token count.
#' @return Character.
#' @keywords internal
#' @export
format_tokens <- function(n) {
    if (n >= 1000000) {
        sprintf("%.1fM", n / 1000000)
    } else if (n >= 1000) {
        sprintf("%.1fK", n / 1000)
    } else {
        as.character(n)
    }
}

#' Format an age in seconds as a compact string (e.g. "12s", "3m", "2h").
#' @keywords internal
#' @export
format_age <- function(seconds) {
    s <- as.numeric(seconds)
    if (is.na(s) || s < 0) {
        return("?")
    }
    if (s < 60) {
        sprintf("%ds", as.integer(round(s)))
    } else if (s < 3600) {
        sprintf("%dm", as.integer(round(s / 60)))
    } else {
        sprintf("%.1fh", s / 3600)
    }
}

#' Format a live-context display like "4.2K/200K" or "?".
#'
#' Used by /agents to summarize live tokens versus model limit.
#' Returns "?" when either value is NA.
#' @keywords internal
#' @export
format_live_ctx <- function(tokens, limit) {
    if (is.na(tokens) || is.na(limit) || is.null(tokens) || is.null(limit)) {
        return("ctx ?")
    }
    sprintf("ctx %s/%s", format_tokens(tokens), format_tokens(limit))
}

#' Rough token estimate from raw text.
#'
#' Returns `ceil(nchar(text) / 4)`. Good enough for budget decisions
#' but not a substitute for the provider's real usage count.
#' @param text Character (length 1 or vector; collapsed with newlines).
#' @return Integer.
#' @keywords internal
#' @export
estimate_text_tokens <- function(text) {
    if (is.null(text) || length(text) == 0L) {
        return(0L)
    }
    text <- paste(as.character(text), collapse = "\n")
    if (!nzchar(text)) {
        return(0L)
    }
    as.integer(ceiling(nchar(text, type = "chars", allowNA = FALSE) / 4))
}

#' Best-effort flatten of a message's `content` field into one string.
#'
#' Messages may have content as a plain string or a list of typed
#' blocks (text / tool_use / tool_result). For budget math we just
#' want the textual surface area.
#' @param message Single message list.
#' @return Character.
#' @keywords internal
message_text <- function(message) {
    content <- message$content
    if (is.list(content)) {
        if (length(content) > 0L && !is.null(content[[1]]$text)) {
            return(paste(vapply(
                                content,
                                function(block) as.character(block$text %||% ""),
                                character(1)
                    ), collapse = "\n"))
        }
        return(paste(utils::capture.output(str(content, max.level = 2L)),
                     collapse = "\n"))
    }
    as.character(content %||% "")
}

#' Token estimate for a list of messages (history).
#'
#' Sums text tokens for each message and adds a small framing
#' overhead (6 tokens / message) that the chars/4 estimate misses.
#' @param messages List of message lists, each with `$role` and
#'   `$content`.
#' @return Integer.
#' @keywords internal
#' @export
estimate_history_tokens <- function(messages) {
    messages <- messages %||% list()
    if (length(messages) == 0L) {
        return(0L)
    }
    text_tokens <- sum(vapply(messages, function(m) {
        estimate_text_tokens(sprintf("%s: %s", m$role %||% "unknown",
                                     message_text(m)))
    }, integer(1)))
    as.integer(text_tokens + length(messages) * 6L)
}

#' Token estimate for the tool schema payload.
#'
#' Serializes the tool list as JSON and counts tokens, plus a 12-
#' token overhead per tool for the schema framing.
#' @param tools List of tool definitions (or NULL).
#' @return Integer.
#' @keywords internal
#' @export
estimate_tool_tokens <- function(tools) {
    tools <- tools %||% list()
    if (length(tools) == 0L) {
        return(0L)
    }
    tool_text <- tryCatch(
                          jsonlite::toJSON(tools, auto_unbox = TRUE, null = "null"),
                          error = function(e) ""
    )
    as.integer(estimate_text_tokens(tool_text) + length(tools) * 12L)
}

#' Token estimate for an entire live model-context.
#'
#' Sum of system prompt + tool schema + message history, plus
#' framing overheads. Used to drive auto-compaction triggers.
#' @param session Session-like object with `$messages` list.
#' @param system_prompt Character or NULL.
#' @param tools List of tool definitions or NULL.
#' @return Integer.
#' @keywords internal
#' @export
estimate_live_context_tokens <- function(session, system_prompt = NULL,
    tools = NULL) {
    sys_tok <- estimate_text_tokens(system_prompt %||% "")
    tools_tok <- estimate_tool_tokens(tools)
    # CLI sessions store the live message list under $messages;
    # turn_sessions (used by subagents) use $history. Accept either
    # so both call paths get a correct count.
    messages <- session$messages %||% session$history %||% list()
    history_tok <- estimate_history_tokens(messages)
    as.integer(sys_tok + tools_tok + history_tok)
}

#' Percent of a model's context window used by a session.
#'
#' Convenience wrapper around [estimate_live_context_tokens()] and
#' [context_limit_for_model()]. Returns 0 when the limit is 0 or
#' negative (defensive — shouldn't happen with a real model).
#' @param session Session-like object with `$messages`.
#' @param model Model name used to look up the context limit.
#' @param system_prompt Optional system prompt.
#' @param tools Optional tools list.
#' @return Numeric percentage in `[0, +Inf)`.
#' @keywords internal
#' @export
context_usage_pct <- function(session, model, system_prompt = NULL,
                              tools = NULL) {
    used <- estimate_live_context_tokens(session, system_prompt, tools)
    limit <- context_limit_for_model(model)
    if (is.null(limit) || limit <= 0L) {
        return(0)
    }
    100 * used / limit
}

