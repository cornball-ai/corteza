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
                             "gpt-5.6-sol" = 1050000L,
                             "gpt-5.6-terra" = 1050000L,
                             "gpt-5.6-luna" = 1050000L,
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

# Product endpoints can expose a smaller working window than the public API
# model. The ChatGPT Codex model catalog currently advertises 272K for
# gpt-5.6-sol, while the public API model advertises 1.05M. Keep that
# distinction explicit instead of pretending a model name alone identifies a
# request budget.
PROVIDER_MODEL_CONTEXT_LIMITS <- list(
                                      openai_codex = list("gpt-5.6-sol" = 272000L, "gpt-5.6-terra" = 272000L,
        "gpt-5.6-luna" = 272000L)
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
#' @param provider Optional provider/product endpoint. Provider-specific
#'   limits take precedence over the public model limit.
#' @return Context limit in tokens (integer). Returns 128000L when
#'   no entry matches.
#' @keywords internal
#' @export
context_limit_for_model <- function(model, provider = NULL) {
    # No model named (NULL, length-0, NA, or empty) -> fall through to
    # the default rather than indexing MODEL_CONTEXT_LIMITS[[model]],
    # which errors on a zero-length or NA subscript. A function with an
    # "unknown model" fallback must not crash on "no model".
    if (length(model) != 1L || is.na(model) || !nzchar(model)) {
        return(128000L)
    }
    provider_limits <- if (length(provider) == 1L && !is.na(provider) &&
        nzchar(provider)) {
        PROVIDER_MODEL_CONTEXT_LIMITS[[provider]]
    } else {
        NULL
    }
    if (!is.null(provider_limits)) {
        if (!is.null(provider_limits[[model]])) {
            return(provider_limits[[model]])
        }
        for (name in names(provider_limits)) {
            if (startsWith(model, name) || startsWith(name, model)) {
                return(provider_limits[[name]])
            }
        }
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
    # OpenAI Responses/Codex stores assistant turns and tool results in
    # provider-native entries without role/content. Preserve their visible
    # surface here instead of reporting every such entry as empty.
    if (identical(message$type, ".openai_codex_output")) {
        return(.compact_codex_output_text(message$output))
    }
    if (identical(message$type, "function_call_output")) {
        return(paste(as.character(message$output %||% ""), collapse = "\n"))
    }
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

# Estimate one provider-native history entry. Codex Responses output is sent
# back to the provider verbatim, including function-call arguments and opaque
# reasoning state, so measuring only its rendered answer text is badly low.
# Count the ordinary JSON surface at the normal chars/4 rate. Encrypted
# reasoning is an opaque encoding rather than natural-language text; chars/24
# is a conservative token proxy calibrated against the provider's reported
# input usage, without pretending the ciphertext itself is tokenized as prose.
.estimate_history_entry_tokens <- function(message) {
    if (is.list(message) && identical(message$type, ".openai_codex_output")) {
        output <- message$output %||% list()
        encrypted_chars <- 0
        visible <- lapply(output, function(item) {
            if (!is.list(item)) {
                return(item)
            }
            encrypted <- item$encrypted_content
            if (is.character(encrypted)) {
                encrypted_chars <<- encrypted_chars +
                sum(nchar(encrypted, type = "chars"))
            }
            item$encrypted_content <- NULL
            item
        })
        visible_json <- tryCatch(
                                 jsonlite::toJSON(visible, auto_unbox = TRUE, null = "null"),
                                 error = function(e) ""
        )
        return(as.integer(
                          estimate_text_tokens(visible_json) + ceiling(encrypted_chars / 24)
            ))
    }
    estimate_text_tokens(sprintf("%s: %s",
                                 message$role %||% message$type %||% "unknown",
                                 message_text(message)))
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
    text_tokens <- sum(vapply(messages, .estimate_history_entry_tokens,
                              integer(1)))
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
#' @param provider Optional provider/product endpoint passed to
#'   [context_limit_for_model()].
#' @return Numeric percentage in `[0, +Inf)`.
#' @keywords internal
#' @export
context_usage_pct <- function(session, model, system_prompt = NULL,
                              tools = NULL, provider = NULL) {
    used <- estimate_live_context_tokens(session, system_prompt, tools)
    limit <- context_limit_for_model(model, provider = provider)
    if (is.null(limit) || limit <= 0L) {
        return(0)
    }
    100 * used / limit
}
