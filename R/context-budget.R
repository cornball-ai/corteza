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
#' The largest input each known model takes: the provider's published
#' input limit where it has one, else its context window. Used by
#' [context_limit_for_model()]. Each group names the page its numbers
#' were read from and the day. A model that is not here gets the
#' fallback, 128,000, and compacts early, so add a row when a provider
#' ships a model. `test_compact_trigger.R` fails when a default model of
#' llm.api has no row.
#' @keywords internal
MODEL_CONTEXT_LIMITS <- list(
                             # Anthropic, 2026-11-17.
                             # https://platform.claude.com/docs/en/build-with-claude/context-windows
                             # names these as having "a 1M-token context
                             # window", and says "1M is the default: you
                             # don't need a beta header".
                             "claude-fable-5-2" = 1000000L,
                             "claude-fable-5-1" = 1000000L,
                             "claude-fable-5" = 1000000L,
                             "claude-mythos-5-2" = 1000000L,
                             "claude-mythos-5-1" = 1000000L,
                             "claude-mythos-5" = 1000000L,
                             "claude-opus-5-5" = 1000000L,
                             "claude-opus-5" = 1000000L,
                             "claude-opus-4-8" = 1000000L,
                             "claude-opus-4-7" = 1000000L,
                             "claude-opus-4-6" = 1000000L,
                             "claude-sonnet-5-5" = 1000000L,
                             "claude-sonnet-5" = 1000000L,
                             "claude-sonnet-4-6" = 1000000L,
                             "claude-haiku-5-5" = 1000000L,
                             # The same page: "Other Claude models ... have
                             # a 200k-token context window." The table at
                             # https://platform.claude.com/docs/en/about-claude/models/overview
                             # lists these at 200K.
                             "claude-haiku-4-5" = 200000L,
                             "claude-opus-4-5" = 200000L,
                             "claude-sonnet-4-5" = 200000L,
                             "claude-opus-4-1" = 200000L,
                             "claude-sonnet-4-20250514" = 200000L,
                             "claude-opus-4-20250514" = 200000L,
                             # Retired by the provider. Kept for a gateway
                             # that still answers to the name.
                             "claude-3-5-sonnet-20241022" = 200000L,
                             "claude-3-opus-20240229" = 200000L,
                             "claude-3-haiku-20240307" = 200000L,
                             # OpenAI, 2026-11-17. Each model's page at
                             # https://developers.openai.com/api/docs/models/<id>
                             # says "1,050,000 context window" and "Maximum
                             # input tokens: 922,000". The input limit is
                             # the one a request can break.
                             "gpt-6.1-sol" = 922000L,
                             "gpt-6.1-terra" = 922000L,
                             "gpt-6-astra" = 922000L,
                             "gpt-6-sol" = 922000L,
                             "gpt-6-luna" = 922000L,
                             "gpt-5.6-sol" = 922000L,
                             "gpt-5.6-terra" = 922000L,
                             "gpt-5.6-luna" = 922000L,
                             # OpenAI, older. Not read again in 2026.
                             "gpt-4o" = 128000L,
                             "gpt-4o-mini" = 128000L,
                             "gpt-4-turbo" = 128000L,
                             "gpt-4" = 8192L,
                             "gpt-3.5-turbo" = 16385L,
                             # Google, 2026-11-17. Each model's page at
                             # https://aistudio.google.com/docs/models/<id>
                             # says "Input token limit 1,048,576"; the 3
                             # Gemini 4 pages say "Context window (unified
                             # input and output token limit) 1,048,576".
                             # gemini-3.5-flash is deprecated there and
                             # "automatically routed to gemini-3.6-flash".
                             "gemini-4-carbon" = 1048576L,
                             "gemini-4-argon" = 1048576L,
                             "gemini-4-hydrogen" = 1048576L,
                             "gemini-3.8-flash" = 1048576L,
                             "gemini-3.6-flash" = 1048576L,
                             "gemini-3.5-flash" = 1048576L,
                             "gemini-3.5-flash-lite" = 1048576L,
                             "gemini-3.1-pro-preview" = 1048576L,
                             "gemini-3.1-flash-lite" = 1048576L,
                             "gemini-3-flash-preview" = 1048576L,
                             # Moonshot, 2026-11-17.
                             # https://platform.kimi.ai/docs/pricing/chat
                             # says "All K3 series models support a
                             # 1,048,576-token context window" and "All K2
                             # series models support a 262,144-token
                             # context window". It names no model id, so
                             # each row stands for its series: kimi-k3.1-pro
                             # resolves to the first, kimi-k2.5 and
                             # kimi-k2.6 to the second.
                             "kimi-k3" = 1048576L,
                             "kimi-k2" = 262144L,
                             # Ollama. The server sets the window in use,
                             # not the model (OLLAMA_CONTEXT_LENGTH; by
                             # default 4k, 32k or 256k by VRAM,
                             # https://docs.ollama.com/context-length), so
                             # these rows are guesses and `context_window`
                             # in the config is the number to set.
                             # qwen3.5 supports a "256K context window"
                             # (https://ollama.com/library/qwen3.5/tags,
                             # 2026-11-17). Its row holds what the model
                             # got before it had one, the fallback: a
                             # guess too low compacts early, and one too
                             # high sends more than the server reads.
                             "qwen3.5" = 128000L,
                             "llama3.2" = 128000L,
                             "llama3.1" = 128000L,
                             "mistral" = 32000L,
                             "mixtral" = 32000L,
                             "qwen2.5" = 32000L
)

# Product endpoints can expose a smaller working window than the public API
# model. Keep that distinction explicit instead of pretending a model name
# alone identifies a request budget.
#
# openai_codex, 2026-11-17: `context_window` of each model in the Codex
# CLI's own catalog,
# https://github.com/openai/codex/blob/main/codex-rs/models-manager/models.json
# The public page of gpt-6.1-sol says it "uses a default context window of
# 500K tokens in Codex"; the catalog says 272000, and the smaller is here.
PROVIDER_MODEL_CONTEXT_LIMITS <- list(
                                      openai_codex = list("gpt-6.1-sol" = 272000L, "gpt-6.1-terra" = 500000L,
        "gpt-6-astra" = 272000L, "gpt-6-sol" = 272000L,
        "gpt-6-luna" = 272000L, "gpt-5.6-sol" = 272000L,
        "gpt-5.6-terra" = 272000L, "gpt-5.6-luna" = 272000L)
)

# What an unknown model gets.
CONTEXT_LIMIT_FALLBACK <- 128000L

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
#' Tries the model's own row, then the longest row name the id starts
#' with (so a dated or suffixed id resolves to its family), then the
#' rows whose names start with the id (so `"claude-3-haiku"` resolves to
#' the dated entry). An id cut short can mean several rows, and it gets
#' the smallest limit among them.
#' @param model Model name (character).
#' @param provider Optional provider/product endpoint. Provider-specific
#'   limits take precedence over the public model limit.
#' @return Context limit in tokens (integer). Returns 128000L when
#'   no entry matches.
#' @keywords internal
#' @export
context_limit_for_model <- function(model, provider = NULL) {
    .context_limit_row(model, provider) %||% CONTEXT_LIMIT_FALLBACK
}

# The table's row for a model, or NULL when it has none. Apart from
# context_limit_for_model() so a test can tell a row from the fallback.
.context_limit_row <- function(model, provider = NULL) {
    # No model named (NULL, length-0, NA, or empty) -> fall through to
    # the default rather than indexing MODEL_CONTEXT_LIMITS[[model]],
    # which errors on a zero-length or NA subscript. A function with an
    # "unknown model" fallback must not crash on "no model".
    if (length(model) != 1L || is.na(model) || !nzchar(model)) {
        return(NULL)
    }
    provider_limits <- if (length(provider) == 1L && !is.na(provider) &&
        nzchar(provider)) {
        PROVIDER_MODEL_CONTEXT_LIMITS[[provider]]
    } else {
        NULL
    }
    .context_limit_match(model, provider_limits) %||%
    .context_limit_match(model, MODEL_CONTEXT_LIMITS)
}

# One table's row for a model id, or NULL. The table holds windows of
# 200K and 1M within one family, so neither prefix rule may depend on
# the order of its rows: the longest name wins where the id is the
# longer, and the smallest limit where the id is the shorter.
.context_limit_match <- function(model, limits) {
    if (!length(limits)) {
        return(NULL)
    }
    if (!is.null(limits[[model]])) {
        return(limits[[model]])
    }
    rows <- names(limits)
    family <- rows[startsWith(model, rows)]
    if (length(family)) {
        return(limits[[family[which.max(nchar(family))]]])
    }
    longer <- rows[startsWith(rows, model)]
    if (length(longer)) {
        return(min(unlist(limits[longer], use.names = FALSE)))
    }
    NULL
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
#' @param seconds Age in seconds.
#' @return A character string.
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
#' @param tokens Live context, in tokens.
#' @param limit The model's context window, in tokens.
#' @return A character string: "tokens/limit", or "?" when either value
#'   is NA.
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
    label <- message$role %||% message$type %||% "unknown"
    # A chat-completions call (OpenAI and the providers that copy it)
    # keeps its arguments beside the content, under tool_calls.
    calls <- if (is.list(message)) message$tool_calls
    calls_tok <- if (length(calls)) {
        estimate_text_tokens(tryCatch(
                                      jsonlite::toJSON(calls, auto_unbox = TRUE, null = "null"),
                                      error = function(e) ""))
    } else {
        0L
    }
    content <- if (is.list(message)) message$content
    if (is.list(content) && !.text_blocks_only(content)) {
        return(as.integer(estimate_text_tokens(paste0(label, ": ")) +
                          .estimate_block_tokens(content) + calls_tok))
    }
    as.integer(estimate_text_tokens(sprintf("%s: %s", label,
                message_text(message))) +
               calls_tok)
}

.text_blocks_only <- function(blocks) {
    all(vapply(blocks, function(b) {
        is.list(b) && identical(b$type %||% "text", "text")
    }, logical(1)))
}

# Anthropic keeps a turn's content as blocks. A tool's result holds its
# text under `content` (a string, or blocks of its own) and a call its
# arguments under `input`, so message_text(), which reads `text`, saw
# them only through str(), which cuts each string to a line: a
# 37,000-character file a tool read counted as about 100 tokens, and a
# session that read files compacted far past its cap. Count the blocks'
# JSON surface at chars/4, as for Codex output, and what is opaque at
# chars/24: a thinking block's signature, a redacted block's data, an
# image's or a document's base64 (an OpenAI-style data URL too). Only a
# "base64" source is opaque: a plain-text document's data is its text.
.estimate_block_tokens <- function(blocks) {
    opaque <- 0
    take <- function(x) {
        if (is.character(x)) {
            opaque <<- opaque + sum(nchar(x, type = "chars"))
        }
    }
    visible <- function(b) {
        if (!is.list(b)) {
            return(b)
        }
        type <- b$type %||% ""
        if (identical(type, "thinking")) {
            take(b$signature)
            b$signature <- NULL
        } else if (identical(type, "redacted_thinking")) {
            take(b$data)
            b$data <- NULL
        } else if (type %in% c("image", "document") && is.list(b$source) &&
            identical(b$source$type, "base64")) {
            take(b$source$data)
            b$source$data <- NULL
        } else if (identical(type, "image_url") && is.list(b$image_url) &&
            is.character(b$image_url$url) && length(b$image_url$url) == 1L &&
            startsWith(b$image_url$url, "data:")) {
            take(b$image_url$url)
            b$image_url$url <- NULL
        } else if (identical(type, "tool_result") && is.list(b$content)) {
            b$content <- lapply(b$content, visible)
        }
        b
    }
    json <- tryCatch(
                     jsonlite::toJSON(lapply(blocks, visible), auto_unbox = TRUE,
                                      null = "null"),
                     error = function(e) ""
    )
    as.integer(estimate_text_tokens(json) + ceiling(opaque / 24))
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

# Approximate the serialized model request size before provider-specific HTTP
# wrapping. Unlike the token estimate above, this deliberately retains opaque
# Codex encrypted reasoning: the gateway has to buffer those bytes even though
# they do not tokenize like natural-language text.
.estimate_live_request_bytes <- function(session, system_prompt = NULL,
    tools = NULL) {
    messages <- session$messages %||% session$history %||% list()
    payload <- list(system = system_prompt %||% "", tools = tools %||% list(),
                    history = messages)
    json <- tryCatch(
                     jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
                     error = function(e) NA_character_
    )
    if (length(json) != 1L || is.na(json)) {
        return(NA_real_)
    }
    as.numeric(nchar(json, type = "bytes", allowNA = FALSE))
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
