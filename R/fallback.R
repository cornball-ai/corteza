# Provider fallback on rate and usage limits.
#
# A session names one provider/model pair. The `fallback` config key
# (a character vector of "model provider" specs, the same shape as the
# /model menu's `models`) names what turn() tries instead when that
# pair refuses the request with a limit error: llm.api's "API error
# (429|503|529)", or a body naming a rate, usage, or quota limit. Any
# other error belongs to the caller and is rethrown untouched.
#
# A provider that hit a limit is skipped by every session in the process.
# Fallback providers use a cooldown (default 30 minutes); the primary can
# instead name a weekly retry boundary such as "Mon 03:00", matching a
# subscription reset without requiring a service restart.
#
# A limit hit part-way through a tool-using run resumes from the captured
# history so completed tool calls are not repeated.

.fallback_state <- new.env(parent = emptyenv())

.FALLBACK_COOLDOWN_MINUTES <- 30

# These provider names are credential modes, not aliases. The *_claude and
# *_codex variants use subscription authentication; the unsuffixed hosted
# providers use API keys and may incur metered charges.
.FALLBACK_SUBSCRIPTION_PROVIDERS <- c("anthropic_claude", "openai_codex")
.FALLBACK_API_KEY_PROVIDERS <- c("anthropic", "openai", "moonshot")

.fallback_route <- function(candidate, index) {
    list(
         model = candidate$model,
         provider = candidate$provider,
         fallback = index > 1L,
         fallback_level = index - 1L,
         subscription = candidate$provider %in% .FALLBACK_SUBSCRIPTION_PROVIDERS,
         api_key = candidate$provider %in% .FALLBACK_API_KEY_PROVIDERS
    )
}

.fallback_route_label <- function(x, max_chars) {
    x <- as.character(x %||% "")
    if (!length(x) || is.na(x[[1L]])) {
        return("")
    }
    x <- gsub("[[:cntrl:]]+", " ", x[[1L]])
    trimws(substr(x, 1L, max_chars))
}

# Deterministic output notice: modest for a subscription-to-subscription hop,
# deliberately loud before any reply whose provider consumes an API key.
.fallback_notice <- function(route) {
    if (!is.list(route) || !isTRUE(route$fallback)) {
        return(NULL)
    }
    model <- .fallback_route_label(route$model, 80L)
    provider <- .fallback_route_label(route$provider, 40L)
    if (isTRUE(route$api_key)) {
        return(paste0(
                      "🚨🚨🚨 PAID API KEY FALLBACK — BILLABLE USAGE 🚨🚨🚨\n",
                      "Subscriptions are limited; using ", model, " via ", provider, "."
            ))
    }
    sprintf("⚡ Automatic subscription failover: %s via %s.", model, provider)
}

.fallback_reply <- function(response) {
    reply <- paste(as.character(response$content %||% ""), collapse = "\n")
    notice <- .fallback_notice(response$corteza_route)
    if (is.null(notice)) {
        reply
    } else {
        paste(notice, reply, sep = "\n\n")
    }
}

# "model provider" -> list(model, provider); a bare model name takes
# default_provider. NULL when the spec is empty or the provider is
# unresolvable.
.parse_model_spec <- function(spec, default_provider = NULL) {
    parts <- strsplit(trimws(spec %||% ""), "\\s+")[[1]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) {
        return(NULL)
    }
    if (length(parts) >= 2L) {
        provider <- parts[[2L]]
    } else {
        provider <- default_provider
    }
    if (is.null(provider) || !nzchar(provider)) {
        return(NULL)
    }
    list(model = parts[[1L]], provider = provider)
}

# The session's fallback chain, primary excluded: session$fallback
# (set by bot_new_session() from the Matrix config) else the cwd
# config's `fallback` key. Empty list when neither is set.
.session_fallback <- function(session) {
    specs <- session$fallback %||% session$config$fallback
    specs <- as.character(specs %||% character())
    out <- lapply(specs, .parse_model_spec, default_provider = session$provider)
    Filter(Negate(is.null), out)
}

.fallback_cooldown <- function(session) {
    minutes <- session$fallback_cooldown %||%
    session$config$fallback_cooldown_minutes %||%
    .FALLBACK_COOLDOWN_MINUTES
    minutes <- suppressWarnings(as.numeric(minutes))
    if (length(minutes) != 1L || is.na(minutes) || minutes < 0) {
        return(.FALLBACK_COOLDOWN_MINUTES)
    }
    minutes
}

# Optional weekly boundary for retrying the primary after a limit. The
# deliberately small grammar is enough for subscription reset schedules and
# avoids pretending this package contains a cron parser.
.fallback_primary_retry_at <- function(session) {
    spec <- session$fallback_primary_retry_at %||%
    session$config$fallback_primary_retry_at
    if (is.null(spec)) {
        return(NULL)
    }
    if (!is.character(spec) || length(spec) != 1L || is.na(spec)) {
        stop("fallback_primary_retry_at must be a string like 'Mon 03:00'",
             call. = FALSE)
    }
    spec <- gsub("\\s+", " ", trimws(spec))
    pattern <- "^(Sun|Mon|Tue|Wed|Thu|Fri|Sat) ([01][0-9]|2[0-3]):[0-5][0-9]$"
    if (!grepl(pattern, spec)) {
        stop("fallback_primary_retry_at must be a string like 'Mon 03:00'",
             call. = FALSE)
    }
    spec
}

.fallback_next_retry <- function(spec, now = Sys.time(), tz = Sys.timezone()) {
    if (!is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz)) {
        tz <- ""
    }
    parts <- strsplit(spec, " ", fixed = TRUE)[[1L]]
    days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    target_wday <- match(parts[[1L]], days) - 1L
    local <- as.POSIXlt(now, tz = tz)
    offset <- (target_wday - local$wday) %% 7L
    date <- as.Date(now, tz = tz) + offset
    candidate <- as.POSIXct(paste(date, parts[[2L]]), tz = tz)
    if (candidate <= now) {
        date <- date + 7L
        candidate <- as.POSIXct(paste(date, parts[[2L]]), tz = tz)
    }
    candidate
}

.fallback_deadline <- function(session, primary = FALSE, now = Sys.time()) {
    if (isTRUE(primary)) {
        spec <- .fallback_primary_retry_at(session)
        if (!is.null(spec)) {
            return(.fallback_next_retry(spec, now = now))
        }
    }
    now + .fallback_cooldown(session) * 60
}

# Is this error a provider telling us to come back later? Status codes
# come from llm.api's "API error (NNN): ..." prefix; the text patterns
# cover the bodies behind them (Anthropic rate_limit_error and
# overloaded_error, OpenAI insufficient_quota, Codex usage limits).
# "exceeded" alone is deliberately not matched: a context-length 400
# says it too, and that is not a reason to switch providers.
.is_limit_error <- function(e) {
    msg <- conditionMessage(e)
    grepl("API error \\((429|503|529)\\)", msg) ||
    grepl("rate[ _-]?limit|usage[ _-]?limit|too many requests|overloaded|quota",
          msg, ignore.case = TRUE)
}

.fallback_mark <- function(provider, minutes, now = Sys.time()) {
    assign(provider, now + minutes * 60, envir = .fallback_state)
    invisible(NULL)
}

.fallback_mark_until <- function(provider, until) {
    assign(provider, until, envir = .fallback_state)
    invisible(NULL)
}

.fallback_until <- function(provider) {
    if (exists(provider, envir = .fallback_state, inherits = FALSE)) {
        get(provider, envir = .fallback_state, inherits = FALSE)
    } else {
        NULL
    }
}

.fallback_limited <- function(provider, now = Sys.time()) {
    until <- .fallback_until(provider)
    !is.null(until) && until > now
}

.fallback_reset <- function() {
    rm(list = ls(.fallback_state, all.names = TRUE), envir = .fallback_state)
    invisible(NULL)
}

# What wire's shape is this history in?
#
# A conversation's history carries the content vocabulary of the wire
# that produced it, and llm.api replays what it is handed: an
# unrecognised block list goes to the provider verbatim
# (llm.api R/openai-codex.R:216 matches neither its llm_content nor its
# character branch and passes the list straight through). The
# vocabularies do not overlap, and the receiving API rejects the
# difference outright rather than ignoring it -- an Anthropic history
# replayed on the Responses wire returns
#   API error (400): Invalid value: 'thinking'.
# which is what a Matrix bot on anthropic_claude produced the moment
# an Anthropic usage limit sent it down its `gpt-5.6-sol openai_codex`
# fallback mid-conversation.
#
# Detected from the history rather than tracked alongside it, because
# the wire that produced it is not always the session's own provider:
# once a fallback answers a turn, the session's primary is unchanged
# while its history belongs to the candidate that replied.
#
# "portable" means nothing wire-specific was found -- plain text, or no
# history at all -- and any provider can take it.
.history_shape <- function(history) {
    anthropic_blocks <- c("thinking", "redacted_thinking", "tool_use",
                          "tool_result")
    for (msg in history %||% list()) {
        if (!is.list(msg)) {
            next
        }
        if (identical(msg$type, ".openai_codex_output") ||
            identical(msg$type, "function_call_output")) {
            return("responses")
        }
        content <- msg$content
        if (is.list(content) && !inherits(content, "llm_content")) {
            for (b in content) {
                if (is.list(b) && is.character(b$type) &&
                    length(b$type) == 1L && b$type %in% anthropic_blocks) {
                    return("anthropic")
                }
            }
        }
    }
    "portable"
}

# Can this provider be handed a history in that shape? Chat-completions
# wires (moonshot, ollama, openai_compatible) accept neither vocabulary,
# so they are only reachable with a portable history.
.history_compatible <- function(shape, provider) {
    switch(shape, anthropic = provider %in% .anthropic_providers,
           responses = provider %in% c("openai", "openai_codex"), TRUE)
}

# Flatten provider-native history into alternating user/assistant text turns.
# Existing renderers already know Anthropic, Responses, and chat-completions
# shapes. Reasoning payloads are omitted; tool names and completed results are
# kept, which is the information a fallback needs to continue safely.

.fallback_portable_history <- function(history) {
    out <- list()
    for (entry in history %||% list()) {
        if (!is.list(entry)) {
            next
        }
        role <- if (identical(entry$type, ".openai_codex_output")) {
            "assistant"
        } else if (identical(entry$type, "function_call_output") ||
            identical(entry$role, "tool")) {
            "user"
        } else {
            entry$role %||% ""
        }
        if (!role %in% c("user", "assistant")) {
            next
        }
        body <- trimws(.compact_entry_body(entry))
        if (!nzchar(body)) {
            next
        }
        n <- length(out)
        if (n > 0L && identical(out[[n]]$role, role)) {
            out[[n]]$content <- paste(out[[n]]$content, body, sep = "\n\n")
        } else {
            out[[n + 1L]] <- list(role = role, content = body)
        }
    }
    out
}

# llm.api::agent() always appends its prompt to history. On a mid-run limit the
# current user prompt and completed tool results are already in the callback
# snapshot, so turn the final user entry into the continuation prompt rather
# than appending a duplicate or replaying the original request.
.fallback_resume_args <- function(args, history) {
    portable <- .fallback_portable_history(history)
    instruction <- paste(
                         "Continue the interrupted request from the completed tool results",
                         "included below. Do not repeat any completed tool call."
    )
    n <- length(portable)
    if (n > 0L && identical(portable[[n]]$role, "user")) {
        args$prompt <- paste(portable[[n]]$content, instruction, sep = "\n\n")
        portable <- portable[-n]
    } else {
        args$prompt <- instruction
    }
    args$history <- portable
    args
}

# Run llm.api::agent with agent_args, walking the session's fallback
# chain on limit errors. `.call` is the seam tests replace; production
# leaves it at the real agent.
.agent_with_fallback <- function(agent_args, session,
                                 .call = function(args) do.call(llm.api::agent, args)) {
    primary <- list(model = agent_args$model, provider = agent_args$provider)
    chain <- c(list(primary), .session_fallback(session))
    resume <- FALSE
    last_error <- NULL

    for (i in seq_along(chain)) {
        cand <- chain[[i]]
        if (.fallback_limited(cand$provider)) {
            next
        }
        args <- agent_args
        if (resume) {
            history <- session$history
        } else {
            history <- args$history
        }
        shape <- .history_shape(history)
        if (resume) {
            args <- .fallback_resume_args(args, history)
            message(sprintf("turn: resuming interrupted run on %s/%s",
                            cand$provider, cand$model))
        } else if (!.history_compatible(shape, cand$provider)) {
            args$history <- .fallback_portable_history(history)
            message(sprintf("turn: bridged %s-shaped history for %s/%s",
                            shape, cand$provider, cand$model))
        }
        args$model <- cand$model
        args$provider <- cand$provider
        if (!is.null(args$web_search) &&
            !.web_search_supported(cand$provider)) {
            args$web_search <- NULL
        }
        # Same reason as web_search above: these were gated for the
        # primary's wire, and this loop just rewrote the provider under
        # them. A reasoning_effort meant for codex is an unknown body
        # field on the anthropic wire (400), and a 400 is not a limit
        # error -- so leaving it in would make the fallback fail harder
        # than no fallback at all.
        args <- .gate_reasoning_args(args, cand$provider)

        before <- length(session$history %||% list())
        result <- tryCatch(.call(args), error = function(e) e)
        if (!inherits(result, "error")) {
            route <- .fallback_route(cand, i)
            result$corteza_route <- route
            session$last_route <- route
            if (i > 1L) {
                message(sprintf("turn: %s/%s answered for %s/%s (limit cooldown)",
                                cand$provider, cand$model,
                                primary$provider, primary$model))
            }
            if (isTRUE(route$fallback) && isTRUE(route$api_key)) {
                message(sprintf(
                                "turn: !!! PAID API KEY FALLBACK !!! %s/%s",
                                cand$provider, cand$model
                    ))
            }
            return(result)
        }
        if (!.is_limit_error(result)) {
            stop(result)
        }

        deadline <- .fallback_deadline(session, primary = i == 1L)
        .fallback_mark_until(cand$provider, deadline)
        message(sprintf("turn: %s/%s hit a limit (%s); skipping %s until %s",
                        cand$provider, cand$model,
                        substr(conditionMessage(result), 1L, 120L),
                        cand$provider,
                        format(deadline, "%a %Y-%m-%d %H:%M %Z")))
        progressed <- length(session$history %||% list()) > before
        if (progressed) {
            resume <- TRUE
        }
        last_error <- result
    }

    if (is.null(last_error)) {
        cooling <- vapply(chain, function(cand) {
            until <- .fallback_until(cand$provider)
            sprintf("%s until %s", cand$provider,
                if (is.null(until)) "?" else format(until, "%H:%M"))
        }, character(1))
        stop("every provider is in a limit cooldown: ",
             paste(unique(cooling), collapse = ", "), call. = FALSE)
    }
    stop(last_error)
}
