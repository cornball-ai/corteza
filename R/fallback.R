# Provider fallback on rate and usage limits.
#
# A session names one provider/model pair. The `fallback` config key
# (a character vector of "model provider" specs, the same shape as the
# /model menu's `models`) names what turn() tries instead when that
# pair refuses the request with a limit error: llm.api's "API error
# (429|503|529)", or a body naming a rate, usage, or quota limit. Any
# other error belongs to the caller and is rethrown untouched.
#
# A provider that hit a limit is skipped for a cooldown (default 30
# minutes) by every session in the process, because the limit belongs
# to the account, not to the room that tripped it. Nothing is mutated on
# the session: once the cooldown lapses the next turn tries the primary
# again, so a subscription that reset overnight is picked back up
# without a restart.
#
# The retry is only taken when the failed attempt made no progress. A
# limit hit part-way through a tool-using run leaves that run's tool
# calls already executed; replaying the prompt on another provider
# would run them again. That case marks the cooldown and rethrows, so
# the room sees the error once and the next message goes to the
# fallback.

.fallback_state <- new.env(parent = emptyenv())

.FALLBACK_COOLDOWN_MINUTES <- 30

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

# Run llm.api::agent with agent_args, walking the session's fallback
# chain on limit errors. `.call` is the seam tests replace; production
# leaves it at the real agent.
.agent_with_fallback <- function(agent_args, session,
                                 .call = function(args) do.call(llm.api::agent, args)) {
    primary <- list(model = agent_args$model, provider = agent_args$provider)
    chain <- c(list(primary), .session_fallback(session))
    minutes <- .fallback_cooldown(session)
    last_error <- NULL

    for (i in seq_along(chain)) {
        cand <- chain[[i]]
        if (.fallback_limited(cand$provider)) {
            next
        }
        args <- agent_args
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
            if (i > 1L) {
                message(sprintf("turn: %s/%s answered for %s/%s (limit cooldown)",
                                cand$provider, cand$model,
                                primary$provider, primary$model))
            }
            return(result)
        }
        if (!.is_limit_error(result)) {
            stop(result)
        }

        .fallback_mark(cand$provider, minutes)
        message(sprintf("turn: %s/%s hit a limit (%s); skipping %s for %s min",
                        cand$provider, cand$model,
                        substr(conditionMessage(result), 1L, 120L),
                        cand$provider, format(minutes)))
        progressed <- length(session$history %||% list()) > before
        if (progressed) {
            # Tool calls from this run already happened once. Surface
            # the error rather than replay them on another provider.
            stop(result)
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
