# When a session compacts: the numbers, read in one place.
#
# Three things start a compaction ahead of a request:
#   - the context reaches a share of the model's window
#     (`context_compact_pct`);
#   - it reaches an absolute size (`context_compact_tokens`), which is
#     what decides on a model with a 1M window;
#   - a session comes back after an idle gap with a large context
#     (`context_idle_minutes`, `context_idle_tokens`).
# maybe_compact_turn_session() decides with these helpers and /context
# prints what context_trigger() returns, so the two cannot differ.
#
# Every count here is corteza's own estimate of the context, about 1
# token per 4 characters (R/context-budget.R). A provider counts with
# its own tokenizer and can count more, so each number is a size by
# corteza's count and not the size a provider bills.

CONTEXT_COMPACT_TOKENS <- 200000L
CONTEXT_IDLE_MINUTES <- 60L
CONTEXT_IDLE_TOKENS <- 100000L

# A count from a config value: one finite number at or above 0, else
# the default. A config file is typed by hand, and a string or a
# negative number there falls back instead of stopping a turn.
.config_count <- function(value, default) {
    if (is.numeric(value) && length(value) == 1L && !is.na(value) &&
        is.finite(value) && value >= 0) {
        return(as.numeric(value))
    }
    as.numeric(default)
}

# The cap on the trigger, or NULL when the config turns it off with 0.
.compact_cap <- function(config) {
    cap <- .config_count(config$context_compact_tokens, CONTEXT_COMPACT_TOKENS)
    if (cap > 0) {
        cap
    } else {
        NULL
    }
}

#' The compaction trigger in force for a session.
#'
#' @param session A session environment from new_session().
#' @param config Full corteza config.
#' @param model Model name. NULL resolves the one the session's next
#'   request names.
#' @param pct Percentage of the window. NULL takes the session's own:
#'   `context_compact_pct`, or a subagent's stricter one. A value that
#'   is not finite means automatic compaction is off, and the cap and
#'   the idle trigger with it.
#' @return A list: `window` (tokens) and its `source` ("session",
#'   "config", "table" or "fallback"), `pct`, `cap` (NULL when it does
#'   not apply), `tokens` (the estimated context at which a compaction
#'   starts; Inf when none can), `rule` ("pct", "cap" or "off"),
#'   `effective_pct` (`tokens` as a share of the window; 100 when the
#'   rule is "off"), and `idle_minutes` and `idle_tokens`
#'   (`idle_minutes` 0 when the idle trigger is off).
#' @noRd
context_trigger <- function(session, config, model = NULL, pct = NULL) {
    if (is.null(model)) {
        model <- session$model_map$cloud %||%
        default_provider_model(session$provider)
    }
    window <- session$context_window
    source <- "session"
    if (is.null(window)) {
        window <- config$context_window
        source <- "config"
    }
    if (is.null(window)) {
        window <- .context_limit_row(model, session$provider)
        source <- "table"
    }
    if (is.null(window)) {
        window <- CONTEXT_LIMIT_FALLBACK
        source <- "fallback"
    }
    if (is.null(pct)) {
        pct <- if (isTRUE(session$is_subagent)) {
            subagent_compact_threshold(config)
        } else {
            as.numeric(config$context_compact_pct %||% 90L)
        }
    }
    # A session that edits its own context is never compacted
    # (maybe_compact_turn_session()), whatever its numbers say.
    sized <- !is.null(pct) && is.finite(pct) && !.context_edit_pure(session)
    if (sized && window > 0) {
        by_pct <- pct / 100 * window
    } else {
        by_pct <- Inf
    }
    cap <- if (sized) .compact_cap(config)
    tokens <- min(by_pct, cap %||% Inf)
    rule <- if (!is.finite(tokens)) {
        "off"
    } else if (!is.null(cap) && cap < by_pct) {
        "cap"
    } else {
        "pct"
    }
    list(window = window, source = source, pct = pct, cap = cap,
         tokens = tokens, rule = rule,
         effective_pct = if (is.finite(tokens) && window > 0) {
            100 * tokens / window
        } else {
            100
        },
         idle_minutes = .config_count(session$context_idle_minutes %||%
                                      config$context_idle_minutes,
                                      CONTEXT_IDLE_MINUTES),
         idle_tokens = .config_count(config$context_idle_tokens,
                                     CONTEXT_IDLE_TOKENS))
}

# How long a provider keeps this session's prompt cache, in minutes. 0
# for a session that sets none.
.cache_minutes <- function(session) {
    cache <- tryCatch(.session_cache(session), error = function(e) NULL)
    switch(cache %||% "none", `5m` = 5, `1h` = 60, 0)
}

# When the model last answered this session: the time turn() recorded,
# or for a session this process has not yet run, the last change its
# journal held when the process took it over.
.session_last_response <- function(session) {
    at <- session$last_response_at
    if (inherits(at, "POSIXct") && length(at) == 1L && !is.na(at)) {
        return(at)
    }
    journal_previous_update(session)
}

# Minutes this session has sat idle, when that calls for a compaction
# ahead of its next request; NULL otherwise. All 3 must hold: the gap
# is at least `context_idle_minutes`, it is longer than the prompt
# cache lives, and the context is at least `context_idle_tokens`. The
# cache has then expired, so the next request pays full price for the
# whole history either way; paid to the summarizer, it makes every
# later request smaller.
.compact_idle_gap <- function(session, trigger, used, now = Sys.time()) {
    if (trigger$idle_minutes <= 0 || used < trigger$idle_tokens) {
        return(NULL)
    }
    last <- .session_last_response(session)
    if (is.null(last)) {
        return(NULL)
    }
    gap <- as.numeric(difftime(now, last, units = "mins"))
    if (!is.finite(gap) || gap < trigger$idle_minutes ||
        gap <= .cache_minutes(session)) {
        return(NULL)
    }
    gap
}

# What /context prints under its bar: where compaction starts and why
# there, one short line for each thing that applies.
.context_trigger_lines <- function(trigger) {
    size <- if (identical(trigger$rule, "off")) {
        "Automatic compaction is off for this session."
    } else if (identical(trigger$rule, "cap")) {
        sprintf("Compacts at %s tokens (context_compact_tokens).",
                format_tokens(trigger$tokens))
    } else {
        sprintf("Compacts at %s tokens (%s%% of the window).",
                format_tokens(trigger$tokens),
                format(trigger$pct, trim = TRUE))
    }
    # Named only where it can start a compaction the size rule would not.
    idle <- if (trigger$idle_minutes > 0 && !identical(trigger$rule, "off") &&
        trigger$idle_tokens < trigger$tokens) {
        sprintf("After %s minutes idle, compacts at %s tokens.",
                format(trigger$idle_minutes, trim = TRUE),
                format_tokens(trigger$idle_tokens))
    }
    window <- switch(trigger$source,
                     session = "Window set on the session.",
                     config = "Window from context_window.",
                     fallback = sprintf(
                                        "No window known for this model: %s assumed.",
                                        format_tokens(CONTEXT_LIMIT_FALLBACK)))
    c(size, idle, window)
}
