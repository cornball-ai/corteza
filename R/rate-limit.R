# Rate Limiting
# Tracks API usage and enforces spending limits

# Package-level state for rate tracking
.rate_state <- new.env(parent = emptyenv())

#' Initialize rate limit tracking
#'
#' @param config Config list with rate_limits section
#' @return Invisible NULL
#' @noRd
rate_limit_init <- function(config = list()) {
    .rate_state$limits <- config$rate_limits %||% list()
    .rate_state$usage <- list()
    .rate_state$window_start <- list()
    invisible(NULL)
}

#' Get rate limits for a provider
#'
#' @param provider Provider name
#' @return List with tokens_per_hour, requests_per_minute, or NULL if no limits
#' @noRd
get_rate_limits <- function(provider) {
    limits <- .rate_state$limits
    if (is.null(limits) || is.null(limits[[provider]])) {
        return(NULL)
    }
    limits[[provider]]
}

#' Track API usage
#'
#' Records token/request usage for rate limit enforcement.
#'
#' @param provider Provider name
#' @param tokens Number of tokens used
#' @param requests Number of requests (default: 1)
#' @return Invisible NULL
#' @noRd
track_usage <- function(provider, tokens, requests = 1L) {
    now <- Sys.time()

    # Initialize provider tracking if needed
    if (is.null(.rate_state$usage[[provider]])) {
        .rate_state$usage[[provider]] <- list(
            tokens_hour = 0L,
            requests_minute = 0L
        )
        .rate_state$window_start[[provider]] <- list(hour = now, minute = now)
    }

    usage <- .rate_state$usage[[provider]]
    windows <- .rate_state$window_start[[provider]]

    # Reset hourly window if needed
    if (difftime(now, windows$hour, units = "hours") >= 1) {
        usage$tokens_hour <- 0L
        windows$hour <- now
    }

    # Reset minute window if needed
    if (difftime(now, windows$minute, units = "mins") >= 1) {
        usage$requests_minute <- 0L
        windows$minute <- now
    }

    # Add usage
    usage$tokens_hour <- usage$tokens_hour + tokens
    usage$requests_minute <- usage$requests_minute + requests

    .rate_state$usage[[provider]] <- usage
    .rate_state$window_start[[provider]] <- windows

    invisible(NULL)
}

#' Check rate limits before making a request
#'
#' @param provider Provider name
#' @param estimated_tokens Estimated tokens for the request (optional)
#' @return List with ok (logical), warning (character or NULL), message (character if blocked)
#' @noRd
check_rate_limit <- function(provider, estimated_tokens = 0L) {
    limits <- get_rate_limits(provider)

    # No limits configured
    if (is.null(limits)) {
        return(list(ok = TRUE, warning = NULL))
    }

    now <- Sys.time()
    usage <- .rate_state$usage[[provider]]
    windows <- .rate_state$window_start[[provider]]

    # Initialize if first check
    if (is.null(usage)) {
        return(list(ok = TRUE, warning = NULL))
    }

    # Check hourly token limit
    if (!is.null(limits$tokens_per_hour)) {
        current_tokens <- usage$tokens_hour
        limit <- limits$tokens_per_hour
        pct <- (current_tokens / limit) * 100

        if (pct >= 100) {
            time_remaining <- 60 - as.numeric(difftime(now, windows$hour,
                    units = "mins"))
            return(list(
                        ok = FALSE,
                        warning = NULL,
                        message = sprintf("Rate limit exceeded: %d/%d tokens this hour. Try again in %.0f minutes.",
                        current_tokens, limit, max(0, time_remaining))
                ))
        }

        if (pct >= 80) {
            return(list(
                        ok = TRUE,
                        warning = sprintf("Approaching token limit: %d/%d (%.0f%%)",
                        current_tokens, limit, pct)
                ))
        }
    }

    # Check requests per minute
    if (!is.null(limits$requests_per_minute)) {
        current_requests <- usage$requests_minute
        limit <- limits$requests_per_minute

        if (current_requests >= limit) {
            time_remaining <- 60 - as.numeric(difftime(now, windows$minute,
                    units = "secs"))
            return(list(
                        ok = FALSE,
                        warning = NULL,
                        message = sprintf("Rate limit exceeded: %d/%d requests this minute. Try again in %.0f seconds.",
                        current_requests, limit, max(0, time_remaining))
                ))
        }
    }

    list(ok = TRUE, warning = NULL)
}

#' Get current usage statistics
#'
#' @param provider Provider name (optional, returns all if NULL)
#' @return List of usage stats per provider
#' @noRd
get_usage_stats <- function(provider = NULL) {
    if (!is.null(provider)) {
        usage <- .rate_state$usage[[provider]]
        limits <- get_rate_limits(provider)
        windows <- .rate_state$window_start[[provider]]

        if (is.null(usage)) {
            return(list(
                        tokens_hour = 0L,
                        requests_minute = 0L,
                        limits = limits
                ))
        }

        list(
             tokens_hour = usage$tokens_hour,
             requests_minute = usage$requests_minute,
             limits = limits,
             window_start = windows
        )
    } else {
        providers <- names(.rate_state$usage)
        stats <- lapply(providers, get_usage_stats)
        names(stats) <- providers
        stats
    }
}

#' Format usage stats for display
#'
#' @param provider Provider name
#' @return Character string for display
#' @noRd
format_usage_stats <- function(provider) {
    stats <- get_usage_stats(provider)

    lines <- sprintf("Usage for %s:", provider)

    if (!is.null(stats$limits$tokens_per_hour)) {
        pct <- (stats$tokens_hour / stats$limits$tokens_per_hour) * 100
        lines <- c(lines, sprintf("  Tokens: %d / %d (%.1f%%)",
                                  stats$tokens_hour, stats$limits$tokens_per_hour, pct))
    } else {
        lines <- c(lines, sprintf("  Tokens: %d (no limit)", stats$tokens_hour))
    }

    if (!is.null(stats$limits$requests_per_minute)) {
        lines <- c(lines, sprintf("  Requests/min: %d / %d",
                                  stats$requests_minute, stats$limits$requests_per_minute))
    }

    paste(lines, collapse = "\n")
}

#' Reset usage tracking
#'
#' @param provider Provider name (optional, resets all if NULL)
#' @return Invisible NULL
#' @noRd
reset_usage <- function(provider = NULL) {
    if (!is.null(provider)) {
        .rate_state$usage[[provider]] <- NULL
        .rate_state$window_start[[provider]] <- NULL
    } else {
        .rate_state$usage <- list()
        .rate_state$window_start <- list()
    }
    invisible(NULL)
}

