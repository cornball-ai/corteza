# Structured Logging for corteza
# JSON-formatted logs to stderr for observability

# Log level enum
LOG_LEVELS <- c(debug = 1L, info = 2L, warn = 3L, error = 4L)

# Package-level log state
.log_state <- new.env(parent = emptyenv())
.log_state$level <- "info"
.log_state$session_id <- NULL
.log_state$enabled <- TRUE

#' Enable or disable logging
#'
#' @param enabled TRUE to enable, FALSE to disable
#' @return Previous state (invisible)
#' @noRd
set_log_enabled <- function(enabled) {
    old <- .log_state$enabled
    .log_state$enabled <- isTRUE(enabled)
    invisible(old)
}

#' Log a structured event
#'
#' Writes a JSON-formatted log entry to stderr.
#'
#' @param event Event name (e.g., "tool_call", "llm_request")
#' @param ... Named arguments to include in the log entry
#' @param level Log level: "debug", "info", "warn", "error"
#' @return Invisible NULL
#' @noRd
log_event <- function(event, ..., level = "info") {
    # Check if logging is enabled
    if (!isTRUE(.log_state$enabled)) {
        return(invisible(NULL))
    }

    # Check log level threshold
    current_level <- LOG_LEVELS[[.log_state$level]] %||% 2L
    event_level <- LOG_LEVELS[[level]] %||% 2L
    if (event_level < current_level) {
        return(invisible(NULL))
    }

    # Build log entry
    entry <- list(
                  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                  level = level,
                  event = event
    )

    # Add session ID if set
    if (!is.null(.log_state$session_id)) {
        entry$session_id <- .log_state$session_id
    }

    # Add additional fields
    extra <- list(...)
    for (name in names(extra)) {
        entry[[name]] <- extra[[name]]
    }

    # Write JSON to stderr
    json <- tryCatch(
                     jsonlite::toJSON(.redact_object(entry), auto_unbox = TRUE,
                                      null = "null"),
                     error = function(e) {
        # Fallback if JSON serialization fails
        sprintf('{"event":"%s","error":"json_serialization_failed"}', event)
    }
    )
    if (.corteza_verbose()) {
        cat(json, "\n", file = stderr())
    }

    invisible(NULL)
}

#' Log a tool call
#'
#' Convenience wrapper for logging tool/skill invocations.
#'
#' @param tool Tool name
#' @param args Tool arguments (will be truncated for large values)
#' @param level Log level
#' @return Invisible NULL
#' @noRd
log_tool_call <- function(tool, args = list(), level = "info") {
    # Truncate large argument values
    args_summary <- lapply(args, function(x) {
        if (is.character(x) && nchar(x) > 100) {
            paste0(substr(x, 1, 97), "...")
        } else {
            x
        }
    })

    log_event("tool_call", tool = tool, args = args_summary, level = level)
}

#' Log a tool result
#'
#' Convenience wrapper for logging tool/skill results.
#'
#' @param tool Tool name
#' @param success TRUE if successful
#' @param result_lines Number of lines in result (for large outputs)
#' @param elapsed_ms Execution time in milliseconds
#' @param level Log level
#' @return Invisible NULL
#' @noRd
log_tool_result <- function(tool, success, result_lines = NULL,
                            elapsed_ms = NULL, level = "info") {
    log_event("tool_result", tool = tool, success = success,
              result_lines = result_lines, elapsed_ms = elapsed_ms,
              level = level)
}

#' Log an error
#'
#' @param message Error message
#' @param error_type Error type/class
#' @param ... Additional context
#' @return Invisible NULL
#' @noRd
log_error <- function(message, error_type = NULL, ...) {
    log_event("error", message = message, error_type = error_type, ...,
              level = "error")
}
