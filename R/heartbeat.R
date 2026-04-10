# Heartbeat Reminders
# Event-driven behavioral nudges injected as user-role messages.
#
# The system prompt fades during long conversations. These reminders
# pull critical instructions back into the model's attention window
# by appearing as recent user messages.
#
# Detectors check runtime conditions (failure streaks, doom loops,
# token pressure, turn count). When triggered, a reminder is injected
# into conversation history before the next LLM call.
#
# Suppression: if a reminder fires 3 times with no behavior change,
# it stops firing to avoid noise.

.heartbeat <- new.env(parent = emptyenv())

# Lifecycle ----

#' Initialize heartbeat state
#'
#' @param config Config list from load_config()
#' @return Invisible NULL
#' @noRd
hb_init <- function(config = list()) {
    hb_cfg <- config$heartbeat %||% list()

    .heartbeat$enabled <- hb_cfg$enabled %||% TRUE
    .heartbeat$failure_threshold <- hb_cfg$failure_threshold %||% 3L
    .heartbeat$doom_threshold <- hb_cfg$doom_threshold %||% 2L
    .heartbeat$context_warn_pct <- hb_cfg$context_warn_pct %||% 80
    .heartbeat$periodic_interval <- hb_cfg$periodic_interval %||% 15L

    hb_reset()
    invisible(NULL)
}

#' Reset heartbeat tracking state
#'
#' @return Invisible NULL
#' @noRd
hb_reset <- function() {
    .heartbeat$tool_history <- list()
    .heartbeat$consecutive_failures <- 0L
    .heartbeat$turn_count <- 0L
    .heartbeat$last_periodic_turn <- 0L

    # Suppression counters: detector_name -> list(fired, ignored)
    .heartbeat$suppression <- list()

    invisible(NULL)
}

# Recording ----

#' Record a tool call outcome
#'
#' @param name Tool name
#' @param args Tool arguments (list)
#' @param result_text Tool result text
#' @param success Logical
#' @return Invisible NULL
#' @noRd
hb_record_tool <- function(name, args, result_text, success) {
    if (!isTRUE(.heartbeat$enabled)) {
        return(invisible(NULL))
    }

    entry <- list(
                  name = name,
                  args_hash = hb_hash_args(name, args),
                  success = success,
                  timestamp = Sys.time()
    )

    .heartbeat$tool_history <- c(.heartbeat$tool_history, list(entry))

    # Update consecutive failure count
    if (isTRUE(success)) {
        .heartbeat$consecutive_failures <- 0L
    } else {
        .heartbeat$consecutive_failures <-
        .heartbeat$consecutive_failures + 1L
    }

    invisible(NULL)
}

#' Record a completed turn
#'
#' Call after each assistant response.
#'
#' @return Invisible turn count
#' @noRd
hb_record_turn <- function() {
    .heartbeat$turn_count <- .heartbeat$turn_count + 1L
    invisible(.heartbeat$turn_count)
}

# Detection ----

#' Check all detectors and return a reminder (or NULL)
#'
#' @param token_pct Current token usage as percentage (0-100)
#' @param project_rules Character string of key project rules to reinforce
#'   (from AGENTS.md / SOUL.md), or NULL
#' @return Character string reminder to inject, or NULL
#' @noRd
hb_check <- function(token_pct = 0, project_rules = NULL) {
    if (!isTRUE(.heartbeat$enabled)) {
        return(NULL)
    }

    # Check detectors in priority order (most urgent first)
    reminder <- hb_detect_doom_loop()
    if (!is.null(reminder)) {
        return(reminder)
    }

    reminder <- hb_detect_failure_streak()
    if (!is.null(reminder)) {
        return(reminder)
    }

    reminder <- hb_detect_high_context(token_pct)
    if (!is.null(reminder)) {
        return(reminder)
    }

    reminder <- hb_detect_periodic(project_rules)
    if (!is.null(reminder)) {
        return(reminder)
    }

    NULL
}

#' Detect consecutive tool failures
#'
#' @return Reminder text or NULL
#' @noRd
hb_detect_failure_streak <- function() {
    threshold <- .heartbeat$failure_threshold %||% 3L
    if (.heartbeat$consecutive_failures < threshold) {
        return(NULL)
    }

    hb_fire("failure_streak", paste0(
                                     "[Reminder] ", .heartbeat$consecutive_failures,
                                     " consecutive tool failures. Step back and reconsider:\n",
                                     "- Check file paths with list_files before file operations\n",
                                     "- Read error messages carefully for the actual cause\n",
                                     "- Try a different approach instead of retrying the same thing"
        ))
}

#' Detect doom loop (same tool+args repeated)
#'
#' @return Reminder text or NULL
#' @noRd
hb_detect_doom_loop <- function() {
    threshold <- .heartbeat$doom_threshold %||% 2L
    history <- .heartbeat$tool_history
    n <- length(history)
    if (n < threshold) {
        return(NULL)
    }

    # Check last N entries for identical tool+args
    recent <- tail(history, threshold)
    hashes <- vapply(recent, function(e) e$args_hash, character(1))

    if (length(unique(hashes)) == 1L) {
        tool_name <- recent[[1]]$name
        hb_fire("doom_loop", paste0(
                                    "[Reminder] You've called ", tool_name, " ", threshold,
                                    " times with the same arguments. This looks like a loop.\n",
                                    "Stop and try a different approach. If the tool keeps failing,\n",
                                    "the problem is upstream of the tool call."
            ))
    } else {
        NULL
    }
}

#' Detect high context usage
#'
#' @param token_pct Token usage percentage
#' @return Reminder text or NULL
#' @noRd
hb_detect_high_context <- function(token_pct) {
    warn_pct <- .heartbeat$context_warn_pct %||% 80
    if (token_pct < warn_pct) {
        return(NULL)
    }

    hb_fire("high_context", paste0(
                                   "[Reminder] Context window is ", round(token_pct),
                                   "% full. Be concise:\n",
                                   "- Avoid reading entire files when you can search\n",
                                   "- Keep tool outputs focused\n",
                                   "- Wrap up the current task soon"
        ))
}

#' Periodic reinforcement of key instructions
#'
#' Fires every N turns to reinforce project rules.
#'
#' @param project_rules Character string of key rules, or NULL
#' @return Reminder text or NULL
#' @noRd
hb_detect_periodic <- function(project_rules = NULL) {
    interval <- .heartbeat$periodic_interval %||% 15L
    turn <- .heartbeat$turn_count
    last <- .heartbeat$last_periodic_turn %||% 0L

    if (turn - last < interval) {
        return(NULL)
    }

    # Only fire if there are project rules to reinforce
    if (is.null(project_rules) || nchar(project_rules) == 0) {
        return(NULL)
    }

    .heartbeat$last_periodic_turn <- turn

    hb_fire("periodic", paste0(
                               "[Reminder] Key guidelines for this session:\n",
                               project_rules
        ))
}

# Suppression ----

#' Fire a reminder with suppression logic
#'
#' Tracks how many times each detector fires. If it fires 3 times
#' without the condition clearing, suppress further reminders.
#'
#' @param detector_name Character detector ID
#' @param text Reminder text
#' @return text if not suppressed, NULL if suppressed
#' @noRd
hb_fire <- function(detector_name, text) {
    sup <- .heartbeat$suppression[[detector_name]]
    if (is.null(sup)) {
        sup <- list(fired = 0L, suppressed = FALSE)
    }

    if (isTRUE(sup$suppressed)) {
        return(NULL)
    }

    sup$fired <- sup$fired + 1L

    if (sup$fired >= 3L) {
        sup$suppressed <- TRUE
    }

    .heartbeat$suppression[[detector_name]] <- sup
    text
}

#' Clear suppression for a detector
#'
#' Call when the condition clears (e.g., a tool succeeds after failures).
#'
#' @param detector_name Character detector ID
#' @return Invisible NULL
#' @noRd
hb_clear_suppression <- function(detector_name) {
    .heartbeat$suppression[[detector_name]] <- NULL
    invisible(NULL)
}

# Utility ----

#' Hash tool name + args for doom loop detection
#'
#' @param name Tool name
#' @param args Tool arguments (list)
#' @return Character hash string
#' @noRd
hb_hash_args <- function(name, args) {
    # Simple deterministic hash: tool name + sorted arg values
    arg_str <- if (is.null(args) || length(args) == 0) {
        ""
    } else {
        paste(sort(paste(names(args), args, sep = "=")), collapse = "|")
    }
    paste(name, arg_str, sep = "::")
}

#' Get heartbeat diagnostics
#'
#' @return Named list of current state
#' @noRd
hb_status <- function() {
    list(
         enabled = .heartbeat$enabled %||% FALSE,
         turn_count = .heartbeat$turn_count %||% 0L,
         consecutive_failures = .heartbeat$consecutive_failures %||% 0L,
         tool_history_length = length(.heartbeat$tool_history %||% list()),
         suppression = .heartbeat$suppression %||% list()
    )
}

