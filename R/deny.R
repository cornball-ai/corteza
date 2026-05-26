# User-deny condition.
#
# When a user picks option "3. Deny" at the tool-approval prompt, we want
# to abort the entire turn (not just decline this single tool call). The
# old behavior returned FALSE from approval_cb, which the turn loop fed
# back to the LLM as a [user declined: ...] tool result. The LLM then
# planned the next tool call -- and again -- and again, forcing the user
# to mash "3" through cascades of dependent calls.
#
# Fix: raise a typed condition that the chat()/CLI outer tryCatch can
# catch separately from a Ctrl+C interrupt or an unhandled error. The
# handler writes a marker into history that names the denied tool and
# tells the LLM to stop and ask the user what to do instead, so the next
# turn picks up with a real human decision instead of another tool call.

#' Build a condition object representing "user denied this tool call".
#'
#' Raised by [chat_approval_cb()] (and the CLI's `cli_approval_cb`) when
#' the user picks "3. Deny". The class deliberately excludes `"error"`
#' so the defensive `tryCatch(error = function(e) FALSE)` wrapper around
#' approval_cb in [.make_tool_handler()] does not swallow it. The
#' `"interrupt"` class lets the existing chat()/CLI interrupt-marker
#' machinery fall through cleanly if a surface forgets to register a
#' `corteza_user_deny` handler.
#'
#' @param tool Character. Name of the denied tool (for the history
#'   marker). Defaults to `"?"` when unavailable.
#' @return A condition object with class
#'   `c("corteza_user_deny", "interrupt", "condition")`.
#' @keywords internal
user_deny_condition <- function(tool = "?") {
    if (length(tool) && nzchar(tool)) {
        tool_str <- as.character(tool)[1]
    } else {
        tool_str <- "?"
    }
    structure(
              class = c("corteza_user_deny", "interrupt", "condition"),
              list(message = sprintf("User denied tool use: %s", tool_str),
                   tool = tool_str, call = NULL)
    )
}

# Shared directive for the user-abort markers (deny, interrupt). Kept in
# one place so both paths give the LLM the same "stop and check in"
# instruction on the next turn instead of one being more directive than
# the other.
.user_abort_directive <- paste0(
                                "Stop and ask the user what to do instead -- do not retry or plan ",
                                "a workaround.")

#' History marker written when a turn is aborted by a user deny.
#'
#' Format chosen so the LLM, reading the marker on the next turn, knows
#' to stop and ask the user how to proceed instead of retrying the same
#' tool or planning a workaround.
#' @param tool Character. Name of the denied tool.
#' @return Character scalar.
#' @keywords internal
user_deny_marker <- function(tool = "?") {
    sprintf("[User denied tool use: %s. %s]",
        if (length(tool) && nzchar(tool)) as.character(tool)[1] else "?",
            .user_abort_directive)
}

#' History marker written when a turn is interrupted (Ctrl+C / Esc).
#'
#' Carries the same "stop and ask the user" directive as
#' [user_deny_marker()] so an interrupt and a deny leave the LLM with
#' the same next-turn instruction -- matching the (Esc)/(Ctrl+C) hint on
#' the approval prompt, which converges on this interrupt path.
#' @return Character scalar.
#' @keywords internal
user_interrupt_marker <- function() {
    sprintf("[Interrupted by user before completing. %s]",
            .user_abort_directive)
}

