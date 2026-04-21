# Boundary-normalized error condition for tool dispatch.
#
# When a tool fails inside the CLI worker session, we want the CLI side
# to see a specific condition class rather than whatever R-level error
# the tool body happened to raise. That lets the CLI format tool errors
# uniformly for the LLM without string-matching message text.
#
# skill_run() / call_tool() already convert most tool failures into
# `err()` envelopes (list(isError = TRUE, content = ...)), so this class
# is mainly for the unusual case where dispatch itself fails: unknown
# tool name, malformed args, unexpected condition leaking out of a tool.

#' Construct a normalized tool-dispatch error.
#'
#' @param tool Tool name.
#' @param args Arguments the tool was called with.
#' @param message Human-readable message.
#' @param original Optional original condition to wrap.
#' @return A `corteza_tool_error` condition ready for `stop()`.
#' @noRd
make_tool_error <- function(tool, args, message, original = NULL) {
    structure(
        class = c("corteza_tool_error", "error", "condition"),
        list(
            message = message,
            call = NULL,
            tool = tool,
            args = args,
            original_class = if (!is.null(original)) class(original) else NULL,
            original_message = if (!is.null(original)) conditionMessage(original) else NULL
        )
    )
}
