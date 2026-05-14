# Utility functions for corteza
# Internal helpers used across the package

#' corteza: AI Agent Runtime in R
#'
#' @importFrom utils capture.output head installed.packages object.size packageVersion str tail
#' @importFrom stats setNames
#' @importFrom codetools findGlobals
#' @importFrom llm.api mcp_connect mcp_close mcp_call
#' @keywords internal
"_PACKAGE"

#' Create successful MCP tool response
#' @param text Character string to return
#' @return List formatted as MCP tool result
#' @noRd
ok <- function(text) {
    list(content = list(list(type = "text", text = text)))
}

#' Successful tool response with an attached human-facing diff payload.
#'
#' Thin builder used only by write-side tools (`write_file`,
#' `replace_in_file`). The base MCP result shape is unchanged so
#' external clients see the same `content` they always did; the extra
#' `diff` field is read by corteza's own CLI / chat display layer for
#' inline diff rendering. We deliberately keep `ok()` itself unaware of
#' diffs so the extension doesn't bleed into every tool author's
#' mental model of what a "successful" result looks like.
#' @param text LLM-facing summary string.
#' @param diff Diff payload from \code{compute_unified_diff()}, or NULL
#'   to skip the field entirely (callers can pass through whatever
#'   \code{compute_unified_diff()} returned without branching).
#' @return List formatted as MCP tool result with optional `diff` field.
#' @noRd
ok_with_diff <- function(text, diff = NULL) {
    res <- ok(text)
    if (!is.null(diff)) {
        res$diff <- diff
    }
    res
}

#' Create error MCP tool response
#' @param text Error message
#' @return List formatted as MCP error result
#' @noRd
err <- function(text) {
    list(isError = TRUE, content = list(list(type = "text", text = text)))
}

#' Is corteza in verbose mode?
#'
#' Gates user-facing console writes throughout the package. Default:
#' TRUE in interactive sessions, FALSE otherwise, so R CMD check and
#' library() users get silent behavior unless they opt in.
#' @noRd
.corteza_verbose <- function() {
    isTRUE(getOption("corteza.verbose", interactive()))
}

#' Log message to stderr (verbose-gated)
#' @param ... Messages to log
#' @noRd
log_msg <- function(...) {
    if (.corteza_verbose()) {
        cat(..., "\n", file = stderr())
    }
}

