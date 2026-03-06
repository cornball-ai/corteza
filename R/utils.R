# Utility functions for llamaR
# Internal helpers used across the package

#' Create successful MCP tool response
#' @param text Character string to return
#' @return List formatted as MCP tool result
#' @noRd
ok <- function(text) {
    list(content = list(list(type = "text", text = text)))
}

#' Create error MCP tool response
#' @param text Error message
#' @return List formatted as MCP error result
#' @noRd
err <- function(text) {
    list(isError = TRUE, content = list(list(type = "text", text = text)))
}

#' Log message to stderr
#' @param ... Messages to log
#' @noRd
log_msg <- function(...) {
    cat(..., "\n", file = stderr())
}

