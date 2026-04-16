# Utility functions for llamaR
# Internal helpers used across the package

#' llamaR: AI Agent Runtime in R
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

