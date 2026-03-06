# MCP JSON-RPC Handler
# Handles MCP protocol requests and dispatches to tools

#' Handle an MCP JSON-RPC request
#' @param req Parsed JSON-RPC request
#' @return JSON-RPC response or NULL for notifications
#' @noRd
handle_request <- function(req) {
    method <- req$method
    id <- req$id
    params <- req$params %||% list()

    result <- tryCatch({
        switch(method,
               "initialize" = list(
                                   protocolVersion = "2024-11-05",
                                   capabilities = list(tools = list()),
                                   serverInfo = list(name = "llamar-mcp",
                    version = as.character(packageVersion("llamaR")))
            ),

               "notifications/initialized" = NULL, # No response for notifications

               "tools/list" = list(tools = get_tools(getOption("llamar.tools"))),

               "tools/call" = call_tool(params$name, params$arguments),

               # Default: method not found
               list(.error = list(code = -32601,
                                  message = paste("Method not found:", method)))
        )
    }, error = function(e) {
        log_error(e$message, error_type = "handler_error", method = method)
        err(paste("Internal error:", e$message))
    })

    # Notifications don't get responses
    if (is.null(result)) {
        return(NULL)
    }

    # Build response
    if (!is.null(result$.error)) {
        list(jsonrpc = "2.0", id = id, error = result$.error)
    } else {
        list(jsonrpc = "2.0", id = id, result = result)
    }
}

#' Process a single JSON-RPC request line
#' @param line Raw JSON string
#' @param send_fn Function to send response
#' @return TRUE (always continues)
#' @noRd
process_request <- function(line, send_fn) {
    # Skip empty lines
    if (nchar(trimws(line)) == 0) {
        return(TRUE)
    }

    # Parse JSON-RPC request
    req <- tryCatch(
                    jsonlite::fromJSON(line, simplifyVector = FALSE),
                    error = function(e) NULL
    )

    if (is.null(req)) {
        log_msg("Invalid JSON received")
        return(TRUE)
    }

    log_msg("Received:", req$method)

    # Handle and respond
    response <- handle_request(req)
    if (!is.null(response)) {
        tryCatch({
            json <- jsonlite::toJSON(response, auto_unbox = TRUE, null = "null")
            send_fn(json)
        }, error = function(e) {
            log_error(e$message, error_type = "send_error", method = req$method)
        })
    }

    TRUE
}

