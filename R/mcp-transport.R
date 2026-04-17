# MCP Transport Layers
# Stdio and socket transports for MCP server

#' Run MCP server with stdio transport
#'
#' Used by Claude Desktop and other MCP clients that communicate via stdin/stdout.
#'
#' @return NULL (runs until client disconnects)
#' @noRd
run_stdio <- function() {
    log_msg("corteza MCP server starting (stdio)...")

    send_fn <- function(json) {
        cat(json, "\n", sep = "", file = stdout())
        flush(stdout())
    }

    while (TRUE) {
        line <- readLines(stdin(), n = 1, warn = FALSE)
        if (length(line) == 0) {
            log_msg("Client disconnected")
            break
        }
        process_request(line, send_fn)
    }

    log_msg("Server stopped")
}

#' Run MCP server with socket transport
#'
#' Used by the corteza CLI and other R clients that connect via TCP socket.
#'
#' @param port Port number to listen on
#' @return NULL (runs until interrupted)
#' @noRd
run_socket <- function(port) {
    log_msg(sprintf("corteza MCP server starting (socket port %d)...", port))

    # Create server socket
    server <- serverSocket(port)
    on.exit(close(server))

    log_msg("Listening on port", port)

    while (TRUE) {
        # Accept client connection
        client <- tryCatch(
                           socketAccept(server, blocking = TRUE, open = "r+b",
                                        timeout = 86400),
                           error = function(e) NULL
        )

        if (is.null(client)) {
            log_msg("Accept failed, retrying...")
            next
        }

        log_msg("Client connected")

        send_fn <- function(json) {
            tryCatch(
                     writeLines(json, client),
                     error = function(e) log_msg("Send error:", e$message)
            )
        }

        # Handle client requests
        tryCatch({
            while (TRUE) {
                line <- readLines(client, n = 1, warn = FALSE)
                if (length(line) == 0) {
                    log_msg("Client disconnected")
                    break
                }
                process_request(line, send_fn)
            }
        }, error = function(e) {
            log_msg("Client error:", e$message)
        })

        tryCatch(close(client), error = function(e) NULL)
    }
}

