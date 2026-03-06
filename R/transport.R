# Transport Abstraction Layer
# Provides a common interface for different messaging channels

#' Create a new transport
#'
#' @param type Transport type: "terminal", "signal"
#' @param config Transport-specific configuration
#' @return Transport object (list with send/receive/close methods)
#' @noRd
transport_new <- function(type, config = list()) {
    switch(type,
           terminal = transport_terminal(config),
           signal = transport_signal(config),
           stop("Unknown transport type: ", type)
    )
}

#' Normalize a message to internal format
#'
#' All transports convert to/from this format:
#' - id: Unique message ID
#' - channel: Transport type (terminal, signal, etc.)
#' - sender: Sender identifier
#' - text: Message text
#' - attachments: List of attachment objects (path, contentType, size, etc.)
#' - timestamp: POSIXct timestamp
#' - metadata: Transport-specific data
#'
#' @param text Message text
#' @param sender Sender identifier
#' @param channel Channel name
#' @param id Optional message ID
#' @param attachments Optional list of attachments
#' @param metadata Optional metadata list
#' @return Normalized message list
#' @noRd
message_normalize <- function(text, sender, channel, id = NULL,
                              attachments = list(), metadata = list()) {
    list(
         id = id %||% paste0(channel, "_",
                             format(as.numeric(Sys.time()) * 1000, scientific = FALSE)),
         channel = channel,
         sender = sender,
         text = text,
         attachments = attachments,
         timestamp = Sys.time(),
         metadata = metadata
    )
}

#' Terminal transport (REPL)
#'
#' Simple stdin/stdout transport for interactive use.
#'
#' @param config Configuration (unused for terminal)
#' @return Transport object
#' @noRd
transport_terminal <- function(config = list()) {
    list(
         type = "terminal",

         # Read one message from stdin
         receive = function() {
        line <- readline("> ")
        if (nchar(line) == 0) return(NULL)
        message_normalize(text = line, sender = "user", channel = "terminal")
    },

         # Print response to stdout
         send = function(msg) {
        cat(msg$text, "\n")
        invisible(TRUE)
    },

         # No cleanup needed
         close = function() {
        invisible(NULL)
    }
    )
}

