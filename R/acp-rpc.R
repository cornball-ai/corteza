# JSON-RPC 2.0 for `corteza acp` (R/acp.R): one message a line on
# standard input and output, as the Agent Client Protocol frames it.

# An empty JSON object. jsonlite writes an unnamed empty list as [].
.acp_object <- function() {
    structure(list(), names = character(0))
}

.acp_encode <- function(msg) {
    as.character(jsonlite::toJSON(msg, auto_unbox = TRUE, null = "null",
                                  digits = NA))
}

# NULL for a blank line, which the loop drops. A line that is not a JSON
# object comes back marked, and is answered with a parse error.
.acp_decode <- function(line) {
    if (!nzchar(trimws(line))) {
        return(NULL)
    }
    msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (!is.list(msg) || is.null(names(msg))) {
        return(list(.unreadable = TRUE))
    }
    msg
}

.acp_request <- function(id, method, params = .acp_object()) {
    list(jsonrpc = "2.0", id = id, method = method, params = params)
}

.acp_notification <- function(method, params = .acp_object()) {
    list(jsonrpc = "2.0", method = method, params = params)
}

.acp_result <- function(id, result = .acp_object()) {
    list(jsonrpc = "2.0", id = id, result = result)
}

.acp_error <- function(id, code, message) {
    list(jsonrpc = "2.0", id = id,
         error = list(code = as.integer(code), message = message))
}

# "request", "notification", "response", or "invalid".
.acp_kind <- function(msg) {
    if (!is.list(msg)) {
        return("invalid")
    }
    method <- is.character(msg$method) && length(msg$method) == 1L
    id <- !is.null(msg$id)
    if (method && id) {
        return("request")
    }
    if (method) {
        return("notification")
    }
    if (id && (!is.null(msg$error) || "result" %in% names(msg))) {
        return("response")
    }
    "invalid"
}

# A refusal raised by a handler, answered as a JSON-RPC error with its
# code (-32602 for bad params and the rest). Anything else a handler
# raises is answered -32603, with its message.
.acp_refuse <- function(message, code = -32602L) {
    stop(structure(class = c("acp_refusal", "error", "condition"),
                   list(message = message, call = NULL, code = as.integer(code))))
}
