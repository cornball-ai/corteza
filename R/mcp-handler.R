# MCP JSON-RPC Handler
# Handles MCP protocol requests and dispatches to tools

#' Tools the MCP server advertises.
#'
#' `get_tools()` filtered by the configured `corteza.tools`, minus the
#' subagent category unless subagent exposure is opted in
#' (`corteza.mcp_expose_subagents`). This keeps `spawn_subagent` and the
#' rest out of an (often unattended) MCP client's reach by default; the
#' in-process chat()/CLI loop does not go through here, so local
#' subagents are unaffected.
#' @noRd
mcp_visible_tools <- function() {
    tools <- get_tools(getOption("corteza.tools"))
    if (!isTRUE(getOption("corteza.mcp_expose_subagents", FALSE))) {
        sub <- .builtin_categories$subagent
        tools <- Filter(function(t) !t$name %in% sub, tools)
    }
    tools
}

#' TRUE when `name` is in the set the MCP server currently advertises.
#'
#' tools/call must reject anything not advertised by tools/list, so a
#' tool hidden by the `corteza.tools` filter (or the subagent gate)
#' cannot be invoked by name behind the listing's back.
#' @noRd
mcp_tool_advertised <- function(name) {
    any(vapply(mcp_visible_tools(), function(t) identical(t$name, name),
               logical(1)))
}

#' Gate an MCP tools/call against subagent policy.
#'
#' Returns NULL when the call may proceed, or an `err()` result to send
#' back instead. Non-subagent tools always pass. Subagent tools are
#' refused unless exposure is opted in; when exposed, the
#' spend-incurring ones (spawn/query) are refused once cumulative
#' subagent spend crosses the configured cap. The cap reads
#' `subagent_spend_total()` -- the same meter `/spent` shows -- so a
#' server restart (which clears the registry + retired accumulator) is
#' the way to reset it.
#' @noRd
mcp_subagent_guard <- function(name) {
    sub <- .builtin_categories$subagent
    if (!name %in% sub) {
        return(NULL)
    }
    if (!isTRUE(getOption("corteza.mcp_expose_subagents", FALSE))) {
        return(err(sprintf(paste0(
                                  "Tool '%s' is not exposed over MCP. Subagents run their own ",
                                  "agent loop and spend autonomously on the host's credentials; ",
                                  "enable with subagents.expose_over_mcp=true or ",
                                  "serve(expose_subagents=TRUE)."), name)))
    }
    # spawn and query are the spend-incurring calls (spawn starts a
    # child; query runs its loop). list/collect/kill never start new
    # spend, so they are allowed up to and past the cap.
    if (name %in% c("spawn_subagent", "query_subagent")) {
        total <- subagent_spend_total()
        cap_usd <- getOption("corteza.mcp_subagent_cap_usd", NULL)
        if (!is.null(cap_usd) && !is.na(cap_usd) && cap_usd > 0 &&
            (total$cost %||% 0) >= cap_usd) {
            return(err(sprintf(paste0(
                                      "MCP subagent spend cap reached (~$%.4f >= $%.2f); '%s' ",
                                      "refused. Raise subagents.mcp_spend_cap_usd or restart ",
                                      "the server."), total$cost %||% 0, cap_usd, name)))
        }
        cap_tok <- getOption("corteza.mcp_subagent_cap_tokens", NULL)
        if (!is.null(cap_tok) && !is.na(cap_tok) && cap_tok > 0 &&
            (total$total_tokens %||% 0L) >= cap_tok) {
            return(err(sprintf(paste0(
                                      "MCP subagent token cap reached (%d >= %d); '%s' refused. ",
                                      "Raise subagents.mcp_spend_cap_tokens or restart the server."),
                               total$total_tokens %||% 0L, as.integer(cap_tok), name)))
        }
    }
    NULL
}

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
                                   protocolVersion = params$protocolVersion %||% "2024-11-05",
                                   capabilities = list(tools = setNames(list(), character(0))),
                                   serverInfo = list(name = "corteza-mcp",
                    version = as.character(packageVersion("corteza")))
            ),

               "notifications/initialized" = NULL, # No response for notifications

               "tools/list" = list(tools = mcp_visible_tools()),

               "tools/call" = {
            blocked <- mcp_subagent_guard(params$name)
            if (!is.null(blocked)) {
                # Subagent policy spoke (not exposed, or over cap):
                # return its specific message.
                blocked
            } else if (!mcp_tool_advertised(params$name)) {
                # Not in the advertised set (filtered out by
                # corteza.tools, or otherwise hidden): refuse rather
                # than dispatch a tool the listing never offered.
                err(sprintf("Tool not available: %s", params$name))
            } else {
                call_tool(params$name, params$arguments)
            }
        },

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
    req <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                    error = function(e) NULL)

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

