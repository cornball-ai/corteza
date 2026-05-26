# Test MCP handler

# Test handle_request for initialize
req <- list(
    jsonrpc = "2.0",
    id = 1,
    method = "initialize",
    params = list()
)
resp <- corteza:::handle_request(req)

expect_equal(resp$jsonrpc, "2.0")
expect_equal(resp$id, 1)
expect_true("result" %in% names(resp))
expect_true("protocolVersion" %in% names(resp$result))
expect_true("serverInfo" %in% names(resp$result))
expect_equal(resp$result$serverInfo$name, "corteza-mcp")

# Test handle_request for tools/list
req <- list(
    jsonrpc = "2.0",
    id = 2,
    method = "tools/list",
    params = list()
)
resp <- corteza:::handle_request(req)

expect_equal(resp$id, 2)
expect_true("tools" %in% names(resp$result))
expect_true(length(resp$result$tools) > 0)

# Test handle_request for tools/call
req <- list(
    jsonrpc = "2.0",
    id = 3,
    method = "tools/call",
    params = list(
        name = "run_r",
        arguments = list(code = "2 + 2")
    )
)
resp <- corteza:::handle_request(req)

expect_equal(resp$id, 3)
expect_true("content" %in% names(resp$result))
expect_true(grepl("4", resp$result$content[[1]]$text))

# Test handle_request for unknown method
req <- list(
    jsonrpc = "2.0",
    id = 4,
    method = "unknown/method",
    params = list()
)
resp <- corteza:::handle_request(req)

expect_true("error" %in% names(resp))
expect_equal(resp$error$code, - 32601)

# initialize echoes the client's protocolVersion
req <- list(jsonrpc = "2.0", id = 5, method = "initialize",
            params = list(protocolVersion = "2025-11-25"))
resp <- corteza:::handle_request(req)
expect_equal(resp$result$protocolVersion, "2025-11-25")

# capabilities.tools must serialize as a JSON object ({}), not an array ([])
json <- as.character(jsonlite::toJSON(resp, auto_unbox = TRUE, null = "null"))
expect_true(grepl("\"tools\":\\{\\}", json))
expect_false(grepl("\"tools\":\\[\\]", json))

# Test notifications return NULL
req <- list(
    jsonrpc = "2.0",
    method = "notifications/initialized",
    params = list()
)
resp <- corteza:::handle_request(req)
expect_null(resp)

# --- subagent exposure gate + spend cap --------------------------------

corteza:::ensure_skills()
sub_tools <- corteza:::.builtin_categories$subagent
tool_names <- function() {
    vapply(corteza:::mcp_visible_tools(), function(t) t$name, character(1))
}
# Save options the gate reads; restore at the end (no top-level on.exit).
o_expose <- getOption("corteza.mcp_expose_subagents")
o_usd <- getOption("corteza.mcp_subagent_cap_usd")
o_tok <- getOption("corteza.mcp_subagent_cap_tokens")
# Bind the env to a local; mutating it mutates the package env. (Direct
# `corteza:::.subagent_spend_retired$x <- v` would reassign through :::
# and error.)
retired <- corteza:::.subagent_spend_retired
reset_retired <- function() {
    retired$cost <- 0; retired$input_tokens <- 0L; retired$output_tokens <- 0L
    retired$total_tokens <- 0L; retired$query_count <- 0L
    retired$n_agents <- 0L; retired$cost_missing <- FALSE
}
reset_retired()

# Default (not exposed): tools/list hides every subagent tool, and a
# subagent tools/call is refused with the opt-in message.
options(corteza.mcp_expose_subagents = FALSE)
expect_false(any(sub_tools %in% tool_names()))
expect_true("run_r" %in% tool_names())                  # non-subagent stays
expect_null(corteza:::mcp_subagent_guard("run_r"))      # non-subagent passes
blk <- corteza:::mcp_subagent_guard("spawn_subagent")
expect_false(is.null(blk))
expect_true(isTRUE(blk$isError))
expect_true(grepl("not exposed over MCP", blk$content[[1]]$text, fixed = TRUE))

# End-to-end: a tools/call for a subagent tool is refused by
# handle_request (short-circuits before any spawn) when not exposed.
resp_sub <- corteza:::handle_request(list(
    jsonrpc = "2.0", id = 9, method = "tools/call",
    params = list(name = "spawn_subagent",
                  arguments = list(task = "noop"))))
expect_true(isTRUE(resp_sub$result$isError))
expect_true(grepl("not exposed over MCP",
                  resp_sub$result$content[[1]]$text, fixed = TRUE))

# Exposed: tools/list includes the subagent tools, and an under-cap
# spawn/query passes the guard.
options(corteza.mcp_expose_subagents = TRUE,
        corteza.mcp_subagent_cap_usd = 5.00,
        corteza.mcp_subagent_cap_tokens = NA_integer_)
expect_true(all(sub_tools %in% tool_names()))
expect_null(corteza:::mcp_subagent_guard("spawn_subagent"))
expect_null(corteza:::mcp_subagent_guard("list_subagents"))

# USD cap: push retired spend over $5 -> spawn/query refused, but the
# read-only tools (list/kill/collect) still pass.
retired$cost <- 6.00
retired$n_agents <- 1L
cap_blk <- corteza:::mcp_subagent_guard("spawn_subagent")
expect_false(is.null(cap_blk))
expect_true(grepl("spend cap reached", cap_blk$content[[1]]$text, fixed = TRUE))
expect_false(is.null(corteza:::mcp_subagent_guard("query_subagent")))
expect_null(corteza:::mcp_subagent_guard("list_subagents"))
expect_null(corteza:::mcp_subagent_guard("kill_subagent"))

# cap_usd <= 0 disables the USD cap
reset_retired()
retired$cost <- 100.00
options(corteza.mcp_subagent_cap_usd = 0)
expect_null(corteza:::mcp_subagent_guard("spawn_subagent"))

# Token cap (for cost-blind providers): trips on total tokens
reset_retired()
options(corteza.mcp_subagent_cap_usd = 0,
        corteza.mcp_subagent_cap_tokens = 1000L)
retired$total_tokens <- 1500L
tok_blk <- corteza:::mcp_subagent_guard("spawn_subagent")
expect_false(is.null(tok_blk))
expect_true(grepl("token cap reached", tok_blk$content[[1]]$text, fixed = TRUE))

# tools/call must honor the tools/list filter: a tool hidden by
# corteza.tools cannot be invoked by name behind the listing's back,
# even when subagent exposure is on. With tools="core", neither
# list_subagents (a subagent tool) nor web_search (web category) is
# advertised, so both calls are refused rather than dispatched.
o_tools <- getOption("corteza.tools")
reset_retired()
options(corteza.tools = "core", corteza.mcp_expose_subagents = TRUE,
        corteza.mcp_subagent_cap_usd = 5.00,
        corteza.mcp_subagent_cap_tokens = NA_integer_)
expect_false("list_subagents" %in% tool_names())
resp_hidden <- corteza:::handle_request(list(
    jsonrpc = "2.0", id = 10, method = "tools/call",
    params = list(name = "list_subagents", arguments = list())))
expect_true(isTRUE(resp_hidden$result$isError))
expect_true(grepl("not available", resp_hidden$result$content[[1]]$text,
                  fixed = TRUE))
resp_web <- corteza:::handle_request(list(
    jsonrpc = "2.0", id = 11, method = "tools/call",
    params = list(name = "web_search", arguments = list(query = "x"))))
expect_true(isTRUE(resp_web$result$isError))
expect_true(grepl("not available", resp_web$result$content[[1]]$text,
                  fixed = TRUE))
options(corteza.tools = o_tools)

# Restore options and shared state.
reset_retired()
options(corteza.mcp_expose_subagents = o_expose,
        corteza.mcp_subagent_cap_usd = o_usd,
        corteza.mcp_subagent_cap_tokens = o_tok)
