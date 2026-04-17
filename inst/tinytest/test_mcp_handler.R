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

# Test notifications return NULL
req <- list(
    jsonrpc = "2.0",
    method = "notifications/initialized",
    params = list()
)
resp <- corteza:::handle_request(req)
expect_null(resp)

