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

