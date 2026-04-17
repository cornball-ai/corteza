# Test transport layer

# Test message_normalize
msg <- corteza:::message_normalize(
    text = "Hello",
    sender = "user1",
    channel = "test"
)
expect_equal(msg$text, "Hello")
expect_equal(msg$sender, "user1")
expect_equal(msg$channel, "test")
expect_true(startsWith(msg$id, "test_"))
expect_true(inherits(msg$timestamp, "POSIXct"))
expect_true(is.list(msg$attachments))
expect_equal(length(msg$attachments), 0)
expect_true(is.list(msg$metadata))

# Test message_normalize with explicit id
msg <- corteza:::message_normalize(
    text = "Test",
    sender = "user",
    channel = "signal",
    id = "custom_id_123"
)
expect_equal(msg$id, "custom_id_123")

# Test message_normalize with attachments
msg <- corteza:::message_normalize(
    text = "Photo",
    sender = "user",
    channel = "signal",
    attachments = list(
        list(path = "/tmp/photo.jpg", contentType = "image/jpeg", size = 1024)
    )
)
expect_equal(length(msg$attachments), 1)
expect_equal(msg$attachments[[1]]$path, "/tmp/photo.jpg")
expect_equal(msg$attachments[[1]]$contentType, "image/jpeg")

# Test message_normalize with metadata
msg <- corteza:::message_normalize(
    text = "Group msg",
    sender = "user",
    channel = "signal",
    metadata = list(
        group_id = "abc123",
        group_name = "Test Group",
        is_group = TRUE
    )
)
expect_equal(msg$metadata$group_id, "abc123")
expect_equal(msg$metadata$group_name, "Test Group")
expect_true(msg$metadata$is_group)

# Test transport_new with unknown type
expect_error(
    corteza:::transport_new("unknown_type"),
    "Unknown transport type"
)

# Test terminal transport creation
term <- corteza:::transport_terminal()
expect_equal(term$type, "terminal")
expect_true(is.function(term$receive))
expect_true(is.function(term$send))
expect_true(is.function(term$close))

# Test terminal close (should not error)
result <- term$close()
expect_null(result)

# Test transport_new creates terminal transport
term2 <- corteza:::transport_new("terminal")
expect_equal(term2$type, "terminal")

# Test signal transport creation requires account
expect_error(
    corteza:::transport_signal(list()),
    "account"
)

# Test signal transport with config
sig <- corteza:::transport_signal(list(
    account = "+15551234567",
    httpHost = "127.0.0.1",
    httpPort = 8080
))
expect_equal(sig$type, "signal")
expect_true(is.function(sig$receive))
expect_true(is.function(sig$send))
expect_true(is.function(sig$close))
expect_true(is.function(sig$send_typing))
expect_true(is.function(sig$send_reaction))
expect_true(is.function(sig$list_groups))
expect_true(is.function(sig$get_account))
expect_true(is.function(sig$check))

# Test signal transport with httpUrl override
sig2 <- corteza:::transport_signal(list(
    account = "+15551234567",
    httpUrl = "http://custom.host:9000"
))
expect_equal(sig2$type, "signal")

# Test signal close
result <- sig$close()
expect_null(result)

# ============================================================================
# Test Signal helper functions (pure, top-level)
# ============================================================================

# Test signal_parse_attachments with NULL
atts <- corteza:::signal_parse_attachments(NULL)
expect_equal(length(atts), 0)

# Test signal_parse_attachments with empty list
atts <- corteza:::signal_parse_attachments(list())
expect_equal(length(atts), 0)

# Test signal_parse_attachments with data
atts <- corteza:::signal_parse_attachments(list(
    list(id = "123", contentType = "image/png", filename = "test.png",
         size = 1024, width = 100, height = 100, file = "/tmp/test.png")
))
expect_equal(length(atts), 1)
expect_equal(atts[[1]]$id, "123")
expect_equal(atts[[1]]$contentType, "image/png")
expect_equal(atts[[1]]$path, "/tmp/test.png")

# Test signal_parse_envelope with NULL
msg <- corteza:::signal_parse_envelope(NULL)
expect_null(msg)

# Test signal_parse_envelope with missing sender
msg <- corteza:::signal_parse_envelope(list(timestamp = 123))
expect_null(msg)

# Test signal_parse_envelope with receipt
envelope <- list(
    source = "+15551234567",
    timestamp = 1234567890,
    receiptMessage = list(isDelivery = TRUE, timestamps = c(123, 456))
)
msg <- corteza:::signal_parse_envelope(envelope)
expect_equal(msg$metadata$type, "receipt")
expect_equal(msg$metadata$receipt_type, "delivery")
expect_equal(msg$sender, "+15551234567")

# Test signal_parse_envelope with reaction
envelope <- list(
    source = "+15551234567",
    timestamp = 1234567890,
    dataMessage = list(
        reaction = list(
            emoji = "👍",
            isRemove = FALSE,
            targetAuthor = "+15559999999",
            targetSentTimestamp = 1234567800
        )
    )
)
msg <- corteza:::signal_parse_envelope(envelope)
expect_equal(msg$metadata$type, "reaction")
expect_equal(msg$metadata$emoji, "👍")
expect_false(msg$metadata$is_remove)

# Test signal_parse_envelope with text message
envelope <- list(
    source = "+15551234567",
    timestamp = 1234567890,
    dataMessage = list(message = "Hello world")
)
msg <- corteza:::signal_parse_envelope(envelope)
expect_equal(msg$text, "Hello world")
expect_equal(msg$sender, "+15551234567")
expect_equal(msg$channel, "signal")
expect_false(msg$metadata$is_group)

# Test signal_parse_envelope with group message
envelope <- list(
    source = "+15551234567",
    timestamp = 1234567890,
    dataMessage = list(
        message = "Group hello",
        groupInfo = list(groupId = "group123", groupName = "Test Group")
    )
)
msg <- corteza:::signal_parse_envelope(envelope)
expect_equal(msg$text, "Group hello")
expect_equal(msg$metadata$group_id, "group123")
expect_equal(msg$metadata$group_name, "Test Group")
expect_true(msg$metadata$is_group)

# Test signal_parse_envelope with allowlist (blocked)
envelope <- list(
    source = "+15551234567",
    timestamp = 1234567890,
    dataMessage = list(message = "Blocked")
)
msg <- corteza:::signal_parse_envelope(envelope, allow_from = c("+15559999999"))
expect_null(msg)

# Test signal_parse_envelope with allowlist (allowed)
msg <- corteza:::signal_parse_envelope(envelope, allow_from = c("+15551234567"))
expect_equal(msg$text, "Blocked")

# Test sse_process_buffer with empty buffer
result <- corteza:::sse_process_buffer("", list())
expect_equal(result$buffer, "")
expect_equal(length(result$messages), 0)

# Test sse_process_buffer with partial line (no newline)
result <- corteza:::sse_process_buffer("data: partial", list())
expect_equal(result$buffer, "data: partial")
expect_equal(length(result$messages), 0)

# Test sse_process_buffer with complete event
json_data <- '{"envelope":{"source":"+15551234567","timestamp":123,"dataMessage":{"message":"Hi"}}}'
buffer <- sprintf("data: %s\n\n", json_data)
result <- corteza:::sse_process_buffer(buffer, list())
expect_equal(result$buffer, "")
expect_equal(length(result$messages), 1)
expect_equal(result$messages[[1]]$text, "Hi")

# Test sse_process_buffer with multiple events
buffer <- sprintf("data: %s\n\ndata: %s\n\n", json_data, json_data)
result <- corteza:::sse_process_buffer(buffer, list())
expect_equal(length(result$messages), 2)

# Test sse_process_buffer preserves partial event
buffer <- sprintf("data: %s\n\ndata: partial", json_data)
result <- corteza:::sse_process_buffer(buffer, list())
expect_equal(length(result$messages), 1)
expect_equal(result$buffer, "data: partial")

