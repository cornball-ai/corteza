local({
    root <- tempfile("audit-payload-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- new.env(parent = emptyenv())
    s$audit_dir <- root
    a <- corteza:::.audit_turn_begin(s)
    text <- 'token = "fixtureSecret123456789"'
    ref <- corteza:::.audit_payload_put(a, text, "tool_full")
    expect_identical(corteza::audit_payload_read(ref, redact = FALSE), text)
    expect_identical(corteza::audit_payload_read(ref), 'token = "[REDACTED]"')
    expect_identical(corteza:::.audit_payload_put(a, text, "tool_full"), ref)
    expect_length(list.files(ref$store, "^[a-f0-9]{64}$"), 1L)
    expect_equal(a$payload_bytes, nchar(text, "bytes"))
    if (.Platform$OS.type != "windows") {
        expect_equal(as.integer(file.info(file.path(ref$store, ref$hash))$mode), strtoi("600", 8L))
        expect_equal(as.integer(file.info(ref$store)$mode), strtoi("700", 8L))
    }
    a$limits$result_bytes <- 3
    cut <- corteza:::.audit_payload_put(a, "too large", "tool_full")
    expect_identical(cut$reason, "per_result")
    expect_error(corteza::audit_payload_read(cut), "per_result")
    a$limits$turn_bytes <- a$payload_bytes + 2
    expect_identical(corteza:::.audit_payload_put(a, "long", "request")$reason, "per_turn")
    a$limits$turn_bytes <- Inf
    a$limits$quota_bytes <- a$payload_bytes + 2
    expect_identical(corteza:::.audit_payload_put(a, "long", "request")$reason, "quota")
    a$limits$quota_bytes <- Inf
    old <- corteza:::.audit_payload_put(a, "old unpinned", "context")
    future <- Sys.time() + 31 * 86400
    corteza::audit_payload_pin(ref)
    expect_identical(corteza::audit_payload_expire(ref$store, now = future), old$hash)
    expect_true(file.exists(file.path(old$store, old$hash)))
    expect_identical(corteza::audit_payload_expire(ref$store, now = future, dry_run = FALSE), old$hash)
    expect_error(corteza::audit_payload_read(old), "missing or expired")
    expect_identical(corteza::audit_payload_read(ref, FALSE), text)
    rows <- list.files(file.path(root, "maintenance"), "jsonl$", full.names = TRUE)
    expect_length(rows, 1L)
    expect_identical(corteza:::auto_log_read(rows[[1L]])$records[[1L]]$reason, "expiry")
    corteza::audit_payload_pin(ref, FALSE)
    expect_identical(corteza::audit_payload_expire(ref$store, now = future), ref$hash)
    writeLines("damaged", file.path(ref$store, ref$hash))
    expect_error(corteza::audit_payload_read(ref), "hash mismatch")
    omitted <- suppressMessages(corteza:::.audit_payload_put(a, text, "context"))
    expect_identical(omitted$reason, "capture_failure")
    corteza:::.audit_append(a$state, "fixture", text = text)
    expect_identical(tail(corteza::audit_read(a$state$path)$records, 1L)[[1L]]$text,
        'token = "[REDACTED]"')
    expect_identical(tail(corteza::audit_read(a$state$path, FALSE)$records, 1L)[[1L]]$text, text)
    cat("{broken}\n", file = a$state$path, append = TRUE)
    report <- corteza::audit_read(a$state$path)
    expect_equal(report$n_malformed, 1L)
    expect_false(report$torn_tail)
    expect_true(report$malformed_tail)
})
