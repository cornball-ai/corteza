# A missing root beneath a symlink has one identity before and after writing.
# This also exercises the macOS temporary-directory alias on Linux.
local({
    if (.Platform$OS.type != "unix") return(invisible(NULL))
    base <- tempfile("audit-symlink-")
    dir.create(base)
    on.exit(unlink(base, recursive = TRUE), add = TRUE)
    target <- file.path(base, "target")
    alias <- file.path(base, "alias")
    dir.create(target)
    expect_true(file.symlink(target, alias))
    s <- new.env(parent = emptyenv())
    s$audit_dir <- file.path(alias, "new", "audit")
    expect_false(dir.exists(s$audit_dir))
    mask <- Sys.umask("0000")
    on.exit(Sys.umask(mask), add = TRUE)
    first <- corteza:::.audit_state(s)
    expect_identical(as.integer(Sys.umask()), 0L)
    expect_true(corteza:::.audit_append(first, "before"))
    expect_identical(corteza:::.audit_state(s), first)
    expect_identical(first$root, normalizePath(s$audit_dir))
    expect_equal(as.integer(file.info(first$root)$mode), strtoi("700", 8L))
    s$audit_dir <- file.path(target, "new", "audit")
    expect_identical(corteza:::.audit_state(s), first)
    expect_true(corteza:::.audit_append(first, "after"))
    rows <- corteza::audit_read(first$path, FALSE)$records
    expect_identical(vapply(rows, `[[`, "", "type"), c("before", "after"))
    expect_length(list.files(first$root, "jsonl$"), 1L)
})

# Optional metadata errors leave the basic turn and terminal record intact.
local({
    root <- tempfile("audit-metadata-")
    s <- corteza::new_session(provider = "openai")
    s$audit_dir <- root
    details <- corteza:::.audit_turn_details
    run <- corteza:::.turn_impl
    on.exit({
        utils::assignInNamespace(".audit_turn_details", details, "corteza")
        utils::assignInNamespace(".turn_impl", run, "corteza")
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    enrichments <- executions <- 0L
    utils::assignInNamespace(".audit_turn_details", function(...) {
        enrichments <<- enrichments + 1L
        stop("fixture pricing lookup failed")
    }, "corteza")
    utils::assignInNamespace(".turn_impl", function(...) {
        executions <<- executions + 1L
        list(reply = "answer", usage = list(total_tokens = 9),
            termination = list(status = "done", reason = "reply"))
    }, "corteza")
    result <- suppressMessages(corteza::turn("test", s))
    expect_equal(enrichments, 1L)
    expect_equal(executions, 1L)
    expect_identical(result$reply, "answer")
    rows <- corteza::audit_read(result$audit$stream, FALSE)$records
    expect_identical(vapply(rows, `[[`, "", "type"), c("turn_start", "turn_metadata", "turn_end"))
    expect_false(rows[[2L]]$complete)
    expect_identical(rows[[3L]]$termination$status, "done")
    expect_equal(rows[[3L]]$usage$total_tokens, 9)
    expect_false(dir.exists(s$.audit_stream$reservation))
})

# Repeated auxiliary calls reuse the caller's store, but receive their own
# turn IDs. The real flush factory shares that writer without sharing history.
local({
    root <- tempfile("audit-reuse-")
    s <- corteza::new_session(provider = "openai", web_search = FALSE)
    s$audit_dir <- root
    s$config <- list(memory_flush_prompt = "fixture flush")
    s$history <- list(list(role = "user", content = "keep caller history"))
    parent <- corteza:::.audit_turn_begin(s)
    corteza:::.audit_turn_end(s, parent)
    calls <- 0L
    fake <- function(..., request_callback = NULL) {
        calls <<- calls + 1L
        if (is.function(request_callback)) {
            event <- list(request_id = paste0("fixture", calls), attempt_id = paste0("attempt", calls),
                provider = "openai", requested_model = "fixture")
            request_callback(c(event, list(phase = "start", serialized_request =
                '{"model":"fixture","messages":[{"role":"user","content":"fixture"}]}')))
            request_callback(c(event, list(phase = "end", status = "complete",
                raw_usage = list(prompt_tokens = 3, completion_tokens = 2))))
        }
        list(content = "summary", usage = list(input_tokens = 3, output_tokens = 2))
    }
    one <- corteza:::run_review("openai", "fixture", "HEAD", "clean", "diff", fake, s)
    two <- corteza:::run_review("openai", "fixture", "HEAD", "clean", "diff", fake, s)
    expect_equal(calls, 2L)
    expect_identical(one$audit$stream, parent$ref$stream)
    expect_identical(two$audit$stream, parent$ref$stream)
    expect_length(unique(c(parent$id, one$audit$turn_id, two$audit$turn_id)), 3L)
    expect_length(list.files(root, "jsonl$"), 1L)
    rows <- corteza::audit_read(parent$ref$stream, FALSE)$records
    starts <- Filter(function(x) identical(x$type, "request_start"), rows)
    expect_length(starts, 2L)
    expect_identical(starts[[1L]]$manifest$request, starts[[2L]]$manifest$request)
    stores <- list.dirs(file.path(root, "payloads"), recursive = FALSE)
    expect_length(stores, 1L)
    run <- corteza:::.turn_impl
    on.exit({
        utils::assignInNamespace(".turn_impl", run, "corteza")
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    utils::assignInNamespace(".turn_impl", function(prompt, session, ...) {
        calls <<- calls + 1L
        session$history <- list()
        list(reply = "flushed", usage = list(total_tokens = 7),
            termination = list(status = "done", reason = "reply"))
    }, "corteza")
    ctx <- list(session = s, config = s$config, cwd = root)
    flushed <- corteza:::run_memory_flush(ctx)
    expect_equal(calls, 3L)
    expect_identical(flushed$audit$stream, parent$ref$stream)
    expect_false(flushed$audit$turn_id %in% c(parent$id, one$audit$turn_id, two$audit$turn_id))
    expect_identical(s$history[[1L]]$content, "keep caller history")
    expect_length(list.files(root, "jsonl$"), 1L)
})

local({
    path <- tempfile("audit-blank-")
    on.exit(unlink(path), add = TRUE)
    writeLines(c('{"type":"run_start"}', "", "   ", '{"type":"run_end"}'), path)
    read <- corteza::audit_read(path)
    expect_equal(read$n_malformed, 2L)
    expect_equal(read$malformed_lines, c(2L, 3L))
    expect_length(read$records, 2L)
})

local({
    root <- tempfile("audit-quota-notice-")
    runtime <- corteza:::.audit_runtime
    noticed <- runtime$noticed
    runtime$noticed <- list()
    on.exit({ runtime$noticed <- noticed; unlink(root, recursive = TRUE) }, add = TRUE)
    s <- new.env(parent = emptyenv())
    s$audit_dir <- root
    s$config <- list(audit = list(limits = list(quota_bytes = 2)))
    a <- corteza:::.audit_turn_begin(s)
    notices <- character()
    withCallingHandlers({
        first <- corteza:::.audit_payload_put(a, "large payload", "request")
        second <- corteza:::.audit_payload_put(a, "another payload", "request")
    }, message = function(m) {
        notices <<- c(notices, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    expect_identical(first$reason, "quota")
    expect_identical(second$reason, "quota")
    expect_length(notices, 1L)
    expect_true(grepl("quota", notices, fixed = TRUE))
    rows <- corteza::audit_read(a$ref$stream)$records
    expect_length(Filter(function(x) identical(x$type, "payload_omitted"), rows), 2L)
})

# Write failures and quota omissions each notify once, regardless of order.
local({
    root <- tempfile("audit-notice-kinds-")
    runtime <- corteza:::.audit_runtime
    previous <- runtime$noticed
    writer <- corteza:::.audit_write
    on.exit({
        runtime$noticed <- previous
        utils::assignInNamespace(".audit_write", writer, "corteza")
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    blocked <- FALSE
    utils::assignInNamespace(".audit_write", function(...) {
        if (blocked) stop("fixture disk failure")
        writer(...)
    }, "corteza")
    for (order in list(c("write", "quota"), c("quota", "write"))) {
        runtime$noticed <- list()
        s <- new.env(parent = emptyenv())
        s$audit_dir <- root
        s$config <- list(audit = list(limits = list(quota_bytes = 0)))
        a <- corteza:::.audit_turn_begin(s)
        notices <- character()
        withCallingHandlers({
            for (kind in rep(order, 2L)) {
                if (kind == "write") {
                    blocked <- TRUE
                    corteza:::.audit_append(a$state, "fixture")
                    blocked <- FALSE
                } else {
                    corteza:::.audit_payload_put(a, "fixture output", "request")
                }
            }
            corteza:::.audit_append(a$state, "recovered")
        }, message = function(m) {
            notices <<- c(notices, conditionMessage(m))
            invokeRestart("muffleMessage")
        })
        expect_length(notices, 2L)
        expect_equal(sum(grepl("recording is failing", notices, fixed = TRUE)), 1L)
        expect_equal(sum(grepl("quota reached", notices, fixed = TRUE)), 1L)
    }
})

# Even an accidentally serialized parent writer cannot be claimed by a child.
# Exercise the real process boundary; the model is a local stand-in.
local({
    root <- tempfile("audit-archival-child-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- new.env(parent = emptyenv())
    s$audit_dir <- root
    a <- corteza:::.audit_turn_begin(s)
    context <- corteza:::.audit_aux_context(s, "archival", share_writer = TRUE)
    before <- readLines(a$ref$stream)
    result <- callr::r(function(context) {
        ns <- asNamespace("corteza")
        s <- new.env(parent = emptyenv())
        s$audit_dir <- context$root
        s$.audit_stream <- context$writer
        state <- get(".audit_state", ns)(s)
        get(".audit_append", ns)(state, "child_dispatch")
        model <- get(".audit_model_call", ns)(function(...) list(content = "fixture summary"),
            list(provider = "openai", model = "fixture"), "archival", context = context)
        list(dispatch = state$path, model = model$audit$stream, reply = model$content)
    }, args = list(context = context), libpath = .libPaths())
    expect_identical(result$reply, "fixture summary")
    expect_false(identical(result$dispatch, a$ref$stream))
    expect_false(identical(result$model, a$ref$stream))
    expect_true(file.exists(result$dispatch))
    expect_true(file.exists(result$model))
    expect_identical(readLines(a$ref$stream), before)
})
