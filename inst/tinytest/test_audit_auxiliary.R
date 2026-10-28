local({
    root <- tempfile("audit-auxiliary-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    parent <- new.env(parent = emptyenv())
    parent$audit_dir <- root
    parent$job_owner <- "fixture-bot"
    parent$job_key <- "fixture-room"
    parent$sessionId <- "fixture-session"
    parent$last_turn_audit <- list(stream = "parent-stream", turn_id = "parent:t1")
    calls <- 0L
    fake <- function(...) {
        calls <<- calls + 1L
        list(content = "summary", finish_reason = "stop",
            usage = list(prompt_tokens = 4L, completion_tokens = 2L))
    }
    result <- corteza:::run_review("openai", "fixture", "HEAD", "clean", "fixture diff",
        chat_fn = fake, session = parent)
    expect_equal(calls, 1L)
    expect_equal(result$usage$total_tokens, 6)
    rows <- corteza:::auto_log_read(result$audit$stream)$records
    expect_identical(rows[[1L]]$origin$parent_turn, parent$last_turn_audit)
    expect_identical(rows[[1L]]$role, "review")
    expect_identical(rows[[1L]]$owner, parent$job_owner)
    expect_identical(rows[[1L]]$session_key, parent$job_key)
    expect_identical(rows[[1L]]$session_id, parent$sessionId)
    expect_equal(tail(rows, 1L)[[1L]]$usage$total_tokens, 6)
    expect_identical(tail(rows, 1L)[[1L]]$usage_source, "canonical")
    # The archival wrapper retains a child reference and records timeout
    # failures. Stub process dispatch only; the actual model/turn audit runs.
    saved_callr <- callr::r
    saved_agent <- llm.api::agent
    on.exit(utils::assignInNamespace("r", saved_callr, "callr"), add = TRUE)
    on.exit(utils::assignInNamespace("agent", saved_agent, "llm.api"), add = TRUE)
    utils::assignInNamespace("agent", fake, "llm.api")
    timed_out <- FALSE
    utils::assignInNamespace("r", function(func, args, ...) {
        if (timed_out) stop(structure(list(message = "fixture timeout", call = NULL),
            class = c("callr_timeout_error", "error", "condition")))
        do.call(func, args)
    }, "callr")
    context <- corteza:::.audit_aux_context(parent, "archival")
    summary <- corteza:::archival_summarize(list(list(role = "user", content = "fixture")),
        style = "paragraph", provider = "openai", model = "fixture", audit_context = context)
    expect_identical(as.character(summary), "summary")
    expect_true(nzchar(attr(summary, "audit")$turn_id))
    expect_equal(attr(summary, "usage")$total_tokens, 6)
    timed_out <- TRUE
    summary <- corteza:::archival_summarize(list(list(role = "user", content = "fixture")),
        style = "paragraph", provider = "openai", model = "fixture", audit_context = context)
    expect_identical(summary, "[summary unavailable]")
    files <- list.files(root, "jsonl$", full.names = TRUE)
    all <- unlist(lapply(files, function(path) corteza:::auto_log_read(path)$records), recursive = FALSE)
    exits <- Filter(function(x) identical(x$type, "auxiliary_exit"), all)
    expect_length(exits, 2L)
    expect_true(any(vapply(exits, function(x) "callr_timeout_error" %in% unlist(x$condition_classes), TRUE)))
    expect_true(any(vapply(exits, function(x) !is.null(x$child$turn_id), TRUE)))
})

# Nested calls keep their parent's audit settings and known identity. The link
# is written before calling the model, so a thrown child still has a parent.
local({
    root <- tempfile("audit-nested-")
    s <- new.env(parent = emptyenv())
    s$audit_dir <- root
    s$job_owner <- "fixture-owner"
    s$job_key <- "fixture-conversation"
    s$config <- list(audit = list(limits = list(result_bytes = 1234)))
    runtime <- corteza:::.audit_runtime
    previous <- runtime$active
    on.exit({
        runtime$active <- previous
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    runtime$active <- corteza:::.audit_turn_begin(s)
    calls <- 0L
    result <- tryCatch(corteza:::.audit_model_call(function(...) {
        calls <<- calls + 1L
        stop("fixture child failure")
    }, list(provider = "openai", model = "fixture"), "review"), error = identity)
    expect_equal(calls, 1L)
    expect_inherits(result, "error")
    rows <- corteza::audit_read(runtime$active$ref$stream, FALSE)$records
    links <- Filter(function(x) identical(x$type, "auxiliary_turn"), rows)
    expect_length(links, 1L)
    child <- corteza::audit_read(links[[1L]]$child$stream, FALSE)$records
    expect_identical(child[[1L]]$owner, s$job_owner)
    expect_identical(child[[1L]]$session_key, s$job_key)
    expect_equal(child[[1L]]$audit_limits$result_bytes, 1234)
    expect_identical(tail(child, 1L)[[1L]]$termination$status, "error")
})

# A consultation snapshot carries the opt-out to the model-only child. It
# still answers, without creating an audit store even with an explicit root.
local({
    root <- tempfile("audit-consult-disabled-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- corteza::new_session(provider = "openai", web_search = FALSE)
    s$config <- list(audit = list(enabled = FALSE))
    s$history <- list(list(role = "user", content = "fixture context"))
    snapshot <- corteza:::.room_consult_snapshot(s, "question", "fixture source")
    snapshot$audit_root <- root
    calls <- 0L
    result <- corteza:::.room_consult_answer(snapshot, .chat = function(...) {
        calls <<- calls + 1L
        list(content = "answer")
    })
    expect_equal(calls, 1L)
    expect_identical(result$reply, "answer")
    expect_null(result$audit)
    expect_false(dir.exists(root))
})

# A real supervised evaluator captures full output before its local cap and
# summary. Its records join the parent's call, without using a shared writer.
local({
    root <- tempfile("audit-evaluator-")
    dir.create(root)
    s <- corteza::new_session(provider = "anthropic", web_search = FALSE)
    s$audit_dir <- root
    s$cwd <- root
    s$config <- list(run_r_mode = "worker")
    on.exit({
        corteza:::.run_r_worker_close(s)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    s$.audit_turn <- corteza:::.audit_turn_begin(s)
    ids <- corteza:::.audit_tool_call(s, "run_r", list(code = "fixture"))
    text <- paste(rep("fixture full output", 200L), collapse = "\n")
    res <- corteza:::.run_r_worker_execute('cat(paste(rep("fixture full output", 200L), collapse="\\n"))',
        timeout = 30, ctx = list(session = s, cwd = root))
    expect_false(isTRUE(res$isError))
    rows <- corteza:::auto_log_read(s$.audit_turn$ref$stream)$records
    links <- Filter(function(x) identical(x$type, "tool_capture_child"), rows)
    expect_length(links, 1L)
    expect_identical(links[[1L]]$call_id, ids$call_id)
    child <- corteza:::auto_log_read(links[[1L]]$capture$stream)$records
    payloads <- Filter(function(x) identical(x$type, "tool_payload"), child)
    expect_true(length(payloads) > 0L)
    expect_true(any(vapply(payloads, function(x) identical(corteza::audit_payload_read(x$full, FALSE), text), TRUE)))
    expect_true(all(vapply(payloads, function(x) identical(x$turn$turn_id, ids$turn_id), TRUE)))
    expect_false(identical(links[[1L]]$capture$stream, s$.audit_turn$ref$stream))
})
