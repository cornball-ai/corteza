# Real agent warnings must identify their emitting invocation. In particular,
# nested do.call() invocations can have identical conditionCall() values.
local({
    old_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
    on.exit(if (is.na(old_key)) Sys.unsetenv("ANTHROPIC_API_KEY") else
        Sys.setenv(ANTHROPIC_API_KEY = old_key), add = TRUE)
    old_warn <- options(warn = 2)
    on.exit(options(old_warn), add = TRUE)
    old_native <- get(".agent_anthropic", asNamespace("llm.api"))
    on.exit(utils::assignInNamespace(".agent_anthropic", old_native, "llm.api"), add = TRUE)
    mode <- "limit"
    requests <- 0L
    executed <- 0L
    args <- list(prompt = "fixture", provider = "anthropic", model = "fixture",
        max_turns = 1L, tools = list(), web_search = FALSE,
        tool_handler = function(...) {
            executed <<- executed + 1L
            if (mode == "tool_warning") warning("Reached max_turns (1)")
            "fixture result"
        })
    native <- function(...) {
        requests <<- requests + 1L
        if (mode == "adapter") warning("Reached max_turns (1)")
        if (mode == "unrelated") warning("unrelated provider warning")
        if (mode == "nested") {
            mode <<- "limit"
            do.call(llm.api::agent, args)
            mode <<- "nested"
        }
        calls <- if (mode %in% c("limit", "tool_warning")) list(list(
            id = "fixture-call", name = "read_file", input = list(path = "fixture"))) else list()
        list(text = "A literal example: [Max turns reached]",
            tool_calls = calls,
            assistant_message = list(role = "assistant", content = if (length(calls)) {
                list(list(type = "tool_use", id = "fixture-call", name = "read_file",
                    input = list(path = "fixture")))
            } else list(list(type = "text", text = "A literal example: [Max turns reached]"))),
            usage = list(input_tokens = 2L, output_tokens = 3L))
    }
    utils::assignInNamespace(".agent_anthropic", native, "llm.api")
    run <- function() suppressMessages(corteza:::.agent_call(args))
    result <- run()
    expect_equal(requests, 1L)
    expect_equal(executed, 1L)
    expect_identical(corteza:::.turn_termination(result),
        list(status = "incomplete", reason = "max_turns"))
    mode <- "quote"
    result <- run()
    expect_identical(corteza:::.turn_termination(result)$status, "done")
    expect_true(grepl("[Max turns reached]", result$content, fixed = TRUE))
    mode <- "adapter"
    expect_error(run(), "Reached max_turns")
    mode <- "unrelated"
    expect_error(run(), "unrelated provider warning")
    for (kind in c("nested", "tool_warning")) {
        mode <- kind
        observed <- character()
        result <- withCallingHandlers(run(), warning = function(w) {
            observed <<- c(observed, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        # The outer capture must see the nested/tool warning: Corteza did
        # not swallow it as its own terminal warning under warn=2.
        expect_identical(observed, "Reached max_turns (1)")
        expect_identical(corteza:::.turn_termination(result)$status,
            if (kind == "nested") "done" else "incomplete")
    }
    expect_equal(requests, 7L)
})

# Use the real turn/retry loop. A retry that runs out of tool turns is still
# incomplete; a later successful turn does not inherit its termination state.
local({
    old_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
    on.exit(if (is.na(old_key)) Sys.unsetenv("ANTHROPIC_API_KEY") else
        Sys.setenv(ANTHROPIC_API_KEY = old_key), add = TRUE)
    original <- get(".agent_anthropic", asNamespace("llm.api"))
    on.exit(utils::assignInNamespace(".agent_anthropic", original, "llm.api"), add = TRUE)
    requests <- 0L
    executed <- 0L
    native <- function(...) {
        requests <<- requests + 1L
        tool <- requests == 2L
        list(text = "fixture response",
            tool_calls = if (tool) list(list(id = "read-1", name = "read_file",
                input = list(path = "fixture"))) else list(),
            assistant_message = list(role = "assistant", content = if (tool) {
                list(list(type = "tool_use", id = "read-1", name = "read_file",
                    input = list(path = "fixture")))
            } else list(list(type = "text", text = "fixture response"))),
            truncated = requests == 1L,
            truncation_reason = if (requests == 1L) "max_tokens",
            usage = list(input_tokens = 2L, output_tokens = 3L))
    }
    utils::assignInNamespace(".agent_anthropic", native, "llm.api")
    s <- corteza::new_session(provider = "anthropic", web_search = FALSE,
        max_turns = 2L, model_map = list(cloud = "fixture"),
        approval_cb = function(...) TRUE)
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = 100,
        output_retries = 1L, output_retry_max_tokens = 8192L)
    result <- suppressWarnings(suppressMessages(corteza::turn("fixture", s,
        tools = list(), tool_executor = function(...) {
            executed <<- executed + 1L
            "result"
        })))
    expect_equal(requests, 2L)
    expect_equal(executed, 1L)
    expect_identical(result$termination$reason, "max_turns")
    expect_false(result$raw$corteza_output_recovered)
    expect_equal(result$usage$total_tokens, 10L)
    next_result <- suppressMessages(corteza::turn("finish", s, tools = list()))
    expect_identical(next_result$termination$status, "done")
    expect_equal(requests, 3L)

    for (reason in c("max_tokens", "model_context_window_exceeded", "content_filter", "cancelled")) {
        utils::assignInNamespace(".agent_anthropic", function(...) {
            requests <<- requests + 1L
            list(text = "partial", tool_calls = list(),
                truncated = reason != "cancelled", cancelled = reason == "cancelled",
                truncation_reason = reason, usage = list(input_tokens = 1L, output_tokens = 1L))
        }, "llm.api")
        out <- suppressWarnings(corteza:::.agent_call(list(prompt = "fixture",
            provider = "anthropic", model = "fixture", tools = list(), web_search = FALSE)))
        termination <- corteza:::.turn_termination(out)
        expect_identical(termination$reason, reason)
        expect_identical(termination$status, if (reason == "cancelled") "cancelled" else "incomplete")
    }
    expect_equal(requests, 7L)
})

# A rate limit after a completed batch preserves its usage across provider
# fallback. An adapter's same-text warning does not mark the fallback incomplete.
local({
    keys <- c("ANTHROPIC_API_KEY", "OPENAI_API_KEY")
    old <- Sys.getenv(keys, unset = NA_character_)
    Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture", OPENAI_API_KEY = "offline-fixture")
    on.exit(for (key in keys) {
        if (is.na(old[[key]])) Sys.unsetenv(key) else
            do.call(Sys.setenv, setNames(list(old[[key]]), key))
    }, add = TRUE)
    native <- c(".agent_anthropic", ".agent_openai")
    originals <- lapply(native, get, envir = asNamespace("llm.api"))
    on.exit(for (i in seq_along(native)) utils::assignInNamespace(native[[i]], originals[[i]], "llm.api"), add = TRUE)
    limits <- corteza:::.fallback_state
    old_limits <- as.list(limits)
    corteza:::.fallback_reset()
    on.exit({
        corteza:::.fallback_reset()
        list2env(old_limits, envir = limits)
    }, add = TRUE)
    requests <- 0L
    secondary <- 0L
    tools <- 0L
    utils::assignInNamespace(".agent_anthropic", function(...) {
        requests <<- requests + 1L
        if (requests > 1L) stop("API error (429): rate limit")
        warning("Reached max_turns (1)")
        call <- list(id = "fixture-read", name = "read_file", input = list(path = "fixture"))
        list(text = "reading", tool_calls = list(call),
            assistant_message = list(role = "assistant", content = list(c(list(type = "tool_use"), call))),
            usage = list(input_tokens = 2L, output_tokens = 3L))
    }, "llm.api")
    utils::assignInNamespace(".agent_openai", function(...) {
        secondary <<- secondary + 1L
        list(text = "finished", tool_calls = list(),
            assistant_message = list(role = "assistant", content = "finished"),
            usage = list(input_tokens = 7L, output_tokens = 11L))
    }, "llm.api")
    s <- corteza::new_session(provider = "anthropic", web_search = FALSE,
        max_turns = 3L, model_map = list(cloud = "fixture"), approval_cb = function(...) TRUE)
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = 100,
        fallback = "fixture openai")
    observed <- character()
    result <- withCallingHandlers(suppressMessages(corteza::turn("fixture", s,
        tools = list(), tool_executor = function(...) { tools <<- tools + 1L; "result" })),
        warning = function(w) {
            observed <<- c(observed, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
    expect_identical(observed, "Reached max_turns (1)")
    expect_equal(requests, 2L)
    expect_equal(secondary, 1L)
    expect_equal(tools, 1L)
    expect_identical(result$termination$status, "done")
    expect_equal(result$usage$total_tokens, 23L)
    expect_equal(result$raw$turns, 2L)
})

# Real child init -> subagent turn -> child checkpoint -> parent settlement.
# The provider is faked in the child; no model, user state, or real key is used.
local({
    root <- tempfile("incomplete-jobs-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA_character_)
    Sys.setenv(CORTEZA_STATE_DIR = file.path(root, "state"))
    on.exit({
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old_state)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    writeLines("fixture", file.path(root, "marker.txt"))
    s <- corteza::new_session("cli", provider = "anthropic", web_search = FALSE)
    s$cwd <- root
    s$config <- list()
    s$job_key <- "incomplete-fixture"
    s$job_worker_spec <- list(init_fn = function(spec) {
        Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
        options(warn = 2)
        corteza::subagent_turn_init(provider = "anthropic", model = "fixture",
            tools_filter = "read_file", system = "fixture", max_turns = 1L,
            web_search = FALSE)
        session <- get(".subagent_state", asNamespace("corteza"))$session
        session$config <- list(context_edit = list(enabled = FALSE),
            context_compact_pct = 100, permissions = list(read_file = "allow"))
        utils::assignInNamespace(".agent_anthropic", function(messages, ...) {
            prompt <- corteza:::archival_history_entry_to_text(tail(messages, 1L)[[1L]])
            finish <- grepl("finish fixture", prompt, fixed = TRUE)
            cancel <- grepl("cancel fixture", prompt, fixed = TRUE)
            call <- list(id = paste0("call-", length(messages)), name = "read_file",
                input = list(path = file.path(getwd(), "marker.txt")))
            list(text = "fixture result", cancelled = cancel,
                tool_calls = if (finish || cancel) list() else list(call),
                assistant_message = list(role = "assistant", content = if (finish || cancel) {
                    list(list(type = "text", text = "fixture result"))
                } else list(c(list(type = "tool_use"), call))),
                usage = list(input_tokens = 2L, output_tokens = 3L))
        }, "llm.api")
    })
    on.exit(corteza:::job_worker_close_all(s), add = TRUE)
    pump <- function(id) {
        deadline <- Sys.time() + 45
        repeat {
            corteza:::job_pump(s)
            j <- corteza:::job_read(id)
            if (j$status %in% corteza:::JOB_STATUSES_FINAL) return(j)
            if (Sys.time() > deadline) stop("fixture job timed out")
            Sys.sleep(0.05)
        }
    }
    id <- corteza:::job_submit(s, "limit fixture", review = TRUE)
    j <- pump(id)
    expect_identical(j$status, "incomplete")
    expect_identical(j$outcome$termination$reason, "max_turns")
    expect_equal(j$outcome$usage$total_tokens, 5L)
    expect_true(j$outcome$checkpoint$ok)
    expect_null(j$outcome$review_job)
    expect_null(s$.job_current)
    expect_length(corteza:::job_list(), 1L)
    expect_true(grepl("incomplete", corteza:::bot_job_result_text(j), fixed = TRUE))
    expect_true(grepl("incomplete", corteza:::.repl_job_result_text(j), fixed = TRUE))
    expect_false(grepl("/auto", corteza:::bot_job_result_text(j), fixed = TRUE))
    expect_true(grepl("not yet available", corteza:::job_result_body(j), fixed = TRUE))
    expect_false(corteza:::job_settle(id, "done", result = "late result"))
    expect_identical(corteza:::job_read(id)$status, "incomplete")
    # Acquiring the same checkout proves that the incomplete writer freed it.
    checkout <- corteza:::job_checkout(root)
    next_id <- corteza:::job_create("lock probe", workspace = root)
    acquired <- corteza:::job_lock_acquire(checkout, next_id, "local")
    expect_true(acquired$ok)
    corteza:::job_lock_release(checkout, next_id)
    corteza:::job_settle(next_id, "cancelled", reason = "fixture finished")

    finished <- pump(corteza:::job_submit(s, "finish fixture"))
    expect_identical(finished$status, "done")
    cancelled <- pump(corteza:::job_submit(s, "cancel fixture"))
    expect_identical(cancelled$status, "cancelled")
    expect_equal(cancelled$outcome$usage$total_tokens, 5L)
    # A checkpoint failure cannot turn a stopped response into success.
    s$.job_worker$run(function() utils::assignInNamespace(
        ".job_worker_child_checkpoint", function(...) stop("fixture disk failure"), "corteza"))
    failed_checkpoint <- pump(corteza:::job_submit(s, "limit fixture", review = TRUE))
    expect_identical(failed_checkpoint$status, "incomplete")
    expect_false(failed_checkpoint$outcome$checkpoint$ok)
    expect_true(grepl("fixture disk failure", failed_checkpoint$outcome$reason, fixed = TRUE))
    expect_null(failed_checkpoint$outcome$review_job)
})
