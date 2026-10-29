# Real monitor turn, then the ordinary parent poller. No model and no live state.
local({
    root <- tempfile("audit-monitor-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    state <- corteza:::.subagent_state
    worker_state <- corteza:::.job_worker_state
    previous <- as.list(state)
    previous_worker <- as.list(worker_state)
    on.exit({
        rm(list = ls(state, all.names = TRUE), envir = state)
        list2env(previous, state)
        rm(list = ls(worker_state, all.names = TRUE), envir = worker_state)
        list2env(previous_worker, worker_state)
    }, add = TRUE)
    key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
    on.exit(if (is.na(key)) Sys.unsetenv("ANTHROPIC_API_KEY") else
        Sys.setenv(ANTHROPIC_API_KEY = key), add = TRUE)
    old <- get(".agent_anthropic", asNamespace("llm.api"))
    on.exit(utils::assignInNamespace(".agent_anthropic", old, "llm.api"), add = TRUE)
    count <- 0L
    utils::assignInNamespace(".agent_anthropic", function(...) {
        count <<- count + 1L
        text <- "REQUEST: request123\nVERDICT: approve\nREASON: fixture read"
        list(text = text, tool_calls = list(),
            assistant_message = list(role = "assistant", content = list(list(type = "text", text = text))),
            usage = list(input_tokens = 11L, output_tokens = 7L))
    }, "llm.api")
    s <- corteza::new_session(provider = "anthropic", model_map = list(cloud = "fixture"), web_search = FALSE)
    s$audit_dir <- root
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = Inf)
    state$session <- s
    state$subagent_id <- NULL
    worker_state$monitor_scope <- "job-fixture"
    worker_state$monitor_run_fn <- NULL
    answer <- corteza:::.job_monitor_child_ask("fixture", "job-fixture",
        list(kind = "approval", role = "monitor", job_id = "job-fixture",
            approval_request_id = "request123"))
    expect_equal(count, 1L)
    expect_equal(answer$usage$total_tokens, 18)
    expect_true(nzchar(answer$audit$turn_id))
    expect_null(s$audit_origin)
    parent <- new.env(parent = emptyenv())
    parent$audit_dir <- file.path(root, "parent")
    parent$.job_monitor_pending <- list(q = list(request_id = "request123", job = "job-fixture"),
        asked_at = Sys.time())
    parent$.job_monitor <- list(is_alive = function() TRUE,
        poll_process = function(...) "ready", read = function() list(result = answer))
    verdict <- corteza:::job_monitor_poll(parent)
    expect_identical(verdict$verdict, "approve")
    expect_identical(verdict$usage_source, "derived")
    expect_equal(verdict$usage$total_tokens, 18)
    expect_identical(verdict$usage_turns[[1]], answer$audit)
    child_rows <- corteza:::auto_log_read(answer$audit$stream)$records
    ends <- Filter(function(x) identical(x$type, "turn_end"), child_rows)
    expect_length(ends, 1L)
    expect_equal(ends[[1]]$usage$total_tokens, 18)
    expect_identical(ends[[1]]$role, "monitor")
    parent_rows <- corteza:::auto_log_read(parent$.audit_stream$path)$records
    expect_length(parent_rows, 1L)
    expect_identical(parent_rows[[1]]$usage_source, "derived")
    expect_identical(parent_rows[[1]]$usage_turns[[1]]$turn_id, answer$audit$turn_id)
})
