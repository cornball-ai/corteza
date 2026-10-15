local({
    ns <- asNamespace("llm.api")
    original <- get("agent", ns)
    on.exit(utils::assignInNamespace("agent", original, "llm.api"), add = TRUE)
    root <- tempfile("output-recovery-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    calls <- list()
    completed <- list(
        list(role = "user", content = "Make the fixture"),
        list(role = "assistant", content = list(list(type = "tool_use",
            id = "finished", name = "write_file", input = list(path = "fixture")))),
        list(role = "user", content = list(list(type = "tool_result",
            tool_use_id = "finished", content = "Saved"))))
    cutoff <- function(reason = "max_tokens") list(
        content = "[Output truncated: max_tokens]", truncated = TRUE,
        truncation_reason = reason, history = completed, turns = 2L,
        provider = "anthropic", usage = list(input_tokens = 10L,
            output_tokens = 4096L, total_tokens = 4106L, cost = 0.1))
    stub <- function(...) {
        args <- list(...)
        calls[[length(calls) + 1L]] <<- args
        if (length(calls) == 1L) return(cutoff())
        list(content = "Finished", history = c(args$history,
             list(list(role = "assistant", content = "Finished"))),
             turns = 1L, provider = "anthropic",
             usage = list(input_tokens = 20L, output_tokens = 30L,
                          total_tokens = 50L, cost = 0.2))
    }
    utils::assignInNamespace("agent", stub, "llm.api")
    make_session <- function() {
        s <- corteza::new_session(provider = "anthropic", web_search = FALSE,
                                  model_map = list(cloud = "fixture-model"))
        s$config <- list(context_compact_pct = 100, context_edit = list(enabled = FALSE))
        s
    }
    s <- make_session()
    corteza:::journal_install(s, file.path(root, "journal"))
    out <- suppressMessages(corteza::turn("Make the fixture", s, tools = list()))
    expect_equal(length(calls), 2L)
    expect_null(calls[[2L]]$prompt)
    expect_true("prompt" %in% names(calls[[2L]]))
    expect_identical(calls[[2L]]$history, completed)
    expect_identical(calls[[2L]]$max_tokens, 8192L)
    expect_identical(calls[[2L]]$max_turns, 8L)
    expect_equal(out$reply, "Finished")
    expect_equal(out$usage$total_tokens, 4156L)
    expect_equal(out$usage$cost, 0.3)
    expect_equal(out$raw$turns, 3L)
    expect_true(out$raw$corteza_output_recovered)
    expect_identical(corteza:::journal_load(file.path(root, "journal"))$live$history,
                     completed)
    expect_equal(s$output_recovery$max_tokens, 8192L)
    # Learned budget applies only to this provider/model; explicit limits win.
    args <- list(provider = "anthropic", model = "fixture-model")
    expect_equal(corteza:::.output_recovery_args(args, s)$max_tokens, 8192L)
    args$model <- "another-model"
    expect_null(corteza:::.output_recovery_args(args, s)$max_tokens)

    # A persistent cutoff gets three retries, then actionable output.
    calls <- list()
    utils::assignInNamespace("agent", function(...) {
        calls[[length(calls) + 1L]] <<- list(...)
        cutoff()
    }, "llm.api")
    out <- suppressMessages(corteza::turn("fixture", make_session(), tools = list()))
    expect_equal(length(calls), 4L)
    expect_equal(vapply(calls[-1L], function(x) x$max_tokens, 1L), c(8192L, 16384L, 32768L))
    expect_true(out$raw$truncated)
    expect_true(grepl("/max-tokens", out$reply, fixed = TRUE))
    expect_equal(out$usage$total_tokens, 4 * 4106L)

    # Deliberately configured limits and disabled retries remain effective.
    for (config in list(list(max_tokens = 4096L), list(output_retries = 0L))) {
        calls <- list()
        s <- make_session()
        s$config <- utils::modifyList(s$config, config)
        out <- suppressMessages(corteza::turn("fixture", s, tools = list()))
        expect_equal(length(calls), 1L)
    }
    # Streaming consumers have already delivered partial text: no replay.
    calls <- list()
    s <- make_session()
    s$on_delta <- function(...) TRUE
    out <- suppressMessages(corteza::turn("fixture", s, tools = list()))
    expect_equal(length(calls), 1L)

    # Codex cannot honor a larger output budget.
    response <- cutoff()
    response$provider <- "openai_codex"
    out <- corteza:::.recover_output(response,
        list(provider = "openai_codex", model = "fixture", max_tokens = 4096L,
             max_turns = 10L), make_session(), function(...) stop("unexpected retry"))
    expect_true(out$truncated)

    # Context exhaustion/content filtering are never output-budget retries.
    for (reason in c("model_context_window_exceeded", "content_filter")) {
        response <- cutoff(reason)
        calls <- 0L
        out <- corteza:::.recover_output(response,
            list(provider = "anthropic", model = "fixture-model", max_turns = 10L),
            make_session(), function(...) { calls <<- calls + 1L; stop("unexpected retry") })
        expect_equal(calls, 0L)
        expect_identical(out, response)
    }
    s <- make_session()
    expect_true(grepl("32768", corteza:::.output_budget_command(s, "32768")))
    expect_identical(s$max_tokens, 32768L)
    expect_error(corteza:::.output_budget_command(s, "-1"), "positive whole number")
    corteza:::.output_budget_command(s, "default")
    expect_null(s$max_tokens)
})

local({
    names <- c("read_file", "read_handle", "write_file", "bash", "run_r",
               "base__file.remove", "edit_context", "task_create")
    tools <- lapply(names, function(name) list(name = name))
    shown <- function(config) vapply(corteza:::.auto_filter_tools(tools, config),
                                     function(x) x$name, "")
    expect_identical(shown(NULL), names)
    expect_identical(shown(list()),
                     c("read_file", "read_handle", "write_file", "edit_context", "task_create"))
    expect_true(all(c("bash", "run_r") %in% shown(list(auto = list(allow_exec = TRUE)))))
    expect_false("base__file.remove" %in% shown(list(auto = list(allow_exec = TRUE))))
    expect_false("read_file" %in% shown(list(auto = list(never_broker = "read_file"))))
})

# Real llm.api agent loop: a complete tool executes once, a truncated tool
# never executes, and the larger request starts from the completed batch.
local({
    original <- get(".agent_anthropic", asNamespace("llm.api"))
    old_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
    on.exit({
        utils::assignInNamespace(".agent_anthropic", original, "llm.api")
        if (is.na(old_key)) Sys.unsetenv("ANTHROPIC_API_KEY") else
            Sys.setenv(ANTHROPIC_API_KEY = old_key)
    }, add = TRUE)
    requests <- list()
    executed <- character()
    native <- function(messages, tools, system, model, config, ...) {
        args <- list(...)
        requests[[length(requests) + 1L]] <<- list(history = messages, args = args)
        n <- length(requests)
        if (n < 3L) {
            name <- if (n == 1L) "read_file" else "write_file"
            id <- if (n == 1L) "complete" else "unfinished"
            return(list(text = "", tool_calls = list(list(id = id, name = name,
                arguments = list(path = "fixture"))),
                assistant_message = list(role = "assistant", content = list(
                    list(type = "tool_use", id = id, name = name, input = list(path = "fixture")))),
                truncated = n == 2L, truncation_reason = if (n == 2L) "max_tokens",
                usage = list(input_tokens = 2L, output_tokens = 5L)))
        }
        list(text = "Finished", tool_calls = list(),
             assistant_message = list(role = "assistant", content = list(list(type = "text", text = "Finished"))),
             usage = list(input_tokens = 2L, output_tokens = 5L))
    }
    utils::assignInNamespace(".agent_anthropic", native, "llm.api")
    s <- corteza::new_session(provider = "anthropic", web_search = FALSE,
        model_map = list(cloud = "claude-fixture"), approval_cb = function(...) TRUE)
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = 100)
    out <- suppressWarnings(suppressMessages(corteza::turn("Read the fixture", s,
        tools = list(), tool_executor = function(name, args) {
            executed <<- c(executed, name)
            "fixture read"
        })))
    expect_equal(length(requests), 3L)
    expect_identical(executed, "read_file")
    expect_equal(requests[[3L]]$args$max_tokens, 8192L)
    expect_identical(requests[[3L]]$history, requests[[2L]]$history)
    expect_equal(length(llm.api::history_tool_calls(s$history)), 1L)
    expect_equal(out$reply, "Finished")
    expect_equal(out$usage$total_tokens, 21L)
})
