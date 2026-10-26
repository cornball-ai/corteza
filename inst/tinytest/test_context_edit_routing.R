# Real turn()/agent() loops with only HTTP transports replaced. These catch
# changes in wire selection before the first response creates native history.
local({
    ns <- asNamespace("llm.api")
    old_post <- get(".post_json", ns)
    old_responses <- get(".openai_codex_post_sse", ns)
    opts <- options(llm.api_base = NULL, llm.api.api_base = NULL,
                    llm.api_key = "fixture-key")
    root <- tempfile("context-routing-")
    dir.create(root)
    on.exit({
        options(opts)
        assignInNamespace(".post_json", old_post, "llm.api")
        assignInNamespace(".openai_codex_post_sse", old_responses, "llm.api")
        unlink(root, recursive = TRUE)
        corteza:::.fallback_reset()
    }, add = TRUE)
    corteza:::.fallback_reset()
    cc <- responses <- list()
    assignInNamespace(".post_json", function(url, body, headers, ...) {
        cc[[length(cc) + 1L]] <<- body
        message <- if (length(cc) %% 2L == 1L) {
            list(role = "assistant", content = NULL, tool_calls = list(
                list(id = paste0("edit-", length(cc)), type = "function",
                     `function` = list(name = "edit_context",
                         arguments = '{"op":"note","text":"Keep task state"}'))))
        } else list(role = "assistant", content = "Completed turn")
        list(choices = list(list(message = message, finish_reason = "stop")),
             usage = list(prompt_tokens = 10L, completion_tokens = 5L))
    }, "llm.api")
    assignInNamespace(".openai_codex_post_sse", function(url, body, headers, on_delta = NULL) {
        responses[[length(responses) + 1L]] <<- body
        list(status = "completed", output = list(list(type = "message",
            role = "assistant", content = list(list(type = "output_text", text = "Completed turn")))),
            usage = list(input_tokens = 10L, output_tokens = 5L))
    }, "llm.api")
    make <- function(model, search = FALSE, strategy = "clm") {
        s <- corteza::new_session(provider = "openai", system = "Fixture instructions",
            model_map = list(cloud = model), web_search = search,
            context_edit = list(enabled = TRUE, cadence = "always", strategy = strategy))
        s$config <- list(context_compact_pct = 1000)
        s$context_window <- 100000L
        corteza:::journal_install(s, tempfile("journal-", tmpdir = root))
        s
    }
    read_tool <- list(list(name = "read_file", description = "Read fixture",
                          input_schema = list(type = "object")))
    s <- make("gpt-5.5")
    for (prompt in c("First task", "Second task")) {
        expect_equal(corteza::turn(prompt, s, tools = read_tool)$reply, "Completed turn")
        expect_true(corteza:::.context_edit_enabled(s))
    }
    expect_equal(length(cc), 4L)
    expect_equal(length(responses), 0L)
    expect_true(all(vapply(cc, function(body) any(vapply(body$tools,
        function(tool) identical(tool$`function`$name, "edit_context"), logical(1))), logical(1))))
    expect_true(grepl("Keep task state", corteza:::.compact_entry_body(
        corteza:::.context_edit_state(s)$note), fixed = TRUE))

    cases <- list(list(model = "gpt-5.5", search = TRUE))
    if ("agent_uses_responses" %in% getNamespaceExports("llm.api")) {
        cases <- c(cases, list(list(model = "gpt-6-astra", search = FALSE)))
    }
    for (case in cases) {
        cc <- responses <- list()
        s <- make(case$model, case$search)
        history <- s$history
        for (prompt in c("First task", "Second task")) {
            expect_error(corteza::turn(prompt, s, tools = read_tool), "Pure CLM requires")
            expect_identical(s$history, history)
        }
        expect_equal(length(cc) + length(responses), 0L)

        s <- make(case$model, case$search, strategy = "hybrid")
        for (prompt in c("First task", "Second task")) {
            expect_equal(corteza::turn(prompt, s, tools = read_tool)$reply, "Completed turn")
        }
        expect_equal(length(responses), 2L)
        expect_equal(length(cc), 0L)
        for (body in responses) {
            expect_false(any(vapply(body$tools, function(tool) identical(tool$name, "edit_context"), logical(1))))
            expect_false(grepl("Conversation context management", body$instructions, fixed = TRUE))
        }
    }

    if ("agent_uses_responses" %in% getNamespaceExports("llm.api")) {
        # The fallback's model, not the session's primary, selects its wire.
        s <- make("gpt-5.5")
        s$provider <- "anthropic"
        s$fallback <- c("gpt-6-astra openai", "fixture openai_compatible")
        called <- character()
        result <- corteza:::.agent_with_fallback(list(model = "fixture-primary",
            provider = "anthropic", tools = read_tool, web_search = FALSE, history = list()), s,
            .call = function(args) {
                called <<- c(called, args$model)
                if (args$model == "fixture-primary") stop("HTTP 429 rate limit")
                expect_true(corteza:::.context_edit_enabled(s))
                list(content = "Fallback completed", history = args$history)
            })
        expect_equal(called, c("fixture-primary", "fixture"))
        expect_equal(result$content, "Fallback completed")
        expect_null(s$context_edit_request)
        corteza:::.fallback_reset()

        # Hybrid fallback can use Responses, but must omit the edit tool
        # before its first response exists. Candidate settings are restored.
        s <- make("gpt-5.5", strategy = "hybrid")
        s$provider <- "anthropic"
        s$fallback <- "gpt-6-astra openai"
        called <- character()
        corteza:::.agent_with_fallback(list(model = "fixture-primary", provider = "anthropic",
            tools = c(read_tool, list(corteza:::.context_edit_schema())),
            web_search = FALSE, history = list()), s, .call = function(args) {
                called <<- c(called, args$model)
                if (args$model == "fixture-primary") stop("HTTP 429 rate limit")
                expect_false(any(vapply(args$tools, function(tool) identical(tool$name, "edit_context"), logical(1))))
                expect_false(corteza:::.context_edit_enabled(s))
                list(content = "Fallback completed", history = args$history)
            })
        expect_equal(called, c("fixture-primary", "gpt-6-astra"))
        expect_null(s$context_edit_request)
    }
})
