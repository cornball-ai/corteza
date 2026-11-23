source("isolate.R", local = TRUE)

local({
    ns <- asNamespace("corteza")
    old_call <- ns$.agent_call
    old_summary <- ns$compact_summarize_slice
    root <- tempfile("context-route-")
    dir.create(root)
    on.exit({
        assignInNamespace(".agent_call", old_call, "corteza")
        assignInNamespace("compact_summarize_slice", old_summary, "corteza")
        ns$.fallback_reset()
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    ns$.fallback_reset()
    s <- corteza::new_session(provider = "anthropic_claude", system = "Fixture instructions",
        model_map = list(cloud = "claude-opus-5-5"), tools_filter = character(),
        web_search = FALSE, context_edit = list(enabled = TRUE, strategy = "clm"))
    s$config <- list(fallback = "gpt-5.6-sol openai_codex", context_compact_tokens = 2000L,
        context_idle_minutes = 0L, subagents = list(context_compaction = list(
            min_messages = 2L, keep_recent_turns = 1L, keep_recent_tokens = 1L)))
    ns$journal_install(s, file.path(root, "journal"))
    s$history <- list(list(role = "user", content = "Earlier request"),
        list(role = "assistant", content = strrep("Earlier completed work. ", 1800L)),
        list(role = "user", content = "Recent request"),
        list(role = "assistant", content = "Recent answer"))
    summaries <- calls <- character()
    assignInNamespace("compact_summarize_slice", function(slice, provider, model, ...) {
        summaries <<- c(summaries, provider)
        expect_identical(provider, "openai_codex")
        expect_identical(model, "gpt-5.6-sol")
        "The earlier request is complete."
    }, "corteza")
    assignInNamespace(".agent_call", function(args) {
        calls <<- c(calls, args$provider)
        if (args$provider == "anthropic_claude") {
            expect_length(summaries, 0L)
            stop("HTTP 429 rate limit")
        }
        expect_identical(args$provider, "openai_codex")
        expect_true(corteza::estimate_history_tokens(args$history) >= 2000L)
        expect_true(ns$.context_edit_pure(s))
        expect_true(any(vapply(args$tools, function(t) identical(t$name, "edit_context"), logical(1))))
        list(content = "Finished", history = args$history, turns = 1L,
             usage = list(input_tokens = 1L, output_tokens = 1L))
    }, "corteza")
    out <- corteza::turn("Continue", s)
    expect_identical(calls, c("anthropic_claude", "openai_codex"))
    expect_length(summaries, 0L)
    expect_identical(s$provider, "anthropic_claude")
    expect_identical(s$context_edit$strategy, "clm")
    expect_true(grepl("pure CLM", ns$.context_edit_status(s)))
    # Overflow recovery after the fallback returns must still use its
    # provider, even though the preferred provider remains unchanged.
    ns$.with_context_route(s, function() {
        expect_identical(s$provider, "openai_codex")
        expect_identical(s$model_map$cloud, "gpt-5.6-sol")
    })
    expect_identical(s$provider, "anthropic_claude")
})

# Returning from Responses restores both the CLM tool and its guidance
# before the first supported-provider request, after its history bridge.
local({
    ns <- asNamespace("corteza")
    ns$.fallback_reset()
    root <- tempfile("return-clm-")
    on.exit({ns$.fallback_reset(); unlink(root, recursive = TRUE)}, add = TRUE)
    s <- corteza::new_session(provider = "anthropic_claude", system = "Fixture instructions",
        model_map = list(cloud = "fixture"), web_search = FALSE,
        context_edit = list(enabled = TRUE, strategy = "clm"))
    ns$journal_install(s, root)
    s$history <- list(list(role = "user", content = "Earlier task"),
        list(type = ".openai_codex_output", output = list(list(type = "message",
            role = "assistant", content = list(list(type = "output_text", text = "Earlier answer"))))))
    s$context_edit_base_system <- "Fixture instructions"
    s$context_edit_system <- ns$.context_edit_system(s$context_edit_base_system, s)
    called <- FALSE
    ns$.agent_with_fallback(list(provider = s$provider, model = "fixture", history = s$history,
        tools = list(), system = s$context_edit_system), s, .call = function(args) {
            called <<- TRUE
            expect_true(ns$.context_edit_pure(s))
            expect_true(any(vapply(args$tools, function(x) identical(x$name, "edit_context"), logical(1))))
            expect_true(grepl("Conversation context management", args$system, fixed = TRUE))
            expect_identical(ns$.history_shape(args$history), "portable")
            list(content = "Done", history = args$history)
        })
    expect_true(called)
})
