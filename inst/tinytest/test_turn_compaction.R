# Shared turn-lifecycle compaction. Everything here is offline: llm.api::agent
# and the summarizer are temporarily replaced inside function scopes.

codex_tool_trajectory <- function() {
    list(
        list(role = "user", content = "play"),
        list(type = ".openai_codex_output", output = list(
            list(type = "function_call", call_id = "a", name = "run_r",
                 arguments = "{}"))),
        list(type = "function_call_output", call_id = "a", output = "a ok"),
        list(type = ".openai_codex_output", output = list(
            list(type = "function_call", call_id = "b", name = "run_r",
                 arguments = "{}"))),
        list(type = "function_call_output", call_id = "b", output = "b ok"),
        list(type = ".openai_codex_output", output = list(
            list(type = "function_call", call_id = "c", name = "run_r",
                 arguments = "{}"))),
        list(type = "function_call_output", call_id = "c", output = "c ok")
    )
}

compact_test_config <- function(threshold = 0) {
    list(
        context_compact_pct = threshold,
        subagents = list(context_compaction = list(
            mode = "inherit_strict",
            compact_pct = threshold,
            keep_recent_turns = 1L,
            keep_recent_tokens = 1L,
            min_messages = 2L,
            timeout_seconds = 1L
        ))
    )
}

stub_summary <- function(...) {
    x <- "durable earlier progress"
    attr(x, "usage") <- list(input_tokens = 2, output_tokens = 1, cost = 0.01)
    x
}

run_checkpoint_compaction_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)

    stub_agent <- function(prompt, tools = list(), tool_handler = NULL,
                           system = NULL, model = NULL,
                           provider = "openai_codex", max_turns = 20L,
                           verbose = TRUE, history = NULL,
                           history_callback = NULL,
                           checkpoint_callback = NULL, ...) {
        h <- codex_tool_trajectory()
        replacement <- checkpoint_callback(h, list(
            agent_turn = 3L, provider = provider, model = model,
            assistant_text = "", tool_call_count = 1L,
            usage = list(input_tokens = 10, output_tokens = 2)
        ))
        if (!is.null(replacement)) h <- replacement$history
        h[[length(h) + 1L]] <- list(
            type = ".openai_codex_output",
            output = list(list(type = "message", role = "assistant",
                               content = list(list(type = "output_text",
                                                   text = "done"))))
        )
        if (is.function(history_callback)) history_callback(h)
        list(content = "done", model = model, provider = provider,
             turns = 4L, history = h,
             usage = list(input_tokens = 10, output_tokens = 2,
                          total_tokens = 12, cost = 0.1))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")
    assignInNamespace("compact_summarize_slice", stub_summary, ns = "corteza")

    session <- corteza::new_session(
        provider = "openai_codex",
        model_map = list(cloud = "gpt-5.6-sol", local = "qwen3.5:9b"),
        verbose = FALSE
    )
    session$config <- compact_test_config(0)
    seen <- NULL
    session$on_compaction <- function(event) seen <<- event
    result <- corteza::turn("play", session, tools = list())

    expect_equal(result$reply, "done")
    expect_equal(session$compaction_count, 1L)
    expect_equal(length(seen$history_before), 7L)
    expect_equal(seen$cut, 5L)
    expect_equal(seen$usage$input_tokens, 2)
    expect_equal(session$history[[1L]]$role, "user")
    expect_equal(session$history[[2L]]$type, ".openai_codex_output")
}
run_checkpoint_compaction_test()

run_overflow_recovery_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)

    calls <- 0L
    resumed_history <- NULL
    stub_agent <- function(prompt, tools = list(), tool_handler = NULL,
                           system = NULL, model = NULL,
                           provider = "openai_codex", max_turns = 20L,
                           verbose = TRUE, history = NULL,
                           history_callback = NULL,
                           checkpoint_callback = NULL, ...) {
        calls <<- calls + 1L
        if (calls == 1L) {
            return(list(
                content = "[Output truncated: model_context_window_exceeded]",
                truncated = TRUE,
                truncation_reason = "model_context_window_exceeded",
                model = model, provider = provider, turns = 1L,
                history = codex_tool_trajectory(),
                usage = list(input_tokens = 100, output_tokens = 5,
                             total_tokens = 105,
                             cache_creation = list(
                                 ephemeral_5m_input_tokens = 3),
                             cost = 0.5),
                citations = list(list(url = "first"))
            ))
        }
        expect_null(prompt)
        resumed_history <<- history
        list(content = "recovered", model = model, provider = provider,
             turns = 2L, history = history,
             usage = list(input_tokens = 20, output_tokens = 4,
                          total_tokens = 24,
                          cache_creation = list(
                              ephemeral_5m_input_tokens = 2),
                          cost = 0.2),
             citations = list(list(url = "second")))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")
    assignInNamespace("compact_summarize_slice", stub_summary, ns = "corteza")

    session <- corteza::new_session(
        provider = "openai_codex",
        model_map = list(cloud = "gpt-5.6-sol", local = "qwen3.5:9b"),
        verbose = FALSE
    )
    session$config <- compact_test_config(90)
    result <- corteza::turn("play", session, tools = list())

    expect_equal(calls, 2L)
    expect_equal(result$reply, "recovered")
    expect_true(isTRUE(result$raw$corteza_overflow_retry))
    expect_true(isTRUE(result$raw$corteza_overflow_recovered))
    expect_equal(result$raw$turns, 3L)
    expect_equal(result$usage$input_tokens, 120)
    expect_equal(result$usage$output_tokens, 9)
    expect_equal(result$usage$total_tokens, 129)
    expect_equal(result$usage$cache_creation$ephemeral_5m_input_tokens, 5)
    expect_equal(result$usage$cost, 0.7)
    expect_equal(length(result$raw$citations), 2L)
    expect_equal(resumed_history[[1L]]$role, "user")
    expect_equal(resumed_history[[2L]]$type, ".openai_codex_output")
}
run_overflow_recovery_test()

# Unknown cost on either attempt must remain unknown in the combined result.
expect_true(is.na(corteza:::.turn_add_usage(
    list(input_tokens = 1, cost = NA_real_),
    list(input_tokens = 2, cost = 0.1)
)$cost))
