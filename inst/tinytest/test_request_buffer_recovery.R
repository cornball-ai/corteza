# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

# Codex request-buffer pressure and recovery. Entirely offline: agent and
# summarizer calls are replaced inside function scopes.

request_buffer_trajectory <- function(id = "a", encrypted_chars = 0L) {
    reasoning <- list(type = "reasoning", content = list())
    if (encrypted_chars > 0L) {
        reasoning$encrypted_content <- paste(rep("x", encrypted_chars),
                                             collapse = "")
    }
    list(
        list(role = "user", content = paste("play", id)),
        list(type = ".openai_codex_output", output = list(
            reasoning,
            list(type = "function_call", call_id = id, name = "run_r",
                 arguments = paste0("{\"code\":\"", id, " <- 1\"}"))
        )),
        list(type = "function_call_output", call_id = id,
             output = paste(id, "ok"))
    )
}

request_buffer_config <- function(byte_limit = 900000L,
                                  token_threshold = 99) {
    list(
        context_compact_pct = token_threshold,
        context_compact_bytes = byte_limit,
        context_request_buffer_retries = 3L,
        subagents = list(context_compaction = list(
            mode = "inherit_strict",
            compact_pct = token_threshold,
            keep_recent_turns = 1L,
            keep_recent_tokens = 1L,
            min_messages = 2L,
            timeout_seconds = 1L
        ))
    )
}

request_buffer_summary <- function(...) {
    value <- "durable compacted progress"
    attr(value, "usage") <- list(input_tokens = 2, output_tokens = 1,
                                 total_tokens = 3, cost = 0.01)
    value
}

run_request_byte_trigger_test <- function() {
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice", request_buffer_summary,
                      ns = "corteza")

    session <- corteza::new_session(
        provider = "openai_codex",
        model_map = list(cloud = "gpt-5.6-sol", local = "qwen3.5:9b"),
        verbose = FALSE
    )
    session$config <- request_buffer_config(byte_limit = 1000L)
    session$context_window <- 100000000L
    session$history <- c(request_buffer_trajectory("a", 4000L),
                         request_buffer_trajectory("b", 4000L))
    seen <- NULL
    session$on_compaction <- function(event) seen <<- event

    compacted <- corteza:::maybe_compact_turn_session(
        session, session$config, tools = list(), system = "test")
    expect_true(isTRUE(compacted))
    expect_equal(seen$trigger, "request_bytes")
    expect_true(seen$request_bytes >= seen$byte_limit)
    expect_true(seen$context_pct < seen$threshold_pct)

    # The default byte ceiling is deliberately Codex-specific. Other wires
    # retain their token policy unless a future transport establishes its own
    # serialized-size constraint.
    other <- corteza::new_session(
        provider = "anthropic",
        model_map = list(cloud = "claude-opus-4-1",
                         local = "qwen3.5:9b"),
        verbose = FALSE
    )
    other$config <- request_buffer_config(byte_limit = 100L)
    other$context_window <- 100000000L
    other$history <- c(request_buffer_trajectory("c", 4000L),
                       request_buffer_trajectory("d", 4000L))
    expect_false(isTRUE(corteza:::maybe_compact_turn_session(
        other, other$config, tools = list(), system = "test")))
}
run_request_byte_trigger_test()

run_repeated_request_byte_compaction_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice", request_buffer_summary,
                      ns = "corteza")

    stub_agent <- function(prompt, tools = list(), tool_handler = NULL,
                           system = NULL, model = NULL,
                           provider = "openai_codex", max_turns = 20L,
                           verbose = TRUE, history = NULL,
                           history_callback = NULL,
                           checkpoint_callback = NULL, ...) {
        h <- c(request_buffer_trajectory("a", 4000L),
               request_buffer_trajectory("b", 4000L))
        replacement <- checkpoint_callback(h, list(
            agent_turn = 2L, provider = provider, model = model,
            assistant_text = "", tool_call_count = 1L,
            usage = list(input_tokens = 10, output_tokens = 2)
        ))
        if (!is.null(replacement)) h <- replacement$history

        h <- c(h, request_buffer_trajectory("c", 4000L),
               request_buffer_trajectory("d", 4000L))
        replacement <- checkpoint_callback(h, list(
            agent_turn = 4L, provider = provider, model = model,
            assistant_text = "", tool_call_count = 1L,
            usage = list(input_tokens = 12, output_tokens = 3)
        ))
        if (!is.null(replacement)) h <- replacement$history
        h[[length(h) + 1L]] <- list(role = "assistant", content = "done")
        if (is.function(history_callback)) history_callback(h)
        list(content = "done", model = model, provider = provider,
             turns = 5L, history = h,
             usage = list(input_tokens = 22, output_tokens = 6,
                          total_tokens = 28, cost = 0.1))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")

    session <- corteza::new_session(
        provider = "openai_codex",
        model_map = list(cloud = "gpt-5.6-sol", local = "qwen3.5:9b"),
        verbose = FALSE
    )
    session$config <- request_buffer_config(byte_limit = 1000L)
    session$context_window <- 100000000L
    result <- corteza::turn("play", session, tools = list())

    expect_equal(result$reply, "done")
    expect_equal(session$compaction_count, 2L)
}
run_repeated_request_byte_compaction_test()

run_request_buffer_507_recovery_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice", request_buffer_summary,
                      ns = "corteza")

    calls <- 0L
    resumed_history <- NULL
    original_history <- NULL
    stub_agent <- function(prompt, tools = list(), tool_handler = NULL,
                           system = NULL, model = NULL,
                           provider = "openai_codex", max_turns = 20L,
                           verbose = TRUE, history = NULL,
                           history_callback = NULL,
                           checkpoint_callback = NULL, ...) {
        calls <<- calls + 1L
        if (calls == 1L) {
            h <- c(request_buffer_trajectory("a"),
                   request_buffer_trajectory("b"),
                   request_buffer_trajectory("c"))
            original_history <<- h
            if (is.function(history_callback)) history_callback(h)
            checkpoint_callback(h, list(
                agent_turn = 3L, provider = provider, model = model,
                assistant_text = "", tool_call_count = 1L,
                usage = list(input_tokens = 10, output_tokens = 2,
                             total_tokens = 12, cost = 0.1)
            ))
            stop("API error (507): exceeded request buffer limit while retrying upstream",
                 call. = FALSE)
        }
        expect_null(prompt)
        resumed_history <<- history
        list(content = "recovered", model = model, provider = provider,
             turns = 2L, history = history,
             usage = list(input_tokens = 20, output_tokens = 4,
                          total_tokens = 24, cost = 0.2))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")

    session <- corteza::new_session(
        provider = "openai_codex",
        model_map = list(cloud = "gpt-5.6-sol", local = "qwen3.5:9b"),
        verbose = FALSE
    )
    session$config <- request_buffer_config()
    seen <- NULL
    session$on_compaction <- function(event) seen <<- event
    result <- corteza::turn("play", session, tools = list())

    expect_equal(calls, 2L)
    expect_equal(result$reply, "recovered")
    expect_equal(seen$reason, "request_buffer_overflow")
    expect_equal(seen$trigger, "forced")
    expect_true(length(resumed_history) < length(original_history))
    expect_equal(result$raw$corteza_request_buffer_retries, 1L)
    expect_true(isTRUE(result$raw$corteza_overflow_retry))
    expect_true(isTRUE(result$raw$corteza_overflow_recovered))
    expect_equal(result$raw$turns, 5L)
    expect_equal(result$usage$input_tokens, 30)
    expect_equal(result$usage$output_tokens, 6)
    expect_equal(result$usage$total_tokens, 36)
    expect_equal(result$usage$cost, 0.3)
}
run_request_buffer_507_recovery_test()

run_context_then_request_overflow_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice", request_buffer_summary,
                      ns = "corteza")

    calls <- 0L
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
                history = c(request_buffer_trajectory("a"),
                            request_buffer_trajectory("b"),
                            request_buffer_trajectory("c")),
                usage = list(input_tokens = 100, output_tokens = 5,
                             total_tokens = 105, cost = 0.5)
            ))
        }
        expect_null(prompt)
        if (calls == 2L) {
            h <- c(history, request_buffer_trajectory("d"),
                   request_buffer_trajectory("e"))
            if (is.function(history_callback)) history_callback(h)
            checkpoint_callback(h, list(
                agent_turn = 3L, provider = provider, model = model,
                assistant_text = "", tool_call_count = 1L,
                usage = list(input_tokens = 10, output_tokens = 2,
                             total_tokens = 12, cost = 0.1)
            ))
            stop("API error (507): exceeded request buffer limit while retrying upstream",
                 call. = FALSE)
        }
        list(content = "recovered twice", model = model, provider = provider,
             turns = 2L, history = history,
             usage = list(input_tokens = 20, output_tokens = 4,
                          total_tokens = 24, cost = 0.2))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")

    session <- corteza::new_session(
        provider = "openai_codex",
        model_map = list(cloud = "gpt-5.6-sol", local = "qwen3.5:9b"),
        verbose = FALSE
    )
    session$config <- request_buffer_config()
    result <- corteza::turn("play", session, tools = list())

    expect_equal(calls, 3L)
    expect_equal(result$reply, "recovered twice")
    expect_equal(result$raw$corteza_request_buffer_retries, 1L)
    expect_true(isTRUE(result$raw$corteza_overflow_recovered))
    expect_equal(result$raw$turns, 6L)
    expect_equal(result$usage$input_tokens, 130)
    expect_equal(result$usage$output_tokens, 11)
    expect_equal(result$usage$total_tokens, 141)
    expect_equal(result$usage$cost, 0.8)
}
run_context_then_request_overflow_test()

expect_true(corteza:::.is_request_buffer_error(simpleError(
    "API error (507): exceeded request buffer limit while retrying upstream")))
expect_false(corteza:::.is_request_buffer_error(simpleError(
    "API error (507): unrelated storage failure")))
expect_false(corteza:::.is_limit_error(simpleError(
    "API error (507): exceeded request buffer limit while retrying upstream")))
