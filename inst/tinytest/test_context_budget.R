# Context-budget helpers. Offline tests — no provider call, no I/O.

# estimate_text_tokens ----

# Empty / NULL / empty-string all return 0.
expect_equal(corteza::estimate_text_tokens(NULL), 0L)
expect_equal(corteza::estimate_text_tokens(character(0)), 0L)
expect_equal(corteza::estimate_text_tokens(""), 0L)

# ceil(n / 4) — basic.
expect_equal(corteza::estimate_text_tokens("abcd"), 1L)
expect_equal(corteza::estimate_text_tokens("abcde"), 2L)
expect_equal(corteza::estimate_text_tokens(strrep("x", 40L)), 10L)

# Vector input gets collapsed with newlines (counted in chars).
expect_equal(corteza::estimate_text_tokens(c("ab", "cd")),
             corteza::estimate_text_tokens("ab\ncd"))

# format_tokens ----

expect_equal(corteza::format_tokens(42L), "42")
expect_equal(corteza::format_tokens(1500L), "1.5K")
expect_equal(corteza::format_tokens(2000000L), "2.0M")

# context_limit_for_model ----

# Known model resolves exactly.
expect_equal(corteza::context_limit_for_model("gpt-4o"), 128000L)
expect_equal(corteza::context_limit_for_model("claude-sonnet-4-20250514"),
             200000L)
# Regression: short-form Claude 4 IDs must resolve to 200K, not the
# 128K unknown-model fallback. "claude-sonnet-4-6" is not a prefix of
# any date-stamped key (e.g. "claude-sonnet-4-20250514"), so without
# explicit short-form entries it would hit the default and trigger
# premature compaction.
expect_equal(corteza::context_limit_for_model("claude-opus-4-7"), 200000L)
expect_equal(corteza::context_limit_for_model("claude-sonnet-4-6"), 200000L)
expect_equal(corteza::context_limit_for_model("claude-haiku-4-5"), 200000L)
# A model id is not enough to identify the usable window. The public OpenAI API
# exposes 1.05M for Sol, while the ChatGPT Codex product catalog used by the
# openai_codex provider exposes a 272K working window.
expect_equal(corteza::context_limit_for_model("gpt-5.6-sol"), 1050000L)
expect_equal(corteza::context_limit_for_model(
    "gpt-5.6-sol", provider = "openai"), 1050000L)
expect_equal(corteza::context_limit_for_model(
    "gpt-5.6-sol", provider = "openai_codex"), 272000L)
expect_equal(corteza::context_limit_for_model(
    "gpt-5.6-sol-2026-09-01", provider = "openai_codex"), 272000L)
# Prefix match works either direction (shorter caller, shorter table key).
expect_equal(corteza::context_limit_for_model("gpt-4o-mini-2024-07-18"),
             128000L)
expect_equal(corteza::context_limit_for_model("claude-3-haiku"),
             200000L)
# Unknown model falls back to a sane default.
expect_equal(corteza::context_limit_for_model("totally-fictional-llm"),
             128000L)

# "No model named" must fall back too, not crash on a zero-length/NA
# subscript (regression: a model-less chat() session passed NULL here
# and blew up the post-turn context meter).
expect_equal(corteza::context_limit_for_model(NULL), 128000L)
expect_equal(corteza::context_limit_for_model(character(0)), 128000L)
expect_equal(corteza::context_limit_for_model(""), 128000L)
expect_equal(corteza::context_limit_for_model(NA_character_), 128000L)

# estimate_history_tokens ----

# Empty history -> 0.
expect_equal(corteza::estimate_history_tokens(list()), 0L)
expect_equal(corteza::estimate_history_tokens(NULL), 0L)

# History with one message: text tokens + 6L overhead.
one_msg <- list(list(role = "user", content = "hello"))
expect_true(corteza::estimate_history_tokens(one_msg) >= 6L)
# Each additional message adds at least 6L.
two_msg <- list(list(role = "user", content = "hello"),
                list(role = "assistant", content = "hi"))
expect_true(corteza::estimate_history_tokens(two_msg) >=
            corteza::estimate_history_tokens(one_msg) + 6L)

# Block-content messages (list with type/text blocks) also count.
block_msg <- list(list(role = "user",
                       content = list(list(type = "text",
                                           text = "hello world"))))
expect_true(corteza::estimate_history_tokens(block_msg) > 6L)

# OpenAI Responses/Codex history is role-less: assistant output lives under
# $output and tool results use function_call_output. Both the call arguments
# and opaque reasoning state are part of the context sent on the next request.
codex_visible <- list(
    list(type = ".openai_codex_output", output = list(
        list(type = "function_call", call_id = "call_1", name = "run_r",
             arguments = '{"code":"x <- 123456789"}')
    )),
    list(type = "function_call_output", call_id = "call_1", output = "[1] 1")
)
expect_true(corteza::estimate_history_tokens(codex_visible) > 20L)
codex_reasoning <- codex_visible
codex_reasoning[[1L]]$output[[1L]]$encrypted_content <- strrep("x", 2400L)
expect_true(corteza::estimate_history_tokens(codex_reasoning) >=
            corteza::estimate_history_tokens(codex_visible) + 100L)

# estimate_tool_tokens ----

# No tools -> 0.
expect_equal(corteza::estimate_tool_tokens(NULL), 0L)
expect_equal(corteza::estimate_tool_tokens(list()), 0L)
# A tool list contributes both JSON text tokens AND 12L per tool overhead.
tools <- list(list(name = "read_file", description = "Read a file"))
expect_true(corteza::estimate_tool_tokens(tools) >= 12L)

# estimate_live_context_tokens ----

session_empty <- list(messages = list())
expect_equal(corteza::estimate_live_context_tokens(session_empty), 0L)

session_msg <- list(messages = list(list(role = "user",
                                         content = "this is the prompt")))
# Including a system prompt and tools should strictly increase the count.
base <- corteza::estimate_live_context_tokens(session_msg)
with_sys <- corteza::estimate_live_context_tokens(
    session_msg, system_prompt = "You are a helpful assistant.")
expect_true(with_sys > base)
with_tools <- corteza::estimate_live_context_tokens(
    session_msg, tools = list(list(name = "x", description = "y")))
expect_true(with_tools > base)

# context_usage_pct ----

# Empty session -> 0%.
expect_equal(corteza::context_usage_pct(session_empty, model = "gpt-4o"), 0)

# A session with content reports a positive but small percentage for
# a 128k-context model.
pct <- corteza::context_usage_pct(session_msg, model = "gpt-4o")
expect_true(pct > 0 && pct < 1)

# CLI vs turn_session shape ----
# CLI sessions store messages under $messages; turn_session (and
# therefore subagents) stores them under $history. The estimator
# must count tokens for either shape, otherwise the upcoming
# subagent compaction path silently undercounts to 0.
msgs <- list(list(role = "user", content = "abcd efgh"))
cli_shape  <- list(messages = msgs)
turn_shape <- list(history  = msgs)
expect_equal(corteza::estimate_live_context_tokens(cli_shape),
             corteza::estimate_live_context_tokens(turn_shape))
expect_true(corteza::estimate_live_context_tokens(turn_shape) > 0L)
