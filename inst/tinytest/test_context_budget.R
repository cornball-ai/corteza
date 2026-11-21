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
# Regression: short-form Claude 4 IDs must resolve to their own rows,
# not the 128K unknown-model fallback. "claude-sonnet-4-6" is not a
# prefix of any date-stamped key (e.g. "claude-sonnet-4-20250514"), so
# without explicit short-form entries it would hit the default and
# trigger premature compaction. Opus 4.7 and Sonnet 4.6 have a 1M
# window; Haiku 4.5 has 200K (test_compact_trigger.R has the rest).
expect_equal(corteza::context_limit_for_model("claude-opus-4-7"), 1000000L)
expect_equal(corteza::context_limit_for_model("claude-sonnet-4-6"), 1000000L)
expect_equal(corteza::context_limit_for_model("claude-haiku-4-5"), 200000L)
# A model id is not enough to identify the usable window. The public OpenAI API
# takes 922,000 input tokens for Sol (in a 1.05M window), while the ChatGPT
# Codex product catalog used by the openai_codex provider exposes a 272K
# working window.
expect_equal(corteza::context_limit_for_model("gpt-5.6-sol"), 922000L)
expect_equal(corteza::context_limit_for_model(
    "gpt-5.6-sol", provider = "openai"), 922000L)
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
# Text blocks alone count as the same text in a string does.
expect_equal(corteza::estimate_history_tokens(block_msg),
             corteza::estimate_history_tokens(list(list(role = "user",
                                                        content = "hello world"))))

# A tool's result and a call's arguments count in full. Before 0.8.1 a
# block list was read for `text` alone, and the rest through str(), which
# cuts each string to a line: a 37,000-character file a tool read counted
# as about 100 tokens.
file_text <- strrep("a line of the file a tool read\n", 1000L)
floor_tok <- nchar(file_text) / 4
read_turn <- list(
    list(role = "assistant", content = list(
        list(type = "tool_use", id = "toolu_1", name = "read_file",
             input = list(path = "long.txt")))),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "toolu_1",
             content = file_text))))
expect_true(corteza::estimate_history_tokens(read_turn) >= floor_tok)
# A result made of blocks.
nested <- read_turn
nested[[2L]]$content[[1L]]$content <- list(list(type = "text", text = file_text))
expect_true(corteza::estimate_history_tokens(nested) >= floor_tok)
# A call's arguments, after text in the same turn.
written <- list(list(role = "assistant", content = list(
    list(type = "text", text = "Writing it."),
    list(type = "tool_use", id = "toolu_2", name = "write_file",
         input = list(path = "out.txt", content = file_text)))))
expect_true(corteza::estimate_history_tokens(written) >= floor_tok)
# A chat-completions call keeps its arguments under tool_calls.
chat_call <- list(list(role = "assistant", content = "", tool_calls = list(list(
    id = "call_1", type = "function", `function` = list(name = "write_file",
        arguments = as.character(jsonlite::toJSON(list(path = "out.txt",
            content = file_text), auto_unbox = TRUE)))))))
expect_true(corteza::estimate_history_tokens(chat_call) >= floor_tok)
# What is opaque counts at chars/24: a thinking block's signature and an
# image's base64.
thought <- list(list(role = "assistant", content = list(
    list(type = "thinking", thinking = "", signature = strrep("s", 2400L)),
    list(type = "text", text = "ok"))))
n <- corteza::estimate_history_tokens(thought)
expect_true(n >= 100L && n < 300L)
pictured <- list(list(role = "user", content = list(
    list(type = "image", source = list(type = "base64", media_type = "image/png",
                                       data = strrep("A", 24000L))),
    list(type = "text", text = "what is this?"))))
n <- corteza::estimate_history_tokens(pictured)
expect_true(n >= 1000L && n < 2000L)
# A PDF's base64 is opaque too; a plain-text document's data is text and
# counts as the same text in a text block does (Codex's case on #304: it
# counted at a sixth of that, and a session holding it never compacted).
filed <- list(list(role = "user", content = list(
    list(type = "document", source = list(type = "base64",
                                          media_type = "application/pdf",
                                          data = strrep("A", 24000L))))))
n <- corteza::estimate_history_tokens(filed)
expect_true(n >= 1000L && n < 2000L)
doc_text <- strrep("ordinary document text. ", 1000L)
as_text <- corteza::estimate_history_tokens(list(list(role = "user",
    content = list(list(type = "text", text = doc_text)))))
as_doc <- corteza::estimate_history_tokens(list(list(role = "user",
    content = list(list(type = "document", source = list(type = "text",
        media_type = "text/plain", data = doc_text))))))
expect_true(as_doc >= 0.95 * as_text && as_doc <= 1.1 * as_text,
            info = sprintf("document %d, text %d", as_doc, as_text))

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
