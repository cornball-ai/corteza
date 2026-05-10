# Trigger logic, tool-call counter, token estimator, unfinished
# tool_use detector. All offline.

trig <- corteza:::archival_should_trigger
count_tools <- corteza:::archival_count_tool_calls
est_tokens <- corteza:::archival_estimate_tokens
unfinished <- corteza:::archival_slice_has_unfinished_tool_use

base_cfg <- list(
    enabled = TRUE,
    trigger = list(
        on_max_turns = TRUE,
        token_threshold = 8000L,
        tool_call_threshold = 10L,
        depth_cap = 3L
    ),
    summary = list(style = "structured")
)

# Empty slice: no triggers fire.
expect_false(trig(base_cfg, list()))

# max_turns_hit: triggers when on_max_turns is TRUE.
expect_true(trig(base_cfg, list(list(role = "user", content = "hi")),
                 max_turns_hit = TRUE))

# max_turns_hit but on_max_turns = FALSE: doesn't fire from that alone.
no_max_cfg <- base_cfg
no_max_cfg$trigger$on_max_turns <- FALSE
expect_false(trig(no_max_cfg, list(list(role = "user", content = "hi")),
                  max_turns_hit = TRUE))

# Depth at cap: never triggers, even with max_turns.
expect_false(trig(base_cfg, list(list(role = "user", content = "x")),
                  depth = 3L, max_turns_hit = TRUE))

# Tool-call threshold: 5 tool_use + 5 tool_result = 5 pairs (>=5 cap).
slice_with_tools <- function(n_pairs) {
    blocks <- list()
    for (i in seq_len(n_pairs)) {
        blocks <- c(blocks, list(list(type = "tool_use", id = paste0("t", i),
                                      name = "read_file", input = list(path = "x"))),
                    list(list(type = "tool_result",
                              tool_use_id = paste0("t", i),
                              content = "ok")))
    }
    list(list(role = "assistant", content = blocks))
}
small_thresh_cfg <- base_cfg
small_thresh_cfg$trigger$tool_call_threshold <- 5L
expect_true(trig(small_thresh_cfg, slice_with_tools(5L)))
expect_false(trig(small_thresh_cfg, slice_with_tools(2L)))

# Token threshold: 1000 chars at threshold 100 -> 250 estimated tokens.
big_text <- paste(rep("x", 1000L), collapse = "")
small_token_cfg <- base_cfg
small_token_cfg$trigger$token_threshold <- 100L
slice_text <- list(list(role = "user", content = big_text))
expect_true(trig(small_token_cfg, slice_text))

# Counter unit tests.
expect_equal(count_tools(list()), 0L)
expect_equal(count_tools(slice_with_tools(0L)), 0L)
expect_equal(count_tools(slice_with_tools(3L)), 3L)
# Plain text content contributes nothing.
expect_equal(count_tools(list(list(role = "user", content = "hi"))), 0L)

# OpenAI-style shape (moonshot/kimi/openai): assistant has tool_calls
# field, results come back as role=="tool". 2 calls + 2 results -> 2
# pairs. This shape is what tripped Troy's first archival run: the
# Anthropic-only counter returned 0 and the threshold never fired.
openai_slice <- list(
    list(role = "user", content = "find foo"),
    list(role = "assistant", content = "",
         tool_calls = list(
             list(id = "c1", type = "function",
                  `function` = list(name = "list_files",
                                    arguments = "{\"path\":\".\"}")),
             list(id = "c2", type = "function",
                  `function` = list(name = "read_file",
                                    arguments = "{\"path\":\"foo.R\"}"))
         )),
    list(role = "tool", content = "ok", tool_call_id = "c1"),
    list(role = "tool", content = "contents", tool_call_id = "c2")
)
expect_equal(count_tools(openai_slice), 2L)

# Token estimator unit tests.
expect_equal(est_tokens(list()), 0L)
# 4 chars -> 1 token.
expect_equal(est_tokens(list(list(role = "user", content = "abcd"))), 1L)
# 8 chars -> 2 tokens.
expect_equal(est_tokens(list(list(role = "user", content = "abcdefgh"))), 2L)

# Unfinished tool_use: assistant ends with tool_use, no matching result.
unfinished_slice <- list(list(role = "assistant", content = list(
    list(type = "tool_use", id = "t1", name = "bash", input = list(command = "ls"))
)))
expect_true(unfinished(unfinished_slice))

# Tool_use with matching result: not unfinished.
finished_slice <- list(
    list(role = "assistant", content = list(
        list(type = "tool_use", id = "t1", name = "bash",
             input = list(command = "ls"))
    )),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "t1", content = "ok")
    ))
)
expect_false(unfinished(finished_slice))

# Slice ending with plain assistant text: not unfinished.
plain_slice <- list(list(role = "assistant", content = "all done"))
expect_false(unfinished(plain_slice))
