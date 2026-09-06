# Pure-function tests for the subagent compaction helpers. These
# exercise the policy resolution, the cut-point finder, and the
# pure history-rewrite. The summarizer call itself is gated to
# at_home in test_subagent_callr.R.

# Summarizer provider arguments --------

run_compact_summary_provider_test <- function() {
    original_chat <- get("chat", envir = asNamespace("llm.api"))
    on.exit(assignInNamespace("chat", original_chat, ns = "llm.api"),
            add = TRUE)
    seen <- NULL
    stub_chat <- function(...) {
        seen <<- list(...)
        list(content = "summary", usage = list())
    }
    assignInNamespace("chat", stub_chat, ns = "llm.api")
    slice <- list(list(role = "user", content = "earlier work"))

    codex <- corteza:::compact_summarize_slice(
        slice, provider = "openai_codex", model = "gpt-5.6-sol")
    expect_equal(as.character(codex), "summary")
    expect_null(seen$temperature)

    anthropic <- corteza:::compact_summarize_slice(
        slice, provider = "anthropic", model = "claude-opus-5")
    expect_equal(as.character(anthropic), "summary")
    expect_equal(seen$temperature, 0.3)
}
run_compact_summary_provider_test()

# Threshold resolution --------

cfg_base <- list(
    context_compact_pct = 90L,
    subagents = list(
        context_compaction = list(
            mode = "inherit_strict",
            compact_pct = 75L,
            keep_recent_turns = 1L,
            min_messages = 6L,
            timeout_seconds = 60L)))

# inherit_strict: effective threshold is the min of parent and child.
expect_equal(corteza:::subagent_compact_threshold(cfg_base), 75)

# inherit: parent's threshold wins.
cfg_inherit <- cfg_base
cfg_inherit$subagents$context_compaction$mode <- "inherit"
expect_equal(corteza:::subagent_compact_threshold(cfg_inherit), 90)

# off: NULL means caller should skip entirely.
cfg_off <- cfg_base
cfg_off$subagents$context_compaction$mode <- "off"
expect_null(corteza:::subagent_compact_threshold(cfg_off))

# inherit_strict still wins when child is set higher than parent
# (strict means equal-or-lower).
cfg_high <- cfg_base
cfg_high$subagents$context_compaction$compact_pct <- 95L
expect_equal(corteza:::subagent_compact_threshold(cfg_high), 90)

# compact_find_cut --------

# Empty history: cut at 0.
expect_equal(corteza:::compact_find_cut(list()), 0L)

# Need at least keep_recent_turns + 1 user turns to compact anything.
one_turn <- list(
    list(role = "user", content = "first"),
    list(role = "assistant", content = "reply"))
expect_equal(corteza:::compact_find_cut(one_turn, keep_recent_turns = 1L), 0L)

# With three turns and keep_recent_turns = 1, the cut lands just
# before the start of the last user turn (index 5 = last user
# message, so cut = 4).
three_turns <- list(
    list(role = "user",      content = "q1"),  # 1
    list(role = "assistant", content = "a1"),  # 2
    list(role = "user",      content = "q2"),  # 3
    list(role = "assistant", content = "a2"),  # 4
    list(role = "user",      content = "q3"),  # 5
    list(role = "assistant", content = "a3"))  # 6
expect_equal(corteza:::compact_find_cut(three_turns, keep_recent_turns = 1L),
             4L)

# keep_recent_turns = 2: keep last two turns, cut after entry 2.
expect_equal(corteza:::compact_find_cut(three_turns, keep_recent_turns = 2L),
             2L)

# Open tool_use/tool_result pair: cut walks back so the pair stays
# together. Construct: user, assistant-with-tool_use, tool_result,
# user, assistant. With keep_recent_turns=1 the natural cut is at 3
# (before the last user), but entry 2 (assistant tool_use) pairs
# with entry 3 (tool_result inside the kept tail), so the cut must
# walk back below 2.
toolchain <- list(
    list(role = "user", content = "do a thing"),
    list(role = "assistant",
         content = list(list(type = "tool_use",
                             id = "tu_1", name = "x", input = list()))),
    list(role = "user",
         content = list(list(type = "tool_result",
                             tool_use_id = "tu_1", content = "ok"))),
    list(role = "user", content = "next"),
    list(role = "assistant", content = "reply"))
cut_toolchain <- corteza:::compact_find_cut(toolchain, keep_recent_turns = 1L)
# Critical: tool_result messages (role == "user" but content is a
# tool_result block) must NOT count as user-turn boundaries — they
# are the second half of the previous assistant tool_use. The only
# real user turns here are entry 1 ("do a thing") and entry 4
# ("next"). With keep_recent_turns = 1 the safe answer is cut = 3
# (summarize entries 1-3, keeping the tool_use/tool_result pair
# intact in the prefix). The unsafe cut is 2, which would split
# the pair across the boundary.
expect_true(cut_toolchain != 2L,
            info = "cut must not split tool_use / tool_result pair")
expect_equal(cut_toolchain, 3L,
             info = "cut sits after the tool_use pair, before the next user turn")

# Regression for the more subtle P1: a long turn with multiple
# tool_use/tool_result rounds before the final assistant. None of
# the user-tagged tool_result messages should look like a new user
# turn, so with keep_recent_turns = 1 the entire turn must stay
# together (cut = 0).
multi_tool_turn <- list(
    list(role = "user", content = "do it"),               # 1 real user
    list(role = "assistant",
         content = list(list(type = "tool_use",
                             id = "tu_a", name = "x", input = list()))),
    list(role = "user",                                    # 3 tool_result, NOT a turn
         content = list(list(type = "tool_result",
                             tool_use_id = "tu_a", content = "..."))),
    list(role = "assistant",
         content = list(list(type = "tool_use",
                             id = "tu_b", name = "y", input = list()))),
    list(role = "user",                                    # 5 tool_result, NOT a turn
         content = list(list(type = "tool_result",
                             tool_use_id = "tu_b", content = "..."))),
    list(role = "assistant", content = "all done"))
expect_equal(
    corteza:::compact_find_cut(multi_tool_turn, keep_recent_turns = 1L),
    0L,
    info = "tool_result-only user msgs must not split a single turn")

# Codex Responses uses role-less output/result entries. A natural cut before
# the latest user prompt may include a complete call pair, but must walk back
# if the call is in the prefix and its result is in the retained tail.
codex_pair <- list(
    list(role = "user", content = "first"),
    list(type = ".openai_codex_output", output = list(
        list(type = "function_call", call_id = "call_1", name = "run_r",
             arguments = '{"code":"1+1"}')
    )),
    list(type = "function_call_output", call_id = "call_1", output = "2"),
    list(role = "user", content = "continue"),
    list(type = ".openai_codex_output", output = list(
        list(type = "message", role = "assistant",
             content = list(list(type = "output_text", text = "done")))
    ))
)
expect_equal(corteza:::compact_find_cut(codex_pair, 1L), 3L)
expect_true(corteza:::compact_prefix_has_unmatched_tool_use(
    codex_pair, 2L))
expect_false(corteza:::compact_prefix_has_unmatched_tool_use(
    codex_pair, 3L))

# Pi-style split-turn compaction: one autonomous user request may grow into
# hundreds of complete assistant/tool rounds. With a deliberately tiny retained
# token tail, compact through the last complete pair while keeping the newest
# call and result together.
anthropic_trajectory <- list(
    list(role = "user", content = "play"),
    list(role = "assistant", content = list(
        list(type = "tool_use", id = "a", name = "run_r", input = list()))),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "a", content = "a ok"))),
    list(role = "assistant", content = list(
        list(type = "tool_use", id = "b", name = "run_r", input = list()))),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "b", content = "b ok"))),
    list(role = "assistant", content = list(
        list(type = "tool_use", id = "c", name = "run_r", input = list()))),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "c", content = "c ok")))
)
anthropic_cut <- corteza:::compact_find_cut(
    anthropic_trajectory, keep_recent_tokens = 1L)
expect_equal(anthropic_cut, 5L)
expect_false(corteza:::compact_prefix_has_unmatched_tool_use(
    anthropic_trajectory, anthropic_cut))
expect_false(corteza:::compact_tail_starts_with_tool_result(
    anthropic_trajectory, anthropic_cut))
anthropic_rewrite <- corteza:::compact_rewrite_history(
    anthropic_trajectory, anthropic_cut, "earlier progress")
expect_equal(anthropic_rewrite[[1L]]$role, "user")
expect_equal(anthropic_rewrite[[2L]]$role, "assistant")

codex_trajectory <- list(
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
codex_cut <- corteza:::compact_find_cut(
    codex_trajectory, keep_recent_tokens = 1L)
expect_equal(codex_cut, 5L)
expect_false(corteza:::compact_prefix_has_unmatched_tool_use(
    codex_trajectory, codex_cut))
expect_false(corteza:::compact_tail_starts_with_tool_result(
    codex_trajectory, codex_cut))
codex_rewrite <- corteza:::compact_rewrite_history(
    codex_trajectory, codex_cut, "earlier progress")
expect_equal(codex_rewrite[[1L]]$role, "user")
expect_equal(codex_rewrite[[2L]]$type, ".openai_codex_output")

# Plain character content is not a block list and must not make the pairing
# scanner throw while it checks a mixed provider-native history.
expect_false(corteza:::compact_prefix_has_unmatched_tool_use(
    list(list(role = "user", content = "plain"),
         list(role = "assistant", content = "reply"),
         list(role = "user", content = "next")), 2L))

# compact_entry_is_tool_result_only --------

expect_false(corteza:::compact_entry_is_tool_result_only(
    list(role = "user", content = "plain text")))
expect_false(corteza:::compact_entry_is_tool_result_only(
    list(role = "user",
         content = list(list(type = "text", text = "hi")))))
expect_true(corteza:::compact_entry_is_tool_result_only(
    list(role = "user",
         content = list(list(type = "tool_result",
                             tool_use_id = "tu_1", content = "ok")))))
# Mixed content (text + tool_result) is not tool_result-only.
expect_false(corteza:::compact_entry_is_tool_result_only(
    list(role = "user",
         content = list(list(type = "text", text = "hi"),
                        list(type = "tool_result", tool_use_id = "tu_1",
                             content = "ok")))))

# compact_rewrite_history --------

# Pure rewrite: returns a new list with one summary entry prepended
# to the kept tail; doesn't mutate the input.
hist <- three_turns
new_hist <- corteza:::compact_rewrite_history(hist, cut = 4L,
                                              summary = "summary text")
expect_equal(length(new_hist), 3L,
             info = "rewrite leaves 1 summary + 2 kept entries")
expect_equal(new_hist[[1]]$role, "assistant")
expect_true(grepl("compacted history", new_hist[[1]]$content, fixed = TRUE))
expect_true(grepl("summary text", new_hist[[1]]$content, fixed = TRUE))
expect_equal(new_hist[[2]]$content, "q3")
expect_equal(new_hist[[3]]$content, "a3")
# Original untouched.
expect_equal(length(hist), 6L)

# cut at 0 or >= length leaves history unchanged.
expect_identical(
    corteza:::compact_rewrite_history(hist, cut = 0L, summary = "s"),
    hist)
expect_identical(
    corteza:::compact_rewrite_history(hist, cut = length(hist),
                                      summary = "s"),
    hist)

# maybe_compact_turn_session: archive holders are skipped --------

# When kind == "archive_holder", the helper bails immediately even
# if the threshold would otherwise trigger. We don't need a real
# LLM call to verify this.
fake_session <- new.env(parent = emptyenv())
fake_session$history <- three_turns
fake_session$provider <- "anthropic"
expect_false(
    isTRUE(corteza:::maybe_compact_turn_session(
        fake_session, cfg_base, kind = "archive_holder")))
# History untouched.
expect_equal(length(fake_session$history), 6L)

# Mode "off" also bails immediately.
expect_false(
    isTRUE(corteza:::maybe_compact_turn_session(
        fake_session, cfg_off)))
expect_equal(length(fake_session$history), 6L)

# History shorter than min_messages bails (no LLM call).
short <- one_turn
fake_session$history <- short
expect_false(
    isTRUE(corteza:::maybe_compact_turn_session(
        fake_session, cfg_base)))
expect_equal(length(fake_session$history), 2L)
