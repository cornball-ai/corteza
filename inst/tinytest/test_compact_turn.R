# Pure-function tests for the subagent compaction helpers. These
# exercise the policy resolution, the cut-point finder, and the
# pure history-rewrite. The summarizer call itself is gated to
# at_home in test_subagent_callr.R.

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
# The exact value depends on how user_starts iterate, but the
# constraint is: cut must not land on entry 2 (tool_use paired with
# tool_result still in the kept tail).
expect_true(cut_toolchain != 2L,
            info = "cut must not split tool_use / tool_result pair")

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
