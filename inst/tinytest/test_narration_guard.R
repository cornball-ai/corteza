library(tinytest)

# Silent-streak narration guard (turn.R). Session-scoped state driven by
# the llm.api per-call context snapshot (call$model_context).

ctx <- function(text, idx, cnt) {
    list(assistant_text = text, call_index = idx, call_count = cnt)
}

# --- .update_silent_streak: counts once per turn (call_index == 1) ---
s <- new.env()
# A silent turn (no narration) extends the streak.
corteza:::.update_silent_streak(s, ctx("", 1L, 2L))
expect_equal(s$silent_streak, 1L)
corteza:::.update_silent_streak(s, ctx("", 1L, 1L))
expect_equal(s$silent_streak, 2L)
# Any narration resets it.
corteza:::.update_silent_streak(s, ctx("Now reading the config.", 1L, 1L))
expect_equal(s$silent_streak, 0L)
# Only the first call of a turn counts; later calls are no-ops.
s$silent_streak <- 5L
corteza:::.update_silent_streak(s, ctx("", 2L, 3L))
expect_equal(s$silent_streak, 5L)
# No context (older llm.api / two-arg handler) -> no-op.
corteza:::.update_silent_streak(s, NULL)
expect_equal(s$silent_streak, 5L)

# --- .maybe_append_narration_nudge: final call of a silent batch ---
local({
    op <- options(corteza.narration_streak = 3L)
    on.exit(options(op), add = TRUE)

    s <- new.env()
    s$silent_streak <- 3L
    # Not the last call -> untouched.
    expect_equal(corteza:::.maybe_append_narration_nudge("R", s, ctx("", 1L, 2L)), "R")
    expect_equal(s$silent_streak, 3L)
    # Last call, streak >= threshold -> reminder appended, streak reset.
    out <- corteza:::.maybe_append_narration_nudge("R", s, ctx("", 2L, 2L))
    expect_true(startsWith(out, "R"))
    expect_true(grepl("[corteza]", out, fixed = TRUE))
    expect_equal(s$silent_streak, 0L)

    # Below threshold -> no reminder.
    s$silent_streak <- 2L
    expect_equal(corteza:::.maybe_append_narration_nudge("R", s, ctx("", 1L, 1L)), "R")

    # No context -> untouched.
    s$silent_streak <- 9L
    expect_equal(corteza:::.maybe_append_narration_nudge("R", s, NULL), "R")
})

# --- disabled via option (Inf) ---
local({
    op <- options(corteza.narration_streak = Inf)
    on.exit(options(op), add = TRUE)
    s <- new.env()
    s$silent_streak <- 99L
    expect_equal(corteza:::.maybe_append_narration_nudge("R", s, ctx("", 1L, 1L)), "R")
})

# --- the handler routes EVERY outcome through the nudge, not only the
# executed path. A declined call that is the final call of a silent batch at
# the threshold must still deliver the nudge and reset the streak. ---
local({
    op <- options(corteza.narration_streak = 3L,
                  corteza.personal_paths = c("~/Documents"),
                  corteza.policy = NULL)
    on.exit(options(op), add = TRUE)

    s <- corteza::new_session(
        "cli",
        approval_cb = function(call, decision) FALSE)   # ask -> declined
    s$silent_streak <- 2L
    h <- corteza:::.make_tool_handler(s)

    # cli + personal + read = ask -> declined. Final (and only) silent call of
    # the batch: .update_silent_streak bumps 2 -> 3 (threshold), and the
    # declined return routes through nudge().
    out <- h("read_file", list(path = "~/Documents/private.md"),
             ctx("", 1L, 1L))
    expect_true(grepl("declined", out))
    expect_true(grepl("[corteza]", out, fixed = TRUE))
    expect_equal(s$silent_streak, 0L)
})
