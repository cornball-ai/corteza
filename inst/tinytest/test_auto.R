library(tinytest)

# ---- worktree_digest / worktree_delta ----

mk <- function(root, path, text) {
    full <- file.path(root, path)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, full)
    full
}

wt <- file.path(tempdir(), "corteza-auto-wt")
unlink(wt, recursive = TRUE)
dir.create(wt, recursive = TRUE, showWarnings = FALSE)
mk(wt, "a.txt", "one")
mk(wt, "R/f.R", "f <- function() 1")

base <- corteza:::worktree_digest(wt)
expect_true(length(base) >= 2L)
expect_true("a.txt" %in% names(base))
expect_true("R/f.R" %in% names(base))

# Nothing touched: no delta.
d <- corteza:::worktree_delta(base, corteza:::worktree_digest(wt))
expect_false(d$changed)
expect_equal(length(d$modified), 0L)

# A touch that changes only mtime is NOT progress. This is the case
# size+mtime snapshots get wrong, and the reason the digest hashes
# content: a loop that rewrites a file identically would otherwise look
# like it was working.
Sys.setFileTime(file.path(wt, "a.txt"), Sys.time() + 120)
d <- corteza:::worktree_delta(base, corteza:::worktree_digest(wt))
expect_false(d$changed)

# A same-length edit IS progress. The other half of the same trap: a
# size-based snapshot would miss this entirely.
writeLines("two", file.path(wt, "a.txt"))
d <- corteza:::worktree_delta(base, corteza:::worktree_digest(wt))
expect_true(d$changed)
expect_equal(d$modified, "a.txt")
expect_equal(length(d$added), 0L)

# Additions and removals.
mk(wt, "new.txt", "hi")
file.remove(file.path(wt, "R/f.R"))
d <- corteza:::worktree_delta(base, corteza:::worktree_digest(wt))
expect_true("new.txt" %in% d$added)
expect_true("R/f.R" %in% d$removed)

# ---- pre-existing dirty work is baseline, not progress ----
#
# The user's own uncommitted edits are in the baseline, so a run that
# changes nothing must not take credit for them.

dirty <- file.path(tempdir(), "corteza-auto-dirty")
unlink(dirty, recursive = TRUE)
dir.create(dirty, recursive = TRUE, showWarnings = FALSE)
mk(dirty, "tracked.txt", "committed content")
mk(dirty, "untracked.txt", "never committed")
# Baseline taken with both files already sitting there.
b2 <- corteza:::worktree_digest(dirty)
expect_true("untracked.txt" %in% names(b2))
d <- corteza:::worktree_delta(b2, corteza:::worktree_digest(dirty))
expect_false(d$changed)

# ---- .git churn is not progress ----

repo <- file.path(tempdir(), "corteza-auto-git")
unlink(repo, recursive = TRUE)
dir.create(repo, recursive = TRUE, showWarnings = FALSE)
mk(repo, "keep.txt", "x")
mk(repo, "ignored.log", "noise")
mk(repo, ".gitignore", "*.log")
git_ok <- isTRUE(tryCatch({
    system2("git", c("-C", repo, "init", "-q"), stdout = TRUE, stderr = TRUE)
    system2("git", c("-C", repo, "add", "."), stdout = TRUE, stderr = TRUE)
    TRUE
}, error = function(e) FALSE))

if (git_ok) {
    gb <- corteza:::worktree_digest(repo)
    # .gitignore'd files are out of scope; git ls-files --exclude-standard
    # is what buys that.
    expect_false("ignored.log" %in% names(gb))
    expect_true("keep.txt" %in% names(gb))
    # Nothing under .git is ever hashed -- it churns on every git command
    # and would report progress for a loop that only ran git status.
    expect_false(any(startsWith(names(gb), ".git/")))

    system2("git", c("-C", repo, "status"), stdout = TRUE, stderr = TRUE)
    d <- corteza:::worktree_delta(gb, corteza:::worktree_digest(repo))
    expect_false(d$changed)
}

unlink(c(wt, dirty, repo), recursive = TRUE)

# ---- spend: the monitor counts against the cap ----

# Built through the real accumulator rather than hand-shaped, so the
# fixture cannot quietly diverge from what session_accumulate_spend()
# actually records -- which is how the earlier version of this test
# ended up asserting against a segment shape that no longer existed.
fake_session <- function(cost, tokens, missing = FALSE) {
    s <- new.env(parent = emptyenv())
    corteza:::session_accumulate_spend(
        s, list(total_tokens = tokens,
                cost = if (isTRUE(missing)) NA else cost))
    s
}

s <- fake_session(1.5, 1000)
b <- corteza:::auto_spend_baseline(s)
expect_equal(b$main_cost, 1.5)

# Spend is measured as a delta from the run's start, so money already
# spent in a long-lived chat() session before /auto does not start the
# run over budget.
spent <- corteza:::auto_spend_since(s, b)
expect_equal(spent$cost, 0)
expect_equal(spent$tokens, 0)
expect_true(spent$cost_known)

s$spend$segments[[1]]$cost <- 2.5
s$spend$segments[[1]]$total_tokens <- 3000
spent <- corteza:::auto_spend_since(s, b)
expect_equal(spent$cost, 1.0)
expect_equal(spent$tokens, 2000)

# An unpriced model makes the total a floor, not a number.
s2 <- fake_session(0, 5000, missing = TRUE)
spent <- corteza:::auto_spend_since(s2, corteza:::auto_spend_baseline(
    fake_session(0, 0)))
expect_false(spent$cost_known)

# ---- auto_check_limits ----

auto <- corteza:::get_auto_config(list())
expect_equal(auto$max_loops, 10L)
expect_true(auto$max_tool_calls > 0L)
expect_true(is.finite(auto$max_tokens))

st <- function(...) {
    base <- list(loop = 1L, started = Sys.time(), tool_calls = 0L,
                 stalled = 0L,
                 spend = list(cost = 0, tokens = 0, cost_known = TRUE))
    utils::modifyList(base, list(...))
}

expect_false(corteza:::auto_check_limits(st(), auto)$stop)

r <- corteza:::auto_check_limits(st(loop = 11L), auto)
expect_true(r$stop)
expect_true(grepl("max_loops", r$reason))

r <- corteza:::auto_check_limits(st(tool_calls = 5000L), auto)
expect_true(r$stop)
expect_true(grepl("tool-call cap", r$reason))

r <- corteza:::auto_check_limits(
    st(spend = list(cost = 99, tokens = 0, cost_known = TRUE)), auto)
expect_true(r$stop)
expect_true(grepl("spend cap", r$reason))

r <- corteza:::auto_check_limits(
    st(spend = list(cost = 0, tokens = 1e9, cost_known = TRUE)), auto)
expect_true(r$stop)
expect_true(grepl("token cap", r$reason))

# An unknown price stops the run rather than being counted as zero.
r <- corteza:::auto_check_limits(
    st(spend = list(cost = 0, tokens = 0, cost_known = FALSE)), auto)
expect_true(r$stop)
expect_true(grepl("no price", r$reason))

r <- corteza:::auto_check_limits(st(stalled = 2L), auto)
expect_true(r$stop)
expect_true(grepl("nothing changed", r$reason))

r <- corteza:::auto_check_limits(st(started = Sys.time() - 60 * 60 * 24), auto)
expect_true(r$stop)
expect_true(grepl("time cap", r$reason))

# ---- continuation prompt ----

p <- corteza:::auto_continuation_prompt("make the tests pass", 3L, 10L)
expect_true(grepl("iteration 3 of 10", p))
# The goal is restated every iteration: compaction summarizes old turns,
# and the original instruction is the oldest turn in the run.
expect_true(grepl("make the tests pass", p))
expect_true(grepl("AUTO_STATUS", p))
# The worker's own reply and the diff are deliberately absent -- its
# session already holds both, so repeating them costs context every
# iteration and buys nothing.
expect_true(nchar(p) < 800L)

# ---- auto_parse_status ----

expect_equal(corteza:::auto_parse_status("AUTO_STATUS: done"), "done")
expect_equal(corteza:::auto_parse_status("AUTO_STATUS: continue"), "continue")
expect_equal(corteza:::auto_parse_status("**AUTO_STATUS: done**"), "done")
expect_equal(corteza:::auto_parse_status("work done\nAUTO_STATUS: done"), "done")

# Absent, ambiguous, or mid-sentence reads as continue. Unlike the
# monitor's verdict this defaults permissive, because the cost of a
# spurious extra iteration is one turn and every mechanical cap plus the
# monitor still applies.
expect_equal(corteza:::auto_parse_status("I think we're done here"),
             "continue")
expect_equal(corteza:::auto_parse_status(""), "continue")
expect_equal(corteza:::auto_parse_status(NULL), "continue")
expect_equal(corteza:::auto_parse_status("AUTO_STATUS: done and continue"),
             "continue")

# ---- auto_validate_bounds ----

expect_equal(length(corteza:::auto_validate_bounds(auto)), 0L)

bad <- corteza:::auto_validate_bounds(
    utils::modifyList(auto, list(max_loops = 0L)))
expect_equal(length(bad), 1L)
expect_true(grepl("max_loops", bad))

# Inf is rejected too. It passes a `<= 0` test and then disables the
# bound outright in auto_check_limits() -- an infinite cap on a mode
# whose whole premise is being bounded.
for (field in c("max_loops", "max_minutes", "max_cost", "max_tokens",
                "max_tool_calls", "stall_loops")) {
    for (value in list(0, -1, NA, NA_integer_, Inf, -Inf)) {
        bad <- corteza:::auto_validate_bounds(
            utils::modifyList(auto, stats::setNames(list(value), field)))
        expect_true(length(bad) >= 1L)
        expect_true(any(grepl(field, bad)))
    }
}

# ---- cost_missing is measured by differencing, not by reading a flag ----
#
# cost_missing is sticky per segment and process-wide once aggregated
# across subagents, so it cannot answer "was the spend THIS run created
# priced". missing_tokens accumulates only unpriced tokens, so the
# difference across the run can.

seg <- function(cost, tokens, missing, missing_tokens = 0) {
    list(cost = cost, total_tokens = tokens, cost_missing = missing,
         missing_tokens = missing_tokens)
}
sess <- new.env(parent = emptyenv())
sess$spend <- list(segments = list(seg(1, 500, TRUE, missing_tokens = 500)))
b <- corteza:::auto_spend_baseline(sess)

# An old unpriced segment that gains nothing during the run does not
# make this run's cost unknown.
expect_true(corteza:::auto_spend_since(sess, b)$cost_known)

# The case a grew-and-flagged test gets wrong: a segment already flagged
# from earlier unpriced usage now takes PRICED usage. Tokens grow and the
# sticky flag is still TRUE, but nothing this run spent was unpriced.
sess$spend$segments[[1]]$total_tokens <- 900
sess$spend$segments[[1]]$cost <- 2
expect_true(corteza:::auto_spend_since(sess, b)$cost_known)

# Genuinely new unpriced usage on that same already-flagged segment does
# make the total a floor.
sess$spend$segments[[1]]$missing_tokens <- 800
expect_false(corteza:::auto_spend_since(sess, b)$cost_known)

# A fresh unpriced segment opened during the run also counts.
sess2 <- new.env(parent = emptyenv())
sess2$spend <- list(segments = list(seg(1, 500, FALSE)))
b2 <- corteza:::auto_spend_baseline(sess2)
sess2$spend$segments[[2]] <- seg(0, 300, TRUE, missing_tokens = 300)
expect_false(corteza:::auto_spend_since(sess2, b2)$cost_known)

# ---- spend.R accumulates the counter the difference relies on ----

s3 <- new.env(parent = emptyenv())
corteza:::session_accumulate_spend(s3, list(total_tokens = 100L, cost = 0.5))
expect_equal(s3$spend$segments[[1]]$missing_tokens, 0)
expect_false(s3$spend$segments[[1]]$cost_missing)

# An unpriced query records its tokens, not just the flag.
corteza:::session_accumulate_spend(s3, list(total_tokens = 250L, cost = NA))
expect_equal(s3$spend$segments[[1]]$missing_tokens, 250)
expect_true(s3$spend$segments[[1]]$cost_missing)

# A later priced query leaves the counter alone, which is what lets a
# window after this point read as priced despite the sticky flag.
corteza:::session_accumulate_spend(s3, list(total_tokens = 400L, cost = 1))
expect_equal(s3$spend$segments[[1]]$missing_tokens, 250)
expect_true(s3$spend$segments[[1]]$cost_missing)

# A zero-token query with no price is not unpriced usage.
corteza:::session_accumulate_spend(s3, list(total_tokens = 0L, cost = NA))
expect_equal(s3$spend$segments[[1]]$missing_tokens, 250)

# A registry entry shaped the way subagent_spawn() actually builds one.
# A bare list() is a fixture shaped like nothing real, and it only ever
# tests the branches that happen to tolerate missing fields.
fresh_subagent_entry <- function() {
    list(id = "fixture", seq = 1L,
         cumulative_input_tokens = 0L, cumulative_output_tokens = 0L,
         cumulative_total_tokens = 0L, cumulative_cost = NA_real_,
         cumulative_missing_tokens = 0, cost_missing = FALSE,
         query_count = 0L)
}

# ---- the counter must agree with the flag about what counts as usage ----
#
# .spend_usage_has_tokens() accepts a nonzero input OR output count, so a
# provider that reports those but omits total_tokens sets the "price
# unknown" flag. If the counter reads only total_tokens it stays at zero,
# and anything differencing it concludes the spend was priced -- the flag
# says unknown while the number says known.

expect_equal(corteza:::.spend_normalized_tokens(
    list(total_tokens = 300L, input_tokens = 100L, output_tokens = 50L)), 300)
# No total reported: fall back to input + output.
expect_equal(corteza:::.spend_normalized_tokens(
    list(input_tokens = 100L, output_tokens = 50L)), 150)
expect_equal(corteza:::.spend_normalized_tokens(
    list(total_tokens = 0L, input_tokens = 100L, output_tokens = 50L)), 150)
expect_equal(corteza:::.spend_normalized_tokens(
    list(total_tokens = NA, input_tokens = 7L)), 7)
expect_equal(corteza:::.spend_normalized_tokens(list()), 0)

# The token TOTALS must normalize too, not just the unpriced counter. A
# PRICED query reporting input and output without a total would
# otherwise record known cost and zero tokens, so max_tokens could never
# trip no matter how much the run consumed.
s5 <- new.env(parent = emptyenv())
corteza:::session_accumulate_spend(
    s5, list(input_tokens = 900L, output_tokens = 100L, cost = 0.5))
expect_equal(s5$spend$segments[[1]]$total_tokens, 1000)
expect_false(s5$spend$segments[[1]]$cost_missing)
b5 <- corteza:::auto_spend_baseline(new.env(parent = emptyenv()))
spent5 <- corteza:::auto_spend_since(s5, b5)
expect_equal(spent5$tokens, 1000)
expect_true(spent5$cost_known)
# And that count is what the cap sees.
r5 <- corteza:::auto_check_limits(
    list(loop = 1L, started = Sys.time(), tool_calls = 0L, stalled = 0L,
         spend = spent5),
    utils::modifyList(auto, list(max_tokens = 500)))
expect_true(r5$stop)
expect_true(grepl("token cap", r5$reason))

# Same normalization on the subagent side.
corteza:::subagent_spend_reset()
e5 <- corteza:::subagent_accumulate_usage(
    fresh_subagent_entry(), list(input_tokens = 700L, output_tokens = 300L, cost = 0.2))
expect_equal(e5$cumulative_total_tokens, 1000)
expect_false(isTRUE(e5$cost_missing))

# End to end on the main-agent accumulator: total omitted entirely.
s4 <- new.env(parent = emptyenv())
corteza:::session_accumulate_spend(
    s4, list(input_tokens = 200L, output_tokens = 100L, cost = NA))
expect_true(s4$spend$segments[[1]]$cost_missing)
# The counter moved with the flag rather than staying at zero.
expect_equal(s4$spend$segments[[1]]$missing_tokens, 300)
b4 <- corteza:::auto_spend_baseline(new.env(parent = emptyenv()))
expect_false(corteza:::auto_spend_since(s4, b4)$cost_known)

# And on the subagent accumulator, which feeds the same difference.
corteza:::subagent_spend_reset()
entry <- corteza:::subagent_accumulate_usage(
    fresh_subagent_entry(), list(input_tokens = 400L, output_tokens = 100L, cost = NA))
expect_true(entry$cost_missing)
expect_equal(entry$cumulative_missing_tokens, 500)

# ---- missing_tokens survives retirement ----
#
# Auto runs kill their monitor on the way out, so this fires on every
# run. If the counter were dropped at retirement the process-wide total
# would go DOWN, and a later window differencing it would read the drop
# as "this window's spend was priced" -- the same silent inversion the
# counter exists to prevent.

corteza:::subagent_spend_reset()
reg <- corteza:::.subagent_registry
reg[["retire-test"]] <- list(
    id = "retire-test", seq = 1L,
    cumulative_input_tokens = 400L, cumulative_output_tokens = 100L,
    cumulative_total_tokens = 500L, cumulative_cost = NA,
    cumulative_missing_tokens = 500, cost_missing = TRUE, query_count = 1L)

before <- corteza:::subagent_spend_total()$missing_tokens
corteza:::subagent_retire_spend(reg[["retire-test"]])
rm(list = "retire-test", envir = reg)
after <- corteza:::subagent_spend_total()$missing_tokens

expect_equal(before, 500)
expect_equal(after, before)
corteza:::subagent_spend_reset()
expect_equal(corteza:::subagent_spend_total()$missing_tokens, 0)

# ---- parse_auto_flags ----

f <- corteza:::parse_auto_flags("fix the failing tests")
expect_equal(f$goal, "fix the failing tests")
expect_null(f$loops)
expect_null(f$allow_exec)

f <- corteza:::parse_auto_flags("--loops 5 fix the tests")
expect_equal(f$loops, 5L)
expect_equal(f$goal, "fix the tests")

# Flags are stripped wherever they appear, not just at the front.
f <- corteza:::parse_auto_flags("fix the tests --loops 3")
expect_equal(f$loops, 3L)
expect_equal(f$goal, "fix the tests")

f <- corteza:::parse_auto_flags("--exec build it")
expect_true(f$allow_exec)
expect_equal(f$goal, "build it")

f <- corteza:::parse_auto_flags("--no-exec just read things")
expect_false(f$allow_exec)
expect_equal(f$goal, "just read things")

# --no-exec is the tighter setting, so it wins when both appear rather
# than the later flag winning by accident of parse order.
f <- corteza:::parse_auto_flags("--exec --no-exec do it")
expect_false(f$allow_exec)

f <- corteza:::parse_auto_flags("--loops 2 --exec  ship   it ")
expect_equal(f$loops, 2L)
expect_true(f$allow_exec)
expect_equal(f$goal, "ship it")

# An empty goal is what the usage message keys off.
expect_equal(corteza:::parse_auto_flags("")$goal, "")
expect_equal(corteza:::parse_auto_flags("--loops 4")$goal, "")
expect_equal(corteza:::parse_auto_flags(NULL)$goal, "")

# ---- delta rendering ----

expect_true(grepl("nothing changed",
                  corteza:::format_worktree_delta(list(changed = FALSE))))
txt <- corteza:::format_worktree_delta(
    list(changed = TRUE, modified = "R/a.R", added = "R/b.R",
         removed = character()))
expect_true(grepl("modified \\(1\\)", txt))
expect_true(grepl("added \\(1\\)", txt))
expect_false(grepl("removed", txt))
