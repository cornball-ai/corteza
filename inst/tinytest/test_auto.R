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

fake_session <- function(cost, tokens, missing = FALSE) {
    s <- new.env(parent = emptyenv())
    s$spend <- list(segments = list(list(cost = cost, total_tokens = tokens,
                                         cost_missing = missing)))
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

for (field in c("max_loops", "max_minutes", "max_cost", "max_tokens",
                "max_tool_calls", "stall_loops")) {
    for (value in list(0, -1, NA, NA_integer_)) {
        bad <- corteza:::auto_validate_bounds(
            utils::modifyList(auto, stats::setNames(list(value), field)))
        expect_true(length(bad) >= 1L)
        expect_true(any(grepl(field, bad)))
    }
}

# ---- cost_missing is baseline-relative ----
#
# The flag is sticky per segment and the subagent tally is process-wide,
# so reading either outright means one unpriced model earlier in a long
# session poisons every later auto run -- it would refuse to start over
# spend it did not create.

seg <- function(cost, tokens, missing) {
    list(cost = cost, total_tokens = tokens, cost_missing = missing)
}
sess <- new.env(parent = emptyenv())
sess$spend <- list(segments = list(seg(1, 500, TRUE)))
b <- corteza:::auto_spend_baseline(sess)

# An old unpriced segment that gains nothing during the run does not
# make this run's cost unknown.
expect_true(corteza:::auto_spend_since(sess, b)$cost_known)

# But if that same segment gains tokens during the run, the new spend is
# unpriced too and the total really is a floor.
sess$spend$segments[[1]]$total_tokens <- 900
expect_false(corteza:::auto_spend_since(sess, b)$cost_known)

# A fresh unpriced segment opened during the run also counts.
sess2 <- new.env(parent = emptyenv())
sess2$spend <- list(segments = list(seg(1, 500, FALSE)))
b2 <- corteza:::auto_spend_baseline(sess2)
sess2$spend$segments[[2]] <- seg(0, 300, TRUE)
expect_false(corteza:::auto_spend_since(sess2, b2)$cost_known)

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
