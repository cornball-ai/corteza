# Tests for the persistent task list (R/tasks.R) and its dispatch
# intercept in .make_tool_handler() (R/turn.R). Covers helpers,
# intercept routing (CLI executor never reached), policy/approval
# bypass, /clear semantics, prompt addendum, display, and persistence
# round-trip.

library(tinytest)

create_apply <- corteza:::task_create_apply
update_apply <- corteza:::task_update_apply
intercept <- corteza:::task_tool_intercept
prompt_for <- corteza:::format_task_list_prompt
compose <- corteza:::task_compose_system
display <- corteza:::format_task_list_display

# Helper: build a minimal session env for intercept tests.
new_test_session <- function() {
    e <- new.env(parent = emptyenv())
    e$on_tool <- list()
    e$tasks <- list()
    e
}

# --- helper validation ----------------------------------------------

# task_create_apply replaces any existing list and sets pending status.
s <- new_test_session()
s$tasks <- list(list(text = "old", status = "completed"))
new_list <- create_apply(s, c("a", "b", "c"))
expect_equal(length(new_list), 3L)
expect_equal(s$tasks[[1]]$text, "a")
expect_equal(s$tasks[[2]]$status, "pending")
expect_equal(s$tasks[[3]]$status, "pending")
expect_true(isTRUE(s$tasks_dirty))

# task_create_apply rejects empty input.
expect_error(create_apply(new_test_session(), character(0)),
             "non-empty")
expect_error(create_apply(new_test_session(), list()),
             "non-empty")

# task_update_apply rejects out-of-range index.
expect_error(update_apply(s, 99, "completed"), "out of range")
expect_error(update_apply(s, 0, "completed"), "out of range")
expect_error(update_apply(new_test_session(), 1, "completed"),
             "out of range")

# task_update_apply rejects unknown status.
expect_error(update_apply(s, 1, "wat"),
             "status must be one of")

# task_update_apply auto-demotes other in_progress tasks.
s <- new_test_session()
create_apply(s, c("a", "b", "c"))
update_apply(s, 2, "in_progress")
expect_equal(s$tasks[[2]]$status, "in_progress")
update_apply(s, 1, "in_progress")
expect_equal(s$tasks[[1]]$status, "in_progress")
expect_equal(s$tasks[[2]]$status, "pending")  # demoted
expect_equal(s$tasks[[3]]$status, "pending")

# `cancelled` is supported.
update_apply(s, 3, "cancelled")
expect_equal(s$tasks[[3]]$status, "cancelled")

# --- intercept routing ----------------------------------------------

# Non-task tool names pass through (intercept returns NULL).
expect_null(intercept(new_test_session(), "read_file", list(path = "x")))

# task_create with NO task_approval_cb installed and no test stub
# defaults to DENY (codex caught that base readline() returns ""
# immediately under non-interactive Rscript, which my old code
# treated as auto-approval). The session must be left untouched.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    expect_true(is.na(getOption("corteza.task_approve", NA_character_)))

    s <- new_test_session()  # no task_approval_cb installed
    res <- intercept(s, "task_create", list(tasks = c("a", "b")))
    expect_true(grepl("User rejected the proposed plan", res, fixed = TRUE))
    expect_equal(length(s$tasks), 0L)
})

# task_create with a task_approval_cb that returns TRUE: commits.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    s <- new_test_session()
    s$task_approval_cb <- function() TRUE
    res <- intercept(s, "task_create", list(tasks = c("a")))
    expect_true(grepl("Plan approved", res))
    expect_equal(length(s$tasks), 1L)
})

# task_create with a task_approval_cb that errors: defaults to deny.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    s <- new_test_session()
    s$task_approval_cb <- function() stop("nope")
    res <- intercept(s, "task_create", list(tasks = c("a")))
    expect_true(grepl("User rejected the proposed plan", res, fixed = TRUE))
    expect_equal(length(s$tasks), 0L)
})

# task_create prompts the user via readline. Tests stub that prompt
# through options(corteza.task_approve = "y" | "n") so the intercept
# never blocks. Capture stdout to keep the test output clean (we
# verify presence of key strings via the returned result, not by
# scraping the rendered display).
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    options(corteza.task_approve = "y")
    on.exit(options(corteza.task_approve = NULL), add = TRUE)

    s <- new_test_session()
    res <- intercept(s, "task_create", list(tasks = c("x", "y")))
    expect_true(grepl("Plan approved", res))
    expect_equal(length(s$tasks), 2L)

    res <- intercept(s, "task_update", list(index = 1, status = "completed"))
    expect_true(grepl("Task 1 -> completed", res))
    expect_equal(s$tasks[[1]]$status, "completed")

    # Errors are returned as bracketed strings (so the LLM sees a
    # tool-result rather than crashing the turn).
    res <- intercept(s, "task_update", list(index = 99, status = "completed"))
    expect_true(grepl("\\[task error:", res))
})

# task_create with rejection: tasks are NOT committed; LLM gets a
# marker telling it to stop and ask the user.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    options(corteza.task_approve = "n")
    on.exit(options(corteza.task_approve = NULL), add = TRUE)

    s <- new_test_session()
    res <- intercept(s, "task_create", list(tasks = c("x", "y")))
    expect_true(grepl("User rejected the proposed plan", res, fixed = TRUE))
    expect_true(grepl("ask the user what they'd rather do", res,
                      fixed = TRUE))
    # No mutation on rejection.
    expect_equal(length(s$tasks), 0L)
    expect_false(isTRUE(s$tasks_dirty))
})

# --- .make_tool_handler intercept ----------------------------------

# When .make_tool_handler is asked to run a task tool, the tool_executor
# must never be called. This is the codex finding: the CLI's executor
# dispatches to a callr worker, so any task-state mutation there would
# strand the change in the wrong process.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    options(corteza.task_approve = "y")
    on.exit(options(corteza.task_approve = NULL), add = TRUE)

    s <- new_test_session()
    s$channel <- "cli"
    s$approval_cb <- function(call, decision) TRUE
    s$config <- list()
    executor_called <- new.env(parent = emptyenv())
    executor_called$count <- 0L
    exec <- function(name, args) {
        executor_called$count <- executor_called$count + 1L
        "executor was called"
    }
    handler <- corteza:::.make_tool_handler(s, tool_executor = exec)
    res <- handler("task_create", list(tasks = c("a", "b")))
    expect_equal(executor_called$count, 0L)
    expect_true(grepl("Plan approved", res))
    expect_equal(length(s$tasks), 2L)
})

# (Testing that non-task tools still reach the executor would
# require a full policy/approval scaffold; the executor_called$count
# == 0 assertion above already proves the task_* intercept short-
# circuits before any executor dispatch, which is the codex finding
# we're guarding against.)

# --- approval / policy bypass --------------------------------------

# A task_create call must not invoke approval_cb (no prompt) and
# must not run policy() (which we'd see via a denial string).
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    options(corteza.task_approve = "y")
    on.exit(options(corteza.task_approve = NULL), add = TRUE)

    approval_calls <- new.env(parent = emptyenv())
    approval_calls$count <- 0L
    s <- new_test_session()
    s$channel <- "cli"
    s$approval_cb <- function(call, decision) {
        approval_calls$count <- approval_calls$count + 1L
        FALSE
    }
    s$config <- list()
    handler <- corteza:::.make_tool_handler(s,
        tool_executor = function(n, a) "x")
    res <- handler("task_update", list(index = 1, status = "in_progress"))
    # Index 1 of empty list errors -- but the *error message* is a
    # bracketed [task error: ...], not a policy denial.
    expect_true(grepl("task error", res))
    expect_equal(approval_calls$count, 0L)

    # Now seed and try again -- task_update never invokes approval_cb
    # (only task_create gates with the readline prompt).
    intercept(s, "task_create", list(tasks = c("a", "b")))
    res <- handler("task_update", list(index = 1, status = "in_progress"))
    expect_true(grepl("Task 1 -> in_progress", res))
    expect_equal(approval_calls$count, 0L)
})

# --- prompt addendum -----------------------------------------------

expect_equal(prompt_for(list()), "")

tasks <- list(list(text = "first", status = "pending"),
              list(text = "second", status = "in_progress"),
              list(text = "third", status = "completed"))
out <- prompt_for(tasks)
expect_true(grepl("# Active tasks", out, fixed = TRUE))
expect_true(grepl("1. [ ] first", out, fixed = TRUE))
expect_true(grepl("2. [>] second", out, fixed = TRUE))
expect_true(grepl("3. [x] third", out, fixed = TRUE))
# The "how to use" instructions moved to .task_tool_addendum() so
# the LLM sees them every turn (not just when an active list exists).
# format_task_list_prompt() now renders only the list.
expect_false(grepl("Maintain this list", out, fixed = TRUE))

# compose() always appends the static "how to use task tools"
# addendum so the LLM sees the clarify-then-plan flow on every
# turn, regardless of whether there's an active list.
empty_compose <- compose("BASE", list())
expect_true(startsWith(empty_compose, "BASE\n"))
expect_true(grepl("# Multi-step requests", empty_compose, fixed = TRUE))
expect_true(grepl("Ask clarifying questions first", empty_compose,
                  fixed = TRUE))
expect_true(grepl("user will be asked to approve", empty_compose,
                  fixed = TRUE) ||
            grepl("asks the user to approve", empty_compose,
                  fixed = TRUE))
expect_false(grepl("# Active tasks", empty_compose, fixed = TRUE))

# compose() with tasks appends both the static addendum and the
# active-list block.
res <- compose("BASE", tasks)
expect_true(startsWith(res, "BASE\n"))
expect_true(grepl("# Multi-step requests", res, fixed = TRUE))
expect_true(grepl("# Active tasks", res, fixed = TRUE))

# --- display -------------------------------------------------------

ansi <- list(reset = "\033[0m", bold = "", dim = "\033[2m",
             red = "", green = "\033[32m", yellow = "",
             blue = "", magenta = "", cyan = "", white = "",
             bright_red = "", bright_green = "",
             bright_yellow = "\033[93m", bright_blue = "",
             bright_magenta = "", bright_cyan = "")

expect_null(display(list(), palette = ansi))
out <- display(tasks, palette = ansi)
expect_true(grepl("Tasks:", out, fixed = TRUE))
expect_true(grepl("1. [ ] first", out, fixed = TRUE))
expect_true(grepl("2. [>] second", out, fixed = TRUE))
expect_true(grepl("3. [x] third", out, fixed = TRUE))
# in_progress -> bright_yellow
expect_true(grepl("\033\\[93m2", out))
# completed -> green
expect_true(grepl("\033\\[32m3", out))

# --- persistence round-trip ----------------------------------------

# Tests need a writable session store. Skip during R CMD check since
# session_new() / session_save() write to ~/.cache/corteza-style paths
# and the CI runner's HOME is locked down; the at_home() guard keeps
# this local-only.
if (at_home()) {
    sess <- corteza:::session_new(provider = "anthropic", cwd = tempdir())
    expect_true(is.list(sess$tasks))
    expect_equal(length(sess$tasks), 0L)

    sess$tasks <- list(list(text = "a", status = "pending"),
                       list(text = "b", status = "in_progress"))
    corteza:::session_save(sess)

    loaded <- corteza:::session_load(sess$sessionKey)
    expect_false(is.null(loaded))
    expect_equal(length(loaded$tasks), 2L)
    expect_equal(loaded$tasks[[2]]$status, "in_progress")
    expect_equal(loaded$tasks[[1]]$text, "a")
}
