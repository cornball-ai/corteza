# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

# Tests for the persistent task list (R/tasks.R) and its dispatch
# intercept in .make_tool_handler() (R/turn.R). Covers helpers,
# intercept routing (CLI executor never reached), policy/approval
# bypass, /clear semantics, prompt addendum, display, and persistence
# round-trip.

library(tinytest)

update_apply <- corteza:::task_update_apply
intercept <- corteza:::task_tool_intercept
prompt_for <- corteza:::format_task_list_prompt
compose <- corteza:::task_compose_system
display <- corteza:::format_task_list_display

# Helper: build a minimal session env for intercept tests.
new_test_session <- function(channel = "console", enabled = TRUE) {
    e <- new.env(parent = emptyenv())
    e$on_tool <- list()
    e$tasks <- list()
    # Default to a task-supporting channel; tests that exercise the
    # matrix / unsupported-channel branches pass channel = "matrix"
    # (or another value) explicitly.
    e$channel <- channel
    # The task list is off unless the config turns it on. The tests of
    # the list itself run with it on; the ones of the switch say so.
    e$config <- list(task_list_enabled = enabled)
    e
}

# --- helper validation ----------------------------------------------

s <- new_test_session()
s$tasks <- list(list(text = "a", status = "pending"))

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
s$tasks <- lapply(c("a", "b", "c"), function(text) list(text = text, status = "pending"))
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

# task_create inside an auto run auto-approves: the run is authorized as a
# whole, so blocking on a human reader (or defaulting to deny) would hang
# or stall it. The human cb is never consulted -- an errored one still
# commits the plan.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    s <- new_test_session()
    s$auto_run_id <- "auto-xyz"
    s$task_approval_cb <- function() stop("must not run in auto mode")
    res <- intercept(s, "task_create", list(tasks = c("a", "b")))
    expect_true(grepl("Plan approved", res))
    expect_equal(length(s$tasks), 2L)
})

# The system text describes the attended flow. An /auto run is told at
# the head of its first prompt that nobody answers (WP14), with no
# ask-first or wait-for-approval steps, which would hang it.
att_add <- corteza:::.task_tool_addendum()
aut_note <- corteza:::.auto_note(TRUE)
expect_true(grepl("Ask clarifying questions first", att_add, fixed = TRUE))
expect_false(grepl("Ask clarifying questions first", aut_note, fixed = TRUE))
expect_true(grepl("unattended /auto run", aut_note, fixed = TRUE))
expect_true(grepl("accepted automatically", aut_note, fixed = TRUE))
expect_true(grepl("ask before you assume", corteza:::.auto_note(FALSE), fixed = TRUE))
expect_true(grepl("Ask clarifying questions first",
                  compose("BASE", list(), on = TRUE),
                  fixed = TRUE))
# A session with the task list off gets no task text, in an /auto run
# or out of one.
expect_identical(compose("BASE", list(), on = FALSE), "BASE")
# The task list's note: the whole list, or that it is empty.
expect_identical(corteza:::.task_list_note(list()), "[The task list is empty.]")
listed <- corteza:::.task_list_note(list(list(text = "a", status = "pending"),
                                         list(text = "b", status = "completed")))
expect_true(grepl("^\\[Task list:\n1\\. \\[.\\] a\n2\\. \\[.\\] b\\]$", listed))

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
# must never be called. Skill handlers run in the main process, so the
# intercept mutates the live session directly; routing through an
# executor would strand the task-state change in the wrong place.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    options(corteza.task_approve = "y")
    on.exit(options(corteza.task_approve = NULL), add = TRUE)

    s <- new_test_session()
    s$channel <- "cli"
    s$approval_cb <- function(call, decision) TRUE
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

# compose() for a session with the task list on appends the static
# "how to use task tools" addendum on every turn so the LLM sees the
# clarify-then-plan flow.
empty_compose <- compose("BASE", list(), on = TRUE)
expect_true(startsWith(empty_compose, "BASE\n"))
expect_true(grepl("# Multi-step requests", empty_compose, fixed = TRUE))
expect_true(grepl("Ask clarifying questions first", empty_compose,
                  fixed = TRUE))
expect_false(grepl("# Active tasks", empty_compose, fixed = TRUE))

# compose() with tasks appends both the static addendum and the
# active-list block.
res <- compose("BASE", tasks, on = TRUE)
expect_true(startsWith(res, "BASE\n"))
expect_true(grepl("# Multi-step requests", res, fixed = TRUE))
expect_true(grepl("# Active tasks", res, fixed = TRUE))

# Off: neither the addendum nor a list the session still holds reaches
# the model, so it is never told to call tools it was not given.
off_compose <- compose("BASE", tasks, on = FALSE)
expect_equal(off_compose, "BASE")
expect_false(grepl("# Multi-step requests", off_compose, fixed = TRUE))
expect_false(grepl("# Active tasks", off_compose, fixed = TRUE))

# Off is the default, and only TRUE turns it on: a caller that still
# passes a channel name where `on` now sits gets no task text.
expect_equal(compose("BASE", tasks), "BASE")
expect_equal(compose("BASE", tasks, "cli"), "BASE")
expect_equal(compose("BASE", tasks, on = NA), "BASE")
expect_equal(compose("BASE", tasks, on = NULL), "BASE")

# .task_filter_tools strips task_create / task_update unless the task
# list is on.
tool_list <- list(
                  list(name = "read_file"),
                  list(name = "task_create"),
                  list(name = "task_update"),
                  list(name = "bash"))
filtered_off <- corteza:::.task_filter_tools(tool_list, FALSE)
expect_equal(length(filtered_off), 2L)
expect_equal(vapply(filtered_off, function(t) t$name, character(1)),
             c("read_file", "bash"))
expect_equal(length(corteza:::.task_filter_tools(tool_list, TRUE)), 4L)
expect_equal(length(corteza:::.task_filter_tools(tool_list, "cli")), 2L)
expect_equal(length(corteza:::.task_filter_tools(tool_list, NULL)), 2L)

# --- the switch -----------------------------------------------------

# A session has the task list when its channel has the approval prompt
# and its config turns the list on. Either alone is not enough.
task_on <- corteza:::.task_list_on
for (ch in c("cli", "console")) {
    expect_true(task_on(new_test_session(ch, enabled = TRUE)))
    expect_false(task_on(new_test_session(ch, enabled = FALSE)))
}
# Matrix has no synchronous approval prompt, so the config cannot turn
# the list on there, and neither can it for a session with no channel.
expect_false(task_on(new_test_session("matrix", enabled = TRUE)))
local({
    s <- new_test_session(enabled = TRUE)
    s$channel <- NULL
    expect_false(task_on(s))
})
# Off where nothing says on: no config, a config without the key, and
# a value that is not TRUE (a quoted "true" in the JSON among them).
local({
    s <- new_test_session("cli")
    s$config <- NULL
    expect_false(task_on(s))
    s$config <- list()
    expect_false(task_on(s))
    for (bad in list("true", 1L, NA, c(TRUE, TRUE), list(TRUE))) {
        s$config <- list(task_list_enabled = bad)
        expect_false(task_on(s))
    }
})

# The default itself, off, is load_config()'s: test_config.R.

# task_tool_intercept defense-in-depth: even if a Matrix session
# somehow has the tools advertised (e.g. user disables the filter),
# the intercept returns a clear error instead of triggering
# default-deny and a confusing rejection marker.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    s <- new_test_session()
    s$channel <- "matrix"
    s$task_approval_cb <- function() TRUE
    res <- intercept(s, "task_create", list(tasks = c("a")))
    expect_true(grepl("not available on the 'matrix' channel", res,
                      fixed = TRUE))
    expect_equal(length(s$tasks), 0L)
})

# The same with the task list off in the terminal: a history from an
# earlier session can hold task calls the model repeats. Neither tool
# changes the session, an approving callback is not asked, and an auto
# run gets no pass.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    asked <- 0L
    for (ch in c("cli", "console")) {
        s <- new_test_session(ch, enabled = FALSE)
        s$task_approval_cb <- function() {
            asked <<- asked + 1L
            TRUE
        }
        res <- intercept(s, "task_create", list(tasks = c("a", "b")))
        expect_identical(res, paste0("[task error: task_create is not ",
                                     "available: the task list is off ",
                                     "(task_list_enabled)]"))
        expect_equal(length(s$tasks), 0L)
        expect_false(isTRUE(s$tasks_dirty))

        # A list the session still holds is not updated either.
        s$tasks <- list(list(text = "kept", status = "pending"))
        res <- intercept(s, "task_update",
                         list(index = 1, status = "completed"))
        expect_true(grepl("task_update is not available", res, fixed = TRUE))
        expect_identical(s$tasks[[1]]$status, "pending")

        s$auto_run_id <- "auto-xyz"
        res <- intercept(s, "task_create", list(tasks = c("a")))
        expect_true(grepl("the task list is off", res, fixed = TRUE))
        expect_identical(s$tasks[[1]]$text, "kept")

        # Another tool is none of the intercept's business, on or off.
        expect_null(intercept(s, "read_file", list(path = "x")))
    }
    expect_identical(asked, 0L)
})

# Through the tool handler turn() builds: the refusal comes back as the
# tool's result, and neither the executor nor the approval prompt runs.
local({
    sink(tempfile()); on.exit(sink(NULL), add = TRUE)
    s <- new_test_session("cli", enabled = FALSE)
    reached <- 0L
    s$approval_cb <- function(call, decision) {
        reached <<- reached + 1L
        TRUE
    }
    handler <- corteza:::.make_tool_handler(s, tool_executor = function(n, a) {
        reached <<- reached + 1L
        "executor was called"
    })
    res <- handler("task_create", list(tasks = c("a", "b")))
    expect_true(grepl("the task list is off", res, fixed = TRUE))
    expect_equal(length(s$tasks), 0L)
    expect_identical(reached, 0L)
})

# turn() sends neither the tools nor the task text unless the config
# turns the list on. The provider's call is replaced, so nothing leaves
# the machine. The session's tools and system text are fixed (WP14): a
# terminal's end with exit_plan_mode, and a list held from before
# reaches the model as a note ahead of the prompt, never in the system
# text.
local({
    ns <- asNamespace("llm.api")
    orig <- get("agent", envir = ns, inherits = FALSE)
    captured <- NULL
    assignInNamespace("agent", function(...) {
        captured <<- list(...)
        list(content = "ok", history = list(),
             usage = list(input_tokens = 1L, output_tokens = 1L))
    }, ns = "llm.api")
    on.exit(assignInNamespace("agent", orig, ns = "llm.api"), add = TRUE)

    offered <- list(list(name = "read_file", description = "r",
                         input_schema = list(type = "object")),
                    list(name = "task_create", description = "c",
                         input_schema = list(type = "object")),
                    list(name = "task_update", description = "u",
                         input_schema = list(type = "object")))
    sent <- function(channel, config, tasks = list()) {
        s <- corteza::new_session(channel = channel, provider = "openai",
                                  model_map = list(cloud = "test-model",
                                                   local = NULL),
                                  system = "BASE", web_search = FALSE)
        s$config <- config
        s$tasks <- tasks
        captured <<- NULL
        out <- corteza::turn("hi", s, tools = offered)
        expect_identical(out$reply, "ok")
        list(tools = vapply(captured$tools, function(t) t$name, character(1)),
             system = captured$system,
             prompt = paste(unlist(captured$prompt), collapse = " "))
    }
    held <- list(list(text = "kept from before", status = "pending"))

    for (ch in c("cli", "console")) {
        # No key, an empty config, and the key set to false: off.
        for (cfg in list(NULL, list(), list(task_list_enabled = FALSE))) {
            got <- sent(ch, cfg, held)
            expect_identical(got$tools, c("read_file", "exit_plan_mode"))
            expect_false(grepl("Multi-step requests", got$system,
                               fixed = TRUE))
            expect_false(grepl("Active tasks", got$system, fixed = TRUE))
            expect_false(grepl("kept from before",
                               paste(got$system, got$prompt), fixed = TRUE))
        }
        got <- sent(ch, list(task_list_enabled = TRUE), held)
        expect_identical(got$tools, c("read_file", "task_create",
                                      "task_update", "exit_plan_mode"))
        expect_true(grepl("Multi-step requests", got$system, fixed = TRUE))
        expect_false(grepl("kept from before", got$system, fixed = TRUE))
        expect_true(grepl("[Task list:", got$prompt, fixed = TRUE))
        expect_true(grepl("kept from before", got$prompt, fixed = TRUE))
    }
    # A room has no task list whatever its config says.
    got <- sent("matrix", list(task_list_enabled = TRUE), held)
    expect_identical(got$tools, "read_file")
    expect_false(grepl("Multi-step requests", got$system, fixed = TRUE))
    expect_false(grepl("kept from before", got$prompt, fixed = TRUE))
})

# /tools lists what a turn would offer.
local({
    listed <- function(enabled) {
        s <- new_test_session("cli", enabled = enabled)
        s$tools_filter <- c("read_file", "task_create", "task_update")
        corteza:::chat_format_tools_list(s)
    }
    on <- listed(TRUE)
    off <- listed(FALSE)
    expect_true(grepl("read_file", on, fixed = TRUE))
    expect_true(grepl("task_create", on, fixed = TRUE))
    expect_true(grepl("task_update", on, fixed = TRUE))
    expect_true(grepl("read_file", off, fixed = TRUE))
    expect_false(grepl("task_create", off, fixed = TRUE))
    expect_false(grepl("task_update", off, fixed = TRUE))
})

# /tasks names the config file and the key where the list is off.
local({
    note <- corteza:::.task_list_off_note()
    expect_true(grepl("The task list is off", note, fixed = TRUE))
    expect_true(grepl("\"task_list_enabled\": true", note, fixed = TRUE))
    expect_true(grepl(corteza:::corteza_config_path("config.json"), note,
                      fixed = TRUE))
})

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
if (at_home() || identical(Sys.getenv("NOT_CRAN"), "true")) {
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
