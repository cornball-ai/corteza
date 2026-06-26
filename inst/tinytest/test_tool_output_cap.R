library(tinytest)

# Universal tool-output cap (Phase 3). Every tool result funnels through
# admit_tool_result() in .make_tool_handler(); an oversized result is
# capped to a marker + handle so it can't wedge model context.

# ---- admit_tool_result: unit behavior ----

# Small output passes through unchanged.
expect_equal(corteza:::admit_tool_result("hello", tool = "bash"), "hello")

# Multi-line but small passes through unchanged.
small <- paste(sprintf("line %d", 1:10), collapse = "\n")
expect_equal(corteza:::admit_tool_result(small, tool = "bash"), small)

# Non-string passes through untouched.
expect_null(corteza:::admit_tool_result(NULL))

# Over the line cap -> truncated marker + retrievable handle.
local({
    on.exit(corteza:::clear_handles(), add = TRUE)
    big <- paste(sprintf("row %d", 1:50000), collapse = "\n")
    out <- corteza:::admit_tool_result(big, tool = "bash")
    expect_true(grepl("[tool output truncated]", out, fixed = TRUE))
    expect_true(grepl("tool: bash", out, fixed = TRUE))
    expect_true(grepl("50000 lines", out))
    # marker is far smaller than the original
    expect_true(nchar(out) < nchar(big))
    expect_true(nchar(out) < 5000L)
    # handle named in the marker resolves to the full output
    h <- regmatches(out, regexpr("\\.h_[0-9]+", out))
    expect_true(nzchar(h))
    full <- corteza:::get_handle(h)
    expect_equal(length(full), 50000L)
    expect_equal(full[1], "row 1")
    expect_equal(full[50000], "row 50000")
})

# Over the char cap (few lines, one huge line) -> still truncated.
local({
    on.exit(corteza:::clear_handles(), add = TRUE)
    big <- strrep("x", 60000L)
    out <- corteza:::admit_tool_result(big, tool = "grep_files")
    expect_true(grepl("truncated", out))
    expect_true(nchar(out) < nchar(big))
})

# read_file / git_diff get a far larger budget than chatty tools, so a
# whole-file read isn't sliced into 50-line re-reads.
local({
    on.exit(corteza:::clear_handles(), add = TRUE)
    # 200 lines: over the 50-line default cap, well under the read budget.
    body <- paste(sprintf("line %d", 1:200), collapse = "\n")
    expect_equal(corteza:::admit_tool_result(body, tool = "read_file"), body)
    expect_equal(corteza:::admit_tool_result(body, tool = "git_diff"), body)
    # The same body through a chatty tool is still capped.
    expect_true(grepl("truncated", corteza:::admit_tool_result(body, tool = "bash")))
    # A read past the (larger) read budget still stashes to a handle.
    huge <- paste(sprintf("line %d", 1:3000), collapse = "\n")
    expect_true(grepl("truncated", corteza:::admit_tool_result(huge, tool = "read_file")))
})

# ---- handler integration ----

# A fake executor returning 50k lines is capped before the model sees it.
local({
    on.exit({
        options(corteza.policy = NULL)
        corteza:::clear_handles()
    }, add = TRUE)
    options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "test allow")
    })
    fake <- function(name, args) {
        list(content = list(list(type = "text",
                                 text = paste(sprintf("L%d", 1:50000),
                                              collapse = "\n"))))
    }
    s <- corteza::new_session("cli",
                              approval_cb = function(call, decision) TRUE)
    h <- corteza:::.make_tool_handler(s, tool_executor = fake)
    out <- h("grep_files", list(pattern = "x"))
    expect_true(grepl("truncated", out))
    expect_true(nchar(out) < 5000L)
    expect_true(grepl("50000 lines", out))
})

# Dry-run branch is also capped.
local({
    on.exit(corteza:::clear_handles(), add = TRUE)
    fake <- function(name, args) {
        list(content = list(list(type = "text",
                                 text = paste(sprintf("L%d", 1:50000),
                                              collapse = "\n"))))
    }
    s <- corteza::new_session("cli")
    s$dry_run <- TRUE
    h <- corteza:::.make_tool_handler(s, tool_executor = fake)
    out <- h("grep_files", list(pattern = "x"))
    expect_true(grepl("truncated", out))
    expect_true(nchar(out) < 5000L)
})

# Error output (isError result) is capped too.
local({
    on.exit({
        options(corteza.policy = NULL)
        corteza:::clear_handles()
    }, add = TRUE)
    options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "test allow")
    })
    fake <- function(name, args) {
        list(isError = TRUE,
             content = list(list(type = "text",
                                 text = paste(sprintf("err %d", 1:50000),
                                              collapse = "\n"))))
    }
    s <- corteza::new_session("cli",
                              approval_cb = function(call, decision) TRUE)
    h <- corteza:::.make_tool_handler(s, tool_executor = fake)
    out <- h("bash", list(command = "x"))
    expect_true(grepl("truncated", out))
    expect_true(nchar(out) < 5000L)
})

# Non-bash tool: read_handle(op="print") of a huge object can't re-inline
# the full text -- the guard catches it on the way back too.
local({
    on.exit({
        options(corteza.policy = NULL)
        corteza:::clear_handles()
    }, add = TRUE)
    options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "test allow")
    })
    stash <- corteza:::with_handle(sprintf("v%d", 1:50000))
    s <- corteza::new_session("cli",
                              approval_cb = function(call, decision) TRUE)
    h <- corteza:::.make_tool_handler(s) # default executor -> call_skill
    out <- h("read_handle", list(handle = stash$handle, op = "print"))
    expect_true(grepl("truncated", out))
    expect_true(nchar(out) < 5000L)
})

# Real bash path: a command that prints thousands of lines is capped,
# and the full output is recoverable from the handle. Needs a shell and
# the builtin skill registry, so gate behind at_home().
if (at_home()) {
    op_bash <- options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "test allow")
    })
    s <- corteza::new_session("cli",
                              approval_cb = function(call, decision) TRUE)
    h <- corteza:::.make_tool_handler(s)
    out <- h("bash", list(command = "seq 1 5000"))
    expect_true(grepl("truncated", out))
    h_id <- regmatches(out, regexpr("\\.h_[0-9]+", out))
    expect_true(nzchar(h_id))
    full <- corteza:::get_handle(h_id)
    expect_true(length(full) >= 5000L)
    options(op_bash)
    corteza:::clear_handles()
}
