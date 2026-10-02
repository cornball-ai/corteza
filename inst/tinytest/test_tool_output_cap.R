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

# ---- Who can read a cut result, and what the marker tells them ----

corteza::ensure_skills()
big_result <- paste(sprintf("- room %02d", 1:60), collapse = "\n")
allow_all <- function(call) {
    list(model = "cloud", approval = "allow", reason = "test allow")
}
handler_for <- function(s) {
    corteza:::.make_tool_handler(s, tool_executor = function(name, args) {
        if (identical(name, "read_handle")) {
            return(corteza:::call_skill(name, as.list(args),
                                        ctx = list(session = s)))
        }
        list(content = list(list(type = "text", text = big_result)))
    })
}
handle_in <- function(out) regmatches(out, regexpr("\\.[ho]_[0-9]+", out))

# A session with its own store: the result goes there, under a name the
# workspace's handles never use, and the process's store is untouched.
local({
    on.exit({
        options(corteza.policy = NULL)
        corteza:::clear_handles()
    }, add = TRUE)
    options(corteza.policy = allow_all)
    corteza:::clear_handles()
    a <- corteza::new_session("matrix")
    a$handle_store <- new.env(parent = emptyenv())
    b <- corteza::new_session("matrix")
    b$handle_store <- new.env(parent = emptyenv())
    out <- handler_for(a)("grep_files", list(pattern = "x"))
    expect_true(grepl("[tool output truncated]", out, fixed = TRUE))
    id <- handle_in(out)
    expect_identical(id, ".o_001")
    expect_identical(length(get(id, envir = a$handle_store)), 60L)
    expect_identical(length(corteza:::list_handles()), 0L)
    # The marker says how to read on, with the tool this session has.
    expect_true(grepl("read_handle(\".o_001\", op = \"grep\"", out, fixed = TRUE))
    expect_true(grepl("start = 41", out, fixed = TRUE))
    expect_false(grepl("/last", out, fixed = TRUE))
    # The session that got the result can search it.
    found <- handler_for(a)("read_handle", list(handle = id, op = "grep",
                                                pattern = "room 57"))
    expect_true(grepl("57: - room 57", found, fixed = TRUE))
    # Another session in the same process cannot open it.
    other <- handler_for(b)("read_handle", list(handle = id, op = "grep",
                                                pattern = "room 57"))
    expect_true(grepl("Unknown handle", other))
    # A workspace handle of the same number is a different thing, and
    # still readable from a session with its own store.
    ws <- corteza:::with_handle(sprintf("ws %d", 1:3))
    expect_identical(ws$handle, ".h_001")
    expect_true(grepl("ws 2", handler_for(a)("read_handle",
                                              list(handle = ".h_001", op = "print"))))
})

# The marker names only a tool the session has.
local({
    on.exit({
        options(corteza.policy = NULL)
        corteza:::clear_handles()
    }, add = TRUE)
    options(corteza.policy = allow_all)
    marker <- function(tools, own = FALSE) {
        s <- corteza::new_session("matrix")
        s$tools_filter <- tools
        if (own) {
            s$handle_store <- new.env(parent = emptyenv())
        }
        handler_for(s)("grep_files", list(pattern = "x"))
    }
    with_reader <- marker(c("grep_files", "read_handle"))
    expect_true(grepl("search the rest. read_handle(", with_reader, fixed = TRUE))
    # No reader, but R in this process: the handle is a vector there.
    with_r <- marker(c("grep_files", "run_r"))
    expect_false(grepl("read_handle", with_r, fixed = TRUE))
    expect_true(grepl("In run_r it is the character vector `.h_", with_r,
                      fixed = TRUE))
    expect_true(grepl("search the rest", with_r, fixed = TRUE))
    # And it is: run_r reads the name the marker gave.
    in_r <- corteza:::call_tool("run_r", list(
            code = sprintf("grep('room 57', %s, value = TRUE)", handle_in(with_r))))
    expect_true(grepl("- room 57", in_r$content[[1]]$text, fixed = TRUE))
    # The session's own store is not visible to run_r.
    own_r <- marker(c("grep_files", "run_r"), own = TRUE)
    expect_true(grepl("No tool in this session can open it", own_r, fixed = TRUE))
    # Neither: say so, and what to do instead.
    bare <- marker(c("grep_files", "git_log"))
    expect_false(grepl("read_handle|run_r", bare))
    expect_true(grepl("No tool in this session can open it", bare, fixed = TRUE))
    expect_true(grepl("Ask again for less", bare, fixed = TRUE))
    # Every tool (no filter): the reader.
    expect_true(grepl("search the rest. read_handle(", marker(NULL), fixed = TRUE))
})

# A session whose R runs in a worker gets a store of its own, so the
# handle its marker names can be read: read_handle would otherwise ask
# the worker, which never saw a result cut in this process.
local({
    on.exit({
        options(corteza.policy = NULL)
        corteza:::clear_handles()
    }, add = TRUE)
    options(corteza.policy = allow_all)
    corteza:::clear_handles()
    s <- corteza::new_session("cli")
    s$config <- list(run_r_mode = "worker")
    out <- handler_for(s)("grep_files", list(pattern = "x"))
    id <- handle_in(out)
    expect_identical(id, ".o_001")
    expect_identical(length(corteza:::list_handles()), 0L)
    found <- handler_for(s)("read_handle", list(handle = id, op = "lines",
                                                start = 59))
    expect_true(grepl("lines 59 to 60 of 60", found, fixed = TRUE))
})

# Long lines: the preview is cut by characters, so fewer lines are shown
# than the line limit allows. The marker counts the lines it shows and
# reading on starts at the first one not shown whole.
local({
    on.exit(corteza:::clear_handles(), add = TRUE)
    corteza:::clear_handles()
    wide <- sprintf("%02d %s", 1:30, strrep("w", 497L))
    out <- corteza:::admit_tool_result(paste(wide, collapse = "\n"), tool = "bash")
    expect_true(grepl("original: 30 lines, 15029 chars", out, fixed = TRUE))
    expect_true(grepl("showing: first 8 lines / 4000 chars", out, fixed = TRUE))
    expect_true(grepl("start = 8 reads on", out, fixed = TRUE))
    # Line 8 is cut in the preview, and whole when read from there.
    expect_false(grepl(wide[8], out, fixed = TRUE))
    expect_true(grepl(wide[7], out, fixed = TRUE))
    more <- corteza:::tool_read_handle(handle_in(out), op = "lines", start = 8)
    expect_true(grepl(wide[8], more$content[[1]]$text, fixed = TRUE))
})

# Called directly, as the CLI's buffer does: the reader and /last.
local({
    on.exit(corteza:::clear_handles(), add = TRUE)
    out <- corteza:::admit_tool_result(big_result, tool = "bash")
    expect_true(grepl("Before saying what this output does not contain", out,
                      fixed = TRUE))
    expect_true(grepl("read_handle(\".h_", out, fixed = TRUE))
    expect_true(grepl("Or use /last.", out, fixed = TRUE))
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
