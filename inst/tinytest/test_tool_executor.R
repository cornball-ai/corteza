library(tinytest)

expect_true(is.function(llamaR::mcp_tool_executor))

# Pluggable executor: custom function wins over call_skill.
local({
    seen <- list()
    custom <- function(name, args) {
        seen[[length(seen) + 1L]] <<- list(name = name, args = args)
        llamaR:::ok(sprintf("custom-ran-%s", name))
    }

    s <- llamaR::new_session("cli",
                             approval_cb = function(call, decision) TRUE)
    h <- llamaR:::.make_tool_handler(s, tool_executor = custom)

    tmp <- tempfile("exec-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    op <- options(llamaR.personal_paths = character(),
                  llamaR.code_paths = c(tmp),
                  llamaR.policy = NULL)
    on.exit(options(op), add = TRUE)

    out <- h("list_files", list(path = tmp))
    expect_equal(out, "custom-ran-list_files")
    expect_equal(length(seen), 1L)
    expect_equal(seen[[1]]$name, "list_files")
    expect_equal(seen[[1]]$args$path, tmp)
})

# Executor can surface errors via err()
local({
    s <- llamaR::new_session("cli",
                             approval_cb = function(call, decision) TRUE)
    failing <- function(name, args) llamaR:::err("bad tool")
    h <- llamaR:::.make_tool_handler(s, tool_executor = failing)

    op <- options(llamaR.personal_paths = character(),
                  llamaR.policy = NULL)
    on.exit(options(op), add = TRUE)

    out <- h("list_files", list(path = "/tmp"))
    expect_true(grepl("^Error: ", out))
})

# mcp_tool_executor returns a function that forwards to mcp_call.
# Verify shape; live MCP call is not tested here.
local({
    fake_conn <- list(port = 0L, socket = NULL)
    exec <- llamaR::mcp_tool_executor(fake_conn)
    expect_true(is.function(exec))
    # Calling it without a live socket errors — that's fine; we only
    # need to show the closure exists and accepts the right arity.
    expect_error(exec("read_file", list(path = "/tmp/x")))
})
