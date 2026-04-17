library(tinytest)

expect_true(is.function(corteza::mcp_tool_executor))

# Pluggable executor: custom function wins over call_skill.
local({
    seen <- list()
    custom <- function(name, args) {
        seen[[length(seen) + 1L]] <<- list(name = name, args = args)
        corteza:::ok(sprintf("custom-ran-%s", name))
    }

    s <- corteza::new_session("cli",
                             approval_cb = function(call, decision) TRUE)
    h <- corteza:::.make_tool_handler(s, tool_executor = custom)

    tmp <- tempfile("exec-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    op <- options(corteza.personal_paths = character(),
                  corteza.code_paths = c(tmp),
                  corteza.policy = NULL)
    on.exit(options(op), add = TRUE)

    out <- h("list_files", list(path = tmp))
    expect_equal(out, "custom-ran-list_files")
    expect_equal(length(seen), 1L)
    expect_equal(seen[[1]]$name, "list_files")
    expect_equal(seen[[1]]$args$path, tmp)
})

# Executor can surface errors via err()
local({
    s <- corteza::new_session("cli",
                             approval_cb = function(call, decision) TRUE)
    failing <- function(name, args) corteza:::err("bad tool")
    h <- corteza:::.make_tool_handler(s, tool_executor = failing)

    op <- options(corteza.personal_paths = character(),
                  corteza.policy = NULL)
    on.exit(options(op), add = TRUE)

    out <- h("list_files", list(path = "/tmp"))
    expect_true(grepl("^Error: ", out))
})

# mcp_tool_executor returns a function that forwards to mcp_call.
# Verify shape; live MCP call is not tested here.
local({
    fake_conn <- list(port = 0L, socket = NULL)
    exec <- corteza::mcp_tool_executor(fake_conn)
    expect_true(is.function(exec))
    # Calling it without a live socket errors — that's fine; we only
    # need to show the closure exists and accepts the right arity.
    expect_error(exec("read_file", list(path = "/tmp/x")))
})
