# Tests for R/dispatch.R (worker_init) and R/tool_error.R
# (make_tool_error). worker_init runs inside a subagent's callr session
# to set cwd and register skills; make_tool_error builds the
# corteza_tool_error condition.

# make_tool_error is unexported; tests reach it via triple-colon.

# worker_init smoke test: returns TRUE invisibly and registers skills.
res <- corteza:::worker_init(tempdir())
expect_true(res)
expect_true(!is.null(corteza:::get_skill("run_r")))

# make_tool_error preserves original condition details
orig <- simpleError("kaboom")
e <- corteza:::make_tool_error("my_tool", list(x = 1), "wrapped", orig)
expect_inherits(e, "corteza_tool_error")
expect_equal(e$tool, "my_tool")
expect_equal(e$args$x, 1)
expect_equal(conditionMessage(e), "wrapped")
expect_true("simpleError" %in% e$original_class)
expect_equal(e$original_message, "kaboom")
