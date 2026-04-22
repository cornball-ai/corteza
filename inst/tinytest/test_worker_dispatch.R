# Tests for R/dispatch.R and R/tool_error.R — the worker-side entry
# point the CLI calls through callr::r_session in Phase 2.

# Skills are registered lazily by the package; some tests above us may
# not have triggered registration, so do it explicitly.
corteza:::ensure_skills()

# make_tool_error is unexported; tests reach it via triple-colon.
# (The function itself doesn't need to be exported; only worker_*
# functions that are called from inside the callr session do.)

# In-process dispatch smoke test: bash succeeds
if (.Platform$OS.type != "windows" || file.exists(corteza:::.find_bash_exe())) {
    res <- corteza::worker_dispatch("bash", list(command = "echo corteza_ok"))
    expect_false(isTRUE(res$isError))
    expect_true(grepl("corteza_ok", res$content[[1]]$text))
}

# Unknown tool name -> corteza_tool_error
err <- tryCatch(
    corteza::worker_dispatch("no_such_tool_exists_foo", list()),
    error = function(e) e
)
expect_inherits(err, "corteza_tool_error")
expect_equal(err$tool, "no_such_tool_exists_foo")
expect_true(grepl("unknown tool", conditionMessage(err)))

# Empty tool name -> corteza_tool_error
err <- tryCatch(
    corteza::worker_dispatch("", list()),
    error = function(e) e
)
expect_inherits(err, "corteza_tool_error")

# make_tool_error preserves original condition details
orig <- simpleError("kaboom")
e <- corteza:::make_tool_error("my_tool", list(x = 1), "wrapped", orig)
expect_inherits(e, "corteza_tool_error")
expect_equal(e$tool, "my_tool")
expect_equal(e$args$x, 1)
expect_equal(conditionMessage(e), "wrapped")
expect_true("simpleError" %in% e$original_class)
expect_equal(e$original_message, "kaboom")

# callr round-trip: worker_dispatch crosses the session boundary,
# and the corteza_tool_error condition is preserved on the CLI side.
# Skipped on Windows CRAN builder to avoid long-running callr bootstrap.
if (requireNamespace("callr", quietly = TRUE) &&
    !identical(Sys.getenv("NOT_CRAN"), "false") &&
    tinytest::at_home()) {
    session <- callr::r_session$new(wait = TRUE)
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    session$run(function() library(corteza))
    session$run(function() corteza:::worker_init())

    # Unknown tool across the boundary should surface as a corteza_tool_error
    err <- tryCatch(
        session$run(function() corteza::worker_dispatch("no_such_tool_foo", list())),
        error = function(e) e
    )
    cause <- if (!is.null(err$parent)) err$parent else err
    expect_inherits(cause, "corteza_tool_error")
    expect_equal(cause$tool, "no_such_tool_foo")

    # Successful dispatch round-trip (run_r 1+1)
    res <- session$run(function() {
        corteza::worker_dispatch("run_r", list(code = "1 + 1"))
    })
    expect_false(isTRUE(res$isError))
    expect_true(grepl("2", res$content[[1]]$text))
}
