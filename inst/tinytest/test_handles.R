# Tests for R/handles.R — large-result stashing and read_handle.

corteza::ensure_skills()
corteza:::clear_handles()
on.exit(corteza:::clear_handles(), add = TRUE)

# --- is_large_result heuristics ----------------------------------------

expect_false(corteza:::.is_large_result(NULL))
expect_false(corteza:::.is_large_result(1L))
expect_false(corteza:::.is_large_result("x"))
expect_false(corteza:::.is_large_result(TRUE))
# Medium vectors pass through.
expect_false(corteza:::.is_large_result(1:10))
# Long vectors get stashed.
expect_true(corteza:::.is_large_result(1:100))
# Data frames and matrices always handle.
expect_true(corteza:::.is_large_result(data.frame(x = 1, y = 2)))
expect_true(corteza:::.is_large_result(matrix(1:4, 2, 2)))
# Long lists handle.
expect_true(corteza:::.is_large_result(as.list(1:20)))

# --- with_handle / get_handle round trip -------------------------------

df <- data.frame(x = 1:3, y = letters[1:3])
stashed <- corteza:::with_handle(df)
expect_true(is.list(stashed))
expect_true(is.character(stashed$handle))
expect_true(grepl("^\\.h_\\d+$", stashed$handle))
expect_true(is.character(stashed$summary))
expect_true(nchar(stashed$summary) > 0L)

retrieved <- corteza:::get_handle(stashed$handle)
expect_equal(retrieved, df)
expect_true(stashed$handle %in% corteza:::list_handles())

# Unknown handle returns NULL.
expect_null(corteza:::get_handle(".h_does_not_exist"))

# Multiple handles get distinct ids.
h2 <- corteza:::with_handle(matrix(1:4, 2, 2))
expect_false(identical(stashed$handle, h2$handle))

# --- run_r: scalars pass through, large values stash --------------------

corteza:::clear_handles()

# Scalar result prints normally, no handle.
res <- corteza:::call_tool("run_r", list(code = "2 + 2"))
expect_false(isTRUE(res$isError))
expect_true(grepl("^\\[1\\] 4", res$content[[1]]$text))
expect_equal(length(corteza:::list_handles()), 0L)

# Data frame stashed as handle; output is summary + marker.
res <- corteza:::call_tool("run_r",
                           list(code = "data.frame(a = 1:5, b = letters[1:5])"))
expect_false(isTRUE(res$isError))
text <- res$content[[1]]$text
expect_true(grepl("stored as \\.h_\\d+", text))
# str() output mentions columns.
expect_true(grepl("'data.frame'", text, fixed = TRUE))
expect_equal(length(corteza:::list_handles()), 1L)

# Invisible assignments persist in globalenv. run_r evaluates in
# globalenv (PR <fix> 2026-05-20) so `<-` matches the tool's
# docstring; the earlier child-env behavior silently dropped
# assignments. The handle stash should still be empty since `NULL`
# produced no visible large result. Cleanup is inline (on.exit at
# tinytest top-level fires immediately after the expression that
# registered it, so it would nuke the variable before the
# assertion below could see it).
corteza:::clear_handles()
suppressWarnings(rm("x_internal_assign", envir = globalenv()))
res <- corteza:::call_tool("run_r",
                           list(code = "x_internal_assign <- 1:1000; NULL"))
expect_false(isTRUE(res$isError))
expect_true("x_internal_assign" %in% ls(globalenv()))
expect_equal(length(corteza:::list_handles()), 0L)
suppressWarnings(rm("x_internal_assign", envir = globalenv()))

# --- Handle visible in subsequent run_r --------------------------------

corteza:::clear_handles()
h <- corteza:::with_handle(data.frame(x = 1:10, y = 11:20))
res <- corteza:::call_tool("run_r",
                           list(code = sprintf("nrow(%s)", h$handle)))
expect_false(isTRUE(res$isError))
expect_true(grepl("^\\[1\\] 10", res$content[[1]]$text))

# Regression (codex 2026-05-20): handle_eval_env() used to skip
# reassignment when the .h_NNN symbol already existed in globalenv,
# so re-binding a handle in the store left the old globalenv copy
# stale. Force a rebind and verify the globalenv binding reflects
# the new value.
corteza:::clear_handles()
h1 <- corteza:::with_handle(data.frame(x = 1:10, y = 11:20))
# Replace the same handle id with a smaller frame.
assign(h1$handle, data.frame(x = 1:5), envir = corteza:::.handle_store)
res <- corteza:::call_tool("run_r",
                           list(code = sprintf("nrow(%s)", h1$handle)))
expect_false(isTRUE(res$isError))
expect_true(grepl("^\\[1\\] 5", res$content[[1]]$text))

# Stale handles are removed from globalenv when they drop out of
# the store. After clear_handles(), the previously copied .h_NNN
# symbol must not linger -- clear_handles() itself sweeps the
# managed bindings out of globalenv.
corteza:::clear_handles()
h2 <- corteza:::with_handle(data.frame(x = 1:3))
corteza:::call_tool("run_r", list(code = sprintf("nrow(%s)", h2$handle)))
expect_true(exists(h2$handle, envir = globalenv(), inherits = FALSE))
corteza:::clear_handles()
expect_false(exists(h2$handle, envir = globalenv(), inherits = FALSE))

# --- read_handle ops ---------------------------------------------------

corteza:::clear_handles()
h <- corteza:::with_handle(data.frame(x = 1:6, y = letters[1:6]))

res <- corteza:::call_tool("read_handle", list(handle = h$handle, op = "head"))
expect_false(isTRUE(res$isError))
expect_true(grepl("x y", res$content[[1]]$text))

res <- corteza:::call_tool("read_handle", list(handle = h$handle, op = "str"))
expect_false(isTRUE(res$isError))
expect_true(grepl("'data.frame'", res$content[[1]]$text, fixed = TRUE))

res <- corteza:::call_tool("read_handle", list(handle = h$handle, op = "summary"))
expect_false(isTRUE(res$isError))

# Default op is "str".
res <- corteza:::call_tool("read_handle", list(handle = h$handle))
expect_false(isTRUE(res$isError))

# Unknown handle produces a clean error, not a crash.
res <- corteza:::call_tool("read_handle",
                           list(handle = ".h_does_not_exist", op = "str"))
expect_true(isTRUE(res$isError))
expect_true(grepl("Unknown handle", res$content[[1]]$text))

# Unknown op errors cleanly.
res <- corteza:::call_tool("read_handle",
                           list(handle = h$handle, op = "bogus"))
expect_true(isTRUE(res$isError))
