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

# Invisible assignments don't trigger a handle.
corteza:::clear_handles()
res <- corteza:::call_tool("run_r",
                           list(code = "x_internal_assign <- 1:1000; NULL"))
expect_false(isTRUE(res$isError))
# Assignment happens in eval_env, so globalenv doesn't get polluted.
expect_false("x_internal_assign" %in% ls(globalenv()))

# --- Handle visible in subsequent run_r --------------------------------

corteza:::clear_handles()
h <- corteza:::with_handle(data.frame(x = 1:10, y = 11:20))
res <- corteza:::call_tool("run_r",
                           list(code = sprintf("nrow(%s)", h$handle)))
expect_false(isTRUE(res$isError))
expect_true(grepl("^\\[1\\] 10", res$content[[1]]$text))

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
