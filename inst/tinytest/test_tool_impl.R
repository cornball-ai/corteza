# Test tool implementations

# Test run_r
result <- llamaR:::tool_run_r(list(code = "1 + 1"))
expect_false(isTRUE(result$isError))
expect_true(grepl("2", result$content[[1]]$text))

# Test run_r with error
result <- llamaR:::tool_run_r(list(code = "stop('test error')"))
expect_true(grepl("Error", result$content[[1]]$text))

# Test bash
result <- llamaR:::tool_bash(list(command = "echo hello"))
expect_false(isTRUE(result$isError))
expect_true(grepl("hello", result$content[[1]]$text))

# Test grep_files
tmp_dir <- tempfile("grep_test")
dir.create(tmp_dir)
writeLines(c("find this line", "not this one"), file.path(tmp_dir, "test.R"))
result <- llamaR:::tool_grep_files(list(
    pattern = "find this",
    path = tmp_dir,
    file_pattern = "*.R"
))
expect_false(isTRUE(result$isError))
expect_true(grepl("find this line", result$content[[1]]$text))
unlink(tmp_dir, recursive = TRUE)
