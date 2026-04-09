# Test tool definitions and implementations

# Test get_tools returns expected structure
tools <- llamaR:::get_tools()
expect_true(is.list(tools))
expect_true(length(tools) > 0)

# Check each tool has required fields
for (tool in tools) {
    expect_true("name" %in% names(tool))
    expect_true("description" %in% names(tool))
    expect_true("inputSchema" %in% names(tool))
}

# Test built-in tools still exist
tool_names <- sapply(tools, `[[`, "name")
expect_true("run_r" %in% tool_names)
expect_true("bash" %in% tool_names)
expect_true("r_help" %in% tool_names)
expect_true("grep_files" %in% tool_names)
expect_true("read_file" %in% tool_names)
expect_true("write_file" %in% tool_names)
expect_true("replace_in_file" %in% tool_names)
expect_true("list_files" %in% tool_names)
expect_true("git_status" %in% tool_names)
expect_true("git_diff" %in% tool_names)
expect_true("git_log" %in% tool_names)
expect_true("fetch_url" %in% tool_names)
expect_true("installed_packages" %in% tool_names)

# Test tools still intentionally hidden or absent by default
expect_false("memory_store" %in% tool_names)
expect_false("memory_recall" %in% tool_names)
expect_false("memory_get" %in% tool_names)
expect_false("read_csv" %in% tool_names)
expect_false("chat" %in% tool_names)

# Test ok/err helpers
ok_result <- llamaR:::ok("test")
expect_true(is.list(ok_result))
expect_true("content" %in% names(ok_result))
expect_equal(ok_result$content[[1]]$text, "test")

err_result <- llamaR:::err("error")
expect_true(is.list(err_result))
expect_true(err_result$isError)
expect_equal(err_result$content[[1]]$text, "error")

# Test dynamic tool categories
cats <- llamaR:::get_tool_categories()
expect_true("file" %in% names(cats))
expect_true("read_file" %in% cats$file)
expect_true("code" %in% names(cats))
expect_true("run_r" %in% cats$code)
expect_true("search" %in% names(cats))
expect_true("grep_files" %in% cats$search)
expect_true("git" %in% names(cats))
expect_true("git_status" %in% cats$git)
