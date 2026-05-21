# Test tool definitions and implementations

# Test get_tools returns expected structure
tools <- corteza:::get_tools()
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
# Shell tool is "bash" when a real bash is available (always on POSIX,
# Rtools/Git Bash on Windows) and "cmd" on minimal-install Windows.
expect_true(any(c("bash", "cmd") %in% tool_names))
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
ok_result <- corteza:::ok("test")
expect_true(is.list(ok_result))
expect_true("content" %in% names(ok_result))
expect_equal(ok_result$content[[1]]$text, "test")

err_result <- corteza:::err("error")
expect_true(is.list(err_result))
expect_true(err_result$isError)
expect_equal(err_result$content[[1]]$text, "error")

# Test dynamic tool categories
cats <- corteza:::get_tool_categories()
expect_true("file" %in% names(cats))
expect_true("read_file" %in% cats$file)
expect_true("code" %in% names(cats))
expect_true("run_r" %in% cats$code)
expect_true("search" %in% names(cats))
expect_true("grep_files" %in% cats$search)
expect_true("git" %in% names(cats))
expect_true("git_status" %in% cats$git)

# sanitize_tool_name / unsanitize_tool_name ----
# Anthropic and OpenAI tool names must match [a-zA-Z0-9_-]. Internal
# names can include "::" (package qualifier) and "." (R function names
# like read.csv). Encoding: "::" -> "__", "." -> "_dot_". Hyphens are
# valid in both internal and API names and pass through unchanged.

# Regression: round-trip must be lossless and injective even when the
# internal name contains a literal hyphen. An earlier encoding used
# "-" as the escape for ".", which collapsed "a.b" and "a-b" to the
# same API form and silently misrouted any user-registered skill whose
# name contained a hyphen.

# Hyphens pass through unchanged.
expect_equal(corteza:::sanitize_tool_name("pkg::some-tool"),
             "pkg__some-tool")
expect_equal(corteza:::unsanitize_tool_name("pkg__some-tool"),
             "pkg::some-tool")

# Dots encode as _dot_.
expect_equal(corteza:::sanitize_tool_name("base::read.csv"),
             "base__read_dot_csv")
expect_equal(corteza:::unsanitize_tool_name("base__read_dot_csv"),
             "base::read.csv")

# Names with both '.' and '-' round-trip with neither collapsing into
# the other.
expect_equal(corteza:::sanitize_tool_name("pkg::weird.name-here"),
             "pkg__weird_dot_name-here")
expect_equal(corteza:::unsanitize_tool_name("pkg__weird_dot_name-here"),
             "pkg::weird.name-here")

# Round-trip is identity across a range of realistic name shapes. Note
# that the encoding scheme is safe for any internal name that does not
# itself contain the literal substring "_dot_"; that's not a realistic
# R function name pattern but is worth knowing.
for (name in c("base::readLines",
               "base::read.csv",
               "pkg::some-tool",
               "pkg::weird.name-here",
               "single_word")) {
    expect_equal(corteza:::unsanitize_tool_name(
                     corteza:::sanitize_tool_name(name)),
                 name)
}

# Sanitized form matches the API-allowed character set.
for (name in c("base::read.csv",
               "pkg::some-tool",
               "pkg::weird.name-here")) {
    sanitized <- corteza:::sanitize_tool_name(name)
    expect_true(grepl("^[a-zA-Z0-9_-]+$", sanitized))
}
