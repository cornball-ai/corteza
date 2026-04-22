# Test tool implementations

# Test run_r
result <- corteza:::tool_run_r(code = "1 + 1")
expect_false(isTRUE(result$isError))
expect_true(grepl("2", result$content[[1]]$text))

# Test run_r with error
result <- corteza:::tool_run_r(code = "stop('test error')")
expect_true(grepl("Error", result$content[[1]]$text))

# Test shell tool (bash when available, cmd on minimal-install Windows)
if (.Platform$OS.type == "windows" && !file.exists(corteza:::.find_bash_exe())) {
    result <- corteza:::tool_cmd(command = "echo hello")
} else {
    result <- corteza:::tool_bash(command = "echo hello")
}
expect_false(isTRUE(result$isError))
expect_true(grepl("hello", result$content[[1]]$text))

# Test grep_files
tmp_dir <- tempfile("grep_test")
dir.create(tmp_dir)
writeLines(c("find this line", "not this one"), file.path(tmp_dir, "test.R"))
result <- corteza:::tool_grep_files(
    pattern = "find this",
    path = tmp_dir,
    file_pattern = "*.R"
)
expect_false(isTRUE(result$isError))
expect_true(grepl("find this line", result$content[[1]]$text))
unlink(tmp_dir, recursive = TRUE)

# Test list_files / write_file / read_file / replace_in_file
tmp_dir <- tempfile("file_tools")
dir.create(tmp_dir)
file_path <- file.path(tmp_dir, "notes.txt")

result <- corteza:::tool_write_file(path = file_path, content = "alpha\nbeta\n")
expect_false(isTRUE(result$isError))
expect_true(file.exists(file_path))

result <- corteza:::tool_read_file(path = file_path, from = 2L, lines = 1L)
expect_false(isTRUE(result$isError))
expect_true(grepl("2 \\| beta", result$content[[1]]$text))

result <- corteza:::tool_replace_in_file(
    path = file_path,
    old_text = "beta",
    new_text = "gamma"
)
expect_false(isTRUE(result$isError))
expect_true(grepl("gamma", paste(readLines(file_path), collapse = "\n")))

result <- corteza:::tool_list_files(path = tmp_dir)
expect_false(isTRUE(result$isError))
expect_true(grepl("notes.txt", result$content[[1]]$text))

unlink(tmp_dir, recursive = TRUE)

# Test installed_packages
result <- corteza:::tool_installed_packages(pattern = "jsonlite", limit = 20L)
expect_false(isTRUE(result$isError))
expect_true(grepl("jsonlite", result$content[[1]]$text, ignore.case = TRUE))

# Test git tools
tmp_repo <- tempfile("git_tools")
dir.create(tmp_repo)
tracked <- file.path(tmp_repo, "tracked.txt")
system2("git", c("-C", tmp_repo, "init"))
system2("git", c("-C", tmp_repo, "config", "user.email", "test@example.com"))
system2("git", c("-C", tmp_repo, "config", "user.name", "corteza test"))
writeLines("hello", tracked)
system2("git", c("-C", tmp_repo, "add", "tracked.txt"))
system2("git", c("-C", tmp_repo, "commit", "-m", "initial"))
writeLines("hello world", tracked)

result <- corteza:::tool_git_status(path = tmp_repo)
expect_false(isTRUE(result$isError))
expect_true(grepl("tracked.txt", result$content[[1]]$text, fixed = TRUE))

result <- corteza:::tool_git_diff(path = tmp_repo)
expect_false(isTRUE(result$isError))
expect_true(grepl("-hello", result$content[[1]]$text, fixed = TRUE))
expect_true(grepl("+hello world", result$content[[1]]$text, fixed = TRUE))

result <- corteza:::tool_git_log(path = tmp_repo, n = 1L)
expect_false(isTRUE(result$isError))
expect_true(grepl("initial", result$content[[1]]$text, fixed = TRUE))

unlink(tmp_repo, recursive = TRUE)
