# Test configuration loading

# Setup: use a fresh temp directory
tmpdir <- tempdir()
testdir <- file.path(tmpdir, paste0("cfg_test_", Sys.getpid()))
if (dir.exists(testdir)) unlink(testdir, recursive = TRUE)
dir.create(testdir, recursive = TRUE)

# Test load_config with no config file - context_files defaults to empty
# (saber owns standard context loading)
config <- llamaR:::load_config(testdir)
expect_equal(config$provider, "anthropic")
expect_equal(config$context_files, character(0))
expect_false(isTRUE(config$context_include_memory_logs))
expect_false(isTRUE(config$memory_flush_enabled))
expect_false(isTRUE(config$legacy_memory_tools_enabled))
expect_true("write_file" %in% config$dangerous_tools)

# Test project config is loaded
dir.create(file.path(testdir, ".llamar"), showWarnings = FALSE)
writeLines('{"provider": "ollama", "model": "llama3.2"}',
    file.path(testdir, ".llamar", "config.json"))

config <- llamaR:::load_config(testdir)
expect_equal(config$provider, "ollama")
expect_equal(config$model, "llama3.2")

# Test custom context_files
writeLines('{"context_files": ["README.md", "CUSTOM.md"]}',
    file.path(testdir, ".llamar", "config.json"))

config <- llamaR:::load_config(testdir)
expect_equal(config$context_files, c("README.md", "CUSTOM.md"))

# Test get_context_files uses config
files <- llamaR:::get_context_files(testdir)
expect_equal(files, c("README.md", "CUSTOM.md"))

# Test invalid JSON is handled gracefully (falls back to empty)
writeLines('not valid json', file.path(testdir, ".llamar", "config.json"))
config <- llamaR:::load_config(testdir)
expect_equal(config$context_files, character(0))

# Cleanup
unlink(testdir, recursive = TRUE)
