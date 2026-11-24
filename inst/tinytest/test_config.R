# Test configuration loading

# Setup: use a fresh temp directory. Isolate from the user's real global
# config by pointing R_USER_CONFIG_DIR at a clean empty dir; otherwise a
# user with ~/.config/R/corteza/config.json leaks into these tests.
tmpdir <- tempdir()
testdir <- file.path(tmpdir, paste0("cfg_test_", Sys.getpid()))
if (dir.exists(testdir)) unlink(testdir, recursive = TRUE)
dir.create(testdir, recursive = TRUE)

isolated_global <- file.path(tmpdir, paste0("cfg_test_global_", Sys.getpid()))
if (dir.exists(isolated_global)) unlink(isolated_global, recursive = TRUE)
dir.create(isolated_global, recursive = TRUE)
Sys.setenv(R_USER_CONFIG_DIR = isolated_global)
# A project's config is read from a trusted directory only (R/trust.R;
# test_trust.R covers the other case). The list is the user's and is
# kept in the config directory set above.
corteza:::project_trust_add(testdir)

# Test load_config with no config file - context_files defaults to empty
# (saber owns standard context loading)
config <- corteza:::load_config(testdir)
expect_equal(config$provider, "anthropic")
expect_equal(config$context_files, character(0))
expect_false(isTRUE(config$context_include_memory_logs))
expect_identical(config$memory_flush_enabled, FALSE)
expect_identical(config$task_list_enabled, FALSE)
expect_null(config$cache)
expect_true("write_file" %in% config$dangerous_tools)
expect_equal(config$port, 7850L)
expect_equal(config$context_compact_bytes, 900000L)
expect_equal(config$context_request_buffer_retries, 3L)
expect_true(grepl("Pre-compaction memory flush", config$memory_flush_prompt))
expect_equal(config$skill_timeout, 30L)
expect_equal(config$skill_timeout_max, 1800L)
expect_equal(config$run_r_mode, "in_process")

# Subagent MCP-exposure defaults: opt-in off, USD cap $5, token cap unset.
expect_false(isTRUE(config$subagents$expose_over_mcp))
expect_equal(config$subagents$mcp_spend_cap_usd, 5.00)
expect_true(is.na(config$subagents$mcp_spend_cap_tokens))

# Follow the no-preset spawn path, including defaults supplied by load_config().
spawn_tools <- function(config) {
    corteza:::resolve_subagent_tools(
        default_tools = corteza:::get_subagent_config(config)$default_tools)
}
expect_identical(spawn_tools(config), c("read_file", "skill_instructions", "grep_files",
                                        "r_help", "web_search", "fetch_url"))

# Test project config is loaded
dir.create(file.path(testdir, ".corteza"), showWarnings = FALSE)
writeLines('{"provider": "ollama", "model": "llama3.2"}',
    file.path(testdir, ".corteza", "config.json"))

config <- corteza:::load_config(testdir)
expect_equal(config$provider, "ollama")
expect_equal(config$model, "llama3.2")

writeLines('{"subagents":{"default_tools":["read_file","list_files"]}}',
    file.path(testdir, ".corteza", "config.json"))
expect_identical(spawn_tools(corteza:::load_config(testdir)), c("read_file", "list_files"))

# The memory flush ahead of a compaction and the task list are off by
# default (asserted above), and a config turns each on. A prompt cache
# set here is kept as written; where none is set the default is not
# load_config()'s but the resolver's, by provider (.session_cache()).
writeLines('{"memory_flush_enabled": true, "task_list_enabled": true, "cache": "1h"}',
    file.path(testdir, ".corteza", "config.json"))
config <- corteza:::load_config(testdir)
expect_identical(config$memory_flush_enabled, TRUE)
expect_identical(config$task_list_enabled, TRUE)
expect_identical(config$cache, "1h")

# Test custom context_files
writeLines('{"context_files": ["README.md", "CUSTOM.md"]}',
    file.path(testdir, ".corteza", "config.json"))

config <- corteza:::load_config(testdir)
expect_equal(config$context_files, c("README.md", "CUSTOM.md"))

# Test invalid JSON is handled gracefully (falls back to empty)
writeLines('not valid json', file.path(testdir, ".corteza", "config.json"))
config <- corteza:::load_config(testdir)
expect_equal(config$context_files, character(0))

# skill_packages: object form parses as list-of-lists, not data.frame.
# Without simplifyDataFrame=FALSE, an array of {package, functions}
# objects collapses into a 1-row data.frame and load_skill_packages
# chokes with "$ operator is invalid for atomic vectors".
writeLines(
    '{"skill_packages":[{"package":"fortunes","functions":["fortune"]}]}',
    file.path(testdir, ".corteza", "config.json")
)
config <- corteza:::load_config(testdir)
expect_true(is.list(config$skill_packages))
expect_equal(length(config$skill_packages), 1L)
expect_equal(config$skill_packages[[1]]$package, "fortunes")
expect_equal(config$skill_packages[[1]]$functions, "fortune")

# String form still works.
writeLines(
    '{"skill_packages":["fortunes"]}',
    file.path(testdir, ".corteza", "config.json")
)
config <- corteza:::load_config(testdir)
expect_equal(config$skill_packages, "fortunes")

# Cleanup
unlink(testdir, recursive = TRUE)
