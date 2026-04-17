# Test context loading
#
# Standard context files (memory, SOUL.md, USER.md, CLAUDE.md, AGENTS.md)
# are loaded via saber::agent_context(). Tests for that logic live in saber.
# Here we test the corteza-side assembly: preamble, custom context_files,
# briefing integration, and skill docs.

# Setup: use a fresh temp directory
tmpdir <- tempdir()
testdir <- file.path(tmpdir, paste0("ctx_test_", Sys.getpid()))
if (dir.exists(testdir)) unlink(testdir, recursive = TRUE)
dir.create(testdir, recursive = TRUE)

# --- list_context_files returns empty when no custom files configured ---
files <- corteza:::list_context_files(testdir)
expect_equal(length(files), 0)

# --- load_context with no project files returns NULL or system prompt ---
# (depends on whether workspace files / packages exist on the host)
ctx <- corteza:::load_context(testdir)
expect_true(is.null(ctx) || is.character(ctx))

# --- Custom context_files via project config ---
dir.create(file.path(testdir, ".corteza"), showWarnings = FALSE)
writeLines(c("# My Project", "", "This is the readme."),
           file.path(testdir, "README.md"))
writeLines('{"context_files": ["README.md"]}',
           file.path(testdir, ".corteza", "config.json"))

files <- corteza:::list_context_files(testdir)
expect_equal(length(files), 1)
expect_true(grepl("README.md", files[1]))

ctx <- corteza:::load_context(testdir)
expect_true(is.character(ctx))
expect_true(grepl("README.md", ctx))
expect_true(grepl("My Project", ctx))
expect_true(grepl("You are an AI assistant", ctx))

# --- Multiple custom files ---
writeLines(c("# Plan", "", "Phase 1: Core"), file.path(testdir, "PLAN.md"))
writeLines('{"context_files": ["README.md", "PLAN.md"]}',
           file.path(testdir, ".corteza", "config.json"))

files <- corteza:::list_context_files(testdir)
expect_equal(length(files), 2)

ctx <- corteza:::load_context(testdir)
expect_true(grepl("README.md", ctx))
expect_true(grepl("PLAN.md", ctx))
expect_true(grepl("Phase 1: Core", ctx))

# --- Missing custom files are silently skipped ---
writeLines('{"context_files": ["README.md", "DOES_NOT_EXIST.md"]}',
           file.path(testdir, ".corteza", "config.json"))

files <- corteza:::list_context_files(testdir)
expect_equal(length(files), 1)

ctx <- corteza:::load_context(testdir)
expect_true(grepl("My Project", ctx))
expect_false(grepl("DOES_NOT_EXIST", ctx))

# Cleanup
unlink(testdir, recursive = TRUE)
