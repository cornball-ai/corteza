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

# Scope saber's cache to tempdir for the duration of this file.
# load_context() calls saber::briefing() / saber::agent_context(), which
# default to tools::R_user_dir("saber", "cache"). Without this redirect,
# R CMD check trips the "checking for new files in some other
# directories" NOTE for files left under the user's persistent cache.
# Restored in the cleanup block at the bottom of the file (on.exit() at
# top level fires immediately in tinytest, so we use explicit cleanup).
prev_user_cache_dir <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
Sys.setenv(R_USER_CACHE_DIR = file.path(tmpdir,
                                        paste0("ctx_cache_", Sys.getpid())))

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

# --- Runtime guidance block is present ---
expect_true(grepl("Corteza Runtime Environment", ctx))
expect_true(grepl("persistent R session", ctx))
expect_true(grepl("bash tool makes you a general-purpose agent", ctx))

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
unlink(Sys.getenv("R_USER_CACHE_DIR"), recursive = TRUE)
if (is.na(prev_user_cache_dir)) {
    Sys.unsetenv("R_USER_CACHE_DIR")
} else {
    Sys.setenv(R_USER_CACHE_DIR = prev_user_cache_dir)
}
