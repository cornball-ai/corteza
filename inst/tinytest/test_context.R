# Test context loading
#
# Source discovery, deduplication, and rendering belong to saber. Here we test
# corteza's manifest descriptors, rendered prompt, session-owned sources, and
# the compatibility helper that lists configured files.

# Setup: use a fresh temp directory
tmpdir <- tempdir()
testdir <- file.path(tmpdir, paste0("ctx_test_", Sys.getpid()))
if (dir.exists(testdir)) unlink(testdir, recursive = TRUE)
dir.create(testdir, recursive = TRUE)

# Scope saber's cache to tempdir for the duration of this file.
# load_context() calls saber::briefing() / saber::context_manifest(), which
# use tools::R_user_dir("saber", "cache"). Without this redirect,
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
expect_true(grepl("My Project", ctx))
expect_true(grepl("You are an AI assistant", ctx))

# --- Runtime guidance block is present ---
expect_true(grepl("Corteza Runtime Environment", ctx))
expect_true(grepl("conversation history and R workspace as persistent scratch state",
                  ctx, fixed = TRUE))
expect_true(grepl("persistent R session", ctx))
expect_true(grepl("bash tool makes you a general-purpose agent", ctx))

# --- Multiple custom files ---
writeLines(c("# Plan", "", "Phase 1: Core"), file.path(testdir, "PLAN.md"))
writeLines('{"context_files": ["README.md", "PLAN.md"]}',
           file.path(testdir, ".corteza", "config.json"))

files <- corteza:::list_context_files(testdir)
expect_equal(length(files), 2)

ctx <- corteza:::load_context(testdir)
expect_true(grepl("Phase 1: Core", ctx))
# --- Bundle retains source provenance and renders no eager package help ---
writeLines(c("# Agent Instructions", "", "agent-context-sentinel"),
           file.path(testdir, "AGENTS.md"))
bundle <- corteza:::load_context_bundle(testdir)
expect_true(inherits(bundle$manifest, "saber_context_manifest"))
expect_identical(bundle$system, saber::context_render(bundle$manifest))

sources <- bundle$manifest$sources
expect_true(all(c("corteza_preamble", "corteza_runtime", "project_agents",
                  "configured_context_001", "configured_context_002") %in%
                sources$id))
configured <- sources[sources$id %in%
                      c("configured_context_001", "configured_context_002"), ]
expect_true(all(configured$included))
expect_true(all(configured$emitted_tokens > 0))
expect_true(grepl("agent-context-sentinel", bundle$system, fixed = TRUE))
expect_false(grepl("# Package Tools", bundle$system, fixed = TRUE))
expect_false("package_docs" %in% sources$id)

# The new shared default is saber's compact GLOBAL.md. Corteza no longer
# guesses a ~/.claude/CLAUDE.md file or basename-matches Claude memories.
source_paths <- c(sources$requested_path, sources$path)
expect_false(any(grepl("/.claude/CLAUDE.md", source_paths, fixed = TRUE)))
expect_false(any(grepl("/memory/MEMORY.md", source_paths, fixed = TRUE)))

# --- Missing custom files are silently skipped ---
writeLines('{"context_files": ["README.md", "DOES_NOT_EXIST.md"]}',
           file.path(testdir, ".corteza", "config.json"))

files <- corteza:::list_context_files(testdir)
expect_equal(length(files), 1)

ctx <- corteza:::load_context(testdir)
expect_true(grepl("My Project", ctx))
expect_false(grepl("DOES_NOT_EXIST", ctx))
bundle <- corteza:::load_context_bundle(testdir)
missing <- bundle$manifest$sources[
    bundle$manifest$sources$id == "configured_context_002", ]
expect_equal(nrow(missing), 1)
expect_false(missing$included)
expect_identical(missing$reason, "missing")


# Cleanup
unlink(testdir, recursive = TRUE)
unlink(Sys.getenv("R_USER_CACHE_DIR"), recursive = TRUE)
if (is.na(prev_user_cache_dir)) {
    Sys.unsetenv("R_USER_CACHE_DIR")
} else {
    Sys.setenv(R_USER_CACHE_DIR = prev_user_cache_dir)
}
