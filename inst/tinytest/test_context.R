# Test context loading

# Setup: use a fresh temp directory
tmpdir <- tempdir()
testdir <- file.path(tmpdir, paste0("ctx_test_", Sys.getpid()))
if (dir.exists(testdir)) unlink(testdir, recursive = TRUE)
dir.create(testdir, recursive = TRUE)

# Test list_context_files with no files
files <- llamaR:::list_context_files(testdir)
expect_equal(length(files), 0)

# Test load_context with no project files
# Note: May still contain global files from ~/.llamar/workspace/ or
# package skill docs (from basalt) if available
ctx <- llamaR:::load_context(testdir)
workspace_dir <- llamaR:::get_workspace_dir()
has_global_files <- any(file.exists(file.path(workspace_dir, llamaR:::global_context_files())))
has_basalt <- requireNamespace("basalt", quietly = TRUE)
if (has_global_files || has_basalt) {
    # Global files or package docs exist, so context won't be NULL
    expect_true(is.character(ctx))
} else {
    expect_null(ctx)
}

# Create README.md (now in defaults)
writeLines(c("# My Project", "", "This is the readme."), file.path(testdir, "README.md"))

# Test list_context_files finds README.md
files <- llamaR:::list_context_files(testdir)
expect_equal(length(files), 1)
expect_true(grepl("README.md", files[1]))

# Test load_context includes README.md content
ctx <- llamaR:::load_context(testdir)
expect_true(grepl("README.md", ctx))
expect_true(grepl("My Project", ctx))

# Create PLAN.md
writeLines(c("# Development Plan", "", "Phase 1: Core"), file.path(testdir, "PLAN.md"))

# Test both files are found
files <- llamaR:::list_context_files(testdir)
expect_equal(length(files), 2)

# Create fyi.md
writeLines(c("# fyi: mypackage", "", "This is package info."), file.path(testdir, "fyi.md"))

files <- llamaR:::list_context_files(testdir)
expect_equal(length(files), 3)

# Create AGENTS.md
writeLines("# Agent Guidelines", file.path(testdir, "AGENTS.md"))

files <- llamaR:::list_context_files(testdir)
expect_equal(length(files), 4)

# Test all are included in context
ctx <- llamaR:::load_context(testdir)
expect_true(grepl("README.md", ctx))
expect_true(grepl("PLAN.md", ctx))
expect_true(grepl("fyi.md", ctx))
expect_true(grepl("AGENTS.md", ctx))
expect_true(grepl("Agent Guidelines", ctx))

# Test system prompt structure
expect_true(grepl("You are an AI assistant", ctx))
expect_true(grepl("context about the current project", ctx))

# Test custom config overrides default file list
dir.create(file.path(testdir, ".llamar"), showWarnings = FALSE)
writeLines('{"context_files": ["README.md"]}', file.path(testdir, ".llamar", "config.json"))

files <- llamaR:::list_context_files(testdir)
expect_equal(length(files), 1)
expect_true(grepl("README.md", files[1]))

# Cleanup
unlink(testdir, recursive = TRUE)

