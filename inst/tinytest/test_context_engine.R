# Tests for context engine

# Use a temp project directory for all tests
tmp_proj <- tempfile("proj_")
dir.create(tmp_proj)
dir.create(file.path(tmp_proj, "R"))

writeLines(c("# Test file", "foo <- function(x) x + 1",
             "bar <- function(y) foo(y)"), file.path(tmp_proj, "R", "test.R"))
writeLines(c("# README", "A test project"), file.path(tmp_proj, "README.md"))

# ce_init ----

expect_silent(llamaR:::ce_init(tmp_proj))

# File index ----

llamaR:::ce_init(tmp_proj)
stats <- llamaR:::ce_file_stats()
expect_true(stats[["files"]] >= 2)  # test.R + README.md
expect_true(stats[["lines"]] > 0)

# Should find R files
index <- llamaR:::.context_engine$file_index
r_files <- grep("\\.R$", names(index), value = TRUE)
expect_true(length(r_files) > 0)

# ce_file retrieves content
first_file <- names(index)[1]
lines <- llamaR:::ce_file(first_file)
expect_true(length(lines) > 0)
expect_true(is.character(lines))

# ce_file returns NULL for non-indexed file
expect_null(llamaR:::ce_file("nonexistent_file_xyz.R"))
expect_null(llamaR:::ce_file(""))
expect_null(llamaR:::ce_file(NULL))

# In-memory grep ----

results <- llamaR:::ce_grep("foo")
expect_true(is.data.frame(results))
expect_true(nrow(results) > 0)
expect_true("file" %in% names(results))
expect_true("line_number" %in% names(results))
expect_true("text" %in% names(results))

# Grep with file filter
r_results <- llamaR:::ce_grep("foo", file_glob = "*.R")
expect_true(nrow(r_results) > 0)

# Grep with no matches
empty <- llamaR:::ce_grep("zzz_nonexistent_symbol_xyz")
expect_equal(nrow(empty), 0)

# Conversation indexing ----

llamaR:::ce_init(tmp_proj)

llamaR:::ce_index_turn(1L, "user", "What is the workspace?")
conv <- llamaR:::ce_conversation()
expect_equal(nrow(conv), 1)
expect_equal(conv$role[1], "user")
expect_true(conv$tokens[1] > 0)

llamaR:::ce_index_turn(2L, "assistant", "The workspace stores R objects.")
conv <- llamaR:::ce_conversation()
expect_equal(nrow(conv), 2)

# Conversation search
hits <- llamaR:::ce_search_conversation("workspace")
expect_true(nrow(hits) >= 1)

no_hits <- llamaR:::ce_search_conversation("zzz_no_match_xyz")
expect_equal(nrow(no_hits), 0)

# Token counting
tokens <- llamaR:::ce_conversation_tokens()
expect_true(tokens > 0)

# Payload assembly ----

llamaR:::ce_init(tmp_proj)
llamaR:::ce_index_turn(1L, "user", "Tell me about this project")

payload <- llamaR:::ce_compute_payload(
    prompt = "Tell me about this project",
    system_base = "You are an AI assistant.",
    tools_json = ""
)
expect_true(is.list(payload))
expect_true("system" %in% names(payload))
expect_true("tokens_used" %in% names(payload))
expect_true(nchar(payload$system) > 0)
expect_true(payload$tokens_used > 0)

# File update tracking ----

llamaR:::ce_init(tmp_proj)

# Create a new file in the existing temp project
tmp_file <- file.path(tmp_proj, "R", "new_file.R")
writeLines(c("# new file", "baz <- 1"), tmp_file)
llamaR:::ce_update_files(tmp_file)

# Find the relative path in index
rel <- sub(paste0("^", normalizePath(tmp_proj, mustWork = FALSE), "/?"),
           "", normalizePath(tmp_file, mustWork = FALSE))
lines <- llamaR:::ce_file(rel)
expect_equal(length(lines), 2)

# Modify and re-index
writeLines(c("# new file", "baz <- 1", "qux <- 2"), tmp_file)
llamaR:::ce_update_files(tmp_file)
lines <- llamaR:::ce_file(rel)
expect_true(length(lines) == 3)

# Delete and re-index
unlink(tmp_file)
llamaR:::ce_update_files(tmp_file)
expect_null(llamaR:::ce_file(rel))

# Clean up temp project
unlink(tmp_proj, recursive = TRUE)

# Extract helpers ----

# ce_extract_tool_calls with NULL
expect_equal(length(llamaR:::ce_extract_tool_calls(NULL)), 0)

# ce_extract_files_touched with NULL
expect_equal(length(llamaR:::ce_extract_files_touched(NULL)), 0)

# Shutdown ----

expect_silent(llamaR:::ce_shutdown())
