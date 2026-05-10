# archival_persist_subagent reuses transcript_append. Offline test:
# point R_user_dir at a tempdir, run persist, assert JSONL artifact.

persist <- corteza:::archival_persist_subagent

tmp <- tempfile("corteza_archival_persist_")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
old_home <- Sys.getenv("R_USER_DATA_DIR")
Sys.setenv(R_USER_DATA_DIR = tmp)
on.exit(Sys.setenv(R_USER_DATA_DIR = old_home), add = TRUE)

slice <- list(
    list(role = "user", content = "find foo"),
    list(role = "assistant", content = "found it at src/foo.R")
)

persist(subagent_id = "abc123", history_slice = slice,
        summary = '{"outcome":"located file"}',
        parent_session_id = "parent-sess",
        provider = "anthropic", model = "claude-sonnet-4-6")

# Path: <data>/agents/subagent-abc123/sessions/abc123.jsonl
data_dir <- tools::R_user_dir("corteza", "data")
agent_dir <- file.path(data_dir, "agents", "subagent-abc123", "sessions")
expect_true(dir.exists(agent_dir))

jsonl <- file.path(agent_dir, "abc123.jsonl")
expect_true(file.exists(jsonl))

lines <- readLines(jsonl)
# Header + 2 body lines + 1 summary line = 4 lines minimum.
expect_true(length(lines) >= 4L)

parsed <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)

# First line is the session header.
expect_equal(parsed[[1]]$type, "session")
expect_equal(parsed[[1]]$id, "abc123")

# Body lines preserve roles.
roles <- vapply(parsed[-1], function(p) p$role %||% "", character(1))
expect_true("user" %in% roles)
expect_true("assistant" %in% roles)

# Last assistant line carries the [archival summary] marker.
last <- parsed[[length(parsed)]]
expect_equal(last$role, "assistant")
last_text <- last$content[[1]]$text
expect_true(grepl("archival summary", last_text))
expect_true(grepl("located file", last_text))
