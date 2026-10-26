# archival_persist_subagent reuses transcript_append. Offline test:
# point R_user_dir at a tempdir, run persist, assert JSONL artifact.

local({
persist <- corteza:::archival_persist_subagent

tmp <- tempfile("corteza_archival_persist_")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
old_home <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
Sys.setenv(R_USER_DATA_DIR = tmp)
on.exit({
    if (is.na(old_home)) Sys.unsetenv("R_USER_DATA_DIR") else
        Sys.setenv(R_USER_DATA_DIR = old_home)
}, add = TRUE)

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
expect_true(startsWith(normalizePath(data_dir, winslash = "/", mustWork = FALSE),
                       paste0(normalizePath(tmp, winslash = "/"), "/")))
agent_dir <- file.path(data_dir, "agents", "subagent-abc123", "sessions")
expect_true(dir.exists(agent_dir))

jsonl <- file.path(agent_dir, "abc123.jsonl")
expect_true(file.exists(jsonl))

lines <- readLines(jsonl)
# Exactly one header, two body lines and one summary, even on repeated runs.
expect_equal(length(lines), 4L)

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
})
