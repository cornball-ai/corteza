# Test session management
# Storage format (openclaw compatible):
#   ~/.openclaw/agents/{agent_id}/sessions/sessions.json  - Session metadata
#   ~/.openclaw/agents/{agent_id}/sessions/{id}.jsonl     - Transcript per session

# Use a unique test agent_id to avoid conflicts with real sessions
test_agent_id <- paste0("test_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))

on.exit({
    # Cleanup test sessions directory
    test_dir <- corteza:::sessions_dir(test_agent_id)
    if (dir.exists(test_dir)) {
        unlink(test_dir, recursive = TRUE)
    }
    # Also clean up parent agent directory if empty
    agent_dir <- dirname(test_dir)
    if (dir.exists(agent_dir) && length(list.files(agent_dir)) == 0) {
        unlink(agent_dir, recursive = TRUE)
    }
}, add = TRUE)

# Test session_id generates UUID format
id <- corteza:::session_id()
expect_true(grepl("^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$", id))

# Test session_new creates proper structure
cwd <- getwd()
session <- corteza:::session_new("ollama", "llama3.2", cwd, agent_id = test_agent_id)
expect_equal(session$provider, "ollama")
expect_equal(session$model, "llama3.2")
expect_equal(length(session$messages), 0)
expect_true(nchar(session$sessionId) > 0)
expect_true(grepl("^corteza:", session$sessionKey))
expect_true(is.numeric(session$createdAt))

# Test session_new saves to store
store_path <- corteza:::sessions_store_path(test_agent_id)
expect_true(file.exists(store_path))

# Test sessions directory was created
expect_true(dir.exists(corteza:::sessions_dir(test_agent_id)))

# Test session_save updates store
session$inputTokens <- 100L
corteza:::session_save(session, test_agent_id)
store <- jsonlite::fromJSON(store_path, simplifyVector = FALSE)
expect_equal(store[[session$sessionKey]]$inputTokens, 100L)

# Test session_load retrieves session
loaded <- corteza:::session_load(session$sessionKey, test_agent_id)
expect_equal(loaded$sessionId, session$sessionId)
expect_equal(loaded$provider, session$provider)
expect_equal(loaded$model, session$model)
expect_equal(loaded$inputTokens, 100L)

# Test session_load returns NULL for missing session
missing <- corteza:::session_load("nonexistent-key", test_agent_id)
expect_null(missing)

# Test session_add_message (in-memory) - stores in content block format
session <- corteza:::session_add_message(session, "user", "Hello")
expect_equal(length(session$messages), 1)
expect_equal(session$messages[[1]]$role, "user")
# Content is now a list of content blocks
expect_equal(session$messages[[1]]$content[[1]]$type, "text")
expect_equal(session$messages[[1]]$content[[1]]$text, "Hello")

session <- corteza:::session_add_message(session, "assistant", "Hi there")
expect_equal(length(session$messages), 2)
expect_equal(session$messages[[2]]$role, "assistant")

# Test transcript_append
corteza:::transcript_append(session, "user", "Hello", agent_id = test_agent_id)
corteza:::transcript_append(session, "assistant", "Hi there", agent_id = test_agent_id)

# Check transcript file exists
transcript_path <- corteza:::session_transcript_path(session$sessionId, test_agent_id)
expect_true(file.exists(transcript_path))

# Test transcript has header line
lines <- readLines(transcript_path)
header <- jsonlite::fromJSON(lines[1], simplifyVector = FALSE)
expect_equal(header$type, "session")
expect_equal(header$version, 2L)
expect_equal(header$id, session$sessionId)

# Test transcript_count
count <- corteza:::transcript_count(session$sessionId, test_agent_id)
expect_equal(count, 2L)

# Test transcript_load
messages <- corteza:::transcript_load(session$sessionId, test_agent_id)
expect_equal(length(messages), 2)
expect_equal(messages[[1]]$role, "user")
expect_equal(messages[[2]]$role, "assistant")
# Content is in block format
expect_equal(messages[[1]]$content[[1]]$text, "Hello")
expect_equal(messages[[2]]$content[[1]]$text, "Hi there")

# Test session_list returns sessions
sessions <- corteza:::session_list(test_agent_id)
expect_equal(length(sessions), 1)
expect_equal(sessions[[1]]$sessionKey, session$sessionKey)
expect_equal(sessions[[1]]$messages, 2L)

# Test session_latest returns most recent
latest <- corteza:::session_latest(test_agent_id)
expect_equal(latest$sessionId, session$sessionId)

# Test session_list with different agent_id (empty)
empty_agent <- paste0("empty_", sample(10000:99999, 1))
empty_sessions <- corteza:::session_list(empty_agent)
expect_equal(length(empty_sessions), 0)

# Test session_latest with no sessions
no_latest <- corteza:::session_latest(empty_agent)
expect_null(no_latest)

# Test format_session_list
formatted <- corteza:::format_session_list(sessions)
expect_true(grepl("Sessions:", formatted))
expect_true(grepl(session$sessionKey, formatted))
expect_true(grepl("2 msgs", formatted))

# Test format_session_list with empty
empty_formatted <- corteza:::format_session_list(list())
expect_true(grepl("No sessions found", empty_formatted))

# Test multiple sessions are sorted by time
Sys.sleep(0.1) # Ensure different mtime
session2 <- corteza:::session_new("anthropic", "claude-3", cwd, agent_id = test_agent_id)
corteza:::transcript_append(session2, "user", "Test 2", agent_id = test_agent_id)
corteza:::session_save(session2, test_agent_id)

sessions <- corteza:::session_list(test_agent_id)
expect_equal(length(sessions), 2)
# Most recent should be first
expect_equal(sessions[[1]]$sessionKey, session2$sessionKey)
expect_equal(sessions[[2]]$sessionKey, session$sessionKey)

# Test compaction marker
session3 <- corteza:::session_new("ollama", "test", cwd, agent_id = test_agent_id)
corteza:::transcript_append(session3, "user", "msg1", agent_id = test_agent_id)
corteza:::transcript_append(session3, "assistant", "resp1", agent_id = test_agent_id)
corteza:::transcript_append(session3, "user", "msg2", agent_id = test_agent_id)
corteza:::transcript_append(session3, "assistant", "resp2", agent_id = test_agent_id)

# Add compaction marker
corteza:::transcript_compact(session3, "Summary of conversation", agent_id = test_agent_id)

# Add post-compaction messages
corteza:::transcript_append(session3, "user", "msg3", agent_id = test_agent_id)
corteza:::transcript_append(session3, "assistant", "resp3", agent_id = test_agent_id)

# Load messages (compaction not yet filtering, so get all)
all_messages <- corteza:::transcript_load(session3$sessionId, test_agent_id, from_compaction = FALSE)
expect_equal(length(all_messages), 7)  # 4 original + compaction + 2 new
