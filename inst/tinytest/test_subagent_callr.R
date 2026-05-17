# Subagent transport: spawn / query / kill via callr::r_session.
# Gated at_home() purely for the per-test budget: the single
# subagent_spawn() call cold-starts an r_session and loads corteza
# inside it (~500 ms on Linux), and when ANTHROPIC_API_KEY is set the
# query/collect path adds multi-second LLM round-trips on top. That's
# too much for CRAN's per-test budget on busy CI machines, even though
# the test itself passes under R CMD check on Linux and on Windows
# (R 4.5.3, both CRAN callr 3.7.6 and the dev tree).
#
# Not the r-lib/callr#313 hang: that bug is in setup_callbacks(), which
# is rscript()/r()/rcmd() — not r_session, which uses tempfile redirects
# via rs__prehook/rs__posthook. Verified on Windows: r_session
# spawn/run/run(stop())/close all complete in under a second on both
# pre-fix and post-fix callr.

if (!tinytest::at_home()) {
    exit_file("Gated: spawning r_session + corteza load is slow per test")
}

# Clean registry up-front so prior tests don't leave residue.
for (id in ls(corteza:::.subagent_registry)) {
    try(corteza::subagent_kill(id), silent = TRUE)
}

# Spawn one.
id <- corteza::subagent_spawn(task = "test task",
                              config = list(subagents = list(enabled = TRUE)))
expect_true(is.character(id) && length(id) == 1L && nzchar(id))

# It shows up in the list.
active <- corteza::subagent_list()
expect_equal(length(active), 1L)
expect_equal(active[[1]]$id, id)
expect_equal(active[[1]]$task, "test task")

# Spawn creates a durable transcript file. The file is the on-disk
# record of the child's history — append-only, never rewritten, so
# later context compaction can't lose anything.
transcript_path <- corteza:::session_transcript_path(
    id, agent_id = paste0("subagent-", id))
expect_true(file.exists(transcript_path),
            info = "subagent transcript should exist after spawn")
header_line <- readLines(transcript_path, n = 1L)
expect_true(grepl('"type":"session"', header_line, fixed = TRUE),
            info = "transcript first line should be a session header")

# Query: runs through turn() inside the child, which needs a live
# LLM API key. Skip this check if no provider key is available —
# the spawn+registry+kill round-trip is what matters here.
if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    res <- corteza::subagent_query(id, "Reply with exactly the word 'pong'.")
    expect_true(is.character(res))
    expect_true(nzchar(res))

    # Async round-trip: fire, see pending state, collect.
    invisible(corteza::subagent_query(id, "Reply with exactly 'ping'.",
                                       wait = FALSE))
    info <- corteza:::.subagent_registry[[id]]
    expect_true(!is.null(info[["pending"]]))

    # Non-blocking collect can race the child to completion; if it
    # already returned, that consumed the pending result and we're
    # done. Otherwise block on the second collect.
    poll <- corteza::subagent_collect(id, wait = FALSE)
    expect_true(is.null(poll) || is.character(poll))
    res2 <- if (is.character(poll)) {
        poll
    } else {
        corteza::subagent_collect(id, wait = TRUE, timeout = 60)
    }
    expect_true(is.character(res2))
    expect_true(nzchar(res2))
    info <- corteza:::.subagent_registry[[id]]
    expect_null(info[["pending"]])

    # After two queries the transcript must contain both turns. We
    # don't pin exact content (provider replies vary) but the file
    # should have grown beyond just the header.
    lines <- readLines(transcript_path)
    expect_true(length(lines) >= 3L,
                info = "transcript should hold header + at least one turn")
}

# Kill cleans up registry + closes the session.
expect_true(corteza::subagent_kill(id))
expect_equal(length(corteza::subagent_list()), 0L)

# Killing an unknown id is a no-op, not an error.
expect_false(corteza::subagent_kill("does-not-exist"))

# Query on unknown id raises.
err <- tryCatch(corteza::subagent_query("missing", "1"),
                error = function(e) e)
expect_inherits(err, "error")
