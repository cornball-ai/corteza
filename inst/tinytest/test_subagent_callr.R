# Subagent transport: spawn / query / kill via callr::r_session.
# Gated at_home() because spawning a real r_session + library(corteza)
# inside it adds ~250 ms per test run, which is too much for R CMD
# check's per-test budget on busy CI machines.

if (!tinytest::at_home()) exit_file("subagent callr tests are slow; at_home only")

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
