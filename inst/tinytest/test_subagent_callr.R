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

# Query: evaluates the prompt as R code in the child (current
# subagent_query stub; full agent-loop semantics TODO).
res <- corteza::subagent_query(id, "1 + 1")
expect_true(is.character(res))
expect_true(grepl("2", res))

# Kill cleans up registry + closes the session.
expect_true(corteza::subagent_kill(id))
expect_equal(length(corteza::subagent_list()), 0L)

# Killing an unknown id is a no-op, not an error.
expect_false(corteza::subagent_kill("does-not-exist"))

# Query on unknown id raises.
err <- tryCatch(corteza::subagent_query("missing", "1"),
                error = function(e) e)
expect_inherits(err, "error")
