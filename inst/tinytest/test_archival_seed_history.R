# subagent_seed_history round-trips a history slice into a child's
# .subagent_state. No LLM call. Gated at_home() because spawning a
# callr session adds ~250ms.

if (!tinytest::at_home()) exit_file("subagent seed test is slow; at_home only")

# Clean registry up-front.
for (id in ls(corteza:::.subagent_registry)) {
    try(corteza::subagent_kill(id), silent = TRUE)
}

# Spawn a holder with no tools (matches archival flow).
id <- corteza::subagent_spawn(
    task = "seed-history-test",
    tools = character(0),
    config = list(subagents = list(enabled = TRUE))
)

slice <- list(
    list(role = "user", content = "compute 2+2"),
    list(role = "assistant", content = "4")
)

info <- corteza:::.subagent_registry[[id]]
expect_false(is.null(info))

# Seed the history into the child.
info$session$run(
    function(h) corteza::subagent_seed_history(h),
    list(h = slice)
)

# Reflect the child's history length back so we can assert it.
n <- info$session$run(function() {
    length(corteza:::.subagent_state$session$history %||% list())
})
expect_equal(n, 2L)

# Reflect the first message role to confirm structure survived.
first_role <- info$session$run(function() {
    corteza:::.subagent_state$session$history[[1]]$role
})
expect_equal(first_role, "user")

# Confirm depth was set during init (depth = 1 for direct children of
# parent at depth 0).
child_depth <- info$session$run(function() {
    corteza:::.subagent_state$depth
})
expect_equal(child_depth, 1L)

# Confirm the child knows its own subagent id (set after init).
child_id <- info$session$run(function() {
    corteza:::.subagent_state$subagent_id
})
expect_equal(child_id, id)

# Cleanup. on.exit fires immediately at tinytest's top level, so do it
# explicitly at the end instead.
try(corteza::subagent_kill(id), silent = TRUE)
