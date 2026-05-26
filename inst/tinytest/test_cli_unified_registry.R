# Milestone 2 of the CLI/chat unification: tools run IN-PROCESS, so a
# subagent spawned via the slash-command path (subagent_spawn) and one
# spawned via the model tool path (tool_spawn_subagent) must land in
# the SAME .subagent_registry within one R process. The pre-unification
# CLI ran tools in a callr worker, so the registry the model saw lived
# in the worker while the slash commands' registry lived in the CLI
# process -- two registries, /agents couldn't see model-spawned agents.
#
# This proves both paths share one registry. No LLM/network: we spawn
# and inspect the registry, but never query the children (querying is
# what needs an API key).
#
# Gated at_home() for the per-test budget: each subagent_spawn()
# cold-starts an r_session and loads corteza inside it.

if (!tinytest::at_home()) {
    exit_file("Gated: spawning r_session + corteza load is slow per test")
}

reg <- corteza:::.subagent_registry

# Clean registry up-front so prior tests don't leave residue.
for (id in ls(reg)) {
    try(corteza::subagent_kill(id), silent = TRUE)
}
expect_equal(length(corteza::subagent_list()), 0L)

cfg <- list(subagents = list(enabled = TRUE))

# Path A: slash-command style (what /spawn calls).
id_slash <- corteza::subagent_spawn(task = "slash-spawned task",
                                    preset = "minimal", config = cfg)
expect_true(is.character(id_slash) && nzchar(id_slash))

# Path B: model tool path (what the LLM calls via the spawn_subagent
# tool). tool_spawn_subagent wraps subagent_spawn and returns an MCP
# result; ctx=list() means no parent session (top-level spawn).
res <- corteza:::tool_spawn_subagent(task = "model-spawned task",
                                     preset = "minimal", ctx = list())
expect_false(isTRUE(res$isError))
# The MCP result text embeds the new id: "Spawned subagent <id> for: ..."
id_model <- sub("^Spawned subagent ([^ ]+) for:.*$", "\\1",
                res$content[[1]]$text)
expect_true(is.character(id_model) && nzchar(id_model))
expect_true(!identical(id_slash, id_model))

# Both ids live in the ONE registry.
registry_ids <- ls(reg)
expect_true(id_slash %in% registry_ids)
expect_true(id_model %in% registry_ids)

# Both surface through subagent_list() (the /agents data source).
listed_ids <- vapply(corteza::subagent_list(), function(a) a$id, character(1))
expect_true(id_slash %in% listed_ids)
expect_true(id_model %in% listed_ids)
expect_equal(length(listed_ids), 2L)

# Cleanup: kill both, registry returns to empty.
expect_true(corteza::subagent_kill(id_slash))
expect_true(corteza::subagent_kill(id_model))
expect_equal(length(corteza::subagent_list()), 0L)
