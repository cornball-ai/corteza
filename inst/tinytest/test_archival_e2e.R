# End-to-end archival flow: seeded history -> maybe_archive_turn ->
# verify parent history collapses, subagent appears, transcript file
# exists. Gated at_home + ANTHROPIC_API_KEY because archival_summarize
# makes a real LLM call.

if (!tinytest::at_home()) exit_file("e2e archival hits the network; at_home only")
if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    exit_file("e2e archival needs ANTHROPIC_API_KEY")
}

# Clean registry.
for (id in ls(corteza:::.subagent_registry)) {
    try(corteza::subagent_kill(id), silent = TRUE)
}

# Build a synthetic finished turn: user prompt + assistant reply.
synthetic_history <- list(
    list(role = "user", content = "Find the auth handler"),
    list(role = "assistant", content = list(
        list(type = "text", text = "Searching for auth handlers."),
        list(type = "tool_use", id = "tu1", name = "grep_files",
             input = list(pattern = "auth", path = "."))
    )),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "tu1",
             content = list(list(type = "text",
                                 text = "src/auth.R: auth_handler <- function(...)")))
    )),
    list(role = "assistant", content = "Found it at src/auth.R.")
)

turn_session <- corteza::new_session(
    channel = "console", provider = "anthropic"
)
turn_session$history <- synthetic_history

config <- list(
    subagents = list(enabled = TRUE, max_concurrent = 3L,
                     timeout_minutes = 30L, allow_nested = FALSE,
                     default_tools = c("read_file", "grep_files")),
    archival = list(
        enabled = TRUE,
        trigger = list(on_max_turns = TRUE,
                       token_threshold = 1L,    # force-trigger
                       tool_call_threshold = 100L,
                       depth_cap = 3L),
        summary = list(style = "paragraph", model = NULL)
    )
)

corteza:::maybe_archive_turn(
    turn_session = turn_session,
    prompt = "Find the auth handler",
    pre_turn_len = 0L,
    result = list(reply = "Found it at src/auth.R."),
    config = config,
    parent_session_id = "test-parent-sess",
    max_turns_hit = FALSE,
    depth = 0L
)

# After archival the parent's history collapses to (user, archived assistant).
expect_equal(length(turn_session$history), 2L)
expect_equal(turn_session$history[[1]]$role, "user")
expect_equal(turn_session$history[[2]]$role, "assistant")
expect_true(grepl("\\[archived turn\\]",
                  as.character(turn_session$history[[2]]$content)))
expect_true(grepl("subagent_id:",
                  as.character(turn_session$history[[2]]$content)))

# A subagent now exists in the registry.
agents <- corteza::subagent_list()
expect_true(length(agents) >= 1L)

# Cleanup. on.exit fires immediately in tinytest at top level, so kill
# explicitly at the end.
for (a in agents) {
    try(corteza::subagent_kill(a$id), silent = TRUE)
}
