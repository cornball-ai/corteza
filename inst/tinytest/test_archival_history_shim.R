# archival_history_tool_calls_fallback should behave the same as
# llm.api::history_tool_calls. Tested separately so we know corteza
# users on llm.api 0.1.2.1 (no exported helper) get the same archival
# behavior as users on 0.1.3+.

fb <- corteza:::archival_history_tool_calls_fallback

# ---- Empty / null inputs ----
expect_equal(fb(list()), list())
expect_equal(fb(NULL), list())

# ---- Anthropic shape ----
anth <- list(
    list(role = "user", content = "find auth"),
    list(role = "assistant", content = list(
        list(type = "tool_use", id = "tu1", name = "grep_files",
             input = list(pattern = "auth"))
    )),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "tu1",
             content = "src/auth.R")
    ))
)
calls <- fb(anth)
expect_equal(length(calls), 1L)
expect_equal(calls[[1]]$id, "tu1")
expect_equal(calls[[1]]$name, "grep_files")
expect_equal(calls[[1]]$arguments$pattern, "auth")
expect_equal(calls[[1]]$result, "src/auth.R")
expect_true(calls[[1]]$completed)
expect_equal(calls[[1]]$provider_shape, "anthropic")

# ---- OpenAI shape ----
oai <- list(
    list(role = "user", content = "find auth"),
    list(role = "assistant", content = "",
         tool_calls = list(
             list(id = "c1", type = "function",
                  `function` = list(name = "grep_files",
                                    arguments = "{\"pattern\":\"auth\"}"))
         )),
    list(role = "tool", tool_call_id = "c1",
         name = "grep_files", content = "src/auth.R")
)
calls <- fb(oai)
expect_equal(length(calls), 1L)
expect_equal(calls[[1]]$id, "c1")
expect_equal(calls[[1]]$arguments$pattern, "auth")
expect_equal(calls[[1]]$result, "src/auth.R")
expect_true(calls[[1]]$completed)
expect_equal(calls[[1]]$provider_shape, "openai")

# ---- Unfinished call (assistant emitted, no matching result) ----
unfinished <- list(
    list(role = "user", content = "do thing"),
    list(role = "assistant", content = list(
        list(type = "tool_use", id = "tu_x", name = "bash",
             input = list(command = "ls"))
    ))
)
calls <- fb(unfinished)
expect_equal(length(calls), 1L)
expect_false(calls[[1]]$completed)
expect_null(calls[[1]]$result)

# ---- Delegate-vs-fallback agreement ----
# When llm.api::history_tool_calls is available (any session running
# this against the merged llm.api PR), the public shim should produce
# the same records as the local fallback. Prove the parity directly so
# corteza behavior doesn't drift between the two paths.
shim <- corteza:::archival_history_tool_calls
delegate_records <- shim(anth)
fallback_records <- fb(anth)
expect_equal(length(delegate_records), length(fallback_records))
expect_equal(delegate_records[[1]]$id, fallback_records[[1]]$id)
expect_equal(delegate_records[[1]]$name, fallback_records[[1]]$name)
expect_equal(delegate_records[[1]]$arguments,
             fallback_records[[1]]$arguments)
expect_equal(delegate_records[[1]]$completed,
             fallback_records[[1]]$completed)
