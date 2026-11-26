# Tests for the interrupt / denial repair helpers in R/interrupt.R.
#
# Covers:
# - repair_interrupted_tool_history scoping to current-turn slice
# - Anthropic partial-batch extension via compact_entry_is_tool_result_only
# - OpenAI/Moonshot/Ollama one-message-per-result repair
# - No-callback prompt injection when length(history) == pre_turn_len
# - Stale dangling tool_use from prior turn stays unrepaired (correct)
# - apply_exit_marker mutates the session env in place

library(tinytest)

# --- Anthropic: dangling tool_use gets a synthetic tool_result, then marker
local({
    history <- list(
                    list(role = "user", content = "prompt"),
                    list(role = "assistant", content = list(
                                                          list(type = "text", text = "running"),
                                                          list(type = "tool_use", id = "tu_1",
                                                               name = "echo", input = list(x = 1L))
        ))
    )
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history       = history,
                                                          provider      = "anthropic",
                                                          marker        = "[Interrupted]",
                                                          prompt        = "prompt",
                                                          pre_turn_len  = 0L
    )
    # 1: user prompt, 2: assistant with tool_use, 3: synthesized
    # tool_result (new user message), 4: interrupt marker (assistant).
    expect_equal(length(repaired), 4L)
    expect_equal(repaired[[3L]]$role, "user")
    expect_equal(repaired[[3L]]$content[[1L]]$type, "tool_result")
    expect_equal(repaired[[3L]]$content[[1L]]$tool_use_id, "tu_1")
    expect_equal(repaired[[3L]]$content[[1L]]$content,
                 "[Interrupted before completion]")
    expect_equal(repaired[[4L]]$role, "assistant")
    expect_equal(repaired[[4L]]$content, "[Interrupted]")
})

# --- Anthropic: partial tool_result batch gets extended in place ----
local({
    # Assistant emitted two tool_use blocks; the first completed (its
    # result is in a tool_result user message already), the second
    # was interrupted before completion.
    history <- list(
                    list(role = "user", content = "prompt"),
                    list(role = "assistant", content = list(
                                                          list(type = "tool_use", id = "tu_1",
                                                               name = "echo", input = list(x = 1L)),
                                                          list(type = "tool_use", id = "tu_2",
                                                               name = "echo", input = list(x = 2L))
        )),
                    list(role = "user", content = list(
                                                     list(type = "tool_result", tool_use_id = "tu_1",
                                                          content = "ok")
        ))
    )
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history      = history,
                                                          provider     = "anthropic",
                                                          marker       = "[Interrupted]",
                                                          prompt       = "prompt",
                                                          pre_turn_len = 0L
    )
    # Length unchanged for the partial-batch message (entry 3); the
    # missing tool_result gets appended to its content list. Then the
    # assistant marker arrives as entry 4.
    expect_equal(length(repaired), 4L)
    expect_equal(repaired[[3L]]$role, "user")
    expect_equal(length(repaired[[3L]]$content), 2L)
    expect_equal(repaired[[3L]]$content[[1L]]$tool_use_id, "tu_1")
    expect_equal(repaired[[3L]]$content[[2L]]$tool_use_id, "tu_2")
    expect_equal(repaired[[4L]]$role, "assistant")
})

# --- Anthropic: a regular text user message at the tail is NOT
# mistaken for a partial-batch user message --------------------------
local({
    # tool_use is dangling but the tail is a plain text user message
    # (could happen if a prior turn ended cleanly and the current
    # turn already wrote its prompt into history before the callback
    # mirrored anything else). The repair should NOT try to splice
    # tool_result blocks into a text message; it must start a new one.
    history <- list(
                    list(role = "assistant", content = list(
                                                          list(type = "tool_use", id = "tu_1",
                                                               name = "echo", input = list(x = 1L))
        )),
                    list(role = "user", content = "follow-up text")
    )
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history      = history,
                                                          provider     = "anthropic",
                                                          marker       = "[Interrupted]",
                                                          prompt       = "follow-up text",
                                                          pre_turn_len = 0L
    )
    # 1: assistant tool_use, 2: text user msg (untouched), 3: NEW
    # user msg with tool_result, 4: marker.
    expect_equal(length(repaired), 4L)
    expect_equal(repaired[[2L]]$content, "follow-up text")
    expect_equal(repaired[[3L]]$role, "user")
    expect_equal(repaired[[3L]]$content[[1L]]$type, "tool_result")
    expect_equal(repaired[[4L]]$role, "assistant")
})

# --- OpenAI / Moonshot / Ollama: one role="tool" per missing result -
local({
    history <- list(
                    list(role = "user", content = "prompt"),
                    list(role = "assistant", content = "",
                         tool_calls = list(
                                           list(id = "call_1", type = "function",
                                                `function` = list(name = "echo",
                                                                  arguments = "{\"x\":1}")),
                                           list(id = "call_2", type = "function",
                                                `function` = list(name = "echo",
                                                                  arguments = "{\"x\":2}"))
        ))
    )
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history      = history,
                                                          provider     = "openai",
                                                          marker       = "[Interrupted]",
                                                          prompt       = "prompt",
                                                          pre_turn_len = 0L
    )
    # 1: user, 2: assistant w/ tool_calls, 3: tool result for call_1,
    # 4: tool result for call_2, 5: assistant marker.
    expect_equal(length(repaired), 5L)
    expect_equal(repaired[[3L]]$role, "tool")
    expect_equal(repaired[[3L]]$tool_call_id, "call_1")
    expect_equal(repaired[[4L]]$role, "tool")
    expect_equal(repaired[[4L]]$tool_call_id, "call_2")
    expect_equal(repaired[[5L]]$role, "assistant")
})

# --- No-callback prompt injection -----------------------------------
local({
    # Pre-turn history has one prior exchange. The turn was
    # interrupted before any history_callback fired, so the prompt
    # never escaped llm.api's internal messages list. length(history)
    # equals pre_turn_len. The helper injects the prompt and then
    # the marker.
    history <- list(
                    list(role = "user", content = "previous prompt"),
                    list(role = "assistant", content = "previous reply")
    )
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history      = history,
                                                          provider     = "anthropic",
                                                          marker       = "[Interrupted]",
                                                          prompt       = "current prompt",
                                                          pre_turn_len = 2L
    )
    # No tool_use to repair; just prompt injection + marker.
    expect_equal(length(repaired), 4L)
    expect_equal(repaired[[3L]]$role, "user")
    expect_equal(repaired[[3L]]$content, "current prompt")
    expect_equal(repaired[[4L]]$role, "assistant")
    expect_equal(repaired[[4L]]$content, "[Interrupted]")
})

# --- Stale dangling tool_use from a prior turn stays untouched ------
local({
    # A prior turn's interrupt was never repaired (e.g. session
    # predates the fix). The current turn started AFTER that bad
    # state. Repair scope is current-turn-only, so the helper leaves
    # the stale dangling tool_use alone -- fixing it would put the
    # synthetic result at the tail, in the wrong position relative
    # to its issuing assistant message.
    history <- list(
                    list(role = "user", content = "old prompt"),
                    list(role = "assistant", content = list(
                                                          list(type = "tool_use", id = "tu_stale",
                                                               name = "echo", input = list())
        )),
                    list(role = "user", content = "current prompt"),
                    list(role = "assistant", content = list(
                                                          list(type = "tool_use", id = "tu_current",
                                                               name = "echo", input = list())
        ))
    )
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history      = history,
                                                          provider     = "anthropic",
                                                          marker       = "[Interrupted]",
                                                          prompt       = "current prompt",
                                                          pre_turn_len = 2L
    )
    # The current-turn tu_current gets a synthetic result (entry 5),
    # then the marker (entry 6). tu_stale at index 2 is left alone.
    expect_equal(length(repaired), 6L)
    # Repair only added one tool_result block, not two.
    expect_equal(length(repaired[[5L]]$content), 1L)
    expect_equal(repaired[[5L]]$content[[1L]]$tool_use_id, "tu_current")
    expect_equal(repaired[[6L]]$role, "assistant")
})

# --- apply_exit_marker mutates the session env in place -------------
local({
    session <- new.env(parent = emptyenv())
    session$provider <- "anthropic"
    session$history <- list(
                            list(role = "user", content = "prompt"),
                            list(role = "assistant", content = list(
                                                                  list(type = "tool_use", id = "tu_1",
                                                                       name = "echo", input = list())
        ))
    )
    out <- corteza:::apply_exit_marker(session, prompt = "prompt",
                                       pre_turn_len = 0L,
                                       marker = "[Interrupted]")
    expect_identical(out, session)
    # Mutation visible through the original env reference.
    expect_equal(length(session$history), 4L)
    expect_equal(session$history[[4L]]$content, "[Interrupted]")
})

# --- Empty history with prompt injection ----------------------------
local({
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history      = list(),
                                                          provider     = "anthropic",
                                                          marker       = "[Interrupted]",
                                                          prompt       = "first prompt",
                                                          pre_turn_len = 0L
    )
    expect_equal(length(repaired), 2L)
    expect_equal(repaired[[1L]]$role, "user")
    expect_equal(repaired[[1L]]$content, "first prompt")
    expect_equal(repaired[[2L]]$role, "assistant")
    expect_equal(repaired[[2L]]$content, "[Interrupted]")
})

# --- Custom placeholder threads through for deny-vs-interrupt -------
local({
    history <- list(
                    list(role = "user", content = "prompt"),
                    list(role = "assistant", content = list(
                                                          list(type = "tool_use", id = "tu_1",
                                                               name = "echo", input = list())
        ))
    )
    repaired <- corteza:::repair_interrupted_tool_history(
                                                          history      = history,
                                                          provider     = "anthropic",
                                                          marker       = "[Denied by user]",
                                                          prompt       = "prompt",
                                                          pre_turn_len = 0L,
                                                          placeholder  = "[Denied by user before execution]"
    )
    expect_equal(repaired[[3L]]$content[[1L]]$content,
                 "[Denied by user before execution]")
    expect_equal(repaired[[4L]]$content, "[Denied by user]")
})

# --- Responses: an unfinished function_call gets a function_call_output,
# never a Chat Completions role = "tool" message.
local({
    history <- list(list(role = "user", content = "prompt"),
                    list(type = ".openai_codex_output", output = list(
                        list(type = "function_call", call_id = "call_1",
                             name = "echo", arguments = "{\"x\":1}"))))
    out <- corteza:::.append_responses_tool_results(
        history, list(list(id = "call_1", name = "echo")), "[stopped]")
    expect_equal(length(out), 3L)
    expect_identical(out[[3L]], list(type = "function_call_output",
                                     call_id = "call_1", output = "[stopped]"))

    repaired <- corteza:::repair_interrupted_tool_history(
        history = history, provider = "openai", marker = "[Interrupted]",
        prompt = "prompt", pre_turn_len = 0L)
    roles <- vapply(repaired, function(m) m$role %||% "", character(1))
    expect_false("tool" %in% roles)
    expect_equal(repaired[[length(repaired)]]$content, "[Interrupted]")
    expect_identical(repaired[[3L]]$type, "function_call_output")
    expect_identical(repaired[[3L]]$call_id, "call_1")
})
