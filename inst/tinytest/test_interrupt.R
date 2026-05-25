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

# --- dump_completed_tools_summary ----------------------------------
# The CLI's session$messages is flat text only. dump_completed_tools_summary
# walks the per-turn turn_session$history slice for completed tool
# calls and appends a text summary to the persistent session so the
# next CLI turn's api_history rebuild sees that work.
#
# Tests stub transcript_append so we don't write to disk; the helper
# also calls session_add_message which mutates the session list
# in-process (returned).

ns <- asNamespace("corteza")
orig_transcript_append <- get("transcript_append", envir = ns,
                              inherits = FALSE)

with_stubbed_transcript_append <- function(stub, expr) {
    assignInNamespace("transcript_append", stub, ns = "corteza")
    tryCatch(force(expr),
             finally = assignInNamespace("transcript_append",
                                         orig_transcript_append,
                                         ns = "corteza"))
}

# Helper to build a turn_session env with a history that records a
# completed Anthropic tool_use/tool_result pair plus one dangling
# tool_use (which should be ignored -- only completed calls dump).
make_anthropic_turn_session <- function() {
    s <- new.env(parent = emptyenv())
    s$history <- list(
                      list(role = "user", content = "go"),
                      list(role = "assistant", content = list(
                                                            list(type = "text", text = "ok"),
                                                            list(type = "tool_use", id = "tu_done",
                                                                 name = "echo", input = list(x = 7L)),
                                                            list(type = "tool_use", id = "tu_open",
                                                                 name = "echo", input = list(x = 99L))
        )),
                      list(role = "user", content = list(
                                                       list(type = "tool_result", tool_use_id = "tu_done",
                                                            content = "seven came back")
        ))
    )
    s
}

# Captures arguments to transcript_append so we can assert what
# would have been written.
appended <- list()
fake_transcript_append <- function(session, role, content, ...) {
    appended[[length(appended) + 1L]] <<- list(role = role, content = content)
    invisible(NULL)
}

# --- dump appends summary + leaves dangling alone -------------------
local({
    appended <<- list()
    sess <- list(sessionId = "test", messages = list())
    turn_session <- make_anthropic_turn_session()

    with_stubbed_transcript_append(fake_transcript_append, {
        updated <- corteza:::dump_completed_tools_summary(
                                                          turn_session, sess, pre_turn_len = 0L
        )
        # Session message list grew by one assistant text entry.
        expect_equal(length(updated$messages), 1L)
        expect_equal(updated$messages[[1L]]$role, "assistant")
        text <- updated$messages[[1L]]$content[[1L]]$text
        expect_true(grepl("Completed tool calls before exit", text,
                          fixed = TRUE))
        # The completed call is summarized; the dangling one is not.
        expect_true(grepl("seven came back", text, fixed = TRUE))
        expect_false(grepl("tu_open", text, fixed = TRUE))
        # transcript_append was called once mirroring the same content.
        expect_equal(length(appended), 1L)
        expect_equal(appended[[1L]]$role, "assistant")
        expect_identical(appended[[1L]]$content, text)
    })
})

# --- No completed calls in slice => session unchanged ---------------
local({
    appended <<- list()
    sess <- list(sessionId = "test", messages = list(
                                                     list(role = "user",
                                                          content = list(list(type = "text", text = "prev")))
    ))
    # Empty history below the cursor -> nothing to summarize.
    ts <- new.env(parent = emptyenv())
    ts$history <- list(list(role = "user", content = "go"))

    with_stubbed_transcript_append(fake_transcript_append, {
        updated <- corteza:::dump_completed_tools_summary(
                                                          ts, sess, pre_turn_len = 1L
        )
        # length(history) == pre_turn_len -> early return, session
        # untouched, no transcript_append.
        expect_equal(length(updated$messages), 1L)
        expect_equal(length(appended), 0L)
    })
})

# --- Long results get truncated at max_result_chars -----------------
local({
    appended <<- list()
    long_blob <- strrep("x", 1200L)
    ts <- new.env(parent = emptyenv())
    ts$history <- list(
                       list(role = "user", content = "go"),
                       list(role = "assistant", content = list(
                                                             list(type = "tool_use", id = "tu_blob",
                                                                  name = "echo", input = list())
        )),
                       list(role = "user", content = list(
                                                        list(type = "tool_result", tool_use_id = "tu_blob",
                                                             content = long_blob)
        ))
    )
    sess <- list(sessionId = "test", messages = list())

    with_stubbed_transcript_append(fake_transcript_append, {
        updated <- corteza:::dump_completed_tools_summary(
                                                          ts, sess, pre_turn_len = 0L,
                                                          max_result_chars = 50L
        )
        text <- updated$messages[[1L]]$content[[1L]]$text
        # Truncated: contains the ellipsis marker, doesn't contain the
        # full blob length.
        expect_true(grepl("...", text, fixed = TRUE))
        expect_true(nchar(text) < nchar(long_blob))
    })
})
