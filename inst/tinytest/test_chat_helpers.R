library(tinytest)

# Non-interactive chat() should refuse.
expect_error(llamaR::chat(), "interactive")

# disk_messages_to_history flattens both list-of-blocks and plain-string.
local({
    messages <- list(
        list(role = "user",
             content = list(list(type = "text", text = "hello"))),
        list(role = "assistant", content = "hi there"),
        list(role = "user", content = list())
    )
    out <- llamaR:::disk_messages_to_history(messages)
    expect_equal(length(out), 3L)
    expect_equal(out[[1]]$role, "user")
    expect_equal(out[[1]]$content, "hello")
    expect_equal(out[[2]]$content, "hi there")
})

# chat_trace_observer swallows errors (trace_add may not be fully wired
# in test fixtures).
local({
    session <- llamaR::new_session("console")
    session$sessionId <- "test-session"
    obs <- llamaR:::chat_trace_observer(session)
    # Pass a bogus event; trace_add will fail or succeed — either way
    # the observer must return silently.
    event <- list(
        call = list(tool = "read_file", args = list(path = "x")),
        result = "ok",
        success = TRUE,
        elapsed_ms = 1.2,
        turn_number = 1L
    )
    expect_silent(obs(event))
})
