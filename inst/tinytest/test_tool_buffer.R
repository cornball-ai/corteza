library(tinytest)

# Basic add/get/list on a single session.
local({
    s <- list(sessionId = "test-buf-1")
    on.exit(corteza:::tool_buffer_reset(s), add = TRUE)

    expect_identical(corteza:::tool_buffer_list(s), list())
    expect_null(corteza:::tool_buffer_get(s, 1L))

    corteza:::tool_buffer_add(s, "read_file",
                              list(path = "/tmp/a"),
                              "contents A")
    corteza:::tool_buffer_add(s, "bash",
                              list(command = "ls"),
                              "a\nb\nc")

    outputs <- corteza:::tool_buffer_list(s)
    expect_equal(length(outputs), 2L)
    # Newest first.
    expect_identical(outputs[[1L]]$name, "bash")
    expect_identical(outputs[[2L]]$name, "read_file")

    first <- corteza:::tool_buffer_get(s, 1L)
    expect_identical(first$name, "bash")
    expect_identical(first$result, "a\nb\nc")

    second <- corteza:::tool_buffer_get(s, 2L)
    expect_identical(second$name, "read_file")

    # Past-the-end returns NULL, not an error.
    expect_null(corteza:::tool_buffer_get(s, 3L))
    expect_null(corteza:::tool_buffer_get(s, 0L))
})

# Two sessions don't see each other's buffers. Subagents and parents
# rely on this for isolation.
local({
    a <- list(sessionId = "test-buf-iso-A")
    b <- list(sessionId = "test-buf-iso-B")
    on.exit({
        corteza:::tool_buffer_reset(a)
        corteza:::tool_buffer_reset(b)
    }, add = TRUE)

    corteza:::tool_buffer_add(a, "tool_a", list(), "result A")
    corteza:::tool_buffer_add(b, "tool_b", list(), "result B")

    out_a <- corteza:::tool_buffer_list(a)
    out_b <- corteza:::tool_buffer_list(b)
    expect_equal(length(out_a), 1L)
    expect_equal(length(out_b), 1L)
    expect_identical(out_a[[1L]]$name, "tool_a")
    expect_identical(out_b[[1L]]$name, "tool_b")
})

# Size cap: default is 20 entries. Adding more drops the oldest.
local({
    s <- list(sessionId = "test-buf-cap")
    on.exit(corteza:::tool_buffer_reset(s), add = TRUE)

    for (i in seq_len(25L)) {
        corteza:::tool_buffer_add(s, sprintf("tool_%02d", i),
                                  list(idx = i), sprintf("r%02d", i))
    }
    outputs <- corteza:::tool_buffer_list(s)
    expect_equal(length(outputs), 20L)
    # Newest still on top.
    expect_identical(outputs[[1L]]$name, "tool_25")
    # tool_05 (the oldest still in the cap) should be at index 20.
    expect_identical(outputs[[20L]]$name, "tool_06")
})

# Reset clears the per-session record without touching others.
local({
    a <- list(sessionId = "test-buf-reset-A")
    b <- list(sessionId = "test-buf-reset-B")
    on.exit({
        corteza:::tool_buffer_reset(a)
        corteza:::tool_buffer_reset(b)
    }, add = TRUE)

    corteza:::tool_buffer_add(a, "ta", list(), "ra")
    corteza:::tool_buffer_add(b, "tb", list(), "rb")
    corteza:::tool_buffer_reset(a)
    expect_identical(corteza:::tool_buffer_list(a), list())
    expect_equal(length(corteza:::tool_buffer_list(b)), 1L)
})

# Missing/empty sessionId: defensive — add silently no-ops, get/list
# return NULL / empty.
local({
    no_id <- list()
    corteza:::tool_buffer_add(no_id, "x", list(), "y")
    expect_identical(corteza:::tool_buffer_list(no_id), list())
    expect_null(corteza:::tool_buffer_get(no_id, 1L))

    empty_id <- list(sessionId = "")
    corteza:::tool_buffer_add(empty_id, "x", list(), "y")
    expect_identical(corteza:::tool_buffer_list(empty_id), list())
})

# tool_buffer_observer captures successful "ran" events. Non-"ran"
# events and failed "ran" events are ignored.
local({
    s <- list(sessionId = "test-buf-observer")
    on.exit(corteza:::tool_buffer_reset(s), add = TRUE)

    obs <- corteza:::tool_buffer_observer(s)

    # Successful tool call.
    obs(list(outcome = "ran",
             success = TRUE,
             call = list(tool = "read_file",
                         args = list(path = "x.R")),
             result = "abc"))
    expect_equal(length(corteza:::tool_buffer_list(s)), 1L)

    # Start events should NOT be captured.
    obs(list(outcome = "start",
             call = list(tool = "read_file", args = list())))
    expect_equal(length(corteza:::tool_buffer_list(s)), 1L)

    # Failed runs should NOT be captured.
    obs(list(outcome = "ran",
             success = FALSE,
             call = list(tool = "bash",
                         args = list(command = "exit 1")),
             result = "Error: exit 1"))
    expect_equal(length(corteza:::tool_buffer_list(s)), 1L)
})
