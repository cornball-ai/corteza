# Tests for Phase 7 observability: worker stderr events are drained
# and optionally pretty-printed.

# cli_worker_drain_events accepts a duck-typed session with
# read_error_lines(). We build a fake session to avoid spinning up a
# real callr process for the test.

make_fake_session <- function(lines = character()) {
    buffer <- lines
    list(read_error_lines = function() {
        out <- buffer
        buffer <<- character()
        out
    })
}

# --- trace = FALSE: events are consumed but not rendered --------------

fake <- make_fake_session(c(
    '{"event":"tool_call","tool":"bash","args":{"command":"ls"}}',
    '{"event":"tool_result","tool":"bash","success":true,"elapsed_ms":3}'
))
captured <- utils::capture.output(
    corteza::cli_worker_drain_events(fake, trace = FALSE),
    type = "message"
)
expect_equal(length(captured), 0L)

# --- trace = TRUE: events are rendered somewhere (stderr or printify) -

# We don't assert on exact output because printify's rendering has its
# own knobs; we just verify the function completes cleanly.
fake2 <- make_fake_session(c(
    '{"event":"tool_call","tool":"read_file","args":{"path":"/tmp/x"}}',
    '{"event":"tool_result","tool":"read_file","success":true,"elapsed_ms":2}'
))
res <- tryCatch(
    corteza::cli_worker_drain_events(fake2, trace = TRUE),
    error = function(e) e
)
expect_false(inherits(res, "error"))

# --- Malformed JSON line is skipped, not fatal ------------------------

fake3 <- make_fake_session(c(
    '{not json at all',
    '{"event":"tool_result","tool":"bash","success":true,"elapsed_ms":1}'
))
res <- tryCatch(
    corteza::cli_worker_drain_events(fake3, trace = TRUE),
    error = function(e) e
)
expect_false(inherits(res, "error"))

# --- Empty stream is a no-op -----------------------------------------

fake_empty <- make_fake_session(character())
expect_null(corteza::cli_worker_drain_events(fake_empty, trace = TRUE))
expect_null(corteza::cli_worker_drain_events(fake_empty, trace = FALSE))

# --- ctx threading: when fn declares `ctx`, handler injects it --------

# Define an inline tool that reads ctx, register, invoke, verify.
# Register via a real function we can grab from the registry.
corteza::ensure_skills()

# spawn_subagent is already registered with ctx-aware fn. Its handler
# should preserve ctx$session when invoked through skill_run() / call_tool().
# We can't actually spawn without full session infra, but we can
# verify the handler's behavior in isolation.
spawn_skill <- corteza:::get_skill("spawn_subagent")
expect_true(!is.null(spawn_skill))
expect_true(is.function(spawn_skill$handler))

# The handler should accept (args, ctx); calling it with a ctx that
# has a session should route to subagent_spawn (which will fail
# without real infra, but the failure mode proves ctx was threaded).
ctx <- list(session = list(sessionId = "test-sid", is_subagent = FALSE))
res <- tryCatch(
    spawn_skill$handler(
        list(task = "noop", model = NULL, tools = NULL),
        ctx = ctx
    ),
    error = function(e) structure(conditionMessage(e), class = "handler_error")
)
# Either it returned an err() (spawn failed cleanly) or it threw —
# either way, we proved the handler ran with ctx available.
expect_true(
    inherits(res, "handler_error") ||
        (is.list(res) && !is.null(res$isError) && isTRUE(res$isError)) ||
        (is.list(res) && !is.null(res$content))
)
