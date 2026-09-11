# Async query/collect — pure-function checks.
# These exercise the registry-state guards (busy, no-pending,
# unknown-id) and the bounded sync wait without spinning up a callr
# child. The full async round-trip is covered in test_subagent_callr.R
# (at_home-gated).

reg <- corteza:::.subagent_registry

# Snapshot any prior registry state, then start clean. We restore at
# the bottom rather than via on.exit() because top-level on.exit in
# tinytest files attaches to the global frame and isn't reliable.
prior <- as.list(reg)
rm(list = ls(reg), envir = reg)

# Stub a registry entry with no real callr session. Guard paths
# should fire before any session method is reached.
stub_id <- "stub-12345678"
reg[[stub_id]] <- list(
    id = stub_id,
    seq = 1L,
    task = "stub",
    started_at = Sys.time(),
    timeout = Sys.time() + 600,
    pending = NULL,
    pending_started_at = NULL,
    session = NULL
)

# subagent_collect on idle agent → error "No pending query".
err <- tryCatch(corteza::subagent_collect(stub_id),
                error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("No pending query", conditionMessage(err)))

# Flip the stub to busy and verify both wait paths refuse to stack a
# second call. The session=NULL stub means any call past the busy
# guard would NPE on info$session$..., so reaching the guard message
# is itself proof that the guard fires before the session is touched.
reg[[stub_id]]$pending <- "in-flight prompt"
reg[[stub_id]]$pending_started_at <- Sys.time()

err <- tryCatch(
    corteza::subagent_query(stub_id, "second prompt", wait = FALSE),
    error = function(e) e
)
expect_inherits(err, "error")
expect_true(grepl("is busy with", conditionMessage(err)))

# Same guard must apply to the sync path: r_session can only carry one
# in-flight call.
err <- tryCatch(
    corteza::subagent_query(stub_id, "second prompt", wait = TRUE),
    error = function(e) e
)
expect_inherits(err, "error")
expect_true(grepl("is busy with", conditionMessage(err)))

# Unknown id: both surfaces raise.
err <- tryCatch(corteza::subagent_collect("does-not-exist"),
                error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("Subagent not found", conditionMessage(err)))

err <- tryCatch(
    corteza::subagent_query("does-not-exist", "x", wait = FALSE),
    error = function(e) e
)
expect_inherits(err, "error")
expect_true(grepl("Subagent not found", conditionMessage(err)))

# format_subagent_list distinguishes idle vs busy.
reg[[stub_id]]$pending <- NULL
idle_listing <- corteza:::format_subagent_list(corteza::subagent_list())
expect_true(grepl("idle", idle_listing))

reg[[stub_id]]$pending <- "checking the deploy log"
busy_listing <- corteza:::format_subagent_list(corteza::subagent_list())
expect_true(grepl("busy:", busy_listing))
expect_true(grepl("checking the deploy log", busy_listing))

# Bounded sync wait. A fake r_session stands in for callr: call()
# records the fired prompt, poll_process() reports the scripted states
# in order and remembers the timeouts it was given, read() hands back
# the child's result.
fake_session <- function(states, result) {
    env <- new.env()
    env$fired <- list()
    env$polls <- integer()
    env$states <- states
    env$call <- function(func, args) {
        env$fired[[length(env$fired) + 1L]] <- args
        invisible(NULL)
    }
    env$poll_process <- function(timeout) {
        env$polls <- c(env$polls, as.integer(timeout))
        state <- env$states[1]
        if (length(env$states) > 1L) env$states <- env$states[-1]
        state
    }
    env$read <- function() list(result = result, error = NULL)
    env
}
stub_entry <- function(id, session) {
    list(id = id, seq = 2L, task = "stub", started_at = Sys.time(),
         timeout = Sys.time() + 600, pending = NULL,
         pending_started_at = NULL, session = session)
}

# A child that does not answer inside the deadline: the sync query
# returns NULL, the prompt stays pending, and the poll got the deadline
# in milliseconds rather than blocking forever.
slow_id <- "slow-12345678"
reg[[slow_id]] <- stub_entry(slow_id, fake_session(
    c("timeout", "ready"),
    list(reply = "late pong",
         usage = list(input_tokens = 1L, output_tokens = 1L, total_tokens = 2L))
))
res <- corteza::subagent_query(slow_id, "slow prompt", wait = TRUE, timeout = 0.01)
expect_null(res)
expect_equal(reg[[slow_id]][["pending"]], "slow prompt")
expect_equal(reg[[slow_id]]$session$polls, 10L)
expect_equal(reg[[slow_id]]$session$fired[[1]]$p, "slow prompt")

# One in flight: the timed-out query still occupies the slot.
err <- tryCatch(corteza::subagent_query(slow_id, "again", wait = TRUE),
                error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("is busy with", conditionMessage(err)))

# Collecting later picks up the reply, clears the slot, and books usage.
got <- corteza::subagent_collect(slow_id, wait = TRUE, timeout = 1)
expect_equal(got, "late pong")
expect_null(reg[[slow_id]][["pending"]])
expect_equal(reg[[slow_id]]$cumulative_total_tokens, 2L)

# A child that answers in time returns the reply directly, and Inf
# maps to processx's wait-forever sentinel instead of NA.
fast_id <- "fast-12345678"
reg[[fast_id]] <- stub_entry(fast_id, fake_session("ready",
    list(reply = "pong", usage = NULL)))
expect_equal(corteza::subagent_query(fast_id, "ping", wait = TRUE, timeout = 5),
             "pong")
expect_null(reg[[fast_id]][["pending"]])
expect_equal(reg[[fast_id]]$session$polls, 5000L)
expect_equal(corteza::subagent_query(fast_id, "ping", wait = TRUE, timeout = Inf),
             "pong")
expect_equal(reg[[fast_id]]$session$polls, c(5000L, -1L))

# A bad timeout is refused before the prompt is fired, on both
# surfaces. Each of these used to reach poll_process() as -1 and wait
# forever.
fired_before <- length(reg[[fast_id]]$session$fired)
for (bad in list(NA_real_, NaN, "5", -Inf, -1, c(1, 2), NULL)) {
    err <- tryCatch(
        corteza::subagent_query(fast_id, "ping", wait = TRUE, timeout = bad),
        error = function(e) e
    )
    expect_inherits(err, "error")
    expect_true(grepl("timeout must be", conditionMessage(err)))
}
expect_equal(length(reg[[fast_id]]$session$fired), fired_before)
expect_null(reg[[fast_id]][["pending"]])
reg[[fast_id]]$pending <- "parked"
err <- tryCatch(corteza::subagent_collect(fast_id, wait = TRUE, timeout = NA),
                error = function(e) e)
expect_true(grepl("timeout must be", conditionMessage(err)))
reg[[fast_id]]$pending <- NULL
# Oversized finite values take the wait-forever sentinel, not NA.
expect_equal(corteza:::.subagent_poll_ms(1e9), -1L)
expect_equal(corteza:::.subagent_poll_ms(2.5), 2500L)
expect_equal(corteza:::.subagent_poll_ms(Inf, wait = FALSE), 0L)

# The model-facing tool fires and collects by default, and says so.
expect_false(formals(corteza::tool_query_subagent)$wait)
out <- corteza::tool_query_subagent(fast_id, "queued prompt")
expect_true(grepl("Queued for subagent", out$content[[1]]$text))
expect_equal(reg[[fast_id]][["pending"]], "queued prompt")
out <- corteza::tool_collect_subagent(fast_id, wait = TRUE, timeout = 1)
expect_equal(out$content[[1]]$text, "pong")
expect_null(reg[[fast_id]][["pending"]])

# Positional callers keep their meaning: the fourth argument is still
# return_name, and it reaches the child.
out <- corteza::tool_query_subagent(fast_id, "p", TRUE, "artifact")
expect_equal(out$content[[1]]$text, "pong")
fired <- reg[[fast_id]]$session$fired
expect_equal(fired[[length(fired)]]$rn, "artifact")

# Asked to wait, the tool reports a slow child instead of hanging.
reg[[slow_id]]$session$states <- "timeout"
out <- corteza::tool_query_subagent(slow_id, "p", wait = TRUE, timeout = 0.01)
expect_true(grepl("still working", out$content[[1]]$text))
expect_equal(reg[[slow_id]][["pending"]], "p")

# Cleanup: drop stubs, restore prior entries.
rm(list = ls(reg), envir = reg)
for (nm in names(prior)) reg[[nm]] <- prior[[nm]]
