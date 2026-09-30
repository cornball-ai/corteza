library(tinytest)

# The job ledger: intent before dispatch, outcome beside it, and the
# restart verdicts that follow from which files exist. Every check runs
# against a private state directory.

with_state <- function(code) {
    dir <- tempfile("jobs-state")
    dir.create(dir)
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = dir)
    on.exit({
        if (is.na(old)) {
            Sys.unsetenv("CORTEZA_STATE_DIR")
        } else {
            Sys.setenv(CORTEZA_STATE_DIR = old)
        }
        unlink(dir, recursive = TRUE)
    })
    force(code)
}

job_create <- corteza:::job_create
job_read <- corteza:::job_read

# --- Creation writes intent only ---
with_state({
    id <- job_create("summarise R/jobs.R", workspace = tempdir(),
                     requester = "@troy:ex",
                     origin = list(session_key = "!room:ex", thread = NULL))
    expect_true(grepl("^[0-9]{8}T[0-9]{6}-[0-9a-f]{8}$", id))
    dir <- corteza:::job_dir(id)
    expect_identical(sort(list.files(dir)), "intent.json")
    j <- job_read(id)
    expect_identical(j$status, "queued")
    expect_identical(j$task, "summarise R/jobs.R")
    expect_identical(j$requester, "@troy:ex")
    expect_identical(j$origin$session_key, "!room:ex")
    expect_identical(j$hop, 0L)
    expect_identical(j$limits$hops, 3L)
    expect_identical(j$task_digest,
                     digest::digest("summarise R/jobs.R", algo = "sha256",
                                    serialize = FALSE))
    expect_false(j$cancel_requested)
})

# A blank task is refused before anything is written.
with_state({
    expect_error(job_create("  "), "non-empty")
    expect_false(dir.exists(corteza:::job_root()))
})

# --- Ids never name a path outside the ledger ---
expect_error(job_read("../../etc"), "invalid job id")
expect_error(job_read(c("a", "b")), "invalid job id")
expect_error(corteza:::job_settle("x/y", "done"), "invalid job id")
with_state(expect_null(job_read("20260930T120000-00000000")))

# --- Dispatch and settle ---
with_state({
    id <- job_create("t")
    corteza:::job_mark_dispatched(id, worker = list(pid = 123L))
    expect_identical(job_read(id)$status, "running")
    expect_identical(job_read(id)$dispatch$worker$pid, 123L)
    # Dispatching twice is a bug in the caller, not a no-op.
    expect_error(corteza:::job_mark_dispatched(id), "already dispatched")

    expect_true(corteza:::job_settle(id, "done", result = "ok"))
    j <- job_read(id)
    expect_identical(j$status, "done")
    expect_identical(j$outcome$result, "ok")
    # First writer wins: a late result cannot overwrite the outcome.
    expect_false(corteza:::job_settle(id, "failed", error = "late"))
    expect_identical(job_read(id)$status, "done")
    # An ended job cannot be dispatched or cancelled.
    expect_false(corteza:::job_request_cancel(id))
    expect_error(corteza:::job_settle(id, "finished"), "must be one of")
})

with_state({
    id <- job_create("t")
    corteza:::job_settle(id, "cancelled")
    expect_error(corteza:::job_mark_dispatched(id), "already ended")
})

# --- Cancellation is a request, not an ending ---
with_state({
    id <- job_create("t")
    corteza:::job_mark_dispatched(id)
    expect_true(corteza:::job_request_cancel(id, by = "@troy:ex"))
    j <- job_read(id)
    expect_true(j$cancel_requested)
    expect_identical(j$status, "running")
})

# --- Hop limits are enforced in code ---
with_state({
    a <- job_create("a", limits = list(hops = 2L))
    b <- job_create("b", parent = a)
    expect_identical(job_read(b)$hop, 1L)
    c <- job_create("c", parent = b)
    expect_identical(job_read(c)$hop, 2L)
    expect_error(job_create("d", parent = c), "hop limit")
    # A child cannot raise the limit it inherited...
    expect_error(job_create("d", parent = c, limits = list(hops = 10L)),
                 "hop limit")
    expect_identical(job_read(b)$limits$hops, 2L)
    # ...but may lower it.
    e <- job_create("e", parent = a, limits = list(hops = 1L))
    expect_identical(job_read(e)$limits$hops, 1L)
    expect_error(job_create("f", parent = e), "hop limit")
    expect_error(job_create("g", parent = "20260930T120000-00000000"),
                 "parent job not found")
})

# --- Listing ---
with_state({
    expect_identical(corteza:::job_list(), list())
    a <- job_create("a", origin = list(session_key = "k1"))
    b <- job_create("b", origin = list(session_key = "k2"))
    corteza:::job_settle(b, "done")
    all <- corteza:::job_list()
    expect_identical(length(all), 2L)
    expect_identical(vapply(corteza:::job_list(status = "queued"),
                            function(j) j$id, ""), a)
    expect_identical(vapply(corteza:::job_list(origin_key = "k2"),
                            function(j) j$id, ""), b)
    # Stray entries in the ledger directory are not jobs.
    dir.create(file.path(corteza:::job_root(), "not-a-job"))
    expect_identical(length(corteza:::job_list()), 2L)
})

# --- Recovery verdicts ---
with_state({
    never <- job_create("never dispatched")
    lost <- job_create("dispatched, worker died")
    corteza:::job_mark_dispatched(lost)
    live <- job_create("still running")
    corteza:::job_mark_dispatched(live)
    ended <- job_create("finished")
    corteza:::job_settle(ended, "done")

    v <- corteza:::job_recover(live = live)
    expect_identical(sort(v$id), sort(c(never, lost)))
    expect_identical(v$verdict[v$id == never], "retryable")
    expect_identical(v$verdict[v$id == lost], "indeterminate")

    # Never dispatched stays queued: it is safe to run.
    expect_identical(job_read(never)$status, "queued")
    # Dispatched without an outcome is settled, with the reason, and is
    # not re-queued.
    j <- job_read(lost)
    expect_identical(j$status, "indeterminate")
    expect_true(grepl("may have acted", j$outcome$reason))
    # The live job and the ended job are untouched.
    expect_identical(job_read(live)$status, "running")
    expect_identical(job_read(ended)$status, "done")

    # A second pass finds only the still-queued job, and changes nothing.
    v2 <- corteza:::job_recover(live = live)
    expect_identical(v2$id, never)
    expect_identical(v2$verdict, "retryable")

    # A late result for the indeterminate job does not overwrite it.
    expect_false(corteza:::job_settle(lost, "done", result = "late"))
    expect_identical(job_read(lost)$status, "indeterminate")
})

# An empty ledger recovers to an empty frame with the right columns.
with_state({
    v <- corteza:::job_recover()
    expect_identical(nrow(v), 0L)
    expect_identical(names(v), c("id", "verdict", "task"))
})
