library(tinytest)

# One writer per checkout: the lock is taken at dispatch, released when
# the job ends, and taken over when its holder is gone. Every check runs
# against a private state directory.

state <- tempfile("job-lock-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

acquire <- corteza:::job_lock_acquire
release <- corteza:::job_lock_release
holder <- corteza:::job_lock_holder

# --- The checkout is the git toplevel ---
repo <- tempfile("lockrepo")
dir.create(file.path(repo, "sub", "deeper"), recursive = TRUE)
if (nzchar(Sys.which("git"))) {
    system2("git", c("-C", shQuote(repo), "init", "-q"))
    expect_identical(corteza:::job_checkout(file.path(repo, "sub", "deeper")),
                     normalizePath(repo))
}
plain <- tempfile("plaindir")
dir.create(plain)
expect_identical(corteza:::job_checkout(plain), normalizePath(plain))

# --- Only one job holds a checkout ---
a <- corteza:::job_create("a", workspace = plain)
b <- corteza:::job_create("b", workspace = plain)
expect_true(acquire(plain, a, owner = "@claude:ex")$ok)
h <- holder(plain)
expect_identical(h$job, a)
expect_identical(h$owner, "@claude:ex")
expect_identical(h$pid, Sys.getpid())
# Re-acquiring by the holder is fine; anyone else is told who holds it.
expect_true(acquire(plain, a)$ok)
got <- acquire(plain, b, owner = "@codex:ex")
expect_false(got$ok)
expect_identical(got$holder$job, a)
# Only the holder can release.
expect_false(release(plain, b))
expect_true(release(plain, a))
expect_null(holder(plain))
expect_true(acquire(plain, b)$ok)
release(plain, b)

# --- A lock whose job has ended is stale and taken over ---
c1 <- corteza:::job_create("c1", workspace = plain)
c2 <- corteza:::job_create("c2", workspace = plain)
acquire(plain, c1)
corteza:::job_settle(c1, "indeterminate")
expect_true(acquire(plain, c2)$ok)
expect_identical(holder(plain)$job, c2)
release(plain, c2)

# --- A lock whose process is gone is stale ---
d1 <- corteza:::job_create("d1", workspace = plain)
d2 <- corteza:::job_create("d2", workspace = plain)
acquire(plain, d1)
hp <- file.path(corteza:::job_lock_dir(plain), "holder.json")
h <- jsonlite::fromJSON(hp)
if (.Platform$OS.type != "windows") {
    # A pid from a process that has exited.
    p <- processx::process$new("true")
    p$wait()
    h$pid <- p$get_pid()
    corteza:::job_write_file(hp, h)
    expect_true(corteza:::job_lock_process_gone(h))
    expect_true(acquire(plain, d2)$ok)
    release(plain, d2)
}
# A holder on another host is presumed alive.
expect_false(corteza:::job_lock_process_gone(
    list(pid = 1L, host = "some-other-host")))

# --- Dispatch: a second writer waits, then runs ---
make_session <- function(key, owner = "local", run_fn) {
    s <- corteza::new_session("cli")
    s$cwd <- plain
    s$config <- list()
    s$job_key <- key
    s$job_owner <- owner
    s$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE),
                              run_fn = run_fn)
    s
}
sleeper <- function(task) {
    Sys.sleep(as.numeric(task))
    list(reply = "done")
}
pump_until <- function(s, id, timeout = 30) {
    seen <- list()
    deadline <- Sys.time() + timeout
    repeat {
        seen <- c(seen, corteza:::job_pump(s))
        if (corteza:::job_read(id)$status %in% corteza:::JOB_STATUSES_FINAL ||
            Sys.time() > deadline) {
            return(seen)
        }
        Sys.sleep(0.1)
    }
}
types <- function(ev) vapply(ev, function(e) e$type, "")

# Two bots, two rooms, one checkout.
claude <- make_session("!viento:ex", "@claude:ex", sleeper)
codex <- make_session("!vientox:ex", "@codex:ex", sleeper)
first <- corteza:::job_submit(claude, "2")
expect_identical(corteza:::job_read(first)$status, "running")
expect_identical(holder(plain)$job, first)

second <- corteza:::job_submit(codex, "0")
# This pump drains what the submit's own pump collected.
ev <- corteza:::job_pump(codex)
expect_identical(corteza:::job_read(second)$status, "queued")
blocked <- Filter(function(e) identical(e$type, "blocked"), ev)
expect_identical(length(blocked), 1L)
expect_identical(blocked[[1L]]$holder$job, first)
# Reported once: later pumps do not repeat it.
expect_identical(sum(types(corteza:::job_pump(codex)) == "blocked"), 0L)
txt <- corteza:::job_blocked_text(list(job = corteza:::job_read(second),
                                       holder = holder(plain)))
expect_true(grepl(first, txt, fixed = TRUE))
expect_true(grepl("@claude:ex", txt, fixed = TRUE))

# When the first ends, its lock goes and the second runs.
pump_until(claude, first)
expect_identical(corteza:::job_read(first)$status, "done")
expect_false(identical(holder(plain)$job, first))
pump_until(codex, second)
expect_identical(corteza:::job_read(second)$status, "done")
expect_null(holder(plain))

# --- Cancelling a running writer frees the checkout ---
long <- corteza:::job_submit(claude, "30")
expect_identical(holder(plain)$job, long)
corteza:::job_cancel(claude, long)
corteza:::job_pump(claude)
expect_null(holder(plain))

# --- A worker that dies mid-job frees the checkout ---
dying <- make_session("!die:ex", "@claude:ex",
                      function(task) quit(save = "no", status = 1))
dj <- corteza:::job_submit(dying, "x")
pump_until(dying, dj)
expect_identical(corteza:::job_read(dj)$status, "indeterminate")
expect_null(holder(plain))

# --- A worker that cannot start frees the checkout ---
broken <- make_session("!broken:ex", "@claude:ex", sleeper)
broken$job_worker_spec$init_fn <- function(spec) stop("no key")
bj <- corteza:::job_submit(broken, "x")
expect_identical(corteza:::job_read(bj)$status, "failed")
expect_null(holder(plain))

for (s in list(claude, codex, dying, broken)) {
    corteza:::job_worker_close(s)
}
if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(c(state, repo, plain), recursive = TRUE)
