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
cur <- corteza:::job_lock_current(plain)
hp <- file.path(corteza:::job_lock_gen_dir(corteza:::job_lock_root(plain),
                                           cur$gen), "holder.json")
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

# --- Interleavings that used to admit two writers ---
running_job <- function(task) {
    id <- corteza:::job_create(task, workspace = plain)
    corteza:::job_mark_dispatched(id)
    id
}
holder_for <- function(id, n) {
    list(checkout = plain, job = id, owner = "x", pid = Sys.getpid(),
         host = Sys.info()[["nodename"]], gen = n,
         acquired_at = corteza:::job_now())
}

# Two acquirers both see the lock free and both go for the next
# generation. Only one creation can succeed; the other sees it held.
local({
    a <- running_job("a")
    b <- running_job("b")
    n <- (corteza:::job_lock_current(plain)$gen %||% 0L) + 1L
    expect_true(corteza:::job_lock_try(plain, n, holder_for(a, n)))
    expect_false(corteza:::job_lock_try(plain, n, holder_for(b, n)))
    expect_identical(holder(plain)$job, a)
    # The generation never exists without its holder.
    expect_true(file.exists(file.path(corteza:::job_lock_gen_dir(
        corteza:::job_lock_root(plain), n), "holder.json")))
    expect_false(acquire(plain, b)$ok)
    corteza:::job_settle(a, "done")
    corteza:::job_settle(b, "done")
})

# A late release cannot free a newer holder. A's job ends without
# releasing, B takes the next generation, then A's release arrives.
local({
    a <- running_job("a")
    b <- running_job("b")
    c3 <- running_job("c")
    expect_true(acquire(plain, a)$ok)
    corteza:::job_settle(a, "done")          # stale, not yet released
    expect_true(acquire(plain, b)$ok)        # takes over
    release(plain, a)                        # the late release
    expect_identical(holder(plain)$job, b)
    expect_false(acquire(plain, c3)$ok)      # no third writer
    release(plain, b)
    expect_true(acquire(plain, c3)$ok)
    release(plain, c3)
    for (id in c(b, c3)) corteza:::job_settle(id, "done")
    # Old generations are pruned; only the newest remains.
    expect_identical(length(corteza:::job_lock_gens(
        corteza:::job_lock_root(plain))), 1L)
})

# A delayed acquirer cannot recreate a pruned generation. A chooses
# generation n and pauses; B acquires and releases n; C takes n+1 and
# prunes n; A resumes and creates n. A must not hold the lock.
local({
    a <- running_job("a")
    b <- running_job("b")
    c3 <- running_job("c")
    n <- (corteza:::job_lock_current(plain)$gen %||% 0L) + 1L  # A chooses
    expect_true(acquire(plain, b)$ok)                           # B: gen n
    release(plain, b)
    expect_true(acquire(plain, c3)$ok)                          # C: gen n+1
    root <- corteza:::job_lock_root(plain)
    expect_false(n %in% corteza:::job_lock_gens(root))          # n pruned
    expect_false(corteza:::job_lock_commit(plain, n, holder_for(a, n)))
    expect_identical(holder(plain)$job, c3)
    # The recreated generation was withdrawn.
    expect_false(n %in% corteza:::job_lock_gens(root))
    release(plain, c3)
    for (id in c(a, b, c3)) corteza:::job_settle(id, "done")
})

# Real processes racing for one free lock: exactly one wins.
local({
    # Every racer stays alive until all have tried: a holder whose
    # process exits is stale, and taking over from it is correct.
    ids <- vapply(1:6, function(i) running_job(paste("race", i)), "")
    go <- tempfile("go")
    done <- tempfile("done")
    out <- tempfile("race-out")
    dir.create(out)
    procs <- lapply(ids, function(id) {
        callr::r_bg(function(checkout, id, go, done, out) {
            while (!file.exists(go)) Sys.sleep(0.01)
            ok <- corteza:::job_lock_acquire(checkout, id, owner = "racer")$ok
            writeLines(as.character(ok), file.path(out, id))
            while (!file.exists(done)) Sys.sleep(0.01)
            ok
        }, list(checkout = plain, id = id, go = go, done = done, out = out))
    })
    Sys.sleep(1)
    file.create(go)
    deadline <- Sys.time() + 30
    while (length(list.files(out)) < length(ids) && Sys.time() < deadline) {
        Sys.sleep(0.05)
    }
    wins <- vapply(ids, function(id) {
        identical(readLines(file.path(out, id)), "TRUE")
    }, logical(1))
    expect_identical(sum(wins), 1L)
    file.create(done)
    for (p in procs) p$wait(30000)
    for (id in ids) corteza:::job_settle(id, "done")
    unlink(c(go, done, out), recursive = TRUE)
})

# Smoke test: processes cycling acquire / work / release, so acquisitions
# cross releases and prunes. Holding the lock, each enters a critical
# section marked by an atomic dir.create(); finding it already there
# means two holders at once. Timing alone rarely opens the known race
# windows -- with the pruned-generation check disabled this still sees
# no overlap -- so those are covered by the replayed interleavings
# above, not by this.
local({
    go <- tempfile("go")
    crit <- tempfile("critical")
    procs <- lapply(1:6, function(i) {
        callr::r_bg(function(checkout, go, crit, i) {
            while (!file.exists(go)) Sys.sleep(0.01)
            overlaps <- 0L
            held <- 0L
            for (k in 1:25) {
                id <- corteza:::job_create(sprintf("cycle %d-%d", i, k),
                                           workspace = checkout)
                if (corteza:::job_lock_acquire(checkout, id, owner = "c")$ok) {
                    held <- held + 1L
                    if (!dir.create(crit, showWarnings = FALSE)) {
                        overlaps <- overlaps + 1L
                    } else {
                        Sys.sleep(stats::runif(1, 0, 0.01))
                        unlink(crit, recursive = TRUE)
                    }
                    corteza:::job_lock_release(checkout, id)
                }
                corteza:::job_settle(id, "done")
                Sys.sleep(stats::runif(1, 0, 0.005))
            }
            c(overlaps = overlaps, held = held)
        }, list(checkout = plain, go = go, crit = crit, i = i))
    })
    Sys.sleep(1)
    file.create(go)
    for (p in procs) p$wait(120000)
    res <- do.call(rbind, lapply(procs, function(p) p$get_result()))
    expect_identical(sum(res[, "overlaps"]), 0L)
    # The test only means something if the lock changed hands a lot.
    expect_true(sum(res[, "held"]) > 20L)
    unlink(c(go, crit), recursive = TRUE)
})

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
