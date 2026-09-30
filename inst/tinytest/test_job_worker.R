library(tinytest)

# The job worker: one persistent callr process per session, jobs run one
# at a time without blocking the caller, and a checkpoint at each job
# boundary that a replacement worker restores. The child's init and run
# functions are replaced with ones that need no provider, so nothing
# here reaches a model.

state <- tempfile("job-worker-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

make_session <- function(key, run_fn) {
    s <- corteza::new_session("cli")
    s$cwd <- tempdir()
    s$config <- list()
    s$job_key <- key
    s$job_worker_spec <- list(
        init_fn = function(spec) invisible(TRUE),
        run_fn = run_fn
    )
    s
}

# Pump until `id` settles or the deadline passes. Returns every event
# seen, so tests can check what the surface would have been told.
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
types <- function(events) vapply(events, function(e) e$type, "")

# A job that counts how many jobs this worker's global environment has
# seen. The count only goes up if state persists between jobs.
counting <- function(task) {
    n <- get0("jobs_seen", envir = globalenv(), ifnotfound = 0) + 1
    assign("jobs_seen", n, envir = globalenv())
    list(reply = sprintf("%s #%d", task, n), usage = list(input_tokens = 1L))
}

s <- make_session("room-a", counting)

# --- Submitting returns at once; the job runs in the worker ---
t0 <- Sys.time()
a <- corteza:::job_submit(s, "first")
expect_true(corteza:::job_read(a)$status %in% c("running", "done"))
ev <- pump_until(s, a)
j <- corteza:::job_read(a)
expect_identical(j$status, "done")
expect_identical(j$outcome$result, "first #1")
expect_identical(j$origin$session_key, "room-a")
expect_true("started" %in% types(ev))
expect_true("settled" %in% types(ev))
# The first worker had no checkpoint to restore.
expect_false("restored" %in% types(ev))
# The permissions recorded are the role's tools.
expect_true("write_file" %in% j$permissions$tools)

# --- Warm state persists between jobs in the same worker ---
b <- corteza:::job_submit(s, "second")
pump_until(s, b)
expect_identical(corteza:::job_read(b)$outcome$result, "second #2")

# --- A checkpoint was taken at the job boundary ---
cp_dir <- corteza:::job_worker_state_dir("room-a", "doer")
cp <- jsonlite::fromJSON(file.path(cp_dir, "checkpoint.json"))
expect_identical(cp$key, "room-a")
expect_identical(cp$job, b)
expect_true("jobs_seen" %in% cp$objects)
# Keyed per role: a reviewer for the same room gets its own directory.
expect_false(identical(cp_dir,
                       corteza:::job_worker_state_dir("room-a", "reviewer")))

# --- A dead worker is replaced, and the replacement restores ---
s$.job_worker$kill()
c_id <- corteza:::job_submit(s, "third")
ev <- pump_until(s, c_id)
expect_identical(corteza:::job_read(c_id)$outcome$result, "third #3")
restored <- Filter(function(e) identical(e$type, "restored"), ev)
expect_identical(length(restored), 1L)
expect_identical(restored[[1L]]$restore$job, b)
expect_true("jobs_seen" %in% restored[[1L]]$restore$objects)

# --- Jobs queue behind the running one ---
slow <- make_session("room-slow", function(task) {
    Sys.sleep(as.numeric(task))
    list(reply = task)
})
q1 <- corteza:::job_submit(slow, "1")
q2 <- corteza:::job_submit(slow, "0")
expect_identical(corteza:::job_read(q2)$status, "queued")
pump_until(slow, q1)
expect_identical(corteza:::job_read(q1)$status, "done")
pump_until(slow, q2)
expect_identical(corteza:::job_read(q2)$status, "done")

# --- A queued job cancels at once ---
q3 <- corteza:::job_submit(slow, "5")
q4 <- corteza:::job_submit(slow, "0")
expect_true(corteza:::job_cancel(slow, q4, by = "@troy:ex"))
expect_identical(corteza:::job_read(q4)$status, "cancelled")
expect_true(grepl("before it started", corteza:::job_read(q4)$outcome$reason))

# --- A running job cancels on the next pump, stopping the worker ---
t_cancel <- Sys.time()
expect_true(corteza:::job_cancel(slow, q3))
ev <- corteza:::job_pump(slow)
expect_identical(corteza:::job_read(q3)$status, "cancelled")
expect_true(as.numeric(difftime(Sys.time(), t_cancel, units = "secs")) < 5)
expect_false(corteza:::job_worker_alive(slow))
# Cancelling an ended job reports FALSE.
expect_false(corteza:::job_cancel(slow, q3))
# A job from another session cannot be cancelled from this one.
expect_error(corteza:::job_cancel(slow, a), "no job")

# --- A turn error is the job's outcome, and still checkpoints ---
failing <- make_session("room-fail", function(task) {
    assign("before_error", TRUE, envir = globalenv())
    stop("provider said no")
})
f <- corteza:::job_submit(failing, "boom")
pump_until(failing, f)
fj <- corteza:::job_read(f)
expect_identical(fj$status, "failed")
expect_true(grepl("provider said no", fj$outcome$error))
fcp <- jsonlite::fromJSON(file.path(
    corteza:::job_worker_state_dir("room-fail", "doer"), "checkpoint.json"))
expect_true("before_error" %in% fcp$objects)

# --- A worker that dies mid-job leaves the job indeterminate ---
dying <- make_session("room-die", function(task) {
    quit(save = "no", status = 1)
})
d <- corteza:::job_submit(dying, "die")
pump_until(dying, d)
dj <- corteza:::job_read(d)
expect_identical(dj$status, "indeterminate")
expect_true(grepl("may have acted", dj$outcome$reason))

# --- The wall-clock limit ends a job that runs too long ---
w <- corteza:::job_create("30", workspace = tempdir(),
                          origin = list(session_key = "room-slow"),
                          limits = list(wall_seconds = 1))
ev <- pump_until(slow, w, timeout = 15)
wj <- corteza:::job_read(w)
expect_identical(wj$status, "failed")
expect_true(grepl("wall-clock", wj$outcome$reason))

# --- A worker that cannot start fails the job cleanly ---
broken <- make_session("room-broken", counting)
broken$job_worker_spec$init_fn <- function(spec) stop("no provider key")
bad <- corteza:::job_submit(broken, "never runs")
bj <- corteza:::job_read(bad)
expect_identical(bj$status, "failed")
expect_true(grepl("no provider key", bj$outcome$error))
# It never reached a worker, so there is no dispatch record.
expect_null(bj$dispatch)

for (sess in list(s, slow, failing, dying, broken)) {
    corteza:::job_worker_close(sess)
}
if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(state, recursive = TRUE)
