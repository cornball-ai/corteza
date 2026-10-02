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
ident <- corteza:::job_worker_identity(s, "doer")
cp_dir <- corteza:::job_worker_state_dir(ident)
cp <- jsonlite::fromJSON(file.path(cp_dir, "checkpoint.json"))
expect_identical(cp$identity$key, "room-a")
expect_identical(cp$identity$owner, "local")
expect_identical(cp$identity$workspace, normalizePath(tempdir()))
expect_identical(cp$job, b)
expect_true("jobs_seen" %in% cp$objects)

# --- Checkpoints are isolated by owner, workspace, key, and role ---
# Each part alone gives a different directory: two bots in one room, two
# projects with the same key, a reviewer beside a doer.
differs <- function(changed) {
    !identical(cp_dir, corteza:::job_worker_state_dir(
        utils::modifyList(ident, changed)))
}
expect_true(differs(list(owner = "@codex:ex")))
expect_true(differs(list(workspace = "/some/other/project")))
expect_true(differs(list(key = "room-b")))
expect_true(differs(list(role = "reviewer")))
# The surface can separate the checkpoint identity from the job owner
# and key (the REPL does: its job owner is its pid).
s2 <- new.env()
s2$job_owner <- "local:host:123"
s2$job_key <- "repl"
s2$job_checkpoint_owner <- "local:host"
s2$job_checkpoint_key <- "repl:conversation-1"
s2$cwd <- tempdir()
id2 <- corteza:::job_worker_identity(s2)
expect_identical(id2$owner, "local:host")
expect_identical(id2$key, "repl:conversation-1")

# A checkpoint is restored only into the identity it recorded, even if
# it is found in that identity's directory.
local({
    other <- utils::modifyList(ident, list(owner = "@codex:ex"))
    other_dir <- corteza:::job_worker_state_dir(other)
    dir.create(other_dir, recursive = TRUE)
    file.copy(list.files(cp_dir, full.names = TRUE), other_dir)
    r <- callr::r(function(dir, identity) {
        get(".job_worker_child_restore", envir = asNamespace("corteza"))(
            dir, identity)
    }, list(dir = other_dir, identity = other))
    expect_false(r$restored)
    expect_true(isTRUE(r$mismatch))
    unlink(other_dir, recursive = TRUE)
})

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
    corteza:::job_worker_state_dir(corteza:::job_worker_identity(failing)),
    "checkpoint.json"))
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

# --- A hand-off that fails does not strand the job or its lock ---
# The worker's call() throws after the job is marked dispatched. The job
# must end (indeterminate: the worker may have received it), give back
# the checkout, and leave no current job for later pumps to wait on.
local({
    stuck <- make_session("room-handoff", counting)
    fake <- new.env()
    fake$is_alive <- function() TRUE
    fake$get_pid <- function() 1L
    fake$call <- function(...) stop("pipe closed")
    fake$close <- function() invisible(NULL)
    stuck$.job_worker <- fake
    stuck$.job_worker_role <- "doer"
    # As job_worker_start() records for a worker it starts. Without the
    # note the worker is not known to be in the session's directory and
    # is replaced rather than reused.
    corteza:::job_worker_note(stuck, "doer",
                              dir = corteza:::job_worker_dir(stuck))
    h <- corteza:::job_submit(stuck, "never runs")
    hj <- corteza:::job_read(h)
    expect_identical(hj$status, "indeterminate")
    expect_true(grepl("pipe closed", hj$outcome$error))
    expect_null(stuck$.job_current)
    expect_null(stuck$.job_worker)
    expect_null(corteza:::job_lock_holder(corteza:::job_checkout(tempdir())))
    # The session is usable again: the next job gets a real worker.
    nxt <- corteza:::job_submit(stuck, "after")
    pump_until(stuck, nxt)
    expect_identical(corteza:::job_read(nxt)$status, "done")
    corteza:::job_worker_close(stuck)
})

# --- The approval bridge, end to end through a worker ---
# The job asks the way a tool call under an "ask" verdict would: through
# the worker's approval callback, which writes a request and waits.
asking <- function(task) {
    ask <- get(".job_worker_child_ask", envir = asNamespace("corteza"))
    ok <- ask(list(tool = "bash", args = list(cmd = task)),
              list(reason = "code/exec/matrix"))
    list(reply = if (isTRUE(ok)) "approved" else "declined")
}
asker <- make_session("room-ask", asking)

wait_for <- function(s, type, timeout = 15) {
    deadline <- Sys.time() + timeout
    repeat {
        ev <- Filter(function(e) identical(e$type, type),
                     corteza:::job_pump(s))
        if (length(ev) || Sys.time() > deadline) {
            return(ev)
        }
        Sys.sleep(0.1)
    }
}

# Approved: the event carries the request, the answer releases the job.
ap <- corteza:::job_submit(asker, "ls")
ev <- wait_for(asker, "approval")
expect_identical(length(ev), 1L)
req <- ev[[1L]]$request
expect_identical(req$tool, "bash")
expect_identical(req$args$cmd, "ls")
expect_identical(ev[[1L]]$job$id, ap)
# While the worker waits, the loop does not: pumping returns at once
# and reports the same request only once.
t_pump <- Sys.time()
expect_identical(length(Filter(function(e) identical(e$type, "approval"),
                               corteza:::job_pump(asker))), 0L)
expect_true(as.numeric(difftime(Sys.time(), t_pump, units = "secs")) < 1)
expect_true(corteza:::job_answer(asker, ap, req$id, TRUE, by = "@troy:ex"))
pump_until(asker, ap)
expect_identical(corteza:::job_read(ap)$outcome$result, "approved")

# Denied.
dn <- corteza:::job_submit(asker, "rm -rf /")
req <- wait_for(asker, "approval")[[1L]]$request
expect_true(corteza:::job_answer(asker, dn, req$id, FALSE, by = "@troy:ex"))
pump_until(asker, dn)
expect_identical(corteza:::job_read(dn)$outcome$result, "declined")

# Cancelled while waiting: the job ends, and the approval that arrives
# afterwards is refused rather than resuming anything.
cx <- corteza:::job_submit(asker, "sleep")
req <- wait_for(asker, "approval")[[1L]]$request
corteza:::job_cancel(asker, cx)
corteza:::job_pump(asker)
expect_identical(corteza:::job_read(cx)$status, "cancelled")
expect_false(corteza:::job_answer(asker, cx, req$id, TRUE))
# Another session cannot answer this session's request.
expect_error(corteza:::job_answer(s, cx, req$id, TRUE), "no job")
# Nor can another owner's session with the same key: two bots in one
# room share the key.
twin <- make_session("room-ask", asking)
twin$job_owner <- "@codex:ex"
expect_error(corteza:::job_answer(twin, cx, req$id, TRUE), "no job")
expect_error(corteza:::job_cancel(twin, ap), "no job")

# --- The real init installs the bridge and the originating channel ---
# No provider is called: init builds a session object, and the run
# function only inspects it.
probe <- make_session("room-probe", function(task) {
    st <- get(".subagent_state", envir = asNamespace("corteza"))
    ask <- get(".job_worker_child_ask", envir = asNamespace("corteza"))
    list(reply = paste(identical(st$session$approval_cb, ask),
                       st$session$channel))
})
probe$job_worker_spec$init_fn <- NULL
probe$channel <- "matrix"
pr <- corteza:::job_submit(probe, "check")
pump_until(probe, pr)
expect_identical(corteza:::job_read(pr)$outcome$result, "TRUE matrix")

# --- A doer starts with the project's context; a reviewer does not ---
# The task text is all a talker hands over. The project's rules have to
# be in the doer's own prompt, read from the directory it works in, with
# its role ahead of them.
proj <- tempfile("job-project")
dir.create(proj)
writeLines(c("# Agent Instructions", "", "doer-context-sentinel"),
           file.path(proj, "AGENTS.md"))
ctxprobe <- make_session("room-context", function(task) {
    st <- get(".subagent_state", envir = asNamespace("corteza"))
    sys <- paste(st$session$system, collapse = "\n")
    at <- function(x) regexpr(x, sys, fixed = TRUE)[[1L]]
    list(reply = paste(at("doer-context-sentinel") > 0,
                       at("You are the doer") == 1L,
                       at("You are the doer") < at("doer-context-sentinel")))
})
ctxprobe$job_worker_spec$init_fn <- NULL
ctxprobe$cwd <- proj
cp <- corteza:::job_submit(ctxprobe, "check")
pump_until(ctxprobe, cp)
expect_identical(corteza:::job_read(cp)$outcome$result, "TRUE TRUE TRUE")
# Negative control: with the context switched off in the spec, the same
# worker in the same directory has only its role.
corteza:::job_worker_close(ctxprobe)
bare <- make_session("room-bare", function(task) {
    st <- get(".subagent_state", envir = asNamespace("corteza"))
    sys <- paste(st$session$system, collapse = "\n")
    list(reply = paste(grepl("doer-context-sentinel", sys, fixed = TRUE),
                       grepl("You are the doer", sys, fixed = TRUE)))
})
bare$job_worker_spec$init_fn <- NULL
bare$job_worker_spec$project_context <- FALSE
bare$cwd <- proj
bp <- corteza:::job_submit(bare, "check")
pump_until(bare, bp)
expect_identical(corteza:::job_read(bp)$outcome$result, "FALSE TRUE")
expect_true(corteza:::job_worker_spec(ctxprobe)$project_context)
expect_false(corteza:::job_worker_spec(ctxprobe, "reviewer")$project_context)
# The helper itself: role first, project text after, and the role alone
# is never lost.
jc <- corteza:::job_worker_context("ROLE-LINE", cwd = proj)
expect_true(startsWith(jc, "ROLE-LINE"))
expect_true(grepl("doer-context-sentinel", jc, fixed = TRUE))
corteza:::job_worker_close(bare)
unlink(proj, recursive = TRUE)

# --- The job, the session, and the worker name one directory ---
# A worker is a process with a working directory of its own. If the
# session's directory changes, a live worker is still in the old one.
where <- function(task) list(reply = normalizePath(getwd()))
dir_a <- tempfile("job-dir-a")
dir_b <- tempfile("job-dir-b")
dir.create(dir_a)
dir.create(dir_b)
mover <- make_session("room-mover", where)
mover$cwd <- dir_a
m1 <- corteza:::job_submit(mover, "where")
pump_until(mover, m1)
expect_identical(corteza:::job_read(m1)$outcome$result, normalizePath(dir_a))
pid_a <- mover$.job_worker$get_pid()
# Same directory: the live worker is reused.
m2 <- corteza:::job_submit(mover, "where")
pump_until(mover, m2)
expect_identical(mover$.job_worker$get_pid(), pid_a)
# A job queued for one directory is not run after the session moved to
# another: nothing reaches a worker, and the record says why.
m3 <- corteza:::job_create("where", workspace = dir_a,
                           origin = list(session_key = "room-mover"))
expect_identical(corteza:::job_read(m3)$status, "queued")
mover$cwd <- dir_b
pump_until(mover, m3)
r3 <- corteza:::job_read(m3)
expect_identical(r3$status, "failed")
expect_true(grepl("nothing was run", r3$outcome$error, fixed = TRUE))
expect_true(grepl(normalizePath(dir_a), r3$outcome$error, fixed = TRUE))
expect_null(r3$dispatch)
# A job queued after the move runs in the new directory, in a new
# process: the worker left in the old directory is not reused.
m4 <- corteza:::job_submit(mover, "where")
pump_until(mover, m4)
expect_identical(corteza:::job_read(m4)$outcome$result, normalizePath(dir_b))
expect_false(identical(mover$.job_worker$get_pid(), pid_a))
expect_identical(mover$.job_worker_notes$doer$dir, normalizePath(dir_b))

# --- A session replaced mid-job hands its jobs to the replacement ---
# /clear replaces the room's session. The ledger finds jobs by key and
# the pump runs them through whichever session holds it, so the job in
# flight and its worker have to move too.
gate <- tempfile("job-gate")
waiter <- function(task) {
    gate <- Sys.getenv("JOB_TEST_GATE")
    deadline <- Sys.time() + 20
    while (!file.exists(gate) && Sys.time() < deadline) Sys.sleep(0.05)
    list(reply = "finished after the session was replaced")
}
Sys.setenv(JOB_TEST_GATE = gate)
first <- make_session("room-replaced", waiter)
running <- corteza:::job_submit(first, "long job")
for (i in 1:50) {
    corteza:::job_pump(first)
    if (identical(corteza:::job_read(running)$status, "running")) break
    Sys.sleep(0.1)
}
expect_identical(corteza:::job_read(running)$status, "running")
second <- make_session("room-replaced", waiter)
worker_pid <- first$.job_worker$get_pid()
corteza:::job_state_move(first, second)
expect_null(first$.job_worker)
expect_null(first$.job_current)
expect_identical(second$.job_current, running)
expect_identical(second$.job_worker$get_pid(), worker_pid)
file.create(gate)
seen <- pump_until(second, running)
expect_identical(corteza:::job_read(running)$status, "done")
expect_true("settled" %in% types(seen))
Sys.unsetenv("JOB_TEST_GATE")
unlink(c(gate, dir_a, dir_b), recursive = TRUE)
corteza:::job_worker_close_all(mover)
corteza:::job_worker_close_all(second)

for (sess in list(s, slow, failing, dying, broken, asker, probe)) {
    corteza:::job_worker_close(sess)
}
if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(state, recursive = TRUE)
