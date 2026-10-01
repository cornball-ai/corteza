library(tinytest)

# Delegated jobs in the REPL: set up only in talker mode, pumped before
# each prompt, approvals asked inline, /jobs and /cancel, and jobs
# stopped on exit. Driven through run_repl_loop() with scripted input,
# against a private state directory. No provider is called.

state <- tempfile("repl-jobs-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

scripted_input <- function(lines, seen = NULL) {
    i <- 0L
    function(prompt_str) {
        if (!is.null(seen)) {
            seen$prompts <- c(seen$prompts, prompt_str)
        }
        i <<- i + 1L
        if (i <= length(lines)) lines[[i]] else character(0)
    }
}
palette <- list(dim = "", reset = "", cyan = "", bold = "", yellow = "",
                green = "", bright_magenta = "", red = "", magenta = "")

# Each context gets its own directory. A real REPL is one session per
# process; here many share a process, and a shared directory would be a
# shared checkout lock.
make_ctx <- function(lines, run_fn, talker = TRUE, seen = NULL) {
    s <- corteza::new_session("cli", provider = "anthropic",
                              model_map = list(cloud = "claude-opus-5-5"))
    cwd <- tempfile("repl-cwd")
    dir.create(cwd)
    s$cwd <- normalizePath(cwd)
    s$config <- if (talker) list(talker = list(enabled = TRUE)) else list()
    s$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE),
                              run_fn = run_fn)
    ctx <- new.env(parent = emptyenv())
    ctx$session <- s
    ctx$config <- s$config
    ctx$cwd <- s$cwd
    ctx$ws_enabled <- FALSE
    ctx$palette <- palette
    ctx$read_input <- scripted_input(lines, seen)
    ctx$help_text <- function() "HELP"
    ctx$handle_copy <- function(x) invisible(NULL)
    ctx$format_tools <- function(s) "TOOLS"
    ctx$pending_r_context <- character(0)
    ctx$last_assistant_response <- ""
    ctx
}
# Set up jobs, then give the session a key of its own: every context in
# this process has the same owner (the pid), and the REPL's single
# "repl" key would let one test's exit cancel another's jobs.
key_n <- 0L
setup <- function(ctx) {
    out <- capture.output(corteza:::.repl_jobs_setup(ctx))
    key_n <<- key_n + 1L
    ctx$session$job_key <- sprintf("repl-%d", key_n)
    invisible(out)
}
wait_status <- function(id, statuses, timeout = 20) {
    deadline <- Sys.time() + timeout
    while (!corteza:::job_read(id)$status %in% statuses &&
           Sys.time() < deadline) {
        Sys.sleep(0.1)
    }
    corteza:::job_read(id)$status
}

# --- Talker mode off: nothing is set up, /jobs says why ---
local({
    ctx <- make_ctx(c("/jobs"), function(task) list(reply = "x"),
                    talker = FALSE)
    out <- capture.output(corteza:::run_repl_loop(ctx))
    expect_true(any(grepl("talker mode is off", out)))
    expect_null(ctx$session$job_key)
    expect_false(isTRUE(ctx$session$talker))
})

# --- Setup: talker on, process owner, repl key ---
local({
    ctx <- make_ctx(character(), function(task) list(reply = "x"))
    capture.output(corteza:::.repl_jobs_setup(ctx))
    s <- ctx$session
    expect_true(isTRUE(s$talker))
    expect_identical(s$job_key, "repl")
    expect_identical(s$job_owner, corteza:::job_local_owner())
    expect_true(grepl(paste0(":", Sys.getpid(), "$"), s$job_owner))
    expect_identical(s$doer_model, "claude-opus-5-5")
})

# --- A REPL's saved workspace belongs to its conversation ---
# Resuming the same conversation in a new process finds the same
# checkpoint; another conversation open at the same time, in the same
# directory, does not.
local({
    ident <- function(conversation, cwd) {
        ctx <- make_ctx(character(), function(task) list(reply = "x"))
        ctx$session$cwd <- cwd
        ctx$disk_session <- list(sessionId = conversation)
        capture.output(corteza:::.repl_jobs_setup(ctx))
        corteza:::job_worker_identity(ctx$session)
    }
    dir <- tempfile("shared-proj")
    dir.create(dir)
    a1 <- ident("conv-a", dir)
    a2 <- ident("conv-a", dir)
    b <- ident("conv-b", dir)
    expect_identical(a1, a2)
    expect_false(identical(corteza:::job_worker_state_dir(a1),
                           corteza:::job_worker_state_dir(b)))
    expect_false(grepl(as.character(Sys.getpid()), a1$owner, fixed = TRUE))
})

# --- A finished job is shown before the next prompt, into history ---
local({
    ctx <- make_ctx(character(), function(task) list(reply = "report ready"))
    setup(ctx)
    id <- corteza:::job_submit(ctx$session, "write the report")
    wait_status(id, "running")
    Sys.sleep(1)
    # The loop pumps before reading; EOF right after.
    out <- capture.output(corteza:::run_repl_loop(ctx))
    if (!any(grepl("report ready", out))) {
        # A slow machine: one more pump.
        Sys.sleep(1)
        out <- c(out, capture.output(corteza:::.repl_pump_jobs(ctx)))
    }
    expect_true(any(grepl("report ready", out)))
    expect_true(any(grepl(paste("Job", id, "done"), out, fixed = TRUE)))
    last <- ctx$session$history[[length(ctx$session$history)]]
    expect_true(grepl("report ready", last$content))
})

# --- Approvals are asked inline at the prompt ---
asking <- function(task) {
    ask <- get(".job_worker_child_ask", envir = asNamespace("corteza"))
    ok <- ask(list(tool = "bash", args = list(cmd = task)),
              list(reason = "code/exec/cli"))
    list(reply = if (isTRUE(ok)) "approved" else "declined")
}
for (answer in c("y", "n")) {
    local({
        seen <- new.env()
        ctx <- make_ctx(c(answer), asking, seen = seen)
        setup(ctx)
        id <- corteza:::job_submit(ctx$session, "make check")
        deadline <- Sys.time() + 15
        while (!length(corteza:::job_approval_pending(id)) &&
               Sys.time() < deadline) {
            Sys.sleep(0.1)
        }
        out <- capture.output(corteza:::.repl_pump_jobs(ctx))
        expect_true(any(grepl("Approval needed: bash", out)))
        expect_true("Approve? [y/N] " %in% seen$prompts)
        # The outcome is written by the owner's pump, not the worker.
        deadline <- Sys.time() + 15
        while (!corteza:::job_read(id)$status %in%
               corteza:::JOB_STATUSES_FINAL && Sys.time() < deadline) {
            capture.output(corteza:::.repl_pump_jobs(ctx))
            Sys.sleep(0.1)
        }
        expect_identical(corteza:::job_read(id)$outcome$result,
                         if (answer == "y") "approved" else "declined")
        corteza:::job_worker_close(ctx$session)
    })
}

# --- /cancel stops a running job; /jobs lists it ---
local({
    ctx <- make_ctx(character(), function(task) {
        Sys.sleep(30)
        list(reply = "late")
    })
    setup(ctx)
    id <- corteza:::job_submit(ctx$session, "long job")
    ctx$read_input <- scripted_input(c(paste("/cancel", id), "/jobs"))
    out <- capture.output(corteza:::run_repl_loop(ctx))
    expect_true(any(grepl("Cancellation requested", out)))
    expect_identical(corteza:::job_read(id)$status, "cancelled")
    # /jobs lists it with its status.
    listed <- out[grepl(id, out, fixed = TRUE) & grepl("cancelled", out)]
    expect_true(length(listed) >= 1L)
    expect_true(any(grepl("Usage", capture.output(
        corteza:::.repl_cmd_cancel(ctx, "/cancel")))))
})

# --- Exiting stops open jobs with a known outcome ---
local({
    ctx <- make_ctx(character(), function(task) {
        Sys.sleep(30)
        list(reply = "late")
    })
    setup(ctx)
    running <- corteza:::job_submit(ctx$session, "running job")
    queued <- corteza:::job_submit(ctx$session, "queued job")
    out <- capture.output(corteza:::run_repl_loop(ctx))  # EOF
    expect_true(any(grepl("Stopped 2 jobs on exit", out)))
    expect_identical(corteza:::job_read(running)$status, "cancelled")
    expect_identical(corteza:::job_read(queued)$status, "cancelled")
    expect_false(corteza:::job_worker_alive(ctx$session))
})

# --- Jobs left by an exited corteza process are recovered ---
local({
    p <- processx::process$new("true")
    p$wait()
    dead <- sprintf("local:%s:%d", Sys.info()[["nodename"]], p$get_pid())
    ctx <- make_ctx(character(), function(task) list(reply = "x"))
    lost <- corteza:::job_create("lost job", owner = dead,
                                 workspace = ctx$cwd,
                                 origin = list(session_key = "repl"))
    corteza:::job_mark_dispatched(lost)
    waiting <- corteza:::job_create("never started", owner = dead,
                                    workspace = ctx$cwd,
                                    origin = list(session_key = "repl"))
    # A live process's job, and another host's, are left alone.
    live <- corteza:::job_create("live job", owner = corteza:::job_local_owner(),
                                 origin = list(session_key = "other"))
    corteza:::job_mark_dispatched(live)
    remote <- corteza:::job_create("remote job", owner = "local:elsewhere:1",
                                   origin = list(session_key = "repl"))
    corteza:::job_mark_dispatched(remote)
    if (.Platform$OS.type != "windows") {
        out <- capture.output(corteza:::.repl_jobs_setup(ctx))
        expect_identical(corteza:::job_read(lost)$status, "indeterminate")
        expect_identical(corteza:::job_read(waiting)$status, "cancelled")
        expect_identical(corteza:::job_read(live)$status, "running")
        expect_identical(corteza:::job_read(remote)$status, "running")
        expect_true(any(grepl(lost, out, fixed = TRUE)))
    }
    corteza:::job_settle(live, "done")
    corteza:::job_settle(remote, "done")
})

if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(state, recursive = TRUE)
