# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# A stop that reaches a running tool (R/tool-stop.R, WP26 stage 2):
# bash and run_r_script end their process tree, and run_r interrupts its
# worker, ending it when it does not come back.

ns <- asNamespace("corteza")

# ---- The session a waiting tool asks about ----------------------------------
local({
    expect_false(ns$.tool_stop_requested())
    quiet <- new.env()
    expect_false(ns$.tool_stop_with(quiet, ns$.tool_stop_requested()))
    asked <- new.env()
    asked$stop_requested <- function() TRUE
    expect_true(ns$.tool_stop_with(asked, ns$.tool_stop_requested()))
    # The inner call's session, then the outer's again, then none.
    expect_identical(ns$.tool_stop_with(quiet, c(
        ns$.tool_stop_with(asked, ns$.tool_stop_requested()),
        ns$.tool_stop_requested())), c(TRUE, FALSE))
    expect_false(ns$.tool_stop_requested())
    # A stop function that fails says no.
    broken <- new.env()
    broken$stop_requested <- function() stop("no")
    expect_false(ns$.tool_stop_with(broken, ns$.tool_stop_requested()))
})

# What a process wrote, as text; a NUL byte is dropped.
local({
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    writeBin(as.raw(c(0x61, 0x00, 0x62, 0x0a)), f)
    expect_identical(ns$.tool_read_output(f), "ab\n")
    expect_identical(ns$.tool_read_output(tempfile()), "")
})

# ---- Real processes ---------------------------------------------------------
# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# A session whose stop is asked for once `after` seconds have passed
# from the first time it is asked.
stopper <- function(after) {
    s <- new.env()
    start <- NULL
    s$stop_requested <- function() {
        if (is.null(start)) {
            start <<- Sys.time()
        }
        as.numeric(difftime(Sys.time(), start, units = "secs")) > after
    }
    s
}
seconds <- function(expr) {
    t0 <- Sys.time()
    force(expr)
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
}
text_of <- function(res) res$content[[1L]]$text

if (.Platform$OS.type != "windows") {
    # A stopped command ends with its children, and what it wrote stays
    # in the result.
    local({
        pidfile <- tempfile()
        on.exit(unlink(pidfile), add = TRUE)
        res <- NULL
        took <- seconds(res <- ns$.tool_stop_with(stopper(0.5), ns$tool_bash(
            sprintf("echo started; sleep 30 & echo $! > %s; wait",
                    shQuote(pidfile)), timeout = 60L)))
        expect_true(took < 10)
        expect_true(isTRUE(res$isError))
        expect_true(grepl("cancelled", text_of(res), fixed = TRUE))
        expect_true(grepl("started", text_of(res), fixed = TRUE))
        pid <- as.integer(readLines(pidfile))
        Sys.sleep(0.3)
        expect_false(isTRUE(tools::pskill(pid, signal = 0L)))
    })
    # The deadline, as before: what the command wrote, after the note.
    local({
        res <- NULL
        took <- seconds(res <- ns$tool_bash("echo partial; sleep 30", timeout = 1L))
        expect_true(took < 10)
        expect_true(isTRUE(res$isError))
        expect_true(startsWith(text_of(res), "[timed out after 1s]"))
        expect_true(grepl("partial", text_of(res), fixed = TRUE))
    })
    # Output on stderr comes with stdout's.
    local({
        res <- ns$tool_bash("echo out; echo err 1>&2")
        expect_false(isTRUE(res$isError))
        expect_true(grepl("out", text_of(res), fixed = TRUE))
        expect_true(grepl("err", text_of(res), fixed = TRUE))
    })
}

# run_r_script: a stop ends the script's process; the deadline is as
# before.
local({
    res <- NULL
    took <- seconds(res <- ns$.tool_stop_with(stopper(0.5),
        ns$tool_run_r_script("Sys.sleep(30)", timeout = 60L)))
    expect_true(took < 10)
    expect_true(isTRUE(res$isError))
    expect_true(grepl("cancelled", text_of(res), fixed = TRUE))
    took <- seconds(res <- ns$tool_run_r_script("Sys.sleep(30)", timeout = 1L))
    expect_true(took < 10)
    expect_identical(text_of(res), "Error: timed out after 1s")
})

# run_r in its worker: a stop interrupts it, and the objects stay. Code
# deaf to the interrupt has its worker ended after the grace period; the
# objects go, the result says so, and the next run_r starts a new worker.
local({
    s <- new.env()
    s$config <- list(run_r_mode = "worker")
    s$cwd <- tempdir()
    on.exit(ns$.run_r_worker_close(s), add = TRUE)
    ctx <- list(session = s, cwd = tempdir())
    made <- ns$.tool_run_r_session("kept <- 42", NULL, ctx)
    expect_false(isTRUE(made$isError))
    first <- s$.run_r_worker_generation
    res <- NULL
    took <- seconds(res <- ns$.tool_stop_with(stopper(0.5),
        ns$.tool_run_r_session("Sys.sleep(30)", 60, ctx)))
    expect_true(took < 10)
    expect_true(isTRUE(res$isError))
    if (.Platform$OS.type == "windows" &&
        identical(res$execution$status, "killed")) {
        # A worker on Windows with no console of its own takes no
        # interrupt, so the grace period ends it, as for the deaf code
        # below. No surface there sets a stop function in this release:
        # corteza acp is refused on Windows.
        expect_false(res$execution$state_retained)
        expect_true(grepl("R objects are lost", text_of(res), fixed = TRUE))
        made <- ns$.tool_run_r_session("kept <- 42", NULL, ctx)
        expect_false(isTRUE(made$isError))
    } else {
        expect_identical(res$execution$status, "cancelled")
        expect_true(res$execution$state_retained)
        expect_true(grepl("42", text_of(ns$.tool_run_r_session("kept", NULL, ctx)),
                          fixed = TRUE))
    }
    took <- seconds(res <- ns$.tool_stop_with(stopper(0.5),
        ns$.tool_run_r_session(
            "repeat tryCatch(Sys.sleep(60), interrupt = function(e) NULL)",
            60, ctx)))
    expect_true(took >= 5 && took < 20)
    expect_identical(res$execution$status, "killed")
    expect_false(res$execution$state_retained)
    expect_true(grepl("R objects are lost", text_of(res), fixed = TRUE))
    after <- ns$.tool_run_r_session("exists('kept')", NULL, ctx)
    expect_true(grepl("FALSE", text_of(after), fixed = TRUE))
    expect_true(s$.run_r_worker_generation > first)
})
