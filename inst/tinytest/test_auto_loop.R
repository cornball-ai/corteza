library(tinytest)

# Drive run_auto_loop() end to end with no network: the turn function is
# a stub, and the monitor is stubbed by overriding the package's query
# path. What is being tested is the loop's control flow -- which stop
# condition fires, whether the monitor is consulted at all, and whether
# the session is left clean afterwards.

# Redirect session/transcript writes into temp.
old_home <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
tmp_data <- file.path(tempdir(), "corteza-auto-loop-data")
dir.create(tmp_data, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(R_USER_DATA_DIR = tmp_data)

wt <- file.path(tempdir(), "corteza-auto-loop-wt")

reset_wt <- function() {
    unlink(wt, recursive = TRUE)
    dir.create(wt, recursive = TRUE, showWarnings = FALSE)
    writeLines("start", file.path(wt, "a.txt"))
}

empty_palette <- list(dim = "", reset = "", cyan = "", bold = "",
                      yellow = "", green = "", bright_magenta = "",
                      red = "", magenta = "")

# A ctx shaped like chat()'s, with the turn function stubbed.
auto_ctx <- function(turn_fn) {
    ctx <- new.env(parent = emptyenv())
    ctx$ws_enabled <- FALSE
    ctx$palette <- empty_palette
    ctx$help_text <- function() "HELP"
    ctx$handle_copy <- function(x) invisible(NULL)
    ctx$format_tools <- function(s) "TOOLS"
    ctx$pending_r_context <- character(0)
    ctx$last_assistant_response <- ""
    ctx$cwd <- wt
    ctx$config <- list()
    ctx$provider <- "anthropic"
    ctx$model <- "test-model"
    ctx$render_reply <- function(txt) invisible(NULL)
    ctx$turn_fn <- turn_fn
    ctx$session <- corteza::new_session("console")
    ctx$session$turn_number <- 0L
    ctx$disk_session <- list(sessionId = "auto-test",
                             session = corteza:::session_new("anthropic",
                                                             "test-model", wt))
    ctx
}

# Stub the two package functions run_auto_loop reaches out through, so
# no subagent is spawned and no API key is needed.
stub_monitor <- function(verdicts) {
    i <- 0L
    list(
         spawn = function(goal, session = NULL, config = list()) "mon-stub",
         progress = function(id, goal, reply, diff, loop = 1L, max_loops = 10L,
                             request_id = NULL, timeout = 120L) {
        i <<- i + 1L
        v <- if (i <= length(verdicts)) verdicts[[i]] else "continue"
        list(verdict = v, reason = sprintf("stub verdict %d", i))
    })
}

with_stubs <- function(stubs, expr) {
    ns <- asNamespace("corteza")
    old_spawn <- get("monitor_spawn", envir = ns)
    old_prog <- get("monitor_ask_progress", envir = ns)
    old_kill <- get("subagent_kill", envir = ns)
    unlockBinding("monitor_spawn", ns)
    unlockBinding("monitor_ask_progress", ns)
    unlockBinding("subagent_kill", ns)
    assign("monitor_spawn", stubs$spawn, envir = ns)
    assign("monitor_ask_progress", stubs$progress, envir = ns)
    assign("subagent_kill", function(id) TRUE, envir = ns)
    on.exit({
        assign("monitor_spawn", old_spawn, envir = ns)
        assign("monitor_ask_progress", old_prog, envir = ns)
        assign("subagent_kill", old_kill, envir = ns)
    }, add = TRUE)
    force(expr)
}

# ---- stops at max_loops ----

reset_wt()
turns <- 0L
edit_turn <- function(prompt, session) {
    turns <<- turns + 1L
    writeLines(as.character(turns), file.path(wt, sprintf("f%d.txt", turns)))
    session$turn_number <- (session$turn_number %||% 0L) + 2L
    list(reply = "working", session = session,
         usage = list(cost = 0.01, total_tokens = 100L))
}

ctx <- auto_ctx(edit_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "do a thing",
                                                         max_loops = 3L)))
expect_true(any(grepl("max_loops", res)))
expect_equal(turns, 3L)

# The session is handed back in its attended shape. A leftover gate
# would refuse or escalate every later tool call, which reads as corteza
# being broken rather than a run having ended.
expect_null(ctx$session$auto_gate)
expect_null(ctx$auto_halt)

# ---- stops when the monitor says stop ----

reset_wt()
turns <- 0L
ctx <- auto_ctx(edit_turn)
res <- with_stubs(stub_monitor(c("stop")),
                  capture.output(corteza:::run_auto_loop(ctx, "do a thing",
                                                         max_loops = 10L)))
expect_true(any(grepl("monitor said stop", res)))
# One turn ran, then the monitor ended it.
expect_equal(turns, 1L)

# ---- stops when the monitor escalates ----

reset_wt()
turns <- 0L
ctx <- auto_ctx(edit_turn)
res <- with_stubs(stub_monitor(c("escalate")),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
expect_true(any(grepl("monitor said escalate", res)))
expect_equal(turns, 1L)

# ---- stalls out when nothing changes on disk ----

reset_wt()
turns <- 0L
idle_turn <- function(prompt, session) {
    turns <<- turns + 1L
    list(reply = "thinking about it", session = session,
         usage = list(cost = 0.01, total_tokens = 100L))
}
ctx <- auto_ctx(idle_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 20L)))
expect_true(any(grepl("nothing changed", res)))
# Default stall_loops is 2, so it gives up well before max_loops.
expect_true(turns < 10L)

# ---- a worker claiming "done" does not by itself stop the run ----
#
# AUTO_STATUS is evidence for the monitor, not the stop authority. With
# the monitor still saying continue, the run keeps going.

reset_wt()
turns <- 0L
done_turn <- function(prompt, session) {
    turns <<- turns + 1L
    writeLines(as.character(turns), file.path(wt, sprintf("d%d.txt", turns)))
    list(reply = "AUTO_STATUS: done\nall finished", session = session,
         usage = list(cost = 0.01, total_tokens = 100L))
}
ctx <- auto_ctx(done_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 3L)))
expect_equal(turns, 3L)
expect_true(any(grepl("max_loops", res)))
expect_true(any(grepl("worker reported done", res)))

# And when the monitor agrees, it stops.
reset_wt()
turns <- 0L
ctx <- auto_ctx(done_turn)
res <- with_stubs(stub_monitor(c("stop")),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
expect_equal(turns, 1L)

# ---- an escalation raised mid-turn halts the run ----
#
# The gate raises corteza_auto_escalate from inside turn(); the loop's
# handler records it on ctx and the driver stops rather than feeding
# another prompt. Without the handler ordering in run_repl_loop() this
# would be caught as a plain interrupt and the run would continue.

reset_wt()
turns <- 0L
escalating_turn <- function(prompt, session) {
    turns <<- turns + 1L
    if (turns == 2L) {
        stop(corteza:::auto_escalate_condition("touched something odd",
                                               "write_file"))
    }
    writeLines("x", file.path(wt, sprintf("e%d.txt", turns)))
    list(reply = "working", session = session,
         usage = list(cost = 0.01, total_tokens = 100L))
}
ctx <- auto_ctx(escalating_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
expect_equal(turns, 2L)
expect_true(any(grepl("escalated", res)))
expect_true(any(grepl("touched something odd", res)))
# Cleared on the way out, so a later attended turn is unaffected.
expect_null(ctx$auto_halt)

# ---- spend cap ----

reset_wt()
turns <- 0L
pricey_turn <- function(prompt, session) {
    turns <<- turns + 1L
    writeLines(as.character(turns), file.path(wt, sprintf("p%d.txt", turns)))
    list(reply = "working", session = session,
         usage = list(cost = 10, total_tokens = 100L))
}
ctx <- auto_ctx(pricey_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 50L)))
expect_true(any(grepl("spend cap", res)))
expect_true(turns <= 2L)

# ---- /auto is wired into the REPL ----
#
# A bare /auto prints usage and must not spawn anything. The monitor is
# left unstubbed here on purpose: if the command tried to start a run
# without a goal, this would attempt a real subagent spawn and fail
# loudly rather than passing quietly.

reset_wt()
ctx <- auto_ctx(function(prompt, session) {
    stop("turn_fn must not be reached for a bare /auto")
})
scripted <- function(lines) {
    i <- 0L
    function(p) {
        i <<- i + 1L
        if (i <= length(lines)) lines[[i]] else character(0)
    }
}
ctx$read_input <- scripted(c("/auto"))
out <- capture.output(corteza:::run_repl_loop(ctx))
expect_true(any(grepl("Usage: /auto", out)))
expect_null(ctx$session$auto_gate)

# And a goal reaches run_auto_loop with the flags parsed.
reset_wt()
turns <- 0L
ctx <- auto_ctx(edit_turn)
ctx$read_input <- scripted(c("/auto --loops 2 make some files"))
out <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_repl_loop(ctx)))
expect_equal(turns, 2L)
expect_true(any(grepl("goal: make some files", out)))
expect_true(any(grepl("max_loops \\(2\\)", out)))

# The outer REPL is handed back intact: read_input restored, so the
# session keeps working after the run rather than being stuck on the
# driver's injected reader.
expect_null(ctx$session$auto_gate)
expect_true(is.function(ctx$read_input))

unlink(c(wt, tmp_data), recursive = TRUE)
if (is.na(old_home)) {
    Sys.unsetenv("R_USER_DATA_DIR")
} else {
    Sys.setenv(R_USER_DATA_DIR = old_home)
}
