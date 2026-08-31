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

# ---- an errored turn is terminal ----
#
# An attended session prints the error and lets the user decide. An
# unattended one has nobody to decide, and ctx$last_assistant_response
# still holds the PREVIOUS successful turn's reply -- so without an
# explicit outcome the monitor is shown a stale reply, plausibly says
# continue, and the run spins on a turn that keeps failing identically.

reset_wt()
turns <- 0L
flaky_turn <- function(prompt, session) {
    turns <<- turns + 1L
    if (turns == 2L) {
        stop("the model exploded")
    }
    writeLines("x", file.path(wt, sprintf("g%d.txt", turns)))
    list(reply = "working", session = session,
         usage = list(cost = 0.01, total_tokens = 100L))
}
ctx <- auto_ctx(flaky_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
expect_equal(turns, 2L)
expect_true(any(grepl("turn errored", res)))
expect_true(any(grepl("the model exploded", res)))

# ---- an interrupted turn is terminal ----
#
# Ctrl+C is the operator stopping the run. It must stop the run, not
# just the turn inside it.

reset_wt()
turns <- 0L
interrupted_turn <- function(prompt, session) {
    turns <<- turns + 1L
    if (turns == 2L) {
        stop(structure(class = c("interrupt", "condition"),
                       list(message = "", call = NULL)))
    }
    writeLines("x", file.path(wt, sprintf("i%d.txt", turns)))
    list(reply = "working", session = session,
         usage = list(cost = 0.01, total_tokens = 100L))
}
ctx <- auto_ctx(interrupted_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
expect_equal(turns, 2L)
expect_true(any(grepl("interrupted", res)))

# ---- the tool-call cap bites mid-turn, not just between turns ----
#
# The blocker this guards: one worker turn makes many calls, so a cap
# checked only between continuation prompts can be overshot by an entire
# turn's worth of work. Here a single turn tries 100 calls against a cap
# of 5; the gate must refuse partway through rather than after.

reset_wt()
gate_calls <- 0L
refused_at <- NA_integer_
runaway_turn <- function(prompt, session) {
    for (i in 1:100) {
        gate_calls <<- gate_calls + 1L
        res <- session$auto_gate(
            list(tool = "write_file", args = list(path = sprintf("r%d.txt", i)),
                 paths = sprintf("r%d.txt", i)),
            list(approval = "allow", reason = "default: random/write/console"))
        if (!identical(res$action, "proceed")) {
            refused_at <<- i
            break
        }
        writeLines("x", file.path(wt, sprintf("r%d.txt", i)))
    }
    list(reply = "made a lot of calls", session = session,
         usage = list(cost = 0.001, total_tokens = 10L))
}

ctx <- auto_ctx(runaway_turn)
# Caps come off disk via auto_envelope_config(), not from ctx$config, so
# the cap has to be a real project config to take effect.
dir.create(file.path(wt, ".corteza"), recursive = TRUE, showWarnings = FALSE)
writeLines('{"auto": {"max_tool_calls": 5}}',
           file.path(wt, ".corteza", "config.json"))
res <- with_stubs(
    list(spawn = function(...) "mon-stub",
         progress = function(...) list(verdict = "continue", reason = "ok")),
    capture.output({
        ns <- asNamespace("corteza")
        old <- get("monitor_ask_approval", envir = ns)
        unlockBinding("monitor_ask_approval", ns)
        assign("monitor_ask_approval",
               function(...) list(verdict = "approve", reason = "fine"),
               envir = ns)
        on.exit(assign("monitor_ask_approval", old, envir = ns), add = TRUE)
        corteza:::run_auto_loop(ctx, "make files", max_loops = 2L)
    }))

# Refused partway through the turn, not after all 100 landed -- and at
# the exact boundary. The cap counts calls EXECUTED, so a cap of 5
# permits calls 1..5 and refuses the 6th. Asserting the exact number
# rather than a range is the point: an inequality would have hidden the
# off-by-one where a cap of 5 let only 4 calls through.
expect_equal(refused_at, 6L)
expect_equal(gate_calls, 6L)
expect_true(any(grepl("tool-call cap", res)))
# Five files created, one per executed call.
expect_equal(length(list.files(wt, pattern = "^r[0-9]+\\.txt$")), 5L)

# ---- the cap holds through the REAL tool handler ----
#
# The previous version of this called session$auto_gate() directly,
# which bypassed the counter that caused the bug: .make_tool_handler()
# increments session$turn_number BEFORE consulting the gate, so a cap
# read from that counter fires one call early. Driving the handler is
# the only way this assertion means anything.

reset_wt()
executed <- character()
handler_exec <- function(name, args) {
    executed <<- c(executed, args$path %||% name)
    list(content = list(list(type = "text", text = "ok")))
}

handler_turn <- function(prompt, session) {
    h <- corteza:::.make_tool_handler(session, handler_exec)
    for (i in 1:20) {
        out <- h("write_file", list(path = sprintf("h%d.txt", i),
                                    content = "x"))
        if (grepl("monitor refused|policy denied", out)) {
            break
        }
    }
    list(reply = "done calling", session = session,
         usage = list(cost = 0.001, total_tokens = 10L))
}

ctx <- auto_ctx(handler_turn)
dir.create(file.path(wt, ".corteza"), recursive = TRUE, showWarnings = FALSE)
writeLines('{"auto": {"max_tool_calls": 5}}',
           file.path(wt, ".corteza", "config.json"))

res <- with_stubs(
    list(spawn = function(...) "mon-stub",
         progress = function(...) list(verdict = "continue", reason = "ok")),
    capture.output({
        ns <- asNamespace("corteza")
        old <- get("monitor_ask_approval", envir = ns)
        unlockBinding("monitor_ask_approval", ns)
        assign("monitor_ask_approval",
               function(...) list(verdict = "approve", reason = "fine"),
               envir = ns)
        on.exit(assign("monitor_ask_approval", old, envir = ns), add = TRUE)
        corteza:::run_auto_loop(ctx, "write files", max_loops = 1L)
    }))

# Exactly five calls executed through the real dispatch path; the sixth
# was refused. An off-by-one here shows up as 4.
expect_equal(length(executed), 5L)
expect_equal(executed, sprintf("h%d.txt", 1:5))
expect_true(any(grepl("tool-call cap", res)))

# ---- a broken counter stops execution, through the real handler ----
#
# on_approved is the only thing advancing the executed-call cap. If the
# gate swallowed its failure the call would run off the books, and a
# repeatedly failing counter would disable max_tool_calls silently.
# Asserted against the tool executor, not just the gate's return value.

reset_wt()
ran <- character()
broken_turn <- function(prompt, session) {
    # Break the counter the driver installed, then attempt a call.
    gate <- session$auto_gate
    session$auto_gate <- function(call, decision) {
        corteza:::monitor_auto_gate(
            "mon-stub", config = list(), cwd = wt,
            budget_check = function(event) list(stop = FALSE, reason = ""),
            on_approved = function() stop("counter exploded"))(call, decision)
    }
    h <- corteza:::.make_tool_handler(session, function(name, args) {
        ran <<- c(ran, args$path %||% name)
        list(content = list(list(type = "text", text = "ok")))
    })
    # Deliberately uncaught: the escalation has to travel the real path
    # out through turn() to the loop's handler. Catching it here would
    # test the assertion rather than the mechanism.
    h("write_file", list(path = "bad.txt", content = "x"))
    list(reply = "should not get here", session = session,
         usage = list(cost = 0.001, total_tokens = 10L))
}

ctx <- auto_ctx(broken_turn)
res <- with_stubs(
    list(spawn = function(...) "mon-stub",
         progress = function(...) list(verdict = "continue", reason = "ok")),
    capture.output({
        ns <- asNamespace("corteza")
        old <- get("monitor_ask_approval", envir = ns)
        unlockBinding("monitor_ask_approval", ns)
        assign("monitor_ask_approval",
               function(...) list(verdict = "approve", reason = "fine"),
               envir = ns)
        on.exit(assign("monitor_ask_approval", old, envir = ns), add = TRUE)
        corteza:::run_auto_loop(ctx, "x", max_loops = 1L)
    }))

# The tool never executed, even though the monitor approved it, and the
# run halted rather than carrying on with an uncounted call behind it.
expect_equal(ran, character())
expect_true(any(grepl("Halted for a human", res)))
expect_true(any(grepl("accounting failed", res)))
expect_true(any(grepl("escalated", res)))

# ---- the closing report counts the last approved call ----
#
# A terminal error or interrupt used to return before spend was
# refreshed, so the final line quoted figures from before the last
# approved call.

reset_wt()
turns <- 0L
spend_then_die <- function(prompt, session) {
    turns <<- turns + 1L
    corteza:::session_accumulate_spend(session,
                                       list(total_tokens = 700L, cost = 0.25))
    writeLines("x", file.path(wt, sprintf("t%d.txt", turns)))
    if (turns == 2L) {
        stop("died after spending")
    }
    list(reply = "working", session = session,
         usage = list(cost = 0, total_tokens = 0L))
}
ctx <- auto_ctx(spend_then_die)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
expect_true(any(grepl("turn errored", res)))
# Both turns' spend is in the closing line, including the one that died.
expect_true(any(grepl("spent \\$0\\.5000", res)))

# ---- the progress query's own cost is checked before it buys a turn ----
#
# Asking the monitor whether to continue costs tokens. Without a recheck
# after the query, that spend can cross the cap and the verdict still
# authorizes a whole further worker turn -- the most expensive thing the
# run can do. The stub bills the session directly, standing in for a
# monitor query that lands the run over budget.

reset_wt()
turns <- 0L
cheap_turn <- function(prompt, session) {
    turns <<- turns + 1L
    writeLines(as.character(turns), file.path(wt, sprintf("q%d.txt", turns)))
    list(reply = "working", session = session,
         usage = list(cost = 0.001, total_tokens = 10L))
}
ctx <- auto_ctx(cheap_turn)
dir.create(file.path(wt, ".corteza"), recursive = TRUE, showWarnings = FALSE)
writeLines('{"auto": {"max_cost": 1}}', file.path(wt, ".corteza", "config.json"))

expensive_progress <- list(
                           spawn = function(...) "mon-stub",
                           progress = function(id, goal, reply, diff, ...) {
    # The query bills the session on its way to answering "continue".
    corteza:::session_accumulate_spend(ctx$session,
                                       list(total_tokens = 100L, cost = 5))
    list(verdict = "continue", reason = "looks fine, keep going")
})

res <- with_stubs(expensive_progress,
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
# One worker turn ran; the query then blew the cap, so no second turn
# was authorized even though the monitor said continue.
expect_equal(turns, 1L)
expect_true(any(grepl("spend cap", res)))
expect_false(any(grepl("monitor said", res)))
# And the closing report includes the query that ended the run, rather
# than the stale figure from before it.
expect_true(any(grepl("spent \\$5\\.", res)))

# A monitor that says stop is still reported as the monitor stopping,
# not relabelled as a budget stop, even when it also crossed the cap.
reset_wt()
turns <- 0L
ctx <- auto_ctx(cheap_turn)
dir.create(file.path(wt, ".corteza"), recursive = TRUE, showWarnings = FALSE)
writeLines('{"auto": {"max_cost": 1}}', file.path(wt, ".corteza", "config.json"))
stopping_progress <- list(
                          spawn = function(...) "mon-stub",
                          progress = function(id, goal, reply, diff, ...) {
    corteza:::session_accumulate_spend(ctx$session,
                                       list(total_tokens = 100L, cost = 5))
    list(verdict = "stop", reason = "goal met")
})
res <- with_stubs(stopping_progress,
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 10L)))
expect_equal(turns, 1L)
expect_true(any(grepl("monitor said stop", res)))
expect_false(any(grepl("spend cap", res)))

# ---- nonsense bounds refuse to start ----
#
# A cap of 0 is not a tighter setting, it is a broken one, and the
# failure is quiet: the first iteration has nothing to compare against
# and would run before any limit check could bite.

reset_wt()
turns <- 0L
ctx <- auto_ctx(edit_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 0L)))
expect_equal(turns, 0L)
expect_true(any(grepl("refusing to start", res)))
expect_true(any(grepl("max_loops", res)))

reset_wt()
turns <- 0L
ctx <- auto_ctx(edit_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = -3L)))
expect_equal(turns, 0L)
expect_true(any(grepl("refusing to start", res)))

# A sane bound of 1 still runs exactly one turn.
reset_wt()
turns <- 0L
ctx <- auto_ctx(edit_turn)
res <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "x",
                                                         max_loops = 1L)))
expect_equal(turns, 1L)

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
