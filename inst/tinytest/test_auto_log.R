# The auto-run log: every run writes run_start first and run_end last,
# gate decisions carry the authority that made them, and the ids join
# the run log to the session trace and the monitor's transcript.
#
# Stubs override the same package bindings the production loop calls
# through (monitor_spawn / monitor_ask_progress / monitor_ask_approval /
# subagent_kill); everything between -- run_auto_loop, the gate factory,
# auto_log_append -- is the shipped code path.

library(tinytest)

old_home <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
tmp_data <- file.path(tempdir(), "corteza-auto-log-data")
dir.create(tmp_data, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(R_USER_DATA_DIR = tmp_data)

wt <- file.path(tempdir(), "corteza-auto-log-wt")
reset_wt <- function() {
    unlink(wt, recursive = TRUE)
    dir.create(wt, recursive = TRUE, showWarnings = FALSE)
    writeLines("start", file.path(wt, "a.txt"))
}

empty_palette <- list(dim = "", reset = "", cyan = "", bold = "",
                      yellow = "", green = "", bright_magenta = "",
                      red = "", magenta = "")

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
    ctx$disk_session <- list(sessionId = "auto-log-test",
                             session = corteza:::session_new("anthropic",
                                                             "test-model", wt))
    ctx
}

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

# Each run writes its own file; identify runs by diffing the directory.
log_dir <- corteza:::auto_log_dir()
logs_before <- function() list.files(log_dir, full.names = TRUE)
new_log <- function(before) {
    after <- list.files(log_dir, full.names = TRUE)
    fresh <- setdiff(after, before)
    expect_equal(length(fresh), 1L)
    fresh
}

# ---- primitives -------------------------------------------------------

id1 <- corteza:::auto_new_run_id()
id2 <- corteza:::auto_new_run_id()
expect_true(grepl("^[0-9]{8}T[0-9]{6}-[0-9a-f]{8}$", id1))
expect_false(identical(id1, id2))

# append/read round trip through the writer, not hand-built JSON
p <- file.path(tempdir(), "auto-log-rt.jsonl")
unlink(p)
expect_true(corteza:::auto_log_append(p, "run_start", run_id = "r1", n = 1L))
expect_true(corteza:::auto_log_append(p, "gate", run_id = "r1",
                                      authority = "envelope"))
expect_true(corteza:::auto_log_append(p, "run_end", run_id = "r1",
                                      stop_category = "ended"))
lg <- corteza:::auto_log_read(p)
expect_equal(length(lg$records), 3L)
expect_equal(lg$start$type, "run_start")
expect_equal(lg$end$stop_category, "ended")
# every record carries a timestamp and the run id
expect_true(all(vapply(lg$records, function(r) nzchar(r$ts %||% ""),
                       logical(1))))
expect_true(all(vapply(lg$records, function(r) identical(r$run_id, "r1"),
                       logical(1))))
unlink(p)

# a missing run_end reads as died, not as an error
p2 <- file.path(tempdir(), "auto-log-dead.jsonl")
unlink(p2)
corteza:::auto_log_append(p2, "run_start", run_id = "r2")
lg2 <- corteza:::auto_log_read(p2)
expect_null(lg2$end)
expect_false(is.null(lg2$start))
unlink(p2)

# the writer never throws: a path whose parent is an existing FILE
# cannot be created, and returns FALSE instead of erroring
blocker <- file.path(tempdir(), "auto-log-blocker")
writeLines("x", blocker)
expect_false(corteza:::auto_log_append(
    file.path(blocker, "x.jsonl"), "run_start", run_id = "r3"))
unlink(blocker)

# ---- refused start still gets a record --------------------------------

reset_wt()
before <- logs_before()
ctx <- auto_ctx(function(prompt, session) list(reply = "never runs"))
out <- with_stubs(stub_monitor(character()),
                  capture.output(corteza:::run_auto_loop(ctx, "goal",
                                                         max_loops = 0L)))
expect_true(any(grepl("refusing to start", out)))
f <- new_log(before)
lg <- corteza:::auto_log_read(f)
expect_equal(lg$start$goal, "goal")
expect_equal(lg$end$stop_category, "refused_start")
expect_equal(lg$end$loops, 0L)
# refused before the monitor: no monitor record
types <- vapply(lg$records, `[[`, "", "type")
expect_false("monitor" %in% types)

# ---- a full run: start, monitor, progress, categorized end ------------

reset_wt()
turns <- 0L
edit_turn <- function(prompt, session) {
    turns <<- turns + 1L
    writeLines(as.character(turns), file.path(wt, sprintf("f%d.txt", turns)))
    session$turn_number <- (session$turn_number %||% 0L) + 2L
    list(reply = "working", session = session,
         usage = list(cost = 0.01, total_tokens = 100L))
}
before <- logs_before()
ctx <- auto_ctx(edit_turn)
with_stubs(stub_monitor(c("continue", "stop")),
           capture.output(corteza:::run_auto_loop(ctx, "make edits",
                                                  max_loops = 10L)))
f <- new_log(before)
lg <- corteza:::auto_log_read(f)
types <- vapply(lg$records, `[[`, "", "type")

expect_equal(types[[1L]], "run_start")
expect_equal(utils::tail(types, 1L), "run_end")
expect_true("monitor" %in% types)
mon <- lg$records[types == "monitor"][[1L]]
expect_equal(mon$monitor_id, "mon-stub")

# caps recorded as configured for the run
expect_equal(lg$start$caps$max_loops, 10L)
expect_false(isTRUE(lg$start$allow_exec))
expect_equal(lg$start$allow_exec_source, "config")
expect_equal(lg$start$session_id, "auto-log-test")

# one progress record per monitor consultation, verdicts in order
prog <- lg$records[types == "progress"]
expect_equal(length(prog), 2L)
expect_equal(vapply(prog, `[[`, "", "verdict"), c("continue", "stop"))
expect_true(isTRUE(prog[[1L]]$changed))          # the turn edited a file

# the monitor stopped the run, and the category says so mechanically
expect_equal(lg$end$stop_category, "monitor")
expect_true(grepl("stub verdict 2", lg$end$stop_reason))
expect_equal(lg$end$loops, 2L)
expect_true(lg$end$files_changed >= 2L)          # f1.txt, f2.txt
expect_equal(lg$end$spend$cost, 0.02)

# run ids are consistent across every record and match the filename
ids <- vapply(lg$records, `[[`, "", "run_id")
expect_equal(length(unique(ids)), 1L)
expect_equal(paste0(unique(ids), ".jsonl"), basename(f))

# ---- limit stop is categorized "limit" --------------------------------

reset_wt()
turns <- 0L
before <- logs_before()
ctx <- auto_ctx(edit_turn)
with_stubs(stub_monitor(character()),
           capture.output(corteza:::run_auto_loop(ctx, "make edits",
                                                  max_loops = 2L)))
f <- new_log(before)
lg <- corteza:::auto_log_read(f)
expect_equal(lg$end$stop_category, "limit")
expect_true(grepl("max_loops", lg$end$stop_reason))

# ---- errored turn is categorized "error" ------------------------------

reset_wt()
before <- logs_before()
boom <- function(prompt, session) stop("turn exploded")
ctx <- auto_ctx(boom)
with_stubs(stub_monitor(character()),
           capture.output(corteza:::run_auto_loop(ctx, "goal",
                                                  max_loops = 5L)))
f <- new_log(before)
lg <- corteza:::auto_log_read(f)
expect_equal(lg$end$stop_category, "error")
expect_true(grepl("turn exploded", lg$end$stop_reason))

# ---- gate decisions: authority travels with the record ----------------

# Driven through the shipped gate factory (not a reimplementation),
# with the monitor query stubbed at the same namespace seam the loop
# tests use.
ns <- asNamespace("corteza")
old_ask <- get("monitor_ask_approval", envir = ns)
unlockBinding("monitor_ask_approval", ns)
assign("monitor_ask_approval", function(id, call, decision,
                                        request_id = NULL, timeout = 120L) {
    list(verdict = "approve", reason = "stub ok", request_id = request_id)
}, envir = ns)

seen <- list()
gate <- corteza:::monitor_auto_gate(
    "mon-x", config = list(), cwd = wt,
    budget_check = NULL, on_approved = function() NULL,
    on_decision = function(d) seen[[length(seen) + 1L]] <<- d)

# 1. envelope refusal: exec tool with allow_exec off (the default)
r <- gate(list(tool = "bash", args = list(command = "ls")),
          list(approval = "allow", reason = "policy"))
expect_equal(r$action, "escalate")
expect_equal(seen[[1L]]$authority, "envelope")
expect_null(seen[[1L]]$request_id)               # monitor never consulted

# 2. monitor approval: an in-envelope write consults the monitor
r <- gate(list(tool = "write_file",
               args = list(path = file.path(wt, "ok.txt"), content = "x")),
          list(approval = "allow", reason = "policy"))
expect_equal(r$action, "proceed")
expect_equal(seen[[2L]]$authority, "monitor")
expect_equal(seen[[2L]]$verdict, "approve")
expect_true(nzchar(seen[[2L]]$request_id %||% ""))

# 3. budget stop decides before the envelope and is attributed to it
gate_b <- corteza:::monitor_auto_gate(
    "mon-x", config = list(), cwd = wt,
    budget_check = function(event) list(stop = TRUE, reason = "cap hit"),
    on_decision = function(d) seen[[length(seen) + 1L]] <<- d)
r <- gate_b(list(tool = "write_file",
                 args = list(path = file.path(wt, "ok.txt"), content = "x")),
            list(approval = "allow", reason = "policy"))
expect_equal(r$action, "escalate")
expect_equal(seen[[3L]]$authority, "budget")

# 4. post-query budget stop: monitor consulted, budget overrides
calls <- 0L
gate_p <- corteza:::monitor_auto_gate(
    "mon-x", config = list(), cwd = wt,
    budget_check = function(event) {
        if (identical(event, "monitor")) {
            list(stop = TRUE, reason = "query spent the last of it")
        } else {
            list(stop = FALSE)
        }
    },
    on_decision = function(d) seen[[length(seen) + 1L]] <<- d)
r <- gate_p(list(tool = "write_file",
                 args = list(path = file.path(wt, "ok.txt"), content = "x")),
            list(approval = "allow", reason = "policy"))
expect_equal(r$action, "escalate")
expect_equal(seen[[4L]]$authority, "budget")
expect_equal(seen[[4L]]$verdict, "approve")      # the overridden verdict kept
expect_true(nzchar(seen[[4L]]$request_id %||% ""))

# 5. accounting failure is its own authority
gate_a <- corteza:::monitor_auto_gate(
    "mon-x", config = list(), cwd = wt,
    on_approved = function() stop("counter broke"),
    on_decision = function(d) seen[[length(seen) + 1L]] <<- d)
r <- gate_a(list(tool = "write_file",
                 args = list(path = file.path(wt, "ok.txt"), content = "x")),
            list(approval = "allow", reason = "policy"))
expect_equal(r$action, "escalate")
expect_equal(seen[[5L]]$authority, "accounting")

# 6. a throwing recorder does not affect the decision
gate_t <- corteza:::monitor_auto_gate(
    "mon-x", config = list(), cwd = wt,
    on_decision = function(d) stop("recorder broke"))
r <- gate_t(list(tool = "write_file",
                 args = list(path = file.path(wt, "ok.txt"), content = "x")),
            list(approval = "allow", reason = "policy"))
expect_equal(r$action, "proceed")

assign("monitor_ask_approval", old_ask, envir = ns)

# ---- run id joins: trace entries and the monitor header ---------------

# trace_add stamps auto_run_id only when given one; the attended format
# is unchanged.
tid <- "trace-join-test"
tp <- corteza:::trace_add(tid, "write_file", list(path = "x"), "ok",
                          success = TRUE, elapsed_ms = 1,
                          auto_run_id = "run-abc")
entry <- jsonlite::fromJSON(utils::tail(readLines(tp), 1L),
                            simplifyVector = FALSE)
expect_equal(entry$auto_run_id, "run-abc")
corteza:::trace_add(tid, "read_file", list(path = "x"), "ok",
                    success = TRUE, elapsed_ms = 1)
entry2 <- jsonlite::fromJSON(utils::tail(readLines(tp), 1L),
                             simplifyVector = FALSE)
expect_false("auto_run_id" %in% names(entry2))

# transcript_write_header carries extra fields, and drops NULLs so the
# attended header is untouched.
hp <- corteza:::transcript_write_header("hdr-join-test", wt,
                                        extra = list(auto_run_id = "run-abc",
                                                     nothing = NULL))
hdr <- jsonlite::fromJSON(readLines(hp)[[1L]], simplifyVector = FALSE)
expect_equal(hdr$auto_run_id, "run-abc")
expect_false("nothing" %in% names(hdr))
hp2 <- corteza:::transcript_write_header("hdr-plain-test", wt,
                                         extra = list(auto_run_id = NULL))
hdr2 <- jsonlite::fromJSON(readLines(hp2)[[1L]], simplifyVector = FALSE)
expect_false("auto_run_id" %in% names(hdr2))

# ---- cleanup ----------------------------------------------------------

if (is.na(old_home)) {
    Sys.unsetenv("R_USER_DATA_DIR")
} else {
    Sys.setenv(R_USER_DATA_DIR = old_home)
}
unlink(tmp_data, recursive = TRUE)
unlink(wt, recursive = TRUE)
