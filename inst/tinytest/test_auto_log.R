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
expect_true(grepl("^[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$", id1))
expect_false(identical(id1, id2))

# id generation must not touch the user's RNG state: same seed before
# and after, and two ids under the same seed still differ.
set.seed(42)
seed_before <- .Random.seed
ida <- corteza:::auto_new_run_id()
expect_identical(.Random.seed, seed_before)
set.seed(42)
idb <- corteza:::auto_new_run_id()
expect_false(identical(ida, idb))

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
# fresh log files are owner-only, explicitly
if (identical(.Platform$OS.type, "unix")) {
    expect_equal(as.character(file.mode(p)), "600")
}
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
# refused before the monitor: config was recorded, the monitor never was
types <- vapply(lg$records, `[[`, "", "type")
expect_true("config" %in% types)
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

# caps and exec provenance recorded in the config record; identity in
# run_start (which precedes config resolution)
cfg_rec <- lg$records[types == "config"][[1L]]
expect_equal(cfg_rec$caps$max_loops, 10L)
expect_false(isTRUE(cfg_rec$allow_exec))
expect_true(cfg_rec$allow_exec_source %in%
            c("default", "global_config"))
expect_false(isTRUE(cfg_rec$allow_exec_vetoed))
expect_equal(lg$start$session_id, "auto-log-test")
# run_start is the first record: identity precedes all validation
expect_equal(vapply(lg$records, `[[`, "", "type")[[1L]], "run_start")

# one progress record per monitor consultation, verdicts in order,
# each naming the request id its transcript exchange used
prog <- lg$records[types == "progress"]
expect_equal(length(prog), 2L)
expect_equal(vapply(prog, `[[`, "", "verdict"), c("continue", "stop"))
expect_equal(vapply(prog, `[[`, "", "request_id"), c("p1", "p2"))
expect_true(isTRUE(prog[[1L]]$changed))          # the turn edited a file

# the monitor stopped the run, and the category says so mechanically
expect_equal(lg$end$stop_category, "monitor")
expect_true(grepl("stub verdict 2", lg$end$stop_reason))
expect_equal(lg$end$loops, 2L)
expect_true(lg$end$files_changed >= 2L)          # f1.txt, f2.txt
expect_equal(lg$end$spend$cost, 0.02)
# durable delta evidence: paths and hashes, not just a count
added_paths <- vapply(lg$end$delta$added, `[[`, "", "path")
expect_true(all(c("f1.txt", "f2.txt") %in% added_paths))
expect_true(all(nzchar(vapply(lg$end$delta$added, `[[`, "", "hash"))))
expect_false(isTRUE(lg$end$delta$truncated))

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
expect_equal(lg$end$stop_kind, "loops")
expect_true(grepl("max_loops", lg$end$stop_reason))

# every limit the checker can fire carries its machine-readable kind
base_state <- list(loop = 1L, started = Sys.time(), tool_calls = 0L,
                   stalled = 0L,
                   spend = list(cost = 0, tokens = 0, cost_known = TRUE))
base_auto <- list(max_loops = 10L, max_minutes = 30, max_cost = 5,
                  max_tokens = 2e6, max_tool_calls = 300L, stall_loops = 2L)
fire <- function(state_patch = list(), auto_patch = list()) {
    s <- utils::modifyList(base_state, state_patch)
    a <- utils::modifyList(base_auto, auto_patch)
    corteza:::auto_check_limits(s, a)
}
expect_equal(fire(list(loop = 11L))$kind, "loops")
expect_equal(fire(list(started = Sys.time() - 3600))$kind, "minutes")
expect_equal(fire(list(tool_calls = 300L))$kind, "tool_calls")
expect_equal(fire(list(spend = list(cost = 9, tokens = 0,
                                    cost_known = TRUE)))$kind, "cost")
expect_equal(fire(list(spend = list(cost = 0, tokens = 3e6,
                                    cost_known = TRUE)))$kind, "tokens")
expect_equal(fire(list(stalled = 2L))$kind, "stall")
expect_equal(fire(list(spend = list(cost = 0, tokens = 0,
                                    cost_known = FALSE)),
                  list(max_tokens = Inf))$kind, "unbounded")
expect_null(fire()$kind)                          # no stop, no kind

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

# ---- a throwing monitor spawn closes the record before propagating ----

reset_wt()
before <- logs_before()
err <- tryCatch({
    with_stubs(list(spawn = function(...) stop("no child processes"),
                    progress = function(...) list(verdict = "continue",
                                                  reason = "")),
               capture.output(corteza:::run_auto_loop(
                   auto_ctx(function(prompt, session) list(reply = "x")),
                   "goal", max_loops = 3L)))
    NULL
}, error = function(e) conditionMessage(e))
expect_true(grepl("no child processes", err))     # the error still reaches
f <- new_log(before)
lg <- corteza:::auto_log_read(f)
expect_equal(lg$end$stop_category, "unexpected_error")
expect_true(grepl("monitor spawn failed", lg$end$stop_reason))
expect_true(grepl("no child processes", lg$end$stop_reason))
# a closed record means "no run_end" keeps meaning hard termination
expect_false(is.null(lg$end))

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
r <- gate(list(tool = "bash", call_id = "c-env",
               args = list(command = "ls")),
          list(approval = "allow", reason = "policy"))
expect_equal(r$action, "escalate")
expect_equal(seen[[1L]]$authority, "envelope")
expect_null(seen[[1L]]$request_id)               # monitor never consulted
expect_false(isTRUE(seen[[1L]]$envelope_ok))     # evaluated and refused
expect_equal(seen[[1L]]$call_id, "c-env")
expect_equal(seen[[1L]]$policy_approval, "allow")
expect_equal(seen[[1L]]$policy_reason, "policy")

# 2. monitor approval: an in-envelope write consults the monitor
r <- gate(list(tool = "write_file", call_id = "c-mon",
               args = list(path = file.path(wt, "ok.txt"), content = "x")),
          list(approval = "allow", reason = "policy"))
expect_equal(r$action, "proceed")
expect_equal(seen[[2L]]$authority, "monitor")
expect_equal(seen[[2L]]$verdict, "approve")
expect_true(nzchar(seen[[2L]]$request_id %||% ""))
expect_true(isTRUE(seen[[2L]]$envelope_ok))      # evaluated and passed
expect_null(seen[[2L]]$budget_event)             # budget never decided
expect_equal(seen[[2L]]$call_id, "c-mon")

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
expect_equal(seen[[3L]]$budget_event, "call")     # pre-envelope phase
expect_null(seen[[3L]]$envelope_ok)               # never evaluated

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
expect_equal(seen[[4L]]$budget_event, "monitor")  # post-query phase
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

# ---- ACCEPTANCE: reconstruct authority from disk alone ----------------
#
# Four IDENTICAL tool calls (same tool, same args) inside one run, the
# monitor alternating approve/refuse. Afterwards, every conclusion below
# is drawn ONLY from the files: the run's JSONL and the session trace.
# No in-memory state, no ordering assumptions beyond what the records
# themselves carry. This is the property the log exists to provide: two
# calls that look the same from the outside -- one refused, one executed
# -- must reconstruct unambiguously.

reset_wt()
flips <- 0L
handler_turn <- function(prompt, session) {
    h <- corteza:::.make_tool_handler(session, function(name, args) {
        list(content = list(list(type = "text", text = "wrote it")))
    })
    for (i in 1:4) {
        h("write_file", list(path = file.path(wt, "same.txt"),
                             content = "identical"))
    }
    list(reply = "made identical calls", session = session,
         usage = list(cost = 0.001, total_tokens = 10L))
}
ctx <- auto_ctx(handler_turn)
ctx$session$sessionId <- "accept-join"
# The production trace observer, not a copy: if chat()'s observer ever
# stops passing call_id, this test fails.
ctx$session$on_tool <- list(corteza:::chat_trace_observer(ctx$session))

before <- logs_before()
res <- with_stubs(
    list(spawn = function(...) "mon-stub",
         progress = function(...) list(verdict = "stop", reason = "done")),
    capture.output({
        ns2 <- asNamespace("corteza")
        old2 <- get("monitor_ask_approval", envir = ns2)
        unlockBinding("monitor_ask_approval", ns2)
        assign("monitor_ask_approval",
               function(id, call, decision, request_id = NULL,
                        timeout = 120L) {
            flips <<- flips + 1L
            if (flips %% 2L == 1L) {
                list(verdict = "approve", reason = "fine")
            } else {
                list(verdict = "refuse", reason = "not this one")
            }
        }, envir = ns2)
        on.exit(assign("monitor_ask_approval", old2, envir = ns2),
                add = TRUE)
        corteza:::run_auto_loop(ctx, "make identical calls", max_loops = 1L)
    }))

# --- reconstruction, from files only ---
f <- new_log(before)
lg <- corteza:::auto_log_read(f)
rid <- lg$start$run_id
gates <- Filter(function(r) identical(r$type, "gate"), lg$records)
expect_equal(length(gates), 4L)

trace_file <- corteza:::trace_path("accept-join")
trows <- lapply(readLines(trace_file), jsonlite::fromJSON,
                simplifyVector = FALSE)
# every trace row belongs to this run and names its call
expect_true(all(vapply(trows, function(t) identical(t$auto_run_id, rid),
                       logical(1))))

# join on call_id: exactly one gate record and one trace row per call,
# even though tool and args are identical across all four
gid <- vapply(gates, `[[`, "", "call_id")
tid <- vapply(trows, `[[`, "", "call_id")
expect_equal(sort(gid), sort(tid))
expect_equal(anyDuplicated(gid), 0L)
expect_equal(length(unique(c(gid, tid))), 4L)

for (g in gates) {
    tr <- trows[[match(g$call_id, tid)]]
    expect_equal(tr$tool, g$tool)
    # the complete authority chain is in the gate record itself
    expect_equal(g$policy_approval, "allow")
    expect_true(isTRUE(g$envelope_ok))
    expect_equal(g$authority, "monitor")
    expect_true(nzchar(g$request_id))
    if (identical(g$action, "proceed")) {
        # approved -> the trace row shows the executed result
        expect_equal(g$verdict, "approve")
        expect_true(isTRUE(tr$success))
        expect_true(grepl("wrote it", tr$result))
    } else {
        # refused -> the trace row shows the refusal the model saw
        expect_equal(g$action, "refuse")
        expect_equal(g$verdict, "refuse")
        expect_false(isTRUE(tr$success))
        expect_true(grepl("monitor refused", tr$result))
    }
}
# alternation reconstructed: approve/refuse/approve/refuse by seq order
ord <- order(vapply(gates, function(g) g$seq, numeric(1)))
expect_equal(vapply(gates[ord], `[[`, "", "action"),
             c("proceed", "refuse", "proceed", "refuse"))
# and each gate record carries the budget snapshot it decided against
expect_true(all(vapply(gates, function(g) is.numeric(g$spend_cost),
                       logical(1))))
expect_equal(gates[ord][[4L]]$tool_calls, 2L)   # two executed before it

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
