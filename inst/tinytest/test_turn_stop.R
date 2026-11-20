# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# A turn can be asked to stop (R/turn-stop.R; WP20a, rule 10). A session's
# `stop_requested` is asked before each model request, before each tool
# call and on each streamed delta; a stop ends the turn with status
# `cancelled` and a history the next request can use. And a job stopped
# mid-run says what it leaves. Nothing here reaches a model.

ns <- asNamespace("corteza")
stopped_by <- function(expr) {
    tryCatch({
        expr
        "ran"
    }, corteza_stop = function(c) "stopped", llm_cancelled = function(c) "cancelled")
}

# ---- Asking --------------------------------------------------------------------
local({
    s <- new.env()
    expect_false(ns$.turn_stop_requested(s))
    s$stop_requested <- function() FALSE
    expect_false(ns$.turn_stop_requested(s))
    s$stop_requested <- function() TRUE
    expect_true(ns$.turn_stop_requested(s))
    # A function that fails, or answers anything but TRUE, is no stop.
    s$stop_requested <- function() stop("broken")
    expect_false(tryCatch(ns$.turn_stop_requested(s), error = function(e) NA))
    s$stop_requested <- function() NA
    expect_false(ns$.turn_stop_requested(s))
    # The condition passes handlers that catch errors.
    s$stop_requested <- function() TRUE
    expect_identical(stopped_by(tryCatch(ns$.turn_stop_check(s),
                                         error = function(e) "swallowed")),
                     "stopped")

    # The request callback asks at a request's start, before the audit
    # records it, and passes every event on otherwise.
    phases <- character()
    audit <- function(event) phases <<- c(phases, event$phase)
    flag <- FALSE
    s$stop_requested <- function() flag
    cb <- ns$.turn_request_callback(s, audit)
    cb(list(phase = "start"))
    cb(list(phase = "end"))
    expect_identical(phases, c("start", "end"))
    flag <- TRUE
    expect_identical(stopped_by(cb(list(phase = "start"))), "stopped")
    expect_identical(phases, c("start", "end"))
    # A request already sent is recorded to its end.
    expect_identical(stopped_by(cb(list(phase = "end"))), "ran")
    expect_identical(phases, c("start", "end", "end"))
    # Without the audit's, it still asks.
    expect_identical(stopped_by(ns$.turn_request_callback(s, NULL)(
        list(phase = "start"))), "stopped")
    # A session that cannot be stopped keeps the audit's callback as it is.
    plain <- new.env()
    expect_identical(ns$.turn_request_callback(plain, audit), audit)
    expect_null(ns$.turn_request_callback(plain, NULL))

    # The sink asks before each delta goes out, and cancels as the voice
    # brain does.
    pushed <- character()
    push <- function(d) pushed <<- c(pushed, d)
    expect_identical(ns$.turn_stop_sink(plain, push, function() NULL), push)
    noted <- FALSE
    flag <- FALSE
    sink <- ns$.turn_stop_sink(s, push, function() noted <<- TRUE)
    sink("a")
    flag <- TRUE
    expect_identical(stopped_by(sink("b")), "cancelled")
    expect_identical(pushed, "a")
    expect_true(noted)
})

# ---- A turn asked to stop ------------------------------------------------------
# A stand-in for llm.api::agent() that keeps its contract: the request
# callback at each request's start, the history mirrored after each
# message, tools through the handler, and deltas through on_delta, with
# llm_cancelled answered as agent() answers it. `steps` is the run.
fake_agent <- function(steps, requests) {
    function(prompt, tools = list(), tool_handler = NULL, system = NULL,
             model = NULL, provider = "anthropic", max_turns = 20L,
             verbose = TRUE, history = NULL, history_callback = NULL,
             request_callback = NULL, checkpoint_callback = NULL,
             on_delta = NULL, ...) {
        messages <- c(history, list(list(role = "user", content = prompt)))
        mirror <- function() {
            if (is.function(history_callback)) history_callback(messages)
        }
        done <- function(text, cancelled = FALSE) {
            list(content = text, cancelled = cancelled, model = model,
                 provider = provider, turns = 1L, history = messages,
                 usage = list(input_tokens = 1, output_tokens = 1))
        }
        for (step in steps) {
            if (is.function(request_callback)) {
                request_callback(list(phase = "start"))
            }
            requests$n <- requests$n + 1L
            if (identical(step$kind, "tool")) {
                messages <- c(messages, list(list(role = "assistant", content = list(
                    list(type = "tool_use", id = "call-1", name = step$name,
                         input = step$args)))))
                mirror()
                if (is.function(step$then)) step$then()
                out <- tool_handler(step$name, step$args)
                messages <- c(messages, list(list(role = "user", content = list(
                    list(type = "tool_result", tool_use_id = "call-1",
                         content = paste(unlist(out), collapse = " "))))))
                mirror()
                next
            }
            text <- ""
            cancelled <- FALSE
            tryCatch(for (d in step$deltas) {
                if (is.function(step$then)) step$then(d)
                if (is.function(on_delta)) on_delta(d)
                text <- paste0(text, d)
            }, llm_cancelled = function(c) cancelled <<- TRUE)
            if (cancelled) {
                return(done(text, cancelled = TRUE))
            }
            messages <- c(messages, list(list(role = "assistant", content = text)))
            mirror()
            return(done(text))
        }
    }
}
# Run `turn()` against the stand-in. Returns the result, the requests
# that went out and the tools that ran.
run_turn <- function(session, steps) {
    real <- get("agent", envir = asNamespace("llm.api"))
    on.exit(assignInNamespace("agent", real, ns = "llm.api"), add = TRUE)
    requests <- new.env()
    requests$n <- 0L
    assignInNamespace("agent", fake_agent(steps, requests), ns = "llm.api")
    ran <- character()
    executor <- function(name, args) {
        ran <<- c(ran, name)
        list(content = list(list(type = "text", text = "file contents")))
    }
    # A stop or an error that got out of turn() reads as such, not as a
    # failure of this file.
    result <- tryCatch(suppressMessages(corteza::turn("look at it", session,
        tool_executor = executor, tools = list())),
        corteza_stop = function(c) list(termination = list(status = "escaped")),
        error = function(e) list(termination = list(status = "error")))
    list(result = result, requests = requests$n, ran = ran)
}
stoppable <- function() {
    s <- corteza::new_session("cli", provider = "anthropic", verbose = FALSE,
                              model_map = list(cloud = "fixture", local = NULL))
    s$config <- list(approval_mode = "allow")
    s$history <- list(list(role = "user", content = "earlier"),
                      list(role = "assistant", content = "earlier answer"))
    s
}
marker <- ns$user_interrupt_marker()
# The text of message `i` of the history, or NA where there is none.
text_at <- function(s, i) {
    h <- s$history
    if (i < 1L || i > length(h)) {
        return(NA_character_)
    }
    paste(unlist(h[[i]]$content), collapse = " ")
}
last_text <- function(s) text_at(s, length(s$history))
complete <- function(h) {
    all(vapply(llm.api::history_tool_calls(h), function(r) isTRUE(r$completed),
               logical(1)))
}

# Before the first request: nothing goes out, and the prompt and the
# marker follow the history, so the next turn sees the attempt.
local({
    s <- stoppable()
    s$stop_requested <- function() TRUE
    out <- run_turn(s, list(list(kind = "reply", deltas = "never")))
    expect_identical(out$result$termination$status, "cancelled")
    expect_identical(out$requests, 0L)
    expect_identical(length(s$history), 4L)
    expect_identical(text_at(s, 3L), "look at it")
    expect_identical(last_text(s), marker)
})

# Before a tool call: the call does not run, it gets a placeholder
# result, and the history ends on the marker.
local({
    s <- stoppable()
    flag <- FALSE
    s$stop_requested <- function() flag
    out <- run_turn(s, list(
        list(kind = "tool", name = "read_file", args = list(path = "notes.txt"),
             then = function() flag <<- TRUE),
        list(kind = "reply", deltas = "never")))
    expect_identical(out$result$termination$status, "cancelled")
    expect_identical(out$requests, 1L)
    expect_identical(out$ran, character())
    expect_true(complete(s$history))
    expect_identical(last_text(s), marker)
    expect_identical(out$result$session$history, s$history)
})

# While a call waits, at the approval prompt or at the gate (a review,
# the monitor, a person in a room): a stop asked for then ends the turn
# when the wait ends, whatever its answer, and the call does not run
# (Codex, #284). The audit says it was stopped.
local({
    outcomes <- character()
    real <- ns$.audit_tool_event
    assignInNamespace(".audit_tool_event", function(session, event) {
        outcomes <<- c(outcomes, event$outcome)
        invisible(NULL)
    }, ns = "corteza")
    on.exit(assignInNamespace(".audit_tool_event", real, ns = "corteza"),
            add = TRUE)
    write <- list(kind = "tool", name = "write_file",
                  args = list(path = "notes.txt", content = "x"))
    # The approval prompt: the person says yes after the stop.
    s <- stoppable()
    s$config <- list(approval_mode = "ask")
    flag <- FALSE
    s$stop_requested <- function() flag
    asked <- 0L
    s$approval_cb <- function(call, decision) {
        asked <<- asked + 1L
        flag <<- TRUE
        TRUE
    }
    out <- run_turn(s, list(write, list(kind = "reply", deltas = "never")))
    expect_identical(asked, 1L)
    expect_identical(out$result$termination$status, "cancelled")
    expect_identical(out$ran, character())
    expect_true(complete(s$history))
    expect_identical(last_text(s), marker)
    expect_identical(outcomes, c("approval", "stopped"))
    # The gate: it lets the call through after the stop.
    outcomes <- character()
    s <- stoppable()
    flag <- FALSE
    s$stop_requested <- function() flag
    gated <- 0L
    s$auto_gate <- function(call, decision) {
        gated <<- gated + 1L
        flag <<- TRUE
        list(action = "proceed")
    }
    out <- run_turn(s, list(
        list(kind = "tool", name = "read_file", args = list(path = "notes.txt")),
        list(kind = "reply", deltas = "never")))
    expect_identical(gated, 1L)
    expect_identical(out$result$termination$status, "cancelled")
    expect_identical(out$ran, character())
    expect_true(complete(s$history))
    expect_identical(last_text(s), marker)
    expect_identical(outcomes, c("gate", "stopped"))
    # With no stop, each wait's yes runs the call.
    s <- stoppable()
    s$config <- list(approval_mode = "ask")
    s$stop_requested <- function() FALSE
    s$approval_cb <- function(call, decision) TRUE
    out <- run_turn(s, list(write, list(kind = "reply", deltas = "Done.")))
    expect_identical(out$result$termination$status, "done")
    expect_identical(out$ran, "write_file")
    s <- stoppable()
    s$stop_requested <- function() FALSE
    s$auto_gate <- function(call, decision) list(action = "proceed")
    out <- run_turn(s, list(
        list(kind = "tool", name = "read_file", args = list(path = "notes.txt")),
        list(kind = "reply", deltas = "Done.")))
    expect_identical(out$result$termination$status, "done")
    expect_identical(out$ran, "read_file")
})

# On a streamed delta: nothing after the stop reaches the client, the
# reply is what was said before it, and the history ends on the marker.
local({
    s <- stoppable()
    flag <- FALSE
    s$stop_requested <- function() flag
    got <- character()
    s$on_delta <- function(d) {
        got <<- c(got, d)
        TRUE
    }
    out <- run_turn(s, list(list(kind = "reply", deltas = c("Hel", "lo", "!"),
                                 then = function(d) {
        if (identical(d, "lo")) flag <<- TRUE
    })))
    expect_identical(out$result$termination$status, "cancelled")
    expect_identical(paste(got, collapse = ""), "Hel")
    expect_identical(out$result$reply, "Hel")
    expect_identical(last_text(s), marker)
    expect_identical(text_at(s, length(s$history) - 1L), "look at it")
})

# Text the stream held back, to check it for a secret, stays held after a
# stop: nothing more reaches the client. ("Hel" begins the secret, so
# all of it is held, whatever other secrets the environment holds.)
local({
    s <- stoppable()
    s$secret_literals <- "Hel-fixture-secret"
    flag <- FALSE
    s$stop_requested <- function() flag
    got <- character()
    s$on_delta <- function(d) {
        got <<- c(got, d)
        TRUE
    }
    out <- run_turn(s, list(list(kind = "reply", deltas = c("Hel", "lo"),
                                 then = function(d) {
        if (identical(d, "lo")) flag <<- TRUE
    })))
    expect_identical(out$result$termination$status, "cancelled")
    expect_identical(got, character())
    expect_identical(out$result$reply, "Hel")
})

# Not asked to stop, or asked by a function that fails: the turn runs as
# before, its tool runs and its reply is the history's last word.
local({
    for (ask in list(function() FALSE, function() stop("broken"), NULL)) {
        s <- stoppable()
        s$stop_requested <- ask
        out <- run_turn(s, list(
            list(kind = "tool", name = "read_file", args = list(path = "notes.txt")),
            list(kind = "reply", deltas = c("All ", "read."))))
        expect_identical(out$result$termination$status, "done")
        expect_identical(out$ran, "read_file")
        expect_identical(out$result$reply, "All read.")
        expect_identical(last_text(s), "All read.")
    }
})

# ---- What a job stopped mid-run leaves -----------------------------------------
# Its reason names the checkpoint the next job starts from, read from the
# job's own worker identity.
local({
    state <- tempfile("turn-stop-state-")
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = state)
    on.exit({
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(state, recursive = TRUE)
    }, add = TRUE)
    identity <- list(owner = "local:fixture", key = "repl:a",
                     workspace = tempdir(), role = "doer")
    j <- list(dispatch = list(worker = list(identity = identity)))
    reason <- ns$job_cancel_reason
    # No checkpoint: the worker's memory is gone and nothing replaces it.
    expect_true(grepl("Files it wrote stay", reason(j), fixed = TRUE))
    expect_true(grepl("starts without them", reason(j), fixed = TRUE))
    # A job's checkpoint names it.
    dir <- ns$job_worker_state_dir(identity)
    dir.create(dir, recursive = TRUE)
    ns$job_write_file(file.path(dir, "checkpoint.json"),
                      list(identity = identity, job = "20261119T000000-0000abcd"))
    expect_true(grepl("since job 20261119T000000-0000abcd ended", reason(j),
                      fixed = TRUE))
    expect_true(grepl("Files it wrote stay", reason(j), fixed = TRUE))
    # A conversation's starting checkpoint has no job.
    ns$job_write_file(file.path(dir, "checkpoint.json"),
                      list(identity = identity, job = NULL))
    expect_true(grepl("since this conversation began", reason(j), fixed = TRUE))
    # A job dispatched before its record named the identity.
    expect_true(grepl("starts without them", reason(list()), fixed = TRUE))
})
