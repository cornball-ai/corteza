# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

# run_repl_loop(): drive the shared loop with scripted input and stubbed
# hooks, exercising dispatch / state mutation / local eval / a full
# prompt->reply cycle without a live LLM.

# Scripted input reader: yields each line, then character(0) (EOF) to
# break the loop cleanly.
scripted_input <- function(lines) {
    i <- 0L
    function(prompt_str) {
        i <<- i + 1L
        if (i <= length(lines)) lines[[i]] else character(0)
    }
}

empty_palette <- list(dim = "", reset = "", cyan = "", bold = "",
                      yellow = "", green = "", bright_magenta = "",
                      red = "", magenta = "")

base_ctx <- function(lines) {
    ctx <- new.env(parent = emptyenv())
    ctx$palette <- empty_palette
    ctx$read_input <- scripted_input(lines)
    ctx$help_text <- function() "HELP"
    ctx$handle_copy <- function(x) invisible(NULL)
    ctx$format_tools <- function(s) "TOOLS"
    ctx$pending_r_context <- character(0)
    ctx$last_assistant_response <- ""
    ctx
}

# 1. /help then EOF: help hook fires, clean exit, returns NULL.
help_hit <- FALSE
ctx1 <- base_ctx(c("/help"))
ctx1$help_text <- function() {
    help_hit <<- TRUE
    "HELP"
}
expect_null(corteza:::run_repl_loop(ctx1))
expect_true(help_hit)

# 2. /quit: clean exit.
expect_null(corteza:::run_repl_loop(base_ctx(c("/quit"))))

# Both remote-control spellings dispatch locally without starting a model.
for (command in c("/remote-control", "/rc")) {
    output <- capture.output(corteza:::run_repl_loop(base_ctx(paste(command, "status"))))
    expect_true(any(grepl("Remote control is off.", output, fixed = TRUE)))
}

# 3. /model: a slash command that mutates session + ctx state.
ctx3 <- base_ctx(c("/model kimi-test"))
ctx3$session <- new.env(parent = emptyenv())
ctx3$session$model_map <- list(cloud = "old-model")
ctx3$session$thinking <- "between_tools"
ctx3$model <- "old-model"
corteza:::run_repl_loop(ctx3)
expect_equal(ctx3$model, "kimi-test")
expect_equal(ctx3$session$model_map$cloud, "kimi-test")
# The thinking type was the old model's; the new one does not get it.
expect_null(ctx3$session$thinking)

# 4. /r: local-eval path stages output into pending_r_context.
ctx4 <- base_ctx(c("/r 40 + 2"))
corteza:::run_repl_loop(ctx4)
expect_true(any(grepl("42", ctx4$pending_r_context)))

# 5. Normal prompt with turn_fn stubbed: a full prompt->reply cycle
# with no LLM. Redirect the data dir so transcript writes land in temp.
old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
tmp_data <- file.path(tempdir(), "repl_test_data")
Sys.setenv(R_USER_DATA_DIR = tmp_data)

rendered <- NULL
ctx5 <- base_ctx(c("hello"))
ctx5$provider <- "ollama"
ctx5$model <- "llama3.2"
ctx5$config <- list()
ctx5$session <- new.env(parent = emptyenv())
ctx5$session$history <- list()
ctx5$session$tasks <- list()
ctx5$session$tasks_dirty <- FALSE
sess <- corteza:::session_new("ollama", "llama3.2", getwd())
ctx5$disk_session <- list(session = sess, sessionId = sess$sessionId,
                          resumed = FALSE)
ctx5$render_reply <- function(txt) rendered <<- txt
ctx5$turn_fn <- function(prompt, session) {
    list(reply = "stubbed reply", usage = NULL)
}
out5 <- capture.output(corteza:::run_repl_loop(ctx5))
expect_equal(rendered, "stubbed reply")
expect_equal(ctx5$last_assistant_response, "stubbed reply")
# Post-turn context indicator fires on a successful turn (no LLM here;
# short history stays well under the compaction threshold).
expect_true(any(grepl("context .*%", out5)))

# 5b. Regression: a model-less chat() session (ctx$model NULL,
# model_map$cloud unset, provider unset) used to crash the post-turn
# context meter at context_limit_for_model(NULL). The loop must now
# complete and still print the indicator.
ctx5b <- base_ctx(c("hello"))
ctx5b$provider <- "ollama"
ctx5b$model <- NULL
ctx5b$config <- list()
ctx5b$session <- new.env(parent = emptyenv())
ctx5b$session$history <- list()
ctx5b$session$tasks <- list()
ctx5b$session$tasks_dirty <- FALSE
sess5b <- corteza:::session_new("ollama", "llama3.2", getwd())
ctx5b$disk_session <- list(session = sess5b, sessionId = sess5b$sessionId,
                           resumed = FALSE)
ctx5b$render_reply <- function(txt) invisible(NULL)
ctx5b$turn_fn <- function(prompt, session) {
    list(reply = "ok", usage = NULL)
}
out5b <- capture.output(corteza:::run_repl_loop(ctx5b))
expect_equal(ctx5b$last_assistant_response, "ok")   # completed, no crash
expect_true(any(grepl("context .*%", out5b)))

# 6. /clear after /model: the fresh-session hook must see the UPDATED
# model (regression: the hook must read current state, not stale locals
# captured before /model ran). Also asserts disk_session is reassigned.
seen_model <- NULL
ctx6 <- base_ctx(c("/model fresh-model", "/clear"))
ctx6$session <- new.env(parent = emptyenv())
ctx6$session$model_map <- list(cloud = "old")
ctx6$session$on_tool <- list()
ctx6$model <- "old"
ctx6$disk_session <- list(session = list(sessionId = "old-sess"),
                          sessionId = "old-sess")
ctx6$new_session_fn <- function() {
    seen_model <<- ctx6$model
    list(session = list(sessionId = "new-sess"), sessionId = "new-sess",
         resumed = FALSE)
}
corteza:::run_repl_loop(ctx6)
expect_equal(seen_model, "fresh-model")
expect_equal(ctx6$disk_session$sessionId, "new-sess")

# 6b. /clear kills live subagents and retires their spend. Inject a
# fake registry entry, clear, and assert the registry is emptied while
# the spend lands in the process-run total.
corteza:::subagent_spend_reset()
reg <- corteza:::.subagent_registry
r <- corteza:::.subagent_spend_retired
reg[["clear-test-agent"]] <- list(
    id = "clear-test-agent", seq = 1L, session_key = "agent:main:subagent:cta",
    session = NULL, cumulative_input_tokens = 300L,
    cumulative_output_tokens = 100L, cumulative_total_tokens = 400L,
    cumulative_cost = 0.04, cost_missing = FALSE, query_count = 3L)
ctx6b <- base_ctx(c("/clear"))
ctx6b$session <- new.env(parent = emptyenv())
ctx6b$session$model_map <- list(cloud = "m")
ctx6b$session$on_tool <- list()
ctx6b$disk_session <- list(session = list(sessionId = "s0"), sessionId = "s0")
ctx6b$new_session_fn <- function() {
    list(session = list(sessionId = "s1"), sessionId = "s1", resumed = FALSE)
}
out6b <- capture.output(corteza:::run_repl_loop(ctx6b))
expect_equal(length(ls(reg)), 0L)                       # registry emptied
tot <- corteza:::subagent_spend_total()
expect_equal(tot$cost, 0.04)                            # spend retired
expect_equal(tot$total_tokens, 400L)
expect_equal(tot$n_agents, 1L)
expect_true(any(grepl("killed 1 subagent", out6b)))     # user is told
rm(list = ls(reg), envir = reg)
r$cost <- 0; r$input_tokens <- 0L; r$output_tokens <- 0L
r$total_tokens <- 0L; r$query_count <- 0L; r$n_agents <- 0L
r$cost_missing <- FALSE

# Restore the data dir (no top-level on.exit in tinytest).
if (is.na(old_data)) {
    Sys.unsetenv("R_USER_DATA_DIR")
} else {
    Sys.setenv(R_USER_DATA_DIR = old_data)
}
unlink(tmp_data, recursive = TRUE)

# 7. .repl_context_indicator(): non-empty string reflecting the
# percentage. used = limit/2 -> "50".
ind <- corteza:::.repl_context_indicator(
    used = 100000L, limit = 200000L, palette = empty_palette,
    thresholds = list(warn = 75, high = 90, crit = 95, compact = 90)
)
expect_true(is.character(ind) && nzchar(ind))
expect_true(grepl("50", ind))
expect_true(grepl("compact at 90", ind))

# 9. .repl_interruptible(): an interrupt condition yields the sentinel;
# a plain value passes through unchanged.
interrupted <- corteza:::.repl_interruptible(
    stop(structure(class = c("interrupt", "condition"),
                   list(message = "x"))),
    empty_palette
)
expect_true(inherits(interrupted, "repl_interrupted"))
expect_equal(corteza:::.repl_interruptible(42, empty_palette), 42)

# 10. /model persists the model onto the disk session record so the
# end-of-loop session_save writes the current model, not the stale one.
ctx_model <- base_ctx(c("/model newmodel"))
ctx_model$session <- new.env(parent = emptyenv())
ctx_model$session$model_map <- list(cloud = "old")
ctx_model$model <- "old"
ctx_model$disk_session <- list(session = list(sessionId = "s", model = "old"),
                               sessionId = "s")
corteza:::run_repl_loop(ctx_model)
expect_equal(ctx_model$disk_session$session$model, "newmodel")

# 8. tools_filter narrows the /tools view. Regression for the CLI
# repoint: --tools must reach the turn session (its tools_filter), not
# just the banner. chat_format_tools_list is what the loop's /tools
# handler renders, keyed on session$tools_filter.
corteza:::ensure_skills()
sess_filt <- new.env(parent = emptyenv())
sess_filt$tools_filter <- c("read_file")
tools_out <- corteza:::chat_format_tools_list(sess_filt)
expect_true(grepl("read_file", tools_out, fixed = TRUE))
expect_false(grepl("write_file", tools_out, fixed = TRUE))

# 11. The shared REPL installs its durable compaction hook exactly once,
# preserves an existing caller hook, and accounts for the summarizer request.
# Direct turn() callers install their own persistence hook; this pins the
# interactive path that used to own auto-compaction outside turn().
ctx_compact <- base_ctx(character())
ctx_compact$config <- list(memory_flush_enabled = FALSE)
ctx_compact$disk_session <- NULL
ctx_compact$session <- new.env(parent = emptyenv())
ctx_compact$session$sessionId <- "compact-test"
prior_compaction <- NULL
ctx_compact$session$on_compaction <- function(event) {
    prior_compaction <<- event
}
expect_true(isTRUE(corteza:::.repl_install_compaction_hook(ctx_compact)))
expect_false(isTRUE(corteza:::.repl_install_compaction_hook(ctx_compact)))

compact_event <- list(
    reason = "threshold",
    summary = "older work summarized",
    usage = list(input_tokens = 12L, output_tokens = 3L,
                 total_tokens = 15L, cost = 0.01)
)
ctx_compact$session$on_compaction(compact_event)
expect_identical(prior_compaction, compact_event)
expect_equal(length(ctx_compact$session$spend$segments), 1L)
expect_equal(ctx_compact$session$spend$segments[[1L]]$input_tokens, 12L)
expect_equal(ctx_compact$session$spend$segments[[1L]]$output_tokens, 3L)
expect_equal(ctx_compact$session$spend$segments[[1L]]$cost, 0.01)

# 12. An automatic compaction runs the memory flush first only where
# the config turns it on (memory_flush_enabled, off by default). The
# flush is replaced by a counter: this is about whether the hook calls
# it, and nothing is sent.
local({
    flush <- corteza:::run_memory_flush
    flushes <- 0L
    utils::assignInNamespace("run_memory_flush", function(ctx) {
        flushes <<- flushes + 1L
        list(content = "NO_REPLY", history = list(),
             usage = list(input_tokens = 5L, output_tokens = 1L,
                          total_tokens = 6L, cost = 0.001))
    }, "corteza")
    on.exit(utils::assignInNamespace("run_memory_flush", flush, "corteza"),
            add = TRUE)

    fire <- function(config, reason) {
        ctx <- base_ctx(character())
        ctx$config <- config
        ctx$disk_session <- NULL
        ctx$session <- new.env(parent = emptyenv())
        ctx$session$sessionId <- "flush-test"
        corteza:::.repl_install_compaction_hook(ctx)
        before <- flushes
        said <- capture.output(ctx$session$on_compaction(list(
            reason = reason, summary = "older work summarized",
            usage = list(input_tokens = 12L, output_tokens = 3L,
                         total_tokens = 15L, cost = 0.01))))
        list(ran = flushes - before,
             said = any(grepl("Flushing memories", said, fixed = TRUE)),
             spent = ctx$session$spend$segments[[1L]]$input_tokens)
    }
    for (why in c("threshold", "preflight", "checkpoint", "idle",
                  "context_overflow")) {
        # No key, and the key set to false: no flush and no line about
        # one. The session is charged the summary's 12 input tokens.
        for (cfg in list(list(), list(memory_flush_enabled = FALSE),
                         list(memory_flush_enabled = "true"))) {
            got <- fire(cfg, why)
            expect_identical(got$ran, 0L)
            expect_false(got$said)
            expect_equal(got$spent, 12)
        }
        # On: the flush runs first, and its 5 are charged with the 12.
        got <- fire(list(memory_flush_enabled = TRUE), why)
        expect_identical(got$ran, 1L)
        expect_true(got$said)
        expect_equal(got$spent, 17)
    }
    # A compaction asked for by hand never ran the flush, on or off.
    got <- fire(list(memory_flush_enabled = TRUE), "manual")
    expect_identical(got$ran, 0L)
    expect_false(got$said)
})

# 13. /tasks. With the task list off (the default) it says how to turn
# the list on, after any tasks the session still holds from before.
# With it on it lists them, or says there are none.
local({
    tasks_out <- function(enabled, tasks = list()) {
        ctx <- base_ctx(c("/tasks"))
        ctx$session <- new.env(parent = emptyenv())
        ctx$session$channel <- "cli"
        ctx$session$config <- list(task_list_enabled = enabled)
        ctx$session$tasks <- tasks
        capture.output(corteza:::run_repl_loop(ctx))
    }
    held <- list(list(text = "kept from before", status = "pending"))
    off_note <- "The task list is off."
    has <- function(out, text) any(grepl(text, out, fixed = TRUE))

    out <- tasks_out(FALSE)
    expect_true(has(out, off_note))
    expect_true(has(out, "\"task_list_enabled\": true"))
    expect_true(has(out, corteza:::corteza_config_path("config.json")))
    expect_false(has(out, "No tasks."))

    out <- tasks_out(FALSE, held)
    expect_true(has(out, "kept from before"))
    expect_true(has(out, off_note))
    # The list prints first, the note after it. A line that is missing
    # gives NA here, which fails the assertion without stopping the file.
    first <- function(text) grep(text, out, fixed = TRUE)[1L]
    expect_true(isTRUE(first("kept from before") < first(off_note)))

    out <- tasks_out(TRUE)
    expect_true(has(out, "No tasks."))
    expect_false(has(out, off_note))

    out <- tasks_out(TRUE, held)
    expect_true(has(out, "kept from before"))
    expect_false(has(out, off_note))
    expect_false(has(out, "No tasks."))

    # A session with no config at all, as a caller's own ctx may build.
    ctx <- base_ctx(c("/tasks"))
    ctx$session <- new.env(parent = emptyenv())
    ctx$session$channel <- "console"
    expect_true(has(capture.output(corteza:::run_repl_loop(ctx)), off_note))
})

# 14. The compaction check after a reply measures the system text the
# next request sends: with the task list off it holds no task text,
# and with it on it does. The check itself is replaced by a recorder.
local({
    old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
    Sys.setenv(R_USER_DATA_DIR = file.path(tempdir(), "repl_test_data"))
    on.exit(if (is.na(old_data)) {
        Sys.unsetenv("R_USER_DATA_DIR")
    } else {
        Sys.setenv(R_USER_DATA_DIR = old_data)
    }, add = TRUE)
    compact <- corteza:::maybe_compact_turn_session
    measured <- NULL
    utils::assignInNamespace("maybe_compact_turn_session",
        function(session, config, ..., system = NULL) {
            measured <<- system
            FALSE
        }, "corteza")
    on.exit(utils::assignInNamespace("maybe_compact_turn_session", compact,
                                     "corteza"), add = TRUE)

    after_reply <- function(enabled) {
        measured <<- NULL
        ctx <- base_ctx(c("hello"))
        ctx$provider <- "ollama"
        ctx$model <- "llama3.2"
        ctx$config <- list()
        ctx$session <- new.env(parent = emptyenv())
        ctx$session$channel <- "cli"
        ctx$session$system <- "BASE"
        ctx$session$config <- list(task_list_enabled = enabled)
        ctx$session$history <- list()
        ctx$session$tasks <- list()
        ctx$session$tasks_dirty <- FALSE
        sess <- corteza:::session_new("ollama", "llama3.2", getwd())
        ctx$disk_session <- list(session = sess, sessionId = sess$sessionId,
                                 resumed = FALSE)
        ctx$render_reply <- function(txt) invisible(NULL)
        ctx$turn_fn <- function(prompt, session) {
            list(reply = "ok", usage = NULL)
        }
        capture.output(corteza:::run_repl_loop(ctx))
        measured
    }
    expect_identical(after_reply(FALSE), "BASE")
    with_list <- after_reply(TRUE)
    expect_true(startsWith(with_list, "BASE"))
    expect_true(grepl("task_create", with_list, fixed = TRUE))
})
