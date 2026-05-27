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
    ctx$ws_enabled <- FALSE
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

# 3. /model: a slash command that mutates session + ctx state.
ctx3 <- base_ctx(c("/model kimi-test"))
ctx3$session <- new.env(parent = emptyenv())
ctx3$session$model_map <- list(cloud = "old-model")
ctx3$model <- "old-model"
corteza:::run_repl_loop(ctx3)
expect_equal(ctx3$model, "kimi-test")
expect_equal(ctx3$session$model_map$cloud, "kimi-test")

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
reg <- corteza:::.subagent_registry
rm(list = ls(reg), envir = reg)
r <- corteza:::.subagent_spend_retired
r$cost <- 0; r$input_tokens <- 0L; r$output_tokens <- 0L
r$total_tokens <- 0L; r$query_count <- 0L; r$n_agents <- 0L
r$cost_missing <- FALSE
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
