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
corteza:::run_repl_loop(ctx5)
expect_equal(rendered, "stubbed reply")
expect_equal(ctx5$last_assistant_response, "stubbed reply")

# Restore the data dir (no top-level on.exit in tinytest).
if (is.na(old_data)) {
    Sys.unsetenv("R_USER_DATA_DIR")
} else {
    Sys.setenv(R_USER_DATA_DIR = old_data)
}
unlink(tmp_data, recursive = TRUE)
