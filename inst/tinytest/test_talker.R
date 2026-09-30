library(tinytest)

# Talker mode: the session runs on a fast model with read-only tools and
# the job tools; its configured model moves to the doer. Nothing here
# calls a provider -- the job worker's init and run are replaced.

state <- tempfile("talker-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

# --- Config ---
expect_null(corteza:::talker_config(list()))
expect_null(corteza:::talker_config(list(talker = list(enabled = FALSE))))
expect_identical(corteza:::talker_config(
    list(talker = list(enabled = TRUE, model = "m")))$model, "m")

# --- Enabling moves the configured model to the doer ---
s <- corteza::new_session("matrix", provider = "anthropic",
                          model_map = list(cloud = "claude-opus-5-5"),
                          system = "base prompt")
corteza:::talker_enable(s, list(enabled = TRUE))
expect_true(isTRUE(s$talker))
expect_identical(s$model_map$cloud, "claude-haiku-4-5-20251001")
expect_identical(corteza:::.resolve_model(s), "claude-haiku-4-5-20251001")
expect_identical(s$doer_model, "claude-opus-5-5")
expect_identical(s$doer_provider, "anthropic")
expect_identical(s$tools_filter, corteza:::TALKER_TOOLS)
# No write or exec tool is the talker's own.
expect_false(any(c("write_file", "replace_in_file", "bash", "run_r",
                   "run_r_script") %in% s$tools_filter))
expect_true(grepl("^base prompt", s$system))
expect_true(grepl("Never guess", s$system, fixed = TRUE))
# The doer's spec gets the configured model back.
spec <- corteza:::job_worker_spec(s)
expect_identical(spec$model, "claude-opus-5-5")
expect_identical(spec$provider, "anthropic")
expect_identical(spec$channel, "matrix")

# The provider picks the default talker model; config can name another.
o <- corteza::new_session("matrix", provider = "openai",
                          model_map = list(cloud = "gpt-6"))
corteza:::talker_enable(o, list(enabled = TRUE))
expect_identical(o$model_map$cloud, "gpt-6-luna")
expect_identical(o$doer_model, "gpt-6")
x <- corteza::new_session("matrix", provider = "anthropic")
corteza:::talker_enable(x, list(enabled = TRUE, model = "claude-sonnet-5-5"))
expect_identical(x$model_map$cloud, "claude-sonnet-5-5")
# A provider with no default and no configured model is an error, not a
# silent fall-through to some other model.
m <- corteza::new_session("matrix", provider = "anthropic")
m$provider <- "moonshot"
expect_error(corteza:::talker_enable(m, list(enabled = TRUE)),
             "needs a model")

# --- The job tools are registered and shown only to talkers ---
corteza::ensure_skills()
tools <- corteza:::skills_as_api_tools(corteza:::TALKER_TOOLS)
names_of <- function(ts) vapply(ts, function(t) t$name, "")
expect_true(all(c("delegate", "job_status", "job_cancel") %in%
                names_of(tools)))
# The schema comes from the Rd: task is a required string.
del <- tools[[which(names_of(tools) == "delegate")]]
expect_identical(del$input_schema$properties$task$type, "string")
expect_true("task" %in% del$input_schema$required)
expect_false("ctx" %in% names(del$input_schema$properties))
kept <- corteza:::.talker_filter_tools(tools, FALSE)
expect_false(any(c("delegate", "job_status", "job_cancel") %in%
                 names_of(kept)))
expect_true("read_file" %in% names_of(kept))
expect_identical(length(corteza:::.talker_filter_tools(tools, TRUE)),
                 length(tools))

# --- The tools drive the session's jobs ---
t <- corteza::new_session("matrix", provider = "anthropic")
t$cwd <- tempdir()
t$config <- list()
t$job_key <- "!room:ex"
t$job_requester <- "@troy:ex"
t$job_origin <- list(room = "!room:ex", thread = NULL)
t$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE),
                          run_fn = function(task) {
                              Sys.sleep(as.numeric(task))
                              list(reply = "slept")
                          })
ctx <- list(session = t)
res <- corteza:::tool_delegate("5", ctx = ctx)
expect_false(isTRUE(res$isError))
id <- regmatches(res$content[[1L]]$text,
                 regexpr("[0-9]{8}T[0-9]{6}-[0-9a-f]{8}",
                         res$content[[1L]]$text))
j <- corteza:::job_read(id)
expect_identical(j$requester, "@troy:ex")
expect_identical(j$origin$room, "!room:ex")
expect_identical(j$origin$session_key, "!room:ex")
expect_true(grepl("do not answer", res$content[[1L]]$text))

st <- corteza:::tool_job_status(ctx = ctx)
expect_true(grepl(id, st$content[[1L]]$text, fixed = TRUE))
one <- corteza:::tool_job_status(id = id, ctx = ctx)
expect_true(grepl("running", one$content[[1L]]$text))
expect_true(isTRUE(corteza:::tool_job_status(id = "20260101T000000-00000000",
                                              ctx = ctx)$isError))

cx <- corteza:::tool_job_cancel(id, ctx = ctx)
expect_true(grepl("Cancellation requested", cx$content[[1L]]$text))
corteza:::job_pump(t)
expect_identical(corteza:::job_read(id)$status, "cancelled")
expect_identical(corteza:::job_read(id)$outcome$reason,
                 "stopped mid-job; changes it made before stopping are kept")
again <- corteza:::tool_job_cancel(id, ctx = ctx)
expect_true(grepl("already ended", again$content[[1L]]$text))
# Without a session the tools refuse rather than guessing one.
expect_true(isTRUE(corteza:::tool_delegate("x")$isError))

corteza:::job_worker_close(t)
if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(state, recursive = TRUE)
