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
# The talker is told it may search: the provider's web search is on for
# it although no listed tool provides it.
expect_true(grepl("web search is available", s$system, fixed = TRUE))
expect_true(isTRUE(corteza:::.session_web_search(s)))
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
# The subscription providers the bots run on have defaults too, and the
# doer keeps the provider the session was configured with.
for (p in list(c("anthropic_claude", "claude-haiku-4-5-20251001"),
               c("openai_codex", "gpt-6-luna"))) {
    b <- corteza::new_session("matrix", provider = "anthropic",
                              model_map = list(cloud = "claude-opus-5"))
    b$provider <- p[1L]
    corteza:::talker_enable(b, list(enabled = TRUE))
    expect_identical(b$model_map$cloud, p[2L], info = p[1L])
    expect_identical(b$provider, p[1L], info = p[1L])
    expect_identical(b$doer_provider, p[1L], info = p[1L])
    expect_identical(b$doer_model, "claude-opus-5", info = p[1L])
}
# --- Reasoning settings move to the doer with the model ---
# They were chosen for the configured model. Sent with the talker's fast
# model they are a 400 ("This model does not support the effort
# parameter"), so every reply fails -- which is what a bot configured
# with reasoning_effort did the first time talker mode was turned on.
e <- corteza::new_session("matrix", provider = "anthropic",
                          model_map = list(cloud = "claude-opus-5"),
                          reasoning_effort = "xhigh",
                          thinking_budget_tokens = 4096L)
# A config-level setting is the configured model's too.
e$config <- list(reasoning_effort = "max", thinking_budget_tokens = 2048L)
corteza:::talker_enable(e, list(enabled = TRUE))
expect_null(corteza:::.session_reasoning_effort(e))
expect_null(corteza:::.session_thinking_budget(e))
# What turn() would send for the talker carries no effort at all.
sent <- corteza:::.gate_reasoning_args(
    list(reasoning_effort = corteza:::.session_reasoning_effort(e)),
    e$provider)
expect_null(sent$reasoning_effort)
expect_null(sent$output_config)
# The doer gets them.
expect_identical(e$doer_reasoning_effort, "xhigh")
expect_identical(e$doer_thinking_budget, 4096L)
dspec <- corteza:::job_worker_spec(e)
expect_identical(dspec$model, "claude-opus-5")
expect_identical(dspec$reasoning_effort, "xhigh")
expect_identical(dspec$thinking_budget_tokens, 4096L)
# A reviewer on the same model keeps them; on another model it gets the
# provider's defaults unless its own config names a setting.
expect_identical(corteza:::job_worker_spec(e, "reviewer")$reasoning_effort,
                 "xhigh")
e$config$jobs <- list(reviewer = list(model = "claude-haiku-4-5-20251001"))
other <- corteza:::job_worker_spec(e, "reviewer")
expect_identical(other$model, "claude-haiku-4-5-20251001")
expect_null(other$reasoning_effort)
expect_null(other$thinking_budget_tokens)
e$config$jobs$reviewer$reasoning_effort <- "low"
expect_identical(corteza:::job_worker_spec(e, "reviewer")$reasoning_effort,
                 "low")
# A doer moved to another model by jobs$model does not inherit them.
e$config$jobs <- list(model = "claude-sonnet-5-5")
expect_null(corteza:::job_worker_spec(e)$reasoning_effort)
e$config$jobs$reasoning_effort <- "high"
expect_identical(corteza:::job_worker_spec(e)$reasoning_effort, "high")
# Only the config's own effort when nothing was set on the session.
c2 <- corteza::new_session("matrix", provider = "anthropic",
                           model_map = list(cloud = "claude-opus-5"))
c2$config <- list(reasoning_effort = "max")
corteza:::talker_enable(c2, list(enabled = TRUE))
expect_null(corteza:::.session_reasoning_effort(c2))
expect_identical(corteza:::job_worker_spec(c2)$reasoning_effort, "max")
# The talker config can give the talker a setting of its own.
t2 <- corteza::new_session("matrix", provider = "openai",
                           model_map = list(cloud = "gpt-6"),
                           reasoning_effort = "high")
corteza:::talker_enable(t2, list(enabled = TRUE, reasoning_effort = "low"))
expect_identical(corteza:::.session_reasoning_effort(t2), "low")
expect_identical(corteza:::job_worker_spec(t2)$reasoning_effort, "high")
# Without talker mode nothing changes: session, then config.
n <- corteza::new_session("matrix", provider = "anthropic")
n$config <- list(reasoning_effort = "max")
expect_identical(corteza:::.session_reasoning_effort(n), "max")
n$reasoning_effort <- "low"
expect_identical(corteza:::.session_reasoning_effort(n), "low")

# --- A thinking type for the talker's own model ---
# Claude Sonnet 5.5 thinks by default, rejects "disabled", and answers
# without thinking first only under thinking = {type: "between_tools"}.
# The talker config names the type; it is the model's own word.
k <- corteza::new_session("matrix", provider = "anthropic_claude",
                          model_map = list(cloud = "claude-opus-5-5"),
                          reasoning_effort = "xhigh")
k$thinking <- "adaptive"
corteza:::talker_enable(k, list(enabled = TRUE, model = "claude-sonnet-5-5",
                                thinking = "between_tools"))
expect_identical(corteza:::.resolve_model(k), "claude-sonnet-5-5")
expect_identical(corteza:::.session_thinking(k), "between_tools")
# The talker sends no effort, so the API default applies; "between_tools"
# is refused at xhigh, which is the doer's setting.
expect_null(corteza:::.session_reasoning_effort(k))
# The session's own thinking type went to the doer with its model.
kspec <- corteza:::job_worker_spec(k)
expect_identical(kspec$model, "claude-opus-5-5")
expect_identical(kspec$reasoning_effort, "xhigh")
expect_identical(kspec$thinking, "adaptive")
# A reviewer on another model takes neither.
k$config <- list(jobs = list(reviewer = list(model = "claude-haiku-4-5")))
expect_null(corteza:::job_worker_spec(k, "reviewer")$thinking)
# No talker thinking configured: none is sent, and not the config's.
k2 <- corteza::new_session("matrix", provider = "anthropic")
k2$config <- list(thinking = "adaptive")
corteza:::talker_enable(k2, list(enabled = TRUE))
expect_null(corteza:::.session_thinking(k2))
expect_identical(k2$doer_thinking, "adaptive")
# It is the Anthropic body field: kept on that wire, dropped on others.
gated <- corteza:::.gate_reasoning_args(
    list(thinking = list(type = "between_tools")), "anthropic_claude")
expect_identical(gated$thinking, list(type = "between_tools"))
expect_null(corteza:::.gate_reasoning_args(
    list(thinking = list(type = "between_tools")), "openai_codex")$thinking)
# A budget in a config is not a thinking type. `$thinking` on a list
# matches `thinking_budget_tokens` by prefix, so each of these would
# read the budget as the type if looked up that way.
pm <- corteza::new_session("matrix", provider = "anthropic")
pm$config <- list(thinking_budget_tokens = 2048L,
                  jobs = list(thinking_budget_tokens = 4096L,
                              reviewer = list(thinking_budget_tokens = 1024L)))
expect_null(corteza:::.session_thinking(pm))
expect_null(corteza:::job_worker_spec(pm)$thinking)
expect_null(corteza:::job_worker_spec(pm, "reviewer")$thinking)
corteza:::talker_enable(pm, list(enabled = TRUE,
                                 thinking_budget_tokens = 2048L))
expect_null(corteza:::.session_thinking(pm))
expect_null(pm$doer_thinking)
expect_identical(names(corteza:::.gate_reasoning_args(
    list(thinking_budget_tokens = 2048L), "openai")), character())
expect_identical(corteza:::.gate_reasoning_args(
    list(thinking_budget_tokens = 2048L), "anthropic")$thinking_budget_tokens,
    2048L)
# The same prefix rule applies to the call into llm.api::agent(), whose
# `thinking_budget_tokens` formal sits ahead of `...`: `thinking` alone
# is taken as the budget. Naming the budget keeps them apart. Checked
# against agent()'s real formals, with the body swapped for a probe.
# llm.api 0.1.9.13 adds a `thinking` formal; the probe reads the type
# from there when it exists and from the dots when it does not, so the
# prepared call is checked the same way against either.
agent_formals <- formals(llm.api::agent)
probe <- function() NULL
formals(probe) <- agent_formals
body(probe) <- if ("thinking" %in% names(agent_formals)) {
    quote(list(budget = thinking_budget_tokens, type = thinking))
} else {
    quote(list(budget = thinking_budget_tokens,
               type = list(...)[["thinking"]]))
}
th_args <- list(prompt = "p", thinking = list(type = "between_tools"))
if (!"thinking" %in% names(agent_formals)) {
    # Negative control: unprepared, the type lands in the budget.
    expect_identical(do.call(probe, th_args)$budget,
                     list(type = "between_tools"))
}
sent <- do.call(probe, corteza:::.agent_call_args(th_args))
expect_null(sent$budget)
expect_identical(sent$type, list(type = "between_tools"))
# Nothing to do without a thinking type, and a set budget is left alone.
expect_identical(corteza:::.agent_call_args(list(prompt = "p")),
                 list(prompt = "p"))
expect_identical(corteza:::.agent_call_args(
    list(prompt = "p", thinking_budget_tokens = 2048L)),
    list(prompt = "p", thinking_budget_tokens = 2048L))
# A malformed value is an error where it is read.
expect_error(corteza:::talker_enable(
    corteza::new_session("matrix", provider = "anthropic"),
    list(enabled = TRUE, thinking = c("a", "b"))), "thinking must be")
expect_error(corteza:::talker_enable(
    corteza::new_session("matrix", provider = "anthropic"),
    list(enabled = TRUE, thinking = TRUE)), "thinking must be")

# A thinking type belongs to one model. A fallback to another model on
# the same wire must not carry it: "between_tools" is a 400 on Haiku,
# and a 400 is not a limit error, so the fallback would fail outright.
local({
    corteza:::.fallback_reset()
    on.exit(corteza:::.fallback_reset(), add = TRUE)
    seen <- list()
    fake <- function(args) {
        seen[[length(seen) + 1L]] <<- args
        if (identical(args$model, "claude-sonnet-5-5")) {
            stop("API error (429): rate limit")
        }
        list(content = "ok", history = list(), usage = list())
    }
    fs <- new.env()
    fs$config <- list(fallback = "claude-haiku-4-5 anthropic")
    res <- corteza:::.agent_with_fallback(
        list(model = "claude-sonnet-5-5", provider = "anthropic_claude",
             thinking = list(type = "between_tools")), fs, .call = fake)
    expect_identical(res$content, "ok")
    expect_identical(length(seen), 2L)
    expect_identical(seen[[1L]]$thinking, list(type = "between_tools"))
    expect_identical(seen[[2L]]$model, "claude-haiku-4-5")
    expect_null(seen[[2L]]$thinking)
})

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
