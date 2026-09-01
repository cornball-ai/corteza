# Provider fallback on limit errors (turn() -> .agent_with_fallback()).

corteza:::.fallback_reset()

# .parse_model_spec(): "model provider", bare model takes the default,
# empty or provider-less specs are NULL.
expect_equal(corteza:::.parse_model_spec("gpt-5.5 openai_codex"),
             list(model = "gpt-5.5", provider = "openai_codex"))
expect_equal(corteza:::.parse_model_spec("  claude-haiku-4-5   anthropic  "),
             list(model = "claude-haiku-4-5", provider = "anthropic"))
expect_equal(corteza:::.parse_model_spec("gpt-5.5", default_provider = "openai"),
             list(model = "gpt-5.5", provider = "openai"))
expect_null(corteza:::.parse_model_spec("gpt-5.5"))
expect_null(corteza:::.parse_model_spec(""))
expect_null(corteza:::.parse_model_spec(NULL, default_provider = "openai"))

# .session_fallback(): session field wins, cwd config is the fallback,
# nothing configured is an empty chain, junk entries are dropped.
s <- new.env()
s$provider <- "anthropic_claude"
expect_equal(corteza:::.session_fallback(s), list())
s$config <- list(fallback = c("gpt-5.5 openai_codex", "", "claude-haiku-4-5"))
expect_equal(corteza:::.session_fallback(s),
             list(list(model = "gpt-5.5", provider = "openai_codex"),
                  list(model = "claude-haiku-4-5", provider = "anthropic_claude")))
s$fallback <- "qwen3.5:9b ollama"
expect_equal(corteza:::.session_fallback(s),
             list(list(model = "qwen3.5:9b", provider = "ollama")))

# .fallback_cooldown(): session, then config, then the default; junk
# falls back to the default.
expect_equal(corteza:::.fallback_cooldown(new.env()), 30)
c1 <- new.env(); c1$config <- list(fallback_cooldown_minutes = 5)
expect_equal(corteza:::.fallback_cooldown(c1), 5)
c1$fallback_cooldown <- "12"
expect_equal(corteza:::.fallback_cooldown(c1), 12)
c1$fallback_cooldown <- "soon"
expect_equal(corteza:::.fallback_cooldown(c1), 30)
c1$fallback_cooldown <- -1
expect_equal(corteza:::.fallback_cooldown(c1), 30)

# .is_limit_error(): llm.api status prefixes and limit bodies, not
# ordinary client errors or context-length "exceeded".
lim <- function(msg) corteza:::.is_limit_error(simpleError(msg))
expect_true(lim("API error (429): This request would exceed your account's rate limit"))
expect_true(lim("API error (529): Overloaded"))
expect_true(lim("API error (503): Service Unavailable"))
expect_true(lim("API error (400): usage_limit_reached"))
expect_true(lim("insufficient_quota"))
expect_false(lim("API error (400): prompt is too long: context length exceeded"))
expect_false(lim("API error (401): invalid x-api-key"))
expect_false(lim("Tool error: bash exited 1"))

# Cooldown bookkeeping is per provider and time-bounded.
corteza:::.fallback_reset()
t0 <- as.POSIXct("2026-09-01 12:00:00", tz = "UTC")
expect_false(corteza:::.fallback_limited("anthropic_claude", now = t0))
corteza:::.fallback_mark("anthropic_claude", minutes = 30, now = t0)
expect_true(corteza:::.fallback_limited("anthropic_claude", now = t0 + 29 * 60))
expect_false(corteza:::.fallback_limited("anthropic_claude", now = t0 + 31 * 60))
expect_false(corteza:::.fallback_limited("openai_codex", now = t0))
corteza:::.fallback_reset()
expect_null(corteza:::.fallback_until("anthropic_claude"))

# .agent_with_fallback(): a fake agent records every (model, provider)
# it was asked for and answers per a script keyed on provider.
make_call <- function(script, log) {
    function(args) {
        log$calls <- c(log$calls, list(list(model = args$model,
                                            provider = args$provider,
                                            web_search = args$web_search)))
        step <- script[[args$provider]]
        if (is.function(step)) step(args) else step
    }
}
new_fb_session <- function(fallback = c("gpt-5.5 openai_codex",
                                        "claude-haiku-4-5 anthropic")) {
    s <- new.env()
    s$provider <- "anthropic_claude"
    s$fallback <- fallback
    s$fallback_cooldown <- 30
    s$history <- list()
    s
}
base_args <- list(prompt = "hi", model = "claude-opus-5",
                  provider = "anthropic_claude", web_search = TRUE)

# Happy path: primary answers, nothing else is called, no cooldown.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
out <- corteza:::.agent_with_fallback(base_args, new_fb_session(),
    .call = make_call(list(anthropic_claude = list(content = "primary")), log))
expect_equal(out$content, "primary")
expect_equal(length(log$calls), 1L)
expect_false(corteza:::.fallback_limited("anthropic_claude"))

# Run a call while collecting its message() output, muffled.
with_msgs <- function(expr) {
    msgs <- character()
    value <- withCallingHandlers(expr, message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    list(value = value, msgs = msgs)
}

# Primary hits a limit before any progress: the first fallback answers,
# the primary is put in cooldown, and the log says so.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
run <- with_msgs(corteza:::.agent_with_fallback(base_args, new_fb_session(),
    .call = make_call(list(
        anthropic_claude = function(args) stop("API error (429): rate limit"),
        openai_codex = list(content = "codex")), log)))
expect_equal(run$value$content, "codex")
expect_true(any(grepl("anthropic_claude/claude-opus-5 hit a limit", run$msgs)))
expect_true(any(grepl("openai_codex/gpt-5.5 answered", run$msgs)))
expect_equal(vapply(log$calls, `[[`, "", "provider"),
             c("anthropic_claude", "openai_codex"))
expect_equal(log$calls[[2]]$model, "gpt-5.5")
expect_true(corteza:::.fallback_limited("anthropic_claude"))
expect_false(corteza:::.fallback_limited("openai_codex"))

# While the primary is cooling it is skipped without a call; the
# fallback answers directly.
log <- new.env(); log$calls <- list()
run <- with_msgs(corteza:::.agent_with_fallback(base_args, new_fb_session(),
    .call = make_call(list(openai_codex = list(content = "codex again")), log)))
expect_equal(run$value$content, "codex again")
expect_true(any(grepl("limit cooldown", run$msgs)))
expect_equal(vapply(log$calls, `[[`, "", "provider"), "openai_codex")

# Two limits in a row walk the whole chain; the last entry answers and
# both tripped providers are cooling.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
out <- suppressMessages(corteza:::.agent_with_fallback(base_args, new_fb_session(),
    .call = make_call(list(
        anthropic_claude = function(args) stop("API error (429): rate limit"),
        openai_codex = function(args) stop("API error (429): usage_limit_reached"),
        anthropic = list(content = "haiku")), log)))
expect_equal(out$content, "haiku")
expect_equal(vapply(log$calls, `[[`, "", "provider"),
             c("anthropic_claude", "openai_codex", "anthropic"))
expect_true(corteza:::.fallback_limited("openai_codex"))

# Every provider limited: the last limit error is what surfaces.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
expect_error(suppressMessages(corteza:::.agent_with_fallback(base_args, new_fb_session(),
    .call = make_call(list(
        anthropic_claude = function(args) stop("API error (429): a"),
        openai_codex = function(args) stop("API error (429): b"),
        anthropic = function(args) stop("API error (529): c")), log))),
    "API error \\(529\\): c")

# Every provider already cooling: no call is made, the error names them.
log <- new.env(); log$calls <- list()
expect_error(corteza:::.agent_with_fallback(base_args, new_fb_session(),
    .call = make_call(list(), log)),
    "every provider is in a limit cooldown")
expect_equal(length(log$calls), 0L)

# A non-limit error is rethrown as-is, no fallback, no cooldown.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
expect_error(corteza:::.agent_with_fallback(base_args, new_fb_session(),
    .call = make_call(list(
        anthropic_claude = function(args) stop("API error (401): invalid key")), log)),
    "API error \\(401\\)")
expect_equal(length(log$calls), 1L)
expect_false(corteza:::.fallback_limited("anthropic_claude"))

# A limit hit after the run made progress (history grew, so tools ran)
# is not retried: cooldown is set, the error surfaces, no second call.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
s_prog <- new_fb_session()
expect_error(suppressMessages(corteza:::.agent_with_fallback(base_args, s_prog,
    .call = make_call(list(
        anthropic_claude = function(args) {
            s_prog$history <- list(list(role = "user", content = "hi"),
                                   list(role = "assistant", content = "..."))
            stop("API error (429): mid-run")
        }), log))),
    "mid-run")
expect_equal(length(log$calls), 1L)
expect_true(corteza:::.fallback_limited("anthropic_claude"))

# Native web search is dropped for a fallback provider that lacks it.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
suppressMessages(corteza:::.agent_with_fallback(base_args,
    new_fb_session(fallback = "qwen3.5:9b ollama"),
    .call = make_call(list(
        anthropic_claude = function(args) stop("API error (429): rate limit"),
        ollama = list(content = "local")), log)))
expect_true(isTRUE(log$calls[[1]]$web_search))
expect_null(log$calls[[2]]$web_search)

# No chain configured: a limit error is an ordinary error, but the
# cooldown is still recorded.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
expect_error(suppressMessages(corteza:::.agent_with_fallback(base_args,
    new_fb_session(fallback = NULL),
    .call = make_call(list(
        anthropic_claude = function(args) stop("API error (429): rate limit")), log))),
    "API error \\(429\\)")
expect_equal(length(log$calls), 1L)

corteza:::.fallback_reset()
