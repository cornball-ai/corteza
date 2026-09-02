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

# -- cross-wire history ------------------------------------------------
# A history carries the content vocabulary of the wire that produced it,
# and llm.api replays what it is handed. Sending an Anthropic history
# down a Responses-wire fallback is the 400 a Matrix bot hit the moment
# an Anthropic usage limit diverted it mid-conversation:
#   API error (400): Invalid value: 'thinking'.

# .history_shape(): nothing wire-specific is portable.
expect_identical(corteza:::.history_shape(NULL), "portable")
expect_identical(corteza:::.history_shape(list()), "portable")
expect_identical(corteza:::.history_shape(list(
    list(role = "user", content = "hi"),
    list(role = "assistant", content = "hello"))), "portable")
# A Responses-shaped block list is portable to the wires that speak it;
# the shape test only fires on vocabulary that cannot cross.
expect_identical(corteza:::.history_shape(list(
    list(role = "assistant",
         content = list(list(type = "output_text", text = "x"))))),
    "portable")

# Anthropic markers.
expect_identical(corteza:::.history_shape(list(
    list(role = "assistant",
         content = list(list(type = "thinking", thinking = "hmm"),
                        list(type = "text", text = "hi"))))), "anthropic")
expect_identical(corteza:::.history_shape(list(
    list(role = "assistant",
         content = list(list(type = "tool_use", id = "t1", name = "run_r"))))),
    "anthropic")
expect_identical(corteza:::.history_shape(list(
    list(role = "user",
         content = list(list(type = "tool_result", tool_use_id = "t1"))))),
    "anthropic")

# Responses markers, both entry types the codex wire leaves in history.
expect_identical(corteza:::.history_shape(list(
    list(type = ".openai_codex_output", output = list()))), "responses")
expect_identical(corteza:::.history_shape(list(
    list(type = "function_call_output", call_id = "c1", output = "ok"))),
    "responses")

# Junk entries don't derail detection.
expect_identical(corteza:::.history_shape(list("a string", NULL,
    list(role = "assistant",
         content = list(list(type = "thinking", thinking = "x"))))),
    "anthropic")

# .history_compatible(): chat-completions wires speak neither
# vocabulary, so they are only reachable with a portable history.
for (p in c("anthropic", "anthropic_claude", "openai", "openai_codex",
            "moonshot", "ollama")) {
    expect_true(corteza:::.history_compatible("portable", p))
}
expect_true(corteza:::.history_compatible("anthropic", "anthropic"))
expect_true(corteza:::.history_compatible("anthropic", "anthropic_claude"))
expect_false(corteza:::.history_compatible("anthropic", "openai_codex"))
expect_false(corteza:::.history_compatible("anthropic", "moonshot"))
expect_true(corteza:::.history_compatible("responses", "openai_codex"))
expect_true(corteza:::.history_compatible("responses", "openai"))
expect_false(corteza:::.history_compatible("responses", "anthropic_claude"))
expect_false(corteza:::.history_compatible("responses", "ollama"))

# The bot's actual chain: anthropic_claude primary, then codex, then an
# API-key haiku. With an Anthropic-shaped history the codex candidate is
# skipped and haiku answers -- where before, codex returned a 400 that
# (not being a limit error) stopped the walk before haiku was reached.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
anth_hist <- list(list(role = "user", content = "play"),
                  list(role = "assistant",
                       content = list(list(type = "thinking",
                                           thinking = "considering"),
                                      list(type = "text", text = "ok"))))
hist_args <- c(base_args, list(history = anth_hist))
run <- with_msgs(corteza:::.agent_with_fallback(hist_args, new_fb_session(),
    .call = make_call(list(
        anthropic_claude = function(args) stop("API error (429): rate limit"),
        openai_codex = function(args) {
            stop("API error (400): Invalid value: 'thinking'.")
        },
        anthropic = list(content = "haiku answered")), log)))
expect_equal(run$value$content, "haiku answered")
providers <- vapply(log$calls, function(c) c$provider, character(1))
expect_equal(providers, c("anthropic_claude", "anthropic"))
expect_false("openai_codex" %in% providers)
expect_true(any(grepl("cannot replay an anthropic-shaped history|cannot replay a anthropic-shaped history",
                      run$msgs)))

# The primary is never skipped for shape. A session switched to codex by
# hand still gets its own provider tried, even carrying an Anthropic
# history: refusing to call the provider the user chose would be worse
# than the provider's own error.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
codex_primary <- list(prompt = "hi", model = "gpt-5.5",
                      provider = "openai_codex", history = anth_hist)
s_cx <- new.env()
s_cx$provider <- "openai_codex"
s_cx$fallback <- NULL
s_cx$history <- anth_hist
out <- corteza:::.agent_with_fallback(codex_primary, s_cx,
    .call = make_call(list(openai_codex = list(content = "tried")), log))
expect_equal(out$content, "tried")
expect_equal(length(log$calls), 1L)

corteza:::.fallback_reset()
