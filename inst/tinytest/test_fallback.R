# Provider fallback on limit errors (turn() -> .agent_with_fallback()).

corteza:::.fallback_reset()

# .parse_model_spec(): "model provider", bare model takes the default,
# empty or provider-less specs are NULL.
expect_equal(corteza:::.parse_model_spec("gpt-5.6-sol openai_codex"),
             list(model = "gpt-5.6-sol", provider = "openai_codex"))
expect_equal(corteza:::.parse_model_spec("  claude-haiku-4-5   anthropic  "),
             list(model = "claude-haiku-4-5", provider = "anthropic"))
expect_equal(corteza:::.parse_model_spec("gpt-5.6-sol", default_provider = "openai"),
             list(model = "gpt-5.6-sol", provider = "openai"))
expect_null(corteza:::.parse_model_spec("gpt-5.6-sol"))
expect_null(corteza:::.parse_model_spec(""))
expect_null(corteza:::.parse_model_spec(NULL, default_provider = "openai"))

# .session_fallback(): session field wins, cwd config is the fallback,
# nothing configured is an empty chain, junk entries are dropped.
s <- new.env()
s$provider <- "anthropic_claude"
expect_equal(corteza:::.session_fallback(s), list())
s$config <- list(fallback = c("gpt-5.6-sol openai_codex", "", "claude-haiku-4-5"))
expect_equal(corteza:::.session_fallback(s),
             list(list(model = "gpt-5.6-sol", provider = "openai_codex"),
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

# The primary may stay cooling until an account's weekly reset; fallback
# candidates continue to use the ordinary minute cooldown.
reset <- new.env()
reset$fallback_primary_retry_at <- "  Mon   03:00 "
reset$fallback_cooldown <- 30
expect_identical(corteza:::.fallback_primary_retry_at(reset), "Mon 03:00")
tz <- Sys.timezone()
if (is.na(tz) || !nzchar(tz)) tz <- "UTC"
friday <- as.POSIXct("2026-09-04 10:00:00", tz = tz)
next_reset <- corteza:::.fallback_deadline(reset, primary = TRUE,
                                           now = friday)
expect_identical(format(next_reset, tz = tz, usetz = FALSE),
                 "2026-09-07 03:00:00")
on_reset <- as.POSIXct("2026-09-07 03:00:00", tz = tz)
next_week <- corteza:::.fallback_deadline(reset, primary = TRUE,
                                          now = on_reset)
expect_identical(format(next_week, tz = tz, usetz = FALSE),
                 "2026-09-14 03:00:00")
expect_equal(as.numeric(difftime(
    corteza:::.fallback_deadline(reset, primary = FALSE, now = friday),
    friday, units = "mins")), 30)
reset$fallback_primary_retry_at <- "Monday at three"
expect_error(corteza:::.fallback_primary_retry_at(reset), "Mon 03:00")
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
                                            web_search = args$web_search,
                                            prompt = args$prompt,
                                            history = args$history)))
        step <- script[[args$provider]]
        if (is.function(step)) step(args) else step
    }
}
new_fb_session <- function(fallback = c("gpt-5.6-sol openai_codex",
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
expect_identical(out$corteza_route$provider, "anthropic_claude")
expect_false(out$corteza_route$fallback)
expect_true(out$corteza_route$subscription)
expect_false(out$corteza_route$api_key)
expect_null(corteza:::.fallback_notice(out$corteza_route))
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
expect_true(any(grepl("openai_codex/gpt-5.6-sol answered", run$msgs)))
expect_equal(vapply(log$calls, `[[`, "", "provider"),
             c("anthropic_claude", "openai_codex"))
expect_equal(log$calls[[2]]$model, "gpt-5.6-sol")
expect_identical(run$value$corteza_route$provider, "openai_codex")
expect_identical(run$value$corteza_route$fallback_level, 1L)
expect_true(run$value$corteza_route$subscription)
expect_false(run$value$corteza_route$api_key)
expect_true(grepl("Automatic subscription failover",
                  corteza:::.fallback_reply(run$value), fixed = TRUE))
expect_false(grepl("PAID API KEY", corteza:::.fallback_reply(run$value),
                   fixed = TRUE))
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
expect_identical(out$corteza_route$provider, "anthropic")
expect_identical(out$corteza_route$fallback_level, 2L)
expect_false(out$corteza_route$subscription)
expect_true(out$corteza_route$api_key)
paid_reply <- corteza:::.fallback_reply(out)
expect_true(grepl("PAID API KEY FALLBACK", paid_reply, fixed = TRUE))
expect_true(grepl("BILLABLE USAGE", paid_reply, fixed = TRUE))
expect_true(grepl("claude-haiku-4-5 via anthropic", paid_reply, fixed = TRUE))
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

# A limit hit after tools ran resumes on the fallback from portable history.
# The completed result becomes context, not a tool call that can run twice.
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
s_prog <- new_fb_session()
run <- with_msgs(corteza:::.agent_with_fallback(base_args, s_prog,
    .call = make_call(list(
        anthropic_claude = function(args) {
            s_prog$history <- list(
                list(role = "user", content = "hi"),
                list(role = "assistant", content = list(
                    list(type = "thinking", thinking = "secret"),
                    list(type = "text", text = "Checking."),
                    list(type = "tool_use", id = "t1", name = "read_file")
                )),
                list(role = "user", content = list(
                    list(type = "tool_result", tool_use_id = "t1",
                         content = "file contents")
                ))
            )
            stop("API error (429): mid-run")
        },
        openai_codex = list(content = "resumed")), log)))
expect_equal(run$value$content, "resumed")
expect_equal(vapply(log$calls, `[[`, "", "provider"),
             c("anthropic_claude", "openai_codex"))
expect_true(grepl("file contents", log$calls[[2L]]$prompt, fixed = TRUE))
expect_true(grepl("Do not repeat", log$calls[[2L]]$prompt, fixed = TRUE))
expect_false(grepl("secret", paste(unlist(log$calls[[2L]]$history),
                                   collapse = " "), fixed = TRUE))
expect_true(any(grepl("resuming interrupted run", run$msgs)))
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

# The bot's actual chain: an Anthropic-shaped conversation is flattened
# before Codex receives it. Thinking payloads are dropped; visible context
# remains, so the first fallback can answer instead of returning a wire 400.
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
        openai_codex = list(content = "codex answered")), log)))
expect_equal(run$value$content, "codex answered")
providers <- vapply(log$calls, function(c) c$provider, character(1))
expect_equal(providers, c("anthropic_claude", "openai_codex"))
bridged <- log$calls[[2L]]$history
expect_identical(corteza:::.history_shape(bridged), "portable")
expect_true(grepl("ok", paste(unlist(bridged), collapse = " "), fixed = TRUE))
expect_false(grepl("considering", paste(unlist(bridged), collapse = " "),
                   fixed = TRUE))
expect_true(any(grepl("bridged anthropic-shaped history", run$msgs,
                      fixed = TRUE)))

# The primary is never skipped for shape, but its history is bridged. This
# is also the Monday-reset path: Claude can resume a conversation whose last
# successful turns came from the OpenAI Responses wire (and vice versa).
corteza:::.fallback_reset()
log <- new.env(); log$calls <- list()
codex_primary <- list(prompt = "hi", model = "gpt-5.6-sol",
                      provider = "openai_codex", history = anth_hist)
s_cx <- new.env()
s_cx$provider <- "openai_codex"
s_cx$fallback <- NULL
s_cx$history <- anth_hist
out <- corteza:::.agent_with_fallback(codex_primary, s_cx,
    .call = make_call(list(openai_codex = list(content = "tried")), log))
expect_equal(out$content, "tried")
expect_equal(length(log$calls), 1L)
primary_history <- log$calls[[1L]]$history
expect_identical(corteza:::.history_shape(primary_history), "portable")
expect_false(grepl("considering", paste(unlist(primary_history), collapse = " "),
                   fixed = TRUE))

# turn() exposes the chosen route and the paid warning reaches every client,
# including Matrix/FluffyChat, without client-specific UI support.
local({
    corteza:::.fallback_reset()
    on.exit(corteza:::.fallback_reset(), add = TRUE)
    ns <- asNamespace("llm.api")
    original <- get("agent", envir = ns, inherits = FALSE)
    calls <- character()
    stub <- function(...) {
        args <- list(...)
        calls <<- c(calls, args$provider)
        if (args$provider %in% c("anthropic_claude", "openai_codex")) {
            stop("API error (429): account limit")
        }
        list(content = "cheap answer", history = list(), usage = list())
    }
    assignInNamespace("agent", stub, ns = "llm.api")
    on.exit(assignInNamespace("agent", original, ns = "llm.api"), add = TRUE)

    session <- corteza::new_session(
        channel = "console",
        model_map = list(cloud = "claude-opus-5", local = NULL),
        provider = "anthropic_claude",
        web_search = FALSE
    )
    session$fallback <- c("gpt-5.6-sol openai_codex",
                          "claude-haiku-4-5 anthropic")
    answer <- suppressMessages(corteza::turn("hi", session, tools = list()))

    expect_identical(calls,
                     c("anthropic_claude", "openai_codex", "anthropic"))
    expect_identical(answer$route$provider, "anthropic")
    expect_identical(answer$route$model, "claude-haiku-4-5")
    expect_true(answer$route$api_key)
    expect_true(grepl("PAID API KEY FALLBACK", answer$reply, fixed = TRUE))
    expect_true(grepl("cheap answer", answer$reply, fixed = TRUE))
    expect_identical(session$last_route, answer$route)
})

corteza:::.fallback_reset()
