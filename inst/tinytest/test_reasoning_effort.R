# Reasoning-depth wiring (new_session -> .session_reasoning_effort /
# .session_thinking_budget -> llm.api::agent). Two settings, one idea
# ("how hard to think"), but each rides a different field on a different
# wire, so the gate matters as much as the resolution.

# -- reasoning_effort resolution --------------------------------------
s <- new.env()
expect_null(corteza:::.session_reasoning_effort(s))
s$config <- list(reasoning_effort = "medium")
expect_identical(corteza:::.session_reasoning_effort(s), "medium")
s$reasoning_effort <- "high"
expect_identical(corteza:::.session_reasoning_effort(s), "high")
s$reasoning_effort <- NULL
expect_identical(corteza:::.session_reasoning_effort(s), "medium")

# new_session() stores it; NULL stays NULL ("provider default")
ns_set <- corteza::new_session(channel = "console", provider = "openai_codex",
                               reasoning_effort = "high")
expect_identical(ns_set$reasoning_effort, "high")
ns_default <- corteza::new_session(channel = "console", provider = "openai_codex")
expect_null(ns_default$reasoning_effort)

# Not an enum -- the provider owns the vocabulary, so an unfamiliar but
# well-formed value passes through and is the API's business to refuse.
ns_new <- corteza::new_session(channel = "console", provider = "openai_codex",
                               reasoning_effort = "xhigh")
expect_identical(ns_new$reasoning_effort, "xhigh")

# Malformed values are refused here, though: an NA or a length-2 vector
# spliced into a request body is a 400 with no useful message.
for (bad in list("", NA_character_, c("low", "high"), 3, TRUE, list("high"))) {
    expect_error(corteza::new_session(channel = "console",
                                      provider = "openai_codex",
                                      reasoning_effort = bad),
                 pattern = "non-empty string")
}
s_bad <- new.env()
s_bad$config <- list(reasoning_effort = "")
expect_error(corteza:::.session_reasoning_effort(s_bad),
             pattern = "non-empty string")

# -- thinking_budget_tokens resolution --------------------------------
t <- new.env()
expect_null(corteza:::.session_thinking_budget(t))
t$config <- list(thinking_budget_tokens = 4096)
expect_identical(corteza:::.session_thinking_budget(t), 4096L)
t$thinking_budget_tokens <- 8192L
expect_identical(corteza:::.session_thinking_budget(t), 8192L)
t$thinking_budget_tokens <- NULL
expect_identical(corteza:::.session_thinking_budget(t), 4096L)

ns_tb <- corteza::new_session(channel = "console", provider = "anthropic",
                              thinking_budget_tokens = 8192)
expect_identical(ns_tb$thinking_budget_tokens, 8192L)
expect_null(ns_default$thinking_budget_tokens)

# Shares .check_max_tokens with max_tokens, so the same nonsense is
# refused -- but the message names the field that was actually wrong.
expect_error(corteza::new_session(channel = "console", provider = "anthropic",
                                  thinking_budget_tokens = 2.5),
             pattern = "thinking_budget_tokens must be")
for (bad in list(0, -1, Inf, NA_real_, "8192", c(1, 2))) {
    expect_error(corteza::new_session(channel = "console",
                                      provider = "anthropic",
                                      thinking_budget_tokens = bad),
                 pattern = "positive whole number")
}

# -- the wire gate ----------------------------------------------------
# One effort setting, routed to the field the provider's wire uses:
# top-level reasoning_effort on openai/codex, output_config$effort on
# Anthropic (whose API states the scale itself -- "Input should be
# 'low', 'medium', 'high', 'xhigh' or 'max'"). Anthropic refuses
# unknown top-level fields outright, so sending the wrong spelling is a
# 400, not a no-op. thinking_budget_tokens is separate and Anthropic-only.
both <- list(reasoning_effort = "high", thinking_budget_tokens = 8192L,
             max_tokens = 32000L)
for (p in c("openai", "openai_codex")) {
    g <- corteza:::.gate_reasoning_args(both, p)
    expect_identical(g$reasoning_effort, "high")
    expect_null(g$output_config)
    expect_null(g$thinking_budget_tokens)
    expect_identical(g$max_tokens, 32000L)   # unrelated args untouched
}
for (p in c("anthropic", "anthropic_claude")) {
    g <- corteza:::.gate_reasoning_args(both, p)
    expect_null(g$reasoning_effort)
    expect_identical(g$output_config, list(effort = "high"))
    expect_identical(g$thinking_budget_tokens, 8192L)
    expect_identical(g$max_tokens, 32000L)
}
for (p in c("ollama", "moonshot", "openai_compatible")) {
    g <- corteza:::.gate_reasoning_args(both, p)
    expect_null(g$reasoning_effort)
    expect_null(g$output_config)
    expect_null(g$thinking_budget_tokens)
}

# Routing is idempotent and reversible: re-gating already-routed args
# for the other family moves the effort across rather than losing it.
# This is the fallback path -- args routed for the primary get re-gated
# per candidate -- so a value that could only survive one hop would
# silently drop the setting on the provider that answered.
anth <- corteza:::.gate_reasoning_args(list(reasoning_effort = "max"),
                                       "anthropic_claude")
expect_identical(anth$output_config, list(effort = "max"))
back <- corteza:::.gate_reasoning_args(anth, "openai_codex")
expect_identical(back$reasoning_effort, "max")
expect_null(back$output_config)
again <- corteza:::.gate_reasoning_args(anth, "anthropic")
expect_identical(again$output_config, list(effort = "max"))
# and off both families it goes away entirely rather than half-routed
expect_null(corteza:::.gate_reasoning_args(anth, "moonshot")$output_config)

# -- turn() forwarding ------------------------------------------------
# Stubs the agent in llm.api's namespace; tools = list() keeps the whole
# call offline.
local({
    ns <- asNamespace("llm.api")
    orig <- get("agent", envir = ns, inherits = FALSE)
    captured <- NULL
    stub <- function(...) {
        captured <<- list(...)
        list(content = "ok", history = list(),
             usage = list(input_tokens = 1L, output_tokens = 1L))
    }
    assignInNamespace("agent", stub, ns = "llm.api")
    on.exit(assignInNamespace("agent", orig, ns = "llm.api"), add = TRUE)

    sess <- function(provider, ...) {
        corteza::new_session(channel = "console", provider = provider,
                             model_map = list(cloud = "test-model",
                                              local = NULL),
                             web_search = FALSE, ...)
    }

    # codex: effort forwarded, thinking budget dropped
    out <- corteza::turn("hi", sess("openai_codex", reasoning_effort = "high",
                                    thinking_budget_tokens = 8192),
                         tools = list())
    expect_identical(out$reply, "ok")
    expect_identical(captured$reasoning_effort, "high")
    expect_false("thinking_budget_tokens" %in% names(captured))

    # anthropic: thinking budget forwarded as-is, effort re-spelled
    captured <- NULL
    out <- corteza::turn("hi", sess("anthropic", reasoning_effort = "max",
                                    thinking_budget_tokens = 8192),
                         tools = list())
    expect_identical(out$reply, "ok")
    expect_identical(captured$thinking_budget_tokens, 8192L)
    expect_false("reasoning_effort" %in% names(captured))
    expect_identical(captured$output_config, list(effort = "max"))

    # neither set: neither key present, so the provider default stands
    captured <- NULL
    out <- corteza::turn("hi", sess("openai_codex"), tools = list())
    expect_identical(out$reply, "ok")
    expect_false("reasoning_effort" %in% names(captured))
    expect_false("thinking_budget_tokens" %in% names(captured))
})

# -- the gate holds across a provider fallback ------------------------
# .agent_with_fallback() rewrites provider per candidate, so args gated
# for the primary have to be re-gated. Without this a codex session
# falling back to anthropic sends reasoning_effort into the Messages
# body: a 400, which is not a limit error, so the turn dies instead of
# being answered by the fallback.
local({
    corteza:::.fallback_reset()
    on.exit(corteza:::.fallback_reset(), add = TRUE)

    seen <- list()
    calls <- 0L
    fake <- function(args) {
        calls <<- calls + 1L
        seen[[length(seen) + 1L]] <<- args
        if (identical(args$provider, "openai_codex")) {
            stop("rate limit exceeded for this organization")
        }
        list(content = "ok", history = list(), usage = list())
    }

    s <- new.env()
    s$config <- list(fallback = "claude-test anthropic")
    args <- list(model = "gpt-test", provider = "openai_codex",
                 reasoning_effort = "high", max_tokens = 32000L)

    res <- corteza:::.agent_with_fallback(args, s, .call = fake)
    expect_identical(res$content, "ok")
    expect_identical(calls, 2L)
    # primary kept it, the anthropic candidate did not
    expect_identical(seen[[1]]$reasoning_effort, "high")
    expect_identical(seen[[1]]$provider, "openai_codex")
    expect_null(seen[[2]]$reasoning_effort)
    expect_identical(seen[[2]]$provider, "anthropic")
    expect_identical(seen[[2]]$max_tokens, 32000L)
})
