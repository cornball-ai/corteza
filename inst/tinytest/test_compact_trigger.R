# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

# When a session compacts: the table of windows, the cap on the trigger,
# the context_window override, and the idle trigger with a fake clock
# (R/compact-trigger.R, R/context-budget.R). Offline: the summarizer and
# llm.api::agent are replaced inside function scopes, and every file is
# under tempdir().

# ---------------------------------------------------------------
# The table. Each default model of llm.api resolves to a row, so the
# next model llm.api moves to fails here until someone reads its window.
# ---------------------------------------------------------------

# llm.api 0.2.0's providers that have a default model. It exports no
# list of them; openai_compatible has none and is checked below.
default_providers <- c("anthropic", "anthropic_claude", "openai",
                       "openai_codex", "moonshot", "google", "ollama")
for (provider in default_providers) {
    model <- corteza::default_provider_model(provider)
    expect_true(is.character(model) && length(model) == 1L && nzchar(model),
                info = provider)
    expect_false(is.null(corteza:::.context_limit_row(model, provider)),
                 info = sprintf("%s default model %s has no row in MODEL_CONTEXT_LIMITS",
                                provider, format(model)))
}
expect_null(corteza::default_provider_model("openai_compatible"))

# The model corteza names itself: the target of the kimi-k2 alias. (The
# talker's fast models were the others until 0.8.1, when a talker took
# the session's own model.)
expect_false(is.null(corteza:::.context_limit_row(
    corteza:::resolve_provider_model("moonshot", "kimi-k2"), "moonshot")))

# A model with no row gets the fallback, and the two are told apart.
expect_null(corteza:::.context_limit_row("no-such-model-9"))
expect_null(corteza:::.context_limit_row("no-such-model-9", "openai_codex"))
expect_equal(corteza::context_limit_for_model("no-such-model-9"), 128000L)
expect_null(corteza:::.context_limit_row(NULL))
expect_null(corteza:::.context_limit_row(NA_character_))
expect_null(corteza:::.context_limit_row(""))

# A cap follows the route without enlarging smaller windows on fallback.
local({
    s <- corteza::new_session(provider = "anthropic", model_map = list(cloud = "claude-sonnet-4-6"))
    cfg <- list(context_window_cap = 400000)
    capped <- corteza:::context_trigger(s, cfg)
    expect_equal(capped$window, 400000)
    expect_identical(capped$source, "cap")
    expect_equal(capped$tokens, 200000)
    expect_equal(corteza:::context_trigger(s, cfg, "claude-haiku-4-5")$window, 200000)
    s$config <- cfg
    pct <- corteza:::.context_edit_pct(s, "fixture")
    s$config <- list()
    expect_equal(pct, 2.5 * corteza:::.context_edit_pct(s, "fixture"))
})

# The numbers each provider publishes (sources in R/context-budget.R).
expect_equal(corteza::context_limit_for_model("claude-fable-5-2"), 1000000L)
expect_equal(corteza::context_limit_for_model("claude-opus-5-5"), 1000000L)
expect_equal(corteza::context_limit_for_model("claude-opus-4-7"), 1000000L)
expect_equal(corteza::context_limit_for_model("claude-sonnet-4-6"), 1000000L)
expect_equal(corteza::context_limit_for_model("claude-haiku-5-5"), 1000000L)
expect_equal(corteza::context_limit_for_model("claude-haiku-4-5"), 200000L)
expect_equal(corteza::context_limit_for_model("claude-opus-4-5"), 200000L)
expect_equal(corteza::context_limit_for_model("claude-sonnet-4-5"), 200000L)
# OpenAI's API: the input limit, under the 1,050,000 window.
expect_equal(corteza::context_limit_for_model("gpt-6.1-sol"), 922000L)
expect_equal(corteza::context_limit_for_model("gpt-6-astra", "openai"),
             922000L)
expect_equal(corteza::context_limit_for_model("gpt-5.6-luna"), 922000L)
# The Codex backend's own, smaller windows.
expect_equal(corteza::context_limit_for_model("gpt-6.1-sol", "openai_codex"),
             272000L)
expect_equal(corteza::context_limit_for_model("gpt-6.1-terra",
                                              "openai_codex"), 500000L)
expect_equal(corteza::context_limit_for_model("gpt-6-luna", "openai_codex"),
             272000L)
expect_equal(corteza::context_limit_for_model("gemini-3.5-flash", "google"),
             1048576L)
expect_equal(corteza::context_limit_for_model("gemini-4-carbon"), 1048576L)
expect_equal(corteza::context_limit_for_model("kimi-k3.1-pro", "moonshot"),
             1048576L)
expect_equal(corteza::context_limit_for_model("kimi-k2.6", "moonshot"),
             262144L)
expect_equal(corteza::context_limit_for_model("kimi-k2.5", "moonshot"),
             262144L)
# An Ollama server decides the window; the row is a guess, kept at what
# the model resolved to before it had a row.
expect_equal(corteza::context_limit_for_model("qwen3.5:9b", "ollama"),
             128000L)

# A dated id resolves to its family: the longest row name it starts
# with, whatever the order of the rows.
expect_equal(corteza::context_limit_for_model("claude-haiku-4-5-20251001"),
             200000L)
expect_equal(corteza::context_limit_for_model("claude-opus-4-5-20251101"),
             200000L)
expect_equal(corteza::context_limit_for_model("gpt-4-turbo-preview"), 128000L)
# An id cut short can mean a 1M model or a 200K one, and gets the
# smaller: a window guessed too large is a request refused.
expect_equal(corteza::context_limit_for_model("claude-sonnet-4"), 200000L)
expect_equal(corteza::context_limit_for_model("claude-opus-4"), 200000L)
expect_equal(corteza::context_limit_for_model("claude-haiku"), 200000L)
expect_equal(corteza::context_limit_for_model("kimi"), 262144L)

# The same 2 rules on a made-up table, in an order and in its reverse:
# a first match would answer differently for the two.
made_up <- list("fam" = 10L, "fam-big" = 99L, "fam-big-x" = 50L,
                "kin-a-1" = 70L, "kin-a-2" = 30L, "kin-b" = 80L)
for (limits in list(made_up, rev(made_up))) {
    # Its own row.
    expect_equal(corteza:::.context_limit_match("fam", limits), 10L)
    expect_equal(corteza:::.context_limit_match("fam-big", limits), 99L)
    # The longest row name the id starts with.
    expect_equal(corteza:::.context_limit_match("fam-big-2026", limits), 99L)
    expect_equal(corteza:::.context_limit_match("fam-big-x-2026", limits),
                 50L)
    expect_equal(corteza:::.context_limit_match("fam-small", limits), 10L)
    expect_equal(corteza:::.context_limit_match("kin-b-9", limits), 80L)
    # A row the id starts with is asked before the rows that start with
    # the id.
    expect_equal(corteza:::.context_limit_match("fam-b", limits), 10L)
    # An id cut short: the smallest among the rows it could mean.
    expect_equal(corteza:::.context_limit_match("kin-a", limits), 30L)
    expect_equal(corteza:::.context_limit_match("kin", limits), 30L)
    expect_equal(corteza:::.context_limit_match("fa", limits), 10L)
    expect_null(corteza:::.context_limit_match("other", limits))
}
expect_null(corteza:::.context_limit_match("fam", NULL))
expect_null(corteza:::.context_limit_match("fam", list()))

# ---------------------------------------------------------------
# context_trigger(): the smaller of the percentage and the cap.
# ---------------------------------------------------------------

trigger_session <- function(model = "claude-opus-5-5",
                            provider = "anthropic", ...) {
    s <- corteza::new_session(
        provider = provider,
        model_map = list(cloud = model, local = "qwen3.5:9b"),
        verbose = FALSE, ...)
    s
}

trigger_config <- function(...) {
    cfg <- list(
        context_compact_pct = 90L,
        # Off unless a case sets them, so each case names what it tests.
        context_compact_tokens = 0L,
        context_idle_minutes = 0L,
        context_idle_tokens = 1000L,
        subagents = list(context_compaction = list(
            mode = "inherit_strict",
            compact_pct = 75L,
            keep_recent_turns = 1L,
            keep_recent_tokens = 1L,
            min_messages = 2L,
            timeout_seconds = 1L
        ))
    )
    over <- list(...)
    # A name given twice would keep its first value and hide the second.
    stopifnot(!anyDuplicated(names(over)))
    for (name in names(over)) {
        cfg[name] <- list(over[[name]])
    }
    cfg
}

# Defaults: load_config() fills the 3 numbers, and a config without
# them (a caller that never went through load_config) gets the same.
local({
    dir <- tempfile("trigger-cfg-")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    cfg <- corteza:::load_config(dir)
    expect_equal(cfg$context_compact_tokens, 200000L)
    expect_equal(cfg$context_idle_minutes, 60L)
    expect_equal(cfg$context_idle_tokens, 100000L)
})
bare <- corteza:::context_trigger(trigger_session(), list())
expect_equal(bare$cap, 200000)
expect_equal(bare$idle_minutes, 60)
expect_equal(bare$idle_tokens, 100000)

# A 1M window: 90% is 900,000 and the cap decides.
t1m <- corteza:::context_trigger(trigger_session(),
                                 trigger_config(context_compact_tokens = 200000L))
expect_equal(t1m$window, 1000000L)
expect_equal(t1m$source, "table")
expect_equal(t1m$tokens, 200000)
expect_equal(t1m$rule, "cap")
expect_equal(t1m$effective_pct, 20)

# A 200K window: 90% is 180,000, under the cap, and the percentage decides.
t200k <- corteza:::context_trigger(
    trigger_session("claude-haiku-4-5"),
    trigger_config(context_compact_tokens = 200000L))
expect_equal(t200k$window, 200000L)
expect_equal(t200k$tokens, 180000)
expect_equal(t200k$rule, "pct")
expect_equal(t200k$effective_pct, 90)

# The Codex backend's 272,000: 90% is 244,800, and the cap lowers it.
tcodex <- corteza:::context_trigger(
    trigger_session("gpt-6.1-sol", provider = "openai_codex"),
    trigger_config(context_compact_tokens = 200000L))
expect_equal(tcodex$window, 272000L)
expect_equal(tcodex$tokens, 200000)
expect_equal(tcodex$rule, "cap")

# Where the 2 agree, the percentage is the one named.
tsame <- corteza:::context_trigger(
    trigger_session("claude-haiku-4-5"),
    trigger_config(context_compact_tokens = 180000L))
expect_equal(tsame$tokens, 180000)
expect_equal(tsame$rule, "pct")

# 0 turns the cap off.
tnocap <- corteza:::context_trigger(trigger_session(), trigger_config())
expect_null(tnocap$cap)
expect_equal(tnocap$tokens, 900000)
expect_equal(tnocap$rule, "pct")

# A value typed wrong in the config falls back to the default, and a
# turn is never stopped by it.
for (bad in list("200000", -5, NA_real_, c(1, 2), Inf, TRUE)) {
    got <- corteza:::context_trigger(trigger_session(),
                                     trigger_config(context_compact_tokens = bad,
                                                    context_idle_minutes = bad,
                                                    context_idle_tokens = bad))
    what <- paste(format(bad), collapse = ",")
    expect_equal(got$cap, 200000, info = what)
    expect_equal(got$idle_minutes, 60, info = what)
    expect_equal(got$idle_tokens, 100000, info = what)
}

# context_window in the config overrides the table; one set on the
# session overrides both.
tcfg <- corteza:::context_trigger(trigger_session(),
                                  trigger_config(context_window = 50000L,
                                                 context_compact_tokens = 200000L))
expect_equal(tcfg$window, 50000L)
expect_equal(tcfg$source, "config")
expect_equal(tcfg$tokens, 45000)
expect_equal(tcfg$rule, "pct")
s_window <- trigger_session()
s_window$context_window <- 30000L
tses <- corteza:::context_trigger(s_window,
                                  trigger_config(context_window = 50000L))
expect_equal(tses$window, 30000L)
expect_equal(tses$source, "session")
expect_equal(tses$tokens, 27000)

# A window that is not one number above 0 is no window: the next
# source answers. A string in the config used to stop every turn.
for (bad in list("64000", 0, -5, NA_real_, c(1, 2), Inf, TRUE)) {
    what <- paste(deparse(bad), collapse = "")
    got <- corteza:::context_trigger(trigger_session(),
                                     trigger_config(context_window = bad))
    expect_equal(got$window, 1000000L, info = what)
    expect_equal(got$source, "table", info = what)
    s_bad <- trigger_session()
    s_bad$context_window <- bad
    got <- corteza:::context_trigger(s_bad,
                                     trigger_config(context_window = 50000L))
    expect_equal(got$window, 50000L, info = what)
    expect_equal(got$source, "config", info = what)
}
expect_null(corteza:::.window_value(NULL))
expect_equal(corteza:::.window_value(64000), 64000)

# A model with no row: the fallback, and said so.
tfall <- corteza:::context_trigger(trigger_session("no-such-model-9"),
                                   trigger_config())
expect_equal(tfall$window, 128000L)
expect_equal(tfall$source, "fallback")
expect_equal(tfall$tokens, 115200)

# A session with no model named resolves its provider's default.
tdefault <- corteza:::context_trigger(
    corteza::new_session(provider = "anthropic",
                         model_map = list(cloud = NULL, local = "qwen3.5:9b")),
    trigger_config())
expect_equal(tdefault$source, "table")
expect_equal(tdefault$window, corteza::context_limit_for_model(
    corteza::default_provider_model("anthropic"), "anthropic"))

# A subagent takes the stricter percentage.
s_child <- trigger_session()
s_child$is_subagent <- TRUE
expect_equal(corteza:::context_trigger(s_child, trigger_config())$tokens,
             750000)

# A percentage that is not finite turns the size trigger off, and a cap
# cannot turn it back on. The memory flush's session relies on this.
toff <- corteza:::context_trigger(
    trigger_session(),
    trigger_config(context_compact_pct = Inf, context_compact_tokens = 10L))
expect_equal(toff$rule, "off")
expect_null(toff$cap)
expect_true(is.infinite(toff$tokens))
expect_equal(toff$effective_pct, 100)

# A session that edits its own context is never compacted.
s_clm <- trigger_session()
s_clm$config <- list(context_edit = list(enabled = TRUE, strategy = "clm"))
expect_true(corteza:::.context_edit_pure(s_clm))
expect_equal(corteza:::context_trigger(
    s_clm, trigger_config(context_compact_tokens = 10L))$rule, "off")

# The idle numbers: the session's own minutes win over the config's.
tidle <- corteza:::context_trigger(
    trigger_session(),
    trigger_config(context_idle_minutes = 45L, context_idle_tokens = 5000L))
expect_equal(tidle$idle_minutes, 45)
expect_equal(tidle$idle_tokens, 5000)
s_quiet <- trigger_session()
s_quiet$context_idle_minutes <- 0
expect_equal(corteza:::context_trigger(
    s_quiet, trigger_config(context_idle_minutes = 45L))$idle_minutes, 0)

# What /context prints: a line for each thing that applies.
expect_equal(
    corteza:::.context_trigger_lines(t1m),
    "Compacts at 200.0K tokens (context_compact_tokens).")
expect_equal(
    corteza:::.context_trigger_lines(t200k),
    "Compacts at 180.0K tokens (90% of the window).")
expect_equal(
    corteza:::.context_trigger_lines(tidle),
    c("Compacts at 900.0K tokens (90% of the window).",
      "After 45 minutes idle, compacts at 5.0K tokens."))
expect_equal(
    corteza:::.context_trigger_lines(tcfg),
    c("Compacts at 45.0K tokens (90% of the window).",
      "Window from context_window."))
expect_equal(
    corteza:::.context_trigger_lines(tses),
    c("Compacts at 27.0K tokens (90% of the window).",
      "Window set on the session."))
expect_equal(
    corteza:::.context_trigger_lines(tfall),
    c("Compacts at 115.2K tokens (90% of the window).",
      "No window known for this model: 128.0K assumed."))
expect_equal(
    corteza:::.context_trigger_lines(toff),
    "Automatic compaction is off for this session.")
# Off means off: the idle numbers are not offered for a session that
# never compacts.
toff_idle <- corteza:::context_trigger(
    trigger_session(),
    trigger_config(context_compact_pct = Inf, context_idle_minutes = 60L))
expect_equal(corteza:::.context_trigger_lines(toff_idle),
             "Automatic compaction is off for this session.")
# Nor where the size rule always gets there first: a window of 50,000
# compacts at 45,000 and never holds the 100,000 the idle rule needs.
tsmall <- corteza:::context_trigger(
    trigger_session(),
    trigger_config(context_window = 50000L, context_idle_minutes = 60L,
                   context_idle_tokens = 100000L))
expect_equal(corteza:::.context_trigger_lines(tsmall),
             c("Compacts at 45.0K tokens (90% of the window).",
               "Window from context_window."))
# All 3 lines at once.
tall <- corteza:::context_trigger(
    trigger_session(),
    trigger_config(context_window = 500000L, context_compact_tokens = 200000L,
                   context_idle_minutes = 60L, context_idle_tokens = 100000L))
expect_equal(corteza:::.context_trigger_lines(tall),
             c("Compacts at 200.0K tokens (context_compact_tokens).",
               "After 60 minutes idle, compacts at 100.0K tokens.",
               "Window from context_window."))

# The block /context prints carries the line under the breakdown, and
# the tick sits where the trigger is.
local({
    plain <- stats::setNames(as.list(rep("", 16L)),
                             c("reset", "bold", "dim", "red", "green",
                               "yellow", "blue", "magenta", "cyan", "white",
                               "bright_red", "bright_green", "bright_yellow",
                               "bright_blue", "bright_magenta",
                               "bright_cyan"))
    block <- corteza:::format_context_block(
        used = 150000L, limit = tall$window,
        breakdown = list(system = 20000L, tools = 10000L, history = 120000L),
        compact_pct = tall$effective_pct, palette = plain,
        note = corteza:::.context_trigger_lines(tall))
    lines <- strsplit(block, "\n", fixed = TRUE)[[1L]]
    expect_true(any(grepl("compact 40%", lines, fixed = TRUE)))
    without <- corteza:::format_context_block(
        used = 150000L, limit = tall$window,
        breakdown = list(system = 20000L, tools = 10000L, history = 120000L),
        compact_pct = tall$effective_pct, palette = plain)
    expect_false(grepl("Compacts at", without, fixed = TRUE))
    # The 3 lines are the only difference, in their order, and they sit
    # between the breakdown and the blank line ahead of the files.
    expect_equal(setdiff(lines, strsplit(without, "\n", fixed = TRUE)[[1L]]),
                 corteza:::.context_trigger_lines(tall))
    at <- match(corteza:::.context_trigger_lines(tall), lines)
    expect_equal(diff(at), c(1L, 1L))
    expect_true(grepl("history", lines[[at[[1L]] - 1L]], fixed = TRUE))
    expect_equal(lines[[at[[3L]] + 1L]], "")
    # Nothing to say prints nothing.
    for (empty in list(NULL, character(), "", NA_character_)) {
        expect_identical(corteza:::format_context_block(
            used = 150000L, limit = tall$window,
            breakdown = list(system = 20000L, tools = 10000L,
                             history = 120000L),
            compact_pct = tall$effective_pct, palette = plain,
            note = empty), without)
    }
})

# The post-turn meter names the percentage at which the cap compacts.
expect_true(grepl("(compact at 20%)", corteza:::.repl_context_indicator(
    150000L, t1m$window, list(),
    list(compact = t1m$effective_pct)), fixed = TRUE))

# The same through the loop the CLI and chat() share: /context prints
# the trigger in force, and the meter after a reply uses the same
# window. A context_window in the config shows in both.
repl_ctx <- function(lines, config) {
    i <- 0L
    ctx <- new.env(parent = emptyenv())
    ctx$palette <- stats::setNames(as.list(rep("", 9L)),
                                   c("dim", "reset", "cyan", "bold",
                                     "yellow", "green", "bright_magenta",
                                     "red", "magenta"))
    ctx$read_input <- function(prompt_str) {
        i <<- i + 1L
        if (i <= length(lines)) lines[[i]] else character(0)
    }
    ctx$help_text <- function() "HELP"
    ctx$handle_copy <- function(x) invisible(NULL)
    ctx$format_tools <- function(s) "TOOLS"
    ctx$pending_r_context <- character(0)
    ctx$last_assistant_response <- ""
    ctx$provider <- "anthropic"
    ctx$model <- "claude-opus-5-5"
    ctx$cwd <- tempdir()
    ctx$config <- config
    ctx$session <- trigger_session()
    ctx$session$tools_filter <- "read_file"
    ctx$session$tasks <- list()
    ctx$session$tasks_dirty <- FALSE
    ctx$disk_session <- NULL
    ctx
}
run_context_command_test <- function() {
    old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
    tmp_data <- tempfile("trigger-repl-")
    Sys.setenv(R_USER_DATA_DIR = tmp_data)
    on.exit({
        if (is.na(old_data)) {
            Sys.unsetenv("R_USER_DATA_DIR")
        } else {
            Sys.setenv(R_USER_DATA_DIR = old_data)
        }
        unlink(tmp_data, recursive = TRUE)
    }, add = TRUE)
    shipped <- trigger_config(context_compact_tokens = 200000L,
                              context_idle_minutes = 60L,
                              context_idle_tokens = 100000L,
                              memory_flush_enabled = FALSE)

    out <- capture.output(corteza:::run_repl_loop(repl_ctx("/context",
                                                           shipped)))
    expect_true(any(grepl("/ 1.0M", out, fixed = TRUE)))
    expect_true(any(grepl("compact 20%", out, fixed = TRUE)))
    expect_true("Compacts at 200.0K tokens (context_compact_tokens)." %in%
                out)
    expect_true("After 60 minutes idle, compacts at 100.0K tokens." %in% out)
    expect_false(any(startsWith(out, "Window") |
                     startsWith(out, "No window")))

    overridden <- shipped
    overridden$context_window <- 50000L
    out <- capture.output(corteza:::run_repl_loop(repl_ctx("/context",
                                                           overridden)))
    expect_true(any(grepl("/ 50.0K", out, fixed = TRUE)))
    expect_true(any(grepl("compact 90%", out, fixed = TRUE)))
    expect_true("Compacts at 45.0K tokens (90% of the window)." %in% out)
    expect_true("Window from context_window." %in% out)
    expect_false(any(grepl("minutes idle", out, fixed = TRUE)))

    # After a reply.
    ctx <- repl_ctx("hello", shipped)
    ctx$render_reply <- function(txt) invisible(NULL)
    ctx$turn_fn <- function(prompt, session) list(reply = "ok", usage = NULL)
    out <- capture.output(corteza:::run_repl_loop(ctx))
    expect_true(any(grepl("of 1000K (compact at 20%)", out, fixed = TRUE)))
    ctx <- repl_ctx("hello", overridden)
    ctx$render_reply <- function(txt) invisible(NULL)
    ctx$turn_fn <- function(prompt, session) list(reply = "ok", usage = NULL)
    out <- capture.output(corteza:::run_repl_loop(ctx))
    expect_true(any(grepl("of 50K (compact at 90%)", out, fixed = TRUE)))
}
run_context_command_test()

# ---------------------------------------------------------------
# The compactor under the cap and the override.
# ---------------------------------------------------------------

trigger_history <- function(turns = 8L, chars = 4000L) {
    h <- list()
    for (i in seq_len(turns)) {
        h[[length(h) + 1L]] <- list(role = "user",
                                    content = strrep("u", chars))
        h[[length(h) + 1L]] <- list(role = "assistant",
                                    content = strrep("a", chars))
    }
    h
}

history_tokens <- corteza:::estimate_live_context_tokens(
    list(history = trigger_history()), "", list())
# The fixture is the size the cases below assume.
expect_true(history_tokens > 15000L && history_tokens < 20000L)

# One attempt at a compaction with the summarizer replaced. Returns what
# the attempt returned, how often the summarizer ran, and the messages.
compact_once <- function(session, config, reason = "threshold",
                         now = Sys.time(), summary = "earlier work", ...) {
    original <- get("compact_summarize_slice", envir = asNamespace("corteza"))
    on.exit(assignInNamespace("compact_summarize_slice", original,
                              ns = "corteza"), add = TRUE)
    calls <- 0L
    assignInNamespace("compact_summarize_slice", function(...) {
        calls <<- calls + 1L
        summary
    }, ns = "corteza")
    said <- character()
    result <- withCallingHandlers(
        corteza:::maybe_compact_turn_session(
            session, config, tools = list(), system = "", reason = reason,
            now = now, ...),
        message = function(m) {
            said <<- c(said, conditionMessage(m))
            invokeRestart("muffleMessage")
        })
    list(result = isTRUE(result), calls = calls, said = said)
}

# Over the cap on a 1M window: compacts, and the event says where the
# trigger stood.
local({
    s <- trigger_session()
    s$history <- trigger_history()
    got <- compact_once(s, trigger_config(context_compact_tokens = 10000L))
    expect_true(got$result)
    expect_equal(got$calls, 1L)
    expect_equal(s$last_compaction$trigger, "tokens")
    expect_equal(s$last_compaction$reason, "threshold")
    expect_equal(s$last_compaction$trigger_tokens, 10000)
    expect_equal(s$last_compaction$context_window, 1000000L)
    expect_null(s$last_compaction$idle_minutes)
    expect_true(length(s$history) < 16L)
    expect_equal(length(got$said), 0L)
})

# A history over the cap in tools' results compacts: an Anthropic result
# keeps its text in a block, which the estimate counted as a line before
# 0.8.1, so a session that read files went on far past its cap.
local({
    s <- trigger_session()
    h <- list()
    for (i in seq_len(4L)) {
        id <- sprintf("toolu_%d", i)
        h[[length(h) + 1L]] <- list(role = "user",
                                    content = sprintf("Read f%d.txt.", i))
        h[[length(h) + 1L]] <- list(role = "assistant", content = list(
            list(type = "tool_use", id = id, name = "read_file",
                 input = list(path = sprintf("f%d.txt", i)))))
        h[[length(h) + 1L]] <- list(role = "user", content = list(
            list(type = "tool_result", tool_use_id = id,
                 content = strrep("r", 8000L))))
        h[[length(h) + 1L]] <- list(role = "assistant",
                                    content = sprintf("f%d.txt read.", i))
    }
    s$history <- h
    got <- compact_once(s, trigger_config(context_compact_tokens = 6000L))
    expect_true(got$result)
    expect_equal(got$calls, 1L)
    expect_equal(s$last_compaction$trigger, "tokens")
})

# So does one over the cap in a plain-text document, which counts as its
# text: about 24,000 tokens over a 10,000-token cap (Codex's probe on
# #304; it counted 4,063 and did not compact).
local({
    s <- trigger_session()
    s$history <- list(
        list(role = "user", content = list(list(type = "document",
            source = list(type = "text", media_type = "text/plain",
                          data = strrep("ordinary document text. ", 4000L))))),
        list(role = "assistant", content = "Read it."),
        list(role = "user", content = "What did it say?"),
        list(role = "assistant", content = "It is ordinary text."))
    expect_true(corteza::estimate_history_tokens(s$history) > 20000L)
    got <- compact_once(s, trigger_config(context_compact_tokens = 10000L))
    expect_true(got$result)
    expect_equal(got$calls, 1L)
})

# Exactly at the cap compacts; 1 token under does not.
local({
    s <- trigger_session()
    s$history <- trigger_history()
    under <- compact_once(s, trigger_config(
        context_compact_tokens = history_tokens + 1L))
    expect_false(under$result)
    expect_equal(under$calls, 0L)
    expect_equal(length(s$history), 16L)
    at <- compact_once(s, trigger_config(
        context_compact_tokens = history_tokens))
    expect_true(at$result)
})

# With the cap off the same history is 2% of the window and stays.
local({
    s <- trigger_session()
    s$history <- trigger_history()
    got <- compact_once(s, trigger_config())
    expect_false(got$result)
    expect_equal(got$calls, 0L)
    expect_equal(length(s$history), 16L)
})

# The cap does not undo a percentage that turns compaction off.
local({
    s <- trigger_session()
    s$history <- trigger_history()
    got <- compact_once(s, trigger_config(context_compact_pct = Inf,
                                          context_compact_tokens = 10L))
    expect_false(got$result)
    expect_equal(got$calls, 0L)
})

# context_window in the config: 90% of 20,000 is under the history, and
# the event records the window that decided.
local({
    s <- trigger_session()
    s$history <- trigger_history()
    got <- compact_once(s, trigger_config(context_window = 20000L,
                                          context_compact_pct = 80L))
    expect_true(got$result)
    expect_equal(s$last_compaction$context_window, 20000L)
    expect_equal(s$last_compaction$trigger_tokens, 16000)
    # The same window, too large to matter.
    s2 <- trigger_session()
    s2$history <- trigger_history()
    expect_false(compact_once(s2, trigger_config(
        context_window = 500000L))$result)
    # The session's own window wins over the config's.
    s3 <- trigger_session()
    s3$history <- trigger_history()
    s3$context_window <- 20000L
    expect_true(compact_once(s3, trigger_config(context_window = 500000L,
                                                context_compact_pct = 80L))$result)
    expect_equal(s3$last_compaction$context_window, 20000L)
    # A window typed wrong does not stop the check ahead of a turn: the
    # table's 1M answers, and this history is far under 90% of it.
    for (bad in list("64000", NA_real_, c(1, 2), 0)) {
        s4 <- trigger_session()
        s4$history <- trigger_history()
        got <- compact_once(s4, trigger_config(context_window = bad),
                            reason = "preflight")
        expect_false(got$result, info = paste(deparse(bad), collapse = ""))
        expect_equal(got$calls, 0L)
    }
})

# ---------------------------------------------------------------
# What the summary reads: a budget that follows the model's window.
# ---------------------------------------------------------------

# 120,000 characters at the fallback window, and the same share of a
# larger one. A smaller window, and one that is no number, keep 120,000.
expect_equal(corteza:::.compact_summary_chars(128000L), 120000)
expect_equal(corteza:::.compact_summary_chars(1000000L), 937500)
expect_equal(corteza:::.compact_summary_chars(200000L), 187500)
expect_equal(corteza:::.compact_summary_chars(32000L), 120000)
# A window as the table holds it, an integer, and as a config file
# gives it, a double: the same budget, and no integer overflow.
expect_equal(corteza:::.compact_summary_chars(1000000), 937500)
expect_silent(corteza:::.compact_summary_chars(1048576L))
expect_equal(corteza:::.compact_summary_chars(1048576L), 983040)
for (bad in list(NULL, "1000000", 0, NA_real_)) {
    expect_equal(corteza:::.compact_summary_chars(bad), 120000,
                 info = paste(deparse(bad), collapse = ""))
}
# A context at the default cap is 800,000 characters, and a model with
# a 1M window reads all of it, on OpenAI's input limit too.
expect_true(corteza:::.compact_summary_chars(922000L) >=
            4 * corteza:::CONTEXT_COMPACT_TOKENS)
# The budget is about tokens: a 272,000 window reads 255,000 characters
# whatever the session's limit on bytes. That limit is kept on the
# serialized request instead (below).
expect_equal(corteza:::.compact_summary_chars(272000L), 255000)

# Bytes as a JSON request carries them: a control character takes 6,
# a quote 2, a character outside ASCII its UTF-8 bytes.
expect_equal(corteza:::.json_text_bytes(c("abc", "\001\001", "a\"b", "é", "")),
             c(3, 12, 4, 2, 0))
# Trimming by bytes keeps the newest entries that fit, with the note.
local({
    rendered <- c(strrep("\001", 100), strrep("\001", 100), "newest")
    kept <- corteza:::.compact_trim_total(rendered, max_total = 120000L,
                                          max_bytes = 800)
    expect_identical(kept[[1L]],
                     "[1 earlier message(s) elided to fit the compaction budget]")
    expect_identical(kept[-1L], rendered[2:3])
    joined <- paste(kept, collapse = "\n\n")
    expect_true(nchar(jsonlite::toJSON(joined, auto_unbox = TRUE),
                      type = "bytes") - 2 <= 800)
    # With no limit on bytes, characters alone decide, as before.
    expect_identical(corteza:::.compact_trim_total(rendered,
                                                   max_total = 120000L),
                     rendered)
})

# 40 turns that can be told apart, 320,000 characters in all.
numbered_history <- function(turns = 40L, chars = 4000L) {
    h <- list()
    for (i in seq_len(turns)) {
        h[[length(h) + 1L]] <- list(
            role = "user",
            content = paste0("ask-", i, " ", strrep("u", chars)))
        h[[length(h) + 1L]] <- list(
            role = "assistant",
            content = paste0("answer-", i, " ", strrep("a", chars)))
    }
    h
}

# One compaction with the provider's call replaced, so the summarizer
# itself runs. Returns whether it compacted and the prompt it sent.
# With `fail` the provider's call raises an error once it has the
# prompt.
summary_prompt_for <- function(session, config, ..., fail = FALSE) {
    original <- get("chat", envir = asNamespace("llm.api"))
    on.exit(assignInNamespace("chat", original, ns = "llm.api"), add = TRUE)
    seen <- NULL
    seen_system <- NULL
    calls <- 0L
    assignInNamespace("chat", function(prompt, ..., system = NULL) {
        seen <<- prompt
        seen_system <<- system
        calls <<- calls + 1L
        if (fail) {
            stop("no answer in time")
        }
        list(content = "earlier work", usage = list())
    }, ns = "llm.api")
    done <- corteza:::maybe_compact_turn_session(
        session, config, tools = list(), system = "", ...)
    list(done = isTRUE(done), prompt = seen, system = seen_system,
         calls = calls)
}
has <- function(prompt, what) grepl(what, prompt, fixed = TRUE)

# The bytes of the body llm.api 0.2.0 builds for the Codex backend from
# a summary request, serialized as it serializes one.
codex_body_bytes <- function(got, model = "gpt-6.1-sol") {
    body <- list(model = model, instructions = got$system,
                 input = list(list(role = "user", content = list(
                     list(type = "input_text", text = got$prompt)))),
                 stream = TRUE, text = list(verbosity = "low"),
                 reasoning = NULL,
                 include = list("reasoning.encrypted_content"),
                 store = FALSE)
    nchar(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
          type = "bytes")
}

# 40 turns of control characters, each 6 bytes once JSON escapes it:
# 320,000 characters, about 1.9 MB as a request carries them.
control_history <- function(turns = 40L, chars = 4000L) {
    h <- numbered_history(turns, chars)
    for (i in seq_along(h)) {
        h[[i]]$content <- sub("[ua]+$", strrep("\001", chars),
                              h[[i]]$content)
    }
    h
}

# A 1M window: the summary reads every turn it replaces. The newest
# turn is the kept tail and is not in it.
local({
    s <- trigger_session()
    s$history <- numbered_history()
    got <- summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L))
    expect_true(got$done)
    expect_true(nchar(got$prompt) > 300000L)
    expect_false(has(got$prompt, "elided to fit"))
    expect_true(has(got$prompt, "ask-1 "))
    expect_true(has(got$prompt, "answer-39 "))
    expect_false(has(got$prompt, "ask-40 "))
    # The event says how much the summary was allowed to read.
    expect_equal(s$last_compaction$summary_chars, 937500)
})

# A model with no row has the fallback window and the budget it always
# had: the newest 120,000 characters, and a line for the 49 entries
# left out.
local({
    s <- trigger_session("no-such-model-9")
    s$history <- numbered_history()
    got <- summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L))
    expect_true(got$done)
    expect_equal(s$last_compaction$summary_chars, 120000)
    expect_true(has(got$prompt,
                    "[49 earlier message(s) elided to fit the compaction budget]"))
    expect_false(has(got$prompt, "ask-1 "))
    expect_false(has(got$prompt, "ask-25 "))
    expect_true(has(got$prompt, "answer-25 "))
    expect_true(has(got$prompt, "ask-26 "))
    expect_true(has(got$prompt, "answer-39 "))
})

# The Codex backend limits a request's bytes (900,000 by default). Plain
# text at its 272,000 window's 255,000 characters is far under that, so
# the window decides, as it does with no limit at all.
local({
    for (limit in list(NULL, Inf)) {
        s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
        s$history <- numbered_history()
        got <- if (is.null(limit)) {
            summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L))
        } else {
            summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L),
                               byte_limit = limit)
        }
        expect_true(got$done)
        expect_equal(s$last_compaction$summary_chars, 255000)
        expect_false(has(got$prompt, "ask-8 "))
        expect_true(has(got$prompt, "answer-8 "))
        expect_true(has(got$prompt, "ask-9 "))
    }
})

# Text that JSON escapes is where the bytes decide. The window's 255,000
# characters would be about 1.5 MB; the request holds the newest turns
# that fit under 900,000 bytes, framing included.
local({
    s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
    s$history <- control_history()
    got <- summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L))
    expect_true(got$done)
    expect_true(codex_body_bytes(got) <= 900000)
    expect_true(codex_body_bytes(got) > 800000)
    expect_true(has(got$prompt, "elided to fit"))
    expect_true(has(got$prompt, "answer-39 "))
    expect_false(has(got$prompt, "ask-1 "))
})

# A limit the person set, 200,000 bytes. Before this bound the floor of
# 120,000 characters let such a request reach about 350,000.
local({
    config <- trigger_config(context_compact_tokens = 10000L,
                             context_compact_bytes = 200000L)
    s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
    s$history <- control_history()
    got <- summary_prompt_for(s, config)
    expect_true(got$done)
    expect_equal(s$last_compaction$byte_limit, 200000)
    expect_true(codex_body_bytes(got) <= 200000)
    expect_true(has(got$prompt, "answer-39 "))
    # A recovery from a refusal for size, which reads the smaller budget
    # of characters, keeps the bound on bytes as well.
    for (why in c("request_buffer_overflow", "context_overflow")) {
        s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
        s$history <- control_history()
        got <- summary_prompt_for(s, config, threshold = 0,
                                  min_messages = 2L, reason = why)
        expect_true(got$done, info = why)
        expect_equal(s$last_compaction$summary_chars, 120000, info = why)
        expect_true(codex_body_bytes(got) <= 200000, info = why)
    }
    # After a failed summary too.
    s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
    s$history <- control_history()
    s$last_compaction_failure <- list(reason = "summarizer_error")
    got <- summary_prompt_for(s, config)
    expect_true(got$done)
    expect_equal(s$last_compaction$summary_chars, 120000)
    expect_true(codex_body_bytes(got) <= 200000)
})

# A limit under what the request takes with no conversation in it:
# nothing is sent, and the failure says why.
local({
    s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
    s$history <- numbered_history()
    got <- summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L,
                                                context_compact_bytes = 3000L))
    expect_false(got$done)
    expect_equal(got$calls, 0L)
    expect_equal(length(s$history), 80L)
    expect_true(grepl("over the limit of 3,000",
                      s$last_compaction_failure$error, fixed = TRUE))
})

# A limit the framing fits under with no room for one turn of 4,000
# characters. The request would hold only the note that 78 messages were
# left out, and its reply would replace them all. Nothing is sent, and
# the history stays as it was.
local({
    s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
    s$history <- numbered_history()
    before <- s$history
    got <- summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L,
                                                context_compact_bytes = 8000L))
    expect_false(got$done)
    expect_equal(got$calls, 0L)
    expect_identical(s$history, before)
    expect_true(grepl("not one message fits",
                      s$last_compaction_failure$error, fixed = TRUE))
    expect_true(grepl("over its limit of 8,000 bytes",
                      s$last_compaction_failure$error, fixed = TRUE))
})

# With room for a few turns the summary reads them, as before: the
# newest turns of the slice and the note for the rest.
local({
    s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
    s$history <- numbered_history()
    got <- summary_prompt_for(s, trigger_config(context_compact_tokens = 10000L,
                                                context_compact_bytes = 20000L))
    expect_true(got$done)
    expect_equal(got$calls, 1L)
    expect_true(has(got$prompt, "answer-39 "))
    expect_true(has(got$prompt, "elided to fit"))
    expect_false(has(got$prompt, "ask-1 "))
    expect_true(codex_body_bytes(got) <= 20000)
})

# The trim says how many it left out, all of them when the note is all
# that is left.
local({
    kept <- corteza:::.compact_trim_total(c(strrep("a", 500), "b"),
                                          max_total = 120000L, max_bytes = 300)
    expect_equal(attr(kept, "dropped"), 1L)
    expect_identical(kept[-1L], "b")
    kept <- corteza:::.compact_trim_total(strrep("a", 500),
                                          max_total = 120000L, max_bytes = 300)
    expect_equal(attr(kept, "dropped"), 1L)
    expect_equal(length(kept), 1L)
    expect_true(grepl("elided", kept[[1L]], fixed = TRUE))
})

# A summary that fails costs the larger request once. The session's
# next summary reads the budget every model had, 120,000 characters,
# and one that succeeds brings the window's share back.
local({
    config <- trigger_config(context_compact_tokens = 10000L)
    s <- trigger_session()
    s$history <- numbered_history()
    expect_equal(corteza:::.compact_summary_budget(s, 1000000L), 937500)
    got <- summary_prompt_for(s, config, fail = TRUE)
    expect_false(got$done)
    expect_true(has(got$prompt, "ask-1 "))
    expect_equal(length(s$history), 80L)
    expect_equal(s$last_compaction_failure$summary_chars, 937500)
    # The next try reads the newest 120,000 characters.
    expect_equal(corteza:::.compact_summary_budget(s, 1000000L), 120000)
    got <- summary_prompt_for(s, config)
    expect_true(got$done)
    expect_true(has(got$prompt,
                    "[49 earlier message(s) elided to fit the compaction budget]"))
    expect_false(has(got$prompt, "ask-25 "))
    expect_true(has(got$prompt, "answer-25 "))
    expect_equal(s$last_compaction$summary_chars, 120000)
    expect_null(s$last_compaction_failure)
    # That one succeeded, so the summary after it reads every turn again.
    s$history <- numbered_history()
    got <- summary_prompt_for(s, config)
    expect_true(got$done)
    expect_true(has(got$prompt, "ask-1 "))
    expect_false(has(got$prompt, "elided to fit"))
    expect_equal(s$last_compaction$summary_chars, 937500)
})

# 2 failures in a row: the second was already the smaller request, and
# the third try is still the smaller one.
local({
    config <- trigger_config(context_compact_tokens = 10000L)
    s <- trigger_session()
    s$history <- numbered_history()
    summary_prompt_for(s, config, fail = TRUE)
    got <- summary_prompt_for(s, config, fail = TRUE)
    expect_false(got$done)
    expect_false(has(got$prompt, "ask-1 "))
    expect_true(has(got$prompt, "elided to fit"))
    expect_equal(s$last_compaction_failure$summary_chars, 120000)
    expect_equal(corteza:::.compact_summary_budget(s, 1000000L), 120000)
})

# The Codex backend after a failure: 120,000 as well, under its
# window's 255,000.
local({
    s <- trigger_session("gpt-6.1-sol", provider = "openai_codex")
    expect_equal(corteza:::.compact_summary_budget(s, 272000L), 255000)
    s$last_compaction_failure <- list(reason = "summarizer_error")
    expect_equal(corteza:::.compact_summary_budget(s, 272000L), 120000)
})

# A recovery after the provider refused a request for its size reads
# the smaller budget at once: the refusal says corteza's count of the
# conversation ran low, and the summary's own request has to fit.
local({
    s <- trigger_session()
    for (why in c("context_overflow", "request_buffer_overflow")) {
        expect_equal(
            corteza:::.compact_summary_budget(s, 1000000L, reason = why),
            120000, info = why)
    }
    for (why in list(NULL, "preflight", "checkpoint", "manual", "threshold")) {
        expect_equal(
            corteza:::.compact_summary_budget(s, 1000000L, reason = why),
            937500, info = paste(why, collapse = ""))
    }
    # Through the compactor, as turn() calls it for a recovery.
    for (why in c("context_overflow", "request_buffer_overflow")) {
        s <- trigger_session()
        s$history <- numbered_history()
        got <- summary_prompt_for(
            s, trigger_config(context_compact_tokens = 10000L),
            threshold = 0, min_messages = 2L, reason = why)
        expect_true(got$done, info = why)
        expect_true(has(got$prompt, "elided to fit"), info = why)
        expect_false(has(got$prompt, "ask-25 "), info = why)
        expect_true(has(got$prompt, "answer-25 "), info = why)
        expect_equal(s$last_compaction$summary_chars, 120000, info = why)
        expect_equal(s$last_compaction$reason, why, info = why)
    }
    # A manual compaction is no recovery: it reads the window's share.
    s <- trigger_session()
    s$history <- numbered_history()
    got <- summary_prompt_for(
        s, trigger_config(context_compact_tokens = 10000L), reason = "manual")
    expect_true(got$done)
    expect_true(has(got$prompt, "ask-1 "))
    expect_equal(s$last_compaction$summary_chars, 937500)
})

# ---------------------------------------------------------------
# The idle trigger, with a clock the test holds.
# ---------------------------------------------------------------

# A fixed time well before any day this file runs on: the cases below
# measure from it, and never from the machine's clock.
t0 <- as.POSIXct("2026-03-01 12:00:00", tz = "UTC")
minutes <- function(n) t0 + n * 60

idle_session <- function(...) {
    s <- trigger_session(...)
    s$history <- trigger_history()
    s$last_response_at <- t0
    s
}
# Idle after 60 minutes with 1,000 tokens or more; the size trigger far off.
idle_config <- function(context_idle_minutes = 60L, ...) {
    trigger_config(context_idle_minutes = context_idle_minutes, ...)
}
expect_equal(idle_config()$context_idle_minutes, 60L)
expect_equal(idle_config(context_idle_minutes = 3L)$context_idle_minutes, 3L)

# Under the gap: nothing.
local({
    s <- idle_session()
    got <- compact_once(s, idle_config(), reason = "preflight",
                        now = minutes(59))
    expect_false(got$result)
    expect_equal(got$calls, 0L)
    expect_equal(length(s$history), 16L)
})

# Under the size: nothing, however long the gap.
local({
    s <- idle_session()
    got <- compact_once(s, idle_config(
        context_idle_tokens = history_tokens + 1L),
        reason = "preflight", now = minutes(600))
    expect_false(got$result)
    expect_equal(got$calls, 0L)
})

# Over both: compacts, with the reason `idle`, and says what the wait is
# before the summarizer starts.
local({
    s <- idle_session()
    got <- compact_once(s, idle_config(), reason = "preflight",
                        now = minutes(61))
    expect_true(got$result)
    expect_equal(got$calls, 1L)
    expect_equal(s$last_compaction$reason, "idle")
    expect_equal(s$last_compaction$trigger, "idle")
    expect_equal(s$last_compaction$idle_minutes, 61)
    expect_equal(s$compaction_count, 1L)
    expect_true(length(s$history) < 16L)
    expect_true(grepl("[compacted history]", s$history[[1L]]$content,
                      fixed = TRUE))
    expect_equal(length(got$said), 1L)
    expect_true(grepl("idle for 1.0h", got$said, fixed = TRUE))
})

# At the gap exactly, and at the size exactly: both count.
local({
    s <- idle_session()
    expect_true(compact_once(
        s, idle_config(context_idle_tokens = history_tokens),
        reason = "preflight", now = minutes(60))$result)
})

# Only ahead of a turn's first request. The same session at a
# checkpoint, after a turn, or under the default reason is left alone.
for (reason in c("checkpoint", "post_turn", "threshold")) {
    s <- idle_session()
    got <- compact_once(s, idle_config(), reason = reason, now = minutes(600))
    expect_false(got$result, info = reason)
    expect_equal(got$calls, 0L, info = reason)
}

# 0 turns it off, in the config or on the session.
local({
    s <- idle_session()
    expect_false(compact_once(s, idle_config(context_idle_minutes = 0L),
                              reason = "preflight",
                              now = minutes(600))$result)
    s$context_idle_minutes <- 0
    expect_false(compact_once(s, idle_config(), reason = "preflight",
                              now = minutes(600))$result)
})

# The gap must outlast the prompt cache: while the provider still holds
# the prefix, the next request reads it at a fraction of the price and
# a summary would cost more than it saves.
local({
    # 1h cache, 60 minutes configured: 60 is not longer than the cache.
    s <- idle_session(cache = "1h")
    expect_false(compact_once(s, idle_config(), reason = "preflight",
                              now = minutes(60))$result)
    expect_true(compact_once(s, idle_config(), reason = "preflight",
                             now = minutes(61))$result)
    # 1h cache, 30 minutes configured: the cache decides.
    s <- idle_session(cache = "1h")
    expect_false(compact_once(s, idle_config(context_idle_minutes = 30L),
                              reason = "preflight",
                              now = minutes(45))$result)
    expect_true(compact_once(s, idle_config(context_idle_minutes = 30L),
                             reason = "preflight", now = minutes(61))$result)
    # 5m cache, 3 minutes configured.
    s <- idle_session(cache = "5m")
    expect_false(compact_once(s, idle_config(context_idle_minutes = 3L),
                              reason = "preflight", now = minutes(5))$result)
    expect_true(compact_once(s, idle_config(context_idle_minutes = 3L),
                             reason = "preflight", now = minutes(6))$result)
    # No cache: the configured minutes alone. That is a session that
    # sets "none", and any session on a provider with no such cache.
    s <- idle_session(cache = "none")
    expect_true(compact_once(s, idle_config(context_idle_minutes = 3L),
                             reason = "preflight", now = minutes(3))$result)
    s <- idle_session(model = "gpt-6.1-sol", provider = "openai")
    expect_true(compact_once(s, idle_config(context_idle_minutes = 3L),
                             reason = "preflight", now = minutes(3))$result)
    # A session on an Anthropic provider that sets none has the default
    # cache, 5 minutes, and the rule waits for it as for one it set.
    s <- idle_session()
    expect_false(compact_once(s, idle_config(context_idle_minutes = 3L),
                              reason = "preflight", now = minutes(5))$result)
    expect_true(compact_once(s, idle_config(context_idle_minutes = 3L),
                             reason = "preflight", now = minutes(6))$result)
    # The cache named in the config counts as the session's does.
    s <- idle_session()
    s$config <- list(cache = "1h")
    expect_false(compact_once(s, idle_config(context_idle_minutes = 30L),
                              reason = "preflight",
                              now = minutes(45))$result)
})
expect_equal(corteza:::.cache_minutes(trigger_session()), 5)
expect_equal(corteza:::.cache_minutes(
    trigger_session(model = "gpt-6.1-sol", provider = "openai")), 0)
expect_equal(corteza:::.cache_minutes(trigger_session(cache = "none")), 0)
expect_equal(corteza:::.cache_minutes(trigger_session(cache = "5m")), 5)
expect_equal(corteza:::.cache_minutes(trigger_session(cache = "1h")), 60)
# A session that edits its own context gets the Anthropic cache default.
expect_equal(corteza:::.cache_minutes(trigger_session(context_edit = TRUE)), 5)
expect_equal(corteza:::.cache_minutes(
    trigger_session(context_edit = TRUE, cache = "1h")), 60)

# A session that edits its own context is skipped, as it is for size.
local({
    s <- idle_session()
    s$config <- list(context_edit = list(enabled = TRUE, strategy = "clm"))
    got <- compact_once(s, idle_config(), reason = "preflight",
                        now = minutes(600))
    expect_false(got$result)
    expect_equal(got$calls, 0L)
    expect_equal(length(s$history), 16L)
})

# So is an archive holder, and a session whose percentage turns
# compaction off.
local({
    s <- idle_session()
    expect_false(compact_once(s, idle_config(), reason = "preflight",
                              now = minutes(600),
                              kind = "archive_holder")$result)
    expect_false(compact_once(s, idle_config(context_compact_pct = Inf),
                              reason = "preflight",
                              now = minutes(600))$result)
})

# No time on record (a new session, no journal): never idle.
local({
    s <- trigger_session()
    s$history <- trigger_history()
    expect_null(corteza:::.session_last_response(s))
    got <- compact_once(s, idle_config(), reason = "preflight",
                        now = minutes(600))
    expect_false(got$result)
    # A clock that ran backwards is not a gap.
    s$last_response_at <- minutes(600)
    expect_false(compact_once(s, idle_config(), reason = "preflight",
                              now = t0)$result)
    # Neither is a value that is not a time.
    s$last_response_at <- "yesterday"
    expect_null(corteza:::.session_last_response(s))
})

# A summarizer that fails leaves the history whole and records why.
local({
    s <- idle_session()
    got <- compact_once(s, idle_config(), reason = "preflight",
                        now = minutes(61), summary = NULL)
    expect_false(got$result)
    expect_equal(got$calls, 1L)
    expect_equal(length(s$history), 16L)
    expect_equal(s$last_compaction_failure$trigger, "idle")
    expect_null(s$last_compaction)
})

# Size wins where both hold: the event names the size trigger.
local({
    s <- idle_session()
    got <- compact_once(s, idle_config(context_compact_tokens = 10000L),
                        reason = "preflight", now = minutes(600))
    expect_true(got$result)
    expect_equal(s$last_compaction$trigger, "tokens")
    expect_equal(s$last_compaction$reason, "preflight")
    expect_null(s$last_compaction$idle_minutes)
    expect_equal(length(got$said), 0L)
})

# ---------------------------------------------------------------
# After a restart: the time comes from the journal's live snapshot as
# the earlier process left it.
# ---------------------------------------------------------------

local({
    dir <- tempfile("trigger-journal-")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    live <- file.path(dir, "live.json")

    # An earlier process: a session with a journal, mirrored once.
    earlier <- trigger_session()
    earlier$history <- trigger_history()
    corteza:::journal_install(earlier, dir = dir)
    corteza:::journal_snapshot(earlier)
    expect_true(file.exists(live))
    # A new journal had no earlier snapshot.
    expect_null(corteza:::journal_previous_update(earlier))
    Sys.setFileTime(live, t0)

    # A journal not yet set up is read where it lies, and reading it
    # writes nothing.
    waiting <- trigger_session()
    waiting$history <- trigger_history()
    waiting$journal_dir <- dir
    before <- sort(list.files(dir))
    seen <- corteza:::journal_previous_update(waiting)
    expect_true(abs(as.numeric(difftime(seen, t0, units = "secs"))) < 2)
    expect_identical(sort(list.files(dir)), before)
    expect_null(waiting$.journal)
    got <- compact_once(waiting, idle_config(), reason = "preflight",
                        now = minutes(61))
    expect_true(got$result)
    expect_equal(waiting$last_compaction$reason, "idle")

    # The process that takes the journal over: the rollover replaces
    # live.json, and the time it held is kept.
    Sys.setFileTime(live, t0)
    resumed <- trigger_session()
    resumed$history <- trigger_history()
    corteza:::journal_install(resumed, dir = dir)
    expect_true(file.mtime(live) > t0 + 3600)
    kept <- corteza:::journal_previous_update(resumed)
    expect_true(abs(as.numeric(difftime(kept, t0, units = "secs"))) < 2)
    expect_true(abs(as.numeric(difftime(
        corteza:::.session_last_response(resumed), t0, units = "secs"))) < 2)
    # Under the gap as the journal dates it: nothing.
    expect_false(compact_once(resumed, idle_config(), reason = "preflight",
                              now = minutes(30))$result)
    got <- compact_once(resumed, idle_config(), reason = "preflight",
                        now = minutes(61))
    expect_true(got$result)
    expect_equal(resumed$last_compaction$reason, "idle")
    # The journal holds the rewrite under the same reason.
    segments <- list.files(dir, pattern = "^segment-[0-9]+\\.json$",
                           full.names = TRUE)
    reasons <- vapply(segments, function(p) {
        jsonlite::fromJSON(p, simplifyVector = FALSE)$reason
    }, character(1L))
    expect_true("idle" %in% reasons)

    # A response in this process is newer than the journal's time.
    resumed$last_response_at <- minutes(100)
    expect_equal(corteza:::.session_last_response(resumed), minutes(100))

    # A session moved to another journal is not dated by the one it
    # left: the time kept belongs to the journal it was read from.
    other <- file.path(tempfile("trigger-journal-other-"), "never")
    resumed$journal_dir <- other
    expect_null(corteza:::journal_previous_update(resumed))
    expect_false(dir.exists(other))
})

# A session with no journal has no earlier time.
expect_null(corteza:::journal_previous_update(trigger_session()))
expect_null(corteza:::journal_previous_update(NULL))
local({
    # A journal directory that does not exist is not created by asking.
    dir <- file.path(tempfile("trigger-none-"), "never")
    s <- trigger_session()
    s$journal_dir <- dir
    expect_null(corteza:::journal_previous_update(s))
    expect_false(dir.exists(dir))
})

# ---------------------------------------------------------------
# Through turn(): the preflight is where it runs, and a reply sets the
# time the next gap is measured from.
# ---------------------------------------------------------------

run_idle_turn_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    sent <- list()
    stub_agent <- function(prompt, tools = list(), tool_handler = NULL,
                           system = NULL, model = NULL,
                           provider = "anthropic", max_turns = 20L,
                           verbose = TRUE, history = NULL,
                           history_callback = NULL, ...) {
        sent[[length(sent) + 1L]] <<- history
        h <- c(history, list(list(role = "user", content = prompt),
                             list(role = "assistant", content = "done")))
        if (is.function(history_callback)) history_callback(h)
        list(content = "done", model = model, provider = provider,
             turns = 1L, history = h,
             usage = list(input_tokens = 10, output_tokens = 2,
                          total_tokens = 12, cost = 0.1))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")
    summaries <- 0L
    assignInNamespace("compact_summarize_slice", function(...) {
        summaries <<- summaries + 1L
        "earlier work"
    }, ns = "corteza")

    session <- trigger_session()
    session$config <- idle_config()
    session$history <- trigger_history()
    # The model last answered 2 hours ago.
    session$last_response_at <- Sys.time() - 7200
    events <- list()
    session$on_compaction <- function(event) {
        events[[length(events) + 1L]] <<- event
    }
    started <- Sys.time()
    result <- suppressMessages(corteza::turn("next", session, tools = list()))

    expect_equal(result$reply, "done")
    expect_equal(summaries, 1L)
    expect_equal(length(events), 1L)
    expect_equal(events[[1L]]$reason, "idle")
    expect_true(events[[1L]]$idle_minutes >= 120)
    # The request went out on the compacted history.
    expect_equal(length(sent), 1L)
    expect_true(length(sent[[1L]]) < 16L)
    expect_true(grepl("[compacted history]", sent[[1L]][[1L]]$content,
                      fixed = TRUE))
    # The reply reset the clock.
    expect_true(inherits(session$last_response_at, "POSIXct"))
    expect_true(session$last_response_at >= started)

    # The next turn follows at once: no gap, no second summary.
    result2 <- suppressMessages(corteza::turn("again", session,
                                              tools = list()))
    expect_equal(result2$reply, "done")
    expect_equal(summaries, 1L)
    expect_equal(length(events), 1L)
}
run_idle_turn_test()

# A turn on a session that was never idle leaves the history alone.
run_fresh_turn_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    stub_agent <- function(prompt, tools = list(), tool_handler = NULL,
                           system = NULL, model = NULL,
                           provider = "anthropic", max_turns = 20L,
                           verbose = TRUE, history = NULL,
                           history_callback = NULL, ...) {
        h <- c(history, list(list(role = "user", content = prompt),
                             list(role = "assistant", content = "done")))
        if (is.function(history_callback)) history_callback(h)
        list(content = "done", model = model, provider = provider,
             turns = 1L, history = h,
             usage = list(input_tokens = 10, output_tokens = 2,
                          total_tokens = 12, cost = 0.1))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")
    summaries <- 0L
    assignInNamespace("compact_summarize_slice", function(...) {
        summaries <<- summaries + 1L
        "earlier work"
    }, ns = "corteza")

    session <- trigger_session()
    session$config <- idle_config()
    session$history <- trigger_history()
    expect_null(session$last_response_at)
    result <- suppressMessages(corteza::turn("next", session, tools = list()))
    expect_equal(result$reply, "done")
    expect_equal(summaries, 0L)
    expect_equal(length(session$history), 18L)
    expect_true(inherits(session$last_response_at, "POSIXct"))
}
run_fresh_turn_test()
