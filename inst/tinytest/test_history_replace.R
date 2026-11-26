# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

# One operation replaces a session's history, and a refused replay of
# thinking blocks is recovered (R/history-replace.R). Offline throughout:
# the model call is replaced inside function scopes.

ns <- asNamespace("corteza")
`%||%` <- function(a, b) if (is.null(a)) b else a

# The provider's text as llm.api raised it on 2026-11-15, from
# claude-fable-5-2, after the system text changed between 2 requests.
REFUSAL <- paste0(
    "API error (400): messages.1.content.0: Invalid `signature` in ",
    "`thinking` block. The block is bound to a different conversation. ",
    "Remove the block, or set ",
    "`thinking.block_binding.prefix_mismatch_behavior` to \"drop_block\". ",
    "That setting requires the `thinking-binding-controls-2026-08-01` ",
    "value in the `anthropic-beta` header. The `system` prompt differs ",
    "from the one this block was created with.")

# ---- Fixtures --------

thinking <- function(i) {
    list(type = "thinking", thinking = paste("thought", i),
         signature = paste0("sig-", i))
}
redacted <- function(i) {
    list(type = "redacted_thinking", data = paste0("data-", i))
}
text_block <- function(x) list(type = "text", text = x)
user <- function(x) list(role = "user", content = x)

# One tool round the way an Anthropic model that thinks returns it.
tool_round <- function(i, name = "run_r") {
    list(
        list(role = "assistant", content = list(
            thinking(i), text_block(paste("step", i)),
            list(type = "tool_use", id = sprintf("t%d", i), name = name,
                 input = list(code = sprintf("x <- %d", i))))),
        list(role = "user", content = list(list(
            type = "tool_result", tool_use_id = sprintf("t%d", i),
            content = sprintf("[1] %d", i))))
    )
}

answer <- function(i) {
    list(role = "assistant", content = list(
        thinking(i), redacted(i), text_block(paste("answer", i))))
}

count_thinking <- function(history) {
    total <- 0L
    for (entry in history) {
        if (!is.list(entry) || !is.list(entry$content)) {
            next
        }
        for (block in entry$content) {
            if (is.list(block) &&
                isTRUE(block$type %in% c("thinking", "redacted_thinking"))) {
                total <- total + 1L
            }
        }
    }
    total
}

stripped <- function(history) ns$history_strip_thinking(history)$history

thinking_session <- function(history = list(), dir = NULL, ...) {
    s <- corteza::new_session(channel = "cli", provider = "anthropic",
                              model_map = list(cloud = "claude-fable-5-2",
                                               local = NULL), ...)
    s$history <- history
    # No compaction and no project config unless a test asks for one.
    s$config <- list(context_compact_pct = 1000)
    if (!is.null(dir)) {
        ns$journal_install(s, dir = dir)
    }
    s
}

journal_tmp <- function() {
    d <- tempfile("corteza_history_replace_")
    dir.create(d, recursive = TRUE)
    d
}

can_chmod <- .Platform$OS.type != "windows" &&
    !identical(Sys.info()[["effective_user"]], "root")

# Run `run` with the compaction summary written here and not by a model.
with_summary <- function(run) {
    original <- get("compact_summarize_slice", envir = ns)
    on.exit(assignInNamespace("compact_summarize_slice", original,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice", function(...) {
        x <- "earlier work"
        attr(x, "usage") <- list()
        x
    }, ns = "corteza")
    run()
}

# A config under which any history of 2 entries or more is compacted
# down to its last turn.
compact_now <- list(
    context_compact_pct = 0,
    subagents = list(context_compaction = list(
        mode = "inherit_strict", compact_pct = 0, keep_recent_turns = 1L,
        keep_recent_tokens = 1L, min_messages = 2L, timeout_seconds = 1L)))

# ---- The strip --------

local({
    h <- c(list(user("one")), tool_round(1),
           list(answer(2), user("two"),
                list(role = "assistant", content = list(thinking(3)))))
    out <- ns$history_strip_thinking(h)
    # thinking(1), thinking(2), redacted(2), thinking(3).
    expect_equal(out$removed, 4L)
    expect_equal(count_thinking(h), 4L)
    expect_equal(count_thinking(out$history), 0L)
    expect_equal(length(out$history), length(h))
    # Text and the tool call stay, in their order.
    expect_identical(out$history[[2]]$content, h[[2]]$content[-1])
    expect_identical(out$history[[4]]$content, list(text_block("answer 2")))
    # An entry that held nothing else keeps a placeholder, so the entries
    # keep their count and their order.
    expect_identical(out$history[[6]],
                     list(role = "assistant",
                          content = list(text_block("[reasoning removed]"))))
    # An entry with no thinking block is untouched.
    expect_identical(out$history[c(1, 3, 5)], h[c(1, 3, 5)])
    # A second pass changes nothing.
    again <- ns$history_strip_thinking(out$history)
    expect_equal(again$removed, 0L)
    expect_identical(again$history, out$history)
})

# Other providers' shapes have no such block and come back as they were.
local({
    chat <- list(
        user("one"),
        list(role = "assistant", content = "", tool_calls = list(list(
            id = "c1", type = "function",
            `function` = list(name = "run_r", arguments = "{}")))),
        list(role = "tool", tool_call_id = "c1", name = "run_r",
             content = "ok"),
        list(role = "assistant", content = "done"))
    expect_identical(ns$history_strip_thinking(chat),
                     list(history = chat, removed = 0L))

    responses <- list(
        user("play"),
        list(type = ".openai_codex_output", output = list(
            list(type = "reasoning", content = list(),
                 encrypted_content = "xxxx"),
            list(type = "function_call", call_id = "a", name = "run_r",
                 arguments = "{}"))),
        list(type = "function_call_output", call_id = "a", output = "ok"))
    expect_identical(ns$history_strip_thinking(responses),
                     list(history = responses, removed = 0L))

    # llm.api's provider-neutral content, an entry that is no list, a
    # thinking block in a user entry (never produced), and no history.
    neutral <- list(list(role = "assistant", content = structure(
        list(structure(list(text = "x"), class = "llm_text")),
        class = "llm_content")))
    expect_identical(stripped(neutral), neutral)
    expect_identical(stripped(list("stray", user("one"))),
                     list("stray", user("one")))
    odd <- list(list(role = "user", content = list(thinking(1))))
    expect_identical(stripped(odd), odd)
    # A content list need not hold blocks alone.
    mixed <- list(list(role = "assistant",
                       content = list("bare", text_block("x"))))
    expect_identical(stripped(mixed), mixed)
    expect_identical(ns$history_strip_thinking(NULL),
                     list(history = list(), removed = 0L))
})

# ---- history_replace() --------

local({
    before <- c(list(user("one")), tool_round(1), list(answer(2)))
    after <- c(list(user("[compacted history]\n\nearlier")), tool_round(3),
               list(answer(4)))

    # No journal: the replacement is stripped, installed and returned.
    s <- thinking_session(before)
    got <- ns$history_replace(s, after, "test")
    expect_identical(got, stripped(after))
    expect_identical(s$history, stripped(after))
    expect_equal(count_thinking(s$history), 0L)

    # With a journal: the segment holds the history replaced, as it was,
    # and the replacement the session now holds.
    dir <- journal_tmp()
    s <- thinking_session(before, dir)
    ns$history_replace(s, after, "checkpoint", summary = "earlier")
    j <- ns$journal_load(dir)
    expect_equal(length(j$segments), 1L)
    expect_equal(j$segments[[1]]$reason, "checkpoint")
    expect_equal(j$segments[[1]]$summary, "earlier")
    expect_identical(j$segments[[1]]$history, before)
    expect_identical(j$segments[[1]]$after, s$history)
    expect_identical(j$live$history, s$history)
    expect_equal(count_thinking(j$segments[[1]]$history), 3L)
    expect_equal(count_thinking(j$live$history), 0L)
    # The record of the session is what the model produced.
    expect_identical(ns$journal_history(dir), before)

    # `before` is recorded as given.
    dir <- journal_tmp()
    s <- thinking_session(before, dir)
    ns$history_replace(s, after, "bridge", before = list(user("as sent")))
    expect_identical(ns$journal_load(dir)$segments[[1]]$history,
                     list(user("as sent")))

    # An edit segment (context editing) stores a reverse delta, and reads
    # back as the same 2 histories.
    dir <- journal_tmp()
    s <- thinking_session(before, dir)
    ns$history_replace(s, before, "context_index", edits = list())
    seg <- ns$journal_load(dir)$segments[[1]]
    expect_identical(seg$history, before)
    expect_identical(seg$after, stripped(before))

    # A journal that cannot record the replacement stops it.
    if (can_chmod) {
        dir <- journal_tmp()
        s <- thinking_session(before, dir)
        Sys.chmod(dir, mode = "0500")
        on.exit(Sys.chmod(dir, mode = "0700"), add = TRUE)
        expect_error(ns$history_replace(s, after, "checkpoint"), "journal")
        Sys.chmod(dir, mode = "0700")
        expect_identical(s$history, before)
    }
})

# A session that thinks on a budget keeps its blocks: the models that
# take a budget do not check the binding, and want an open tool round
# to keep its thinking.
local({
    h <- c(list(user("one")), tool_round(1))
    named <- thinking_session(h, thinking_budget_tokens = 2048L)
    expect_true(ns$.thinks_on_budget(named))
    ns$history_replace(named, h, "checkpoint")
    expect_identical(named$history, h)
    expect_identical(ns$history_normalize(named, h),
                     list(history = h, removed = 0L))

    configured <- thinking_session(h)
    configured$config$thinking_budget_tokens <- 2048L
    expect_true(ns$.thinks_on_budget(configured))

    # A talker does not take the config's budget (R/turn.R).
    configured$talker <- TRUE
    expect_false(ns$.thinks_on_budget(configured))

    plain <- thinking_session(h)
    expect_false(ns$.thinks_on_budget(plain))
    expect_identical(ns$history_normalize(plain, h),
                     list(history = stripped(h), removed = 1L))

    # A setting that does not validate is not a budget.
    broken <- thinking_session(h)
    broken$config$thinking_budget_tokens <- "many"
    expect_false(ns$.thinks_on_budget(broken))
})

# A history handed to a new session came from another conversation.
local({
    h <- c(list(user("one")), tool_round(1), list(answer(2)))
    s <- corteza::new_session(channel = "cli", history = h)
    expect_identical(s$history, stripped(h))
    expect_null(corteza::new_session(channel = "cli")$history)
    budget <- corteza::new_session(channel = "cli", history = h,
                                   thinking_budget_tokens = 2048L)
    expect_identical(budget$history, stripped(h))
})

# ---- The refusal --------

local({
    is_refusal <- function(text) {
        ns$.is_bound_thinking_error(simpleError(text))
    }
    expect_true(is_refusal(REFUSAL))
    # The sentences after "bound to a different conversation" vary.
    expect_true(is_refusal(paste0(
        "API error (400): messages.7.content.0: Invalid `signature` in ",
        "`thinking` block. The block is bound to a different ",
        "conversation.")))
    expect_true(is_refusal(paste0(
        "API error (400): messages.3.content.1: Invalid `signature` in ",
        "`redacted_thinking` block. The block is bound to a different ",
        "conversation. Content that preceded this block when it was ",
        "created is missing from this request, starting at `messages.2`.")))
    # A signature the provider could not read at all.
    expect_true(is_refusal(paste0(
        "API error (400): messages.5.content.0: Invalid `signature` in ",
        "`thinking` block.")))
    # Either marker is enough: the wording around them is the provider's
    # to change.
    expect_true(is_refusal(paste0(
        "API error (400): messages.2.content.0: `thinking` block refused. ",
        "The block is bound to a different conversation.")))
    # Other refusals are not this one.
    expect_false(is_refusal(
        "API error (400): messages: at least one message is required"))
    expect_false(is_refusal(
        "API error (400): thinking.block_binding: Extra inputs are not permitted"))
    expect_false(is_refusal(sub("400", "429", REFUSAL, fixed = TRUE)))
    expect_false(is_refusal(
        "The block is bound to a different conversation."))
    expect_false(ns$.is_limit_error(simpleError(REFUSAL)))
    expect_false(ns$.is_request_buffer_error(simpleError(REFUSAL)))
})

# ---- Recovery in turn() --------

# Replace llm.api::agent while `run` runs. `script` plays the provider:
# it gets the call (its number, prompt, history and max_turns) and the
# hooks llm.api would use, and returns a reply or stops. `run` gets a
# function that returns the calls made so far.
with_agent <- function(script, run) {
    original <- get("agent", envir = asNamespace("llm.api"))
    on.exit(assignInNamespace("agent", original, ns = "llm.api"), add = TRUE)
    calls <- list()
    stub <- function(prompt, tools = list(), tool_handler = NULL,
                     system = NULL, model = NULL, provider = "anthropic",
                     max_turns = 20L, verbose = TRUE, history = NULL,
                     history_callback = NULL, checkpoint_callback = NULL,
                     ...) {
        call <- list(n = length(calls) + 1L, prompt = prompt,
                     history = history %||% list(), max_turns = max_turns)
        calls[[call$n]] <<- call
        script(call, tool_handler = tool_handler, mirror = history_callback,
               checkpoint = function(h, turn, usage) {
                   checkpoint_callback(h, list(
                       agent_turn = turn, provider = provider, model = model,
                       assistant_text = "", tool_call_count = 1L,
                       usage = usage))
               },
               reply = function(h, text, turns, usage) {
                   list(content = text, model = model, provider = provider,
                        turns = turns, history = h, usage = usage)
               })
    }
    assignInNamespace("agent", stub, ns = "llm.api")
    run(function() calls)
}

# Run a turn and keep what it says on the console.
quiet_turn <- function(...) {
    said <- character()
    result <- withCallingHandlers(corteza::turn(...), message = function(m) {
        said <<- c(said, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    list(result = result, said = said)
}

usage_of <- function(input, output, cost) {
    list(input_tokens = input, output_tokens = output,
         total_tokens = input + output, cost = cost)
}

prompts_in <- function(history, text) {
    sum(vapply(history, function(m) identical(m$content, text), logical(1)))
}

earlier <- c(list(user("one")), tool_round(1), list(answer(2)))

# The first request is refused. Nothing ran and llm.api handed back no
# history, so the second call carries the prompt, once, on a history
# with no thinking block. The journal holds the history as it was.
with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
    if (call$n == 1L) {
        stop(REFUSAL, call. = FALSE)
    }
    h <- c(call$history, list(user(call$prompt), answer(9)))
    mirror(h)
    reply(h, "answer 9", 1L, usage_of(5, 2, 0.01))
}, function(calls) {
    dir <- journal_tmp()
    s <- thinking_session(earlier, dir)
    out <- quiet_turn("two", s, tools = list())
    made <- calls()
    expect_equal(length(made), 2L)
    expect_equal(count_thinking(made[[1]]$history), 3L)
    expect_identical(made[[2]]$prompt, "two")
    expect_identical(made[[2]]$history, stripped(earlier))
    expect_equal(made[[2]]$max_turns, made[[1]]$max_turns)
    expect_equal(out$result$reply, "answer 9")
    expect_equal(out$result$raw$corteza_thinking_removed, 3L)
    expect_equal(out$result$raw$turns, 1L)
    expect_equal(out$result$usage$input_tokens, 5)
    expect_null(out$result$raw$corteza_request_buffer_retries)
    expect_equal(sum(grepl("refused replayed thinking", out$said)), 1L)
    expect_true(any(grepl("removed 3 blocks", out$said, fixed = TRUE)))
    # The session goes on with the new reply's own thinking.
    expect_identical(s$history,
                     c(stripped(earlier), list(user("two"), answer(9))))
    expect_equal(prompts_in(s$history, "two"), 1L)
    j <- ns$journal_load(dir)
    reasons <- vapply(j$segments, function(seg) seg$reason, "")
    expect_identical(reasons, "thinking_refused")
    expect_identical(j$segments[[1]]$history, earlier)
    expect_identical(j$segments[[1]]$after, stripped(earlier))
    expect_identical(j$live$history, s$history)
})

# A tool runs, llm.api mirrors the history, and the next request is
# refused. The run goes on from the mirrored history with a NULL prompt:
# the tool runs once in total, and the turns and the usage add up.
local({
    ran <- 0L
    with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
        if (call$n == 1L) {
            h <- c(call$history, list(user(call$prompt)),
                   tool_round(5, name = "read_file")[1])
            mirror(h)
            result <- tool_handler("read_file", list(path = "same-file"))
            h <- c(h, list(list(role = "user", content = list(list(
                type = "tool_result", tool_use_id = "t5",
                content = result)))))
            mirror(h)
            checkpoint(h, 1L, usage_of(10, 2, 0.1))
            stop(REFUSAL, call. = FALSE)
        }
        h <- c(call$history, list(answer(9)))
        mirror(h)
        reply(h, "answer 9", 1L, usage_of(20, 4, 0.2))
    }, function(calls) {
        dir <- journal_tmp()
        s <- thinking_session(earlier, dir)
        s$approval_cb <- function(...) TRUE
        out <- quiet_turn("two", s,
            tools = list(list(name = "read_file", description = "Read",
                              input_schema = list(type = "object"))),
            tool_executor = function(name, args) {
                ran <<- ran + 1L
                list(content = list(list(type = "text", text = "file text")))
            })
        made <- calls()
        expect_equal(length(made), 2L)
        expect_equal(ran, 1L)
        expect_null(made[[2]]$prompt)
        expect_equal(count_thinking(made[[2]]$history), 0L)
        expect_equal(length(made[[2]]$history), length(earlier) + 3L)
        expect_equal(prompts_in(made[[2]]$history, "two"), 1L)
        last <- made[[2]]$history[[length(made[[2]]$history)]]
        expect_identical(last$content[[1]]$type, "tool_result")
        expect_true(grepl("file text", last$content[[1]]$content,
                          fixed = TRUE))
        # The tool call the result answers is still there.
        call_entry <- made[[2]]$history[[length(made[[2]]$history) - 1L]]
        expect_identical(call_entry$content[[2]]$id, "t5")
        expect_equal(made[[1]]$max_turns, 10L)
        expect_equal(made[[2]]$max_turns, 9L)
        expect_equal(out$result$reply, "answer 9")
        expect_equal(out$result$raw$turns, 2L)
        expect_equal(out$result$usage$input_tokens, 30)
        expect_equal(out$result$usage$output_tokens, 6)
        expect_equal(out$result$usage$total_tokens, 36)
        expect_equal(out$result$usage$cost, 0.3)
        # thinking(1), thinking(2), redacted(2), thinking(5).
        expect_equal(out$result$raw$corteza_thinking_removed, 4L)
        expect_equal(count_thinking(s$history), 2L)
        expect_equal(prompts_in(s$history, "two"), 1L)
        expect_identical(
            vapply(ns$journal_load(dir)$segments, function(seg) seg$reason, ""),
            "thinking_refused")
    })
})

# The same after a compaction inside the loop has made the history
# shorter than it was before the turn. Progress is read from llm.api
# having handed a history back, never from the history's length.
with_summary(function() {
    long <- c(list(user("one")), tool_round(1), tool_round(2), tool_round(3),
              list(answer(4)))
    s <- NULL
    compacted <- NULL
    with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
        if (call$n == 1L) {
            h <- c(call$history, list(user(call$prompt)), tool_round(5))
            mirror(h)
            # The window fills, and the checkpoint compacts.
            s$context_window <- 10L
            replacement <- checkpoint(h, 1L, usage_of(10, 2, 0.1))
            s$context_window <- 1e9
            compacted <<- replacement$history
            h <- replacement$history
            mirror(h)
            h <- c(h, tool_round(6))
            mirror(h)
            checkpoint(h, 2L, usage_of(12, 3, 0.1))
            stop(REFUSAL, call. = FALSE)
        }
        h <- c(call$history, list(answer(9)))
        mirror(h)
        reply(h, "answer 9", 1L, usage_of(20, 4, 0.2))
    }, function(calls) {
        dir <- journal_tmp()
        s <<- thinking_session(long, dir)
        s$config <- list(
            context_compact_pct = 50,
            subagents = list(context_compaction = list(
                mode = "inherit_strict", compact_pct = 50,
                keep_recent_turns = 1L, keep_recent_tokens = 1L,
                min_messages = 2L, timeout_seconds = 1L)))
        s$context_window <- 1e9
        out <- quiet_turn("two", s, tools = list())
        made <- calls()
        expect_equal(length(made), 2L)
        # The compaction kept the prompt and its round, without thinking.
        expect_equal(length(compacted), 4L)
        expect_equal(count_thinking(compacted), 0L)
        expect_true(length(made[[2]]$history) < length(long))
        expect_equal(length(made[[2]]$history), 6L)
        expect_null(made[[2]]$prompt)
        expect_equal(count_thinking(made[[2]]$history), 0L)
        expect_equal(made[[2]]$max_turns, 8L)
        # Only the round after the compaction still held a block.
        expect_equal(out$result$raw$corteza_thinking_removed, 1L)
        expect_equal(out$result$raw$turns, 3L)
        expect_equal(out$result$usage$input_tokens, 42)
        expect_equal(out$result$reply, "answer 9")
        expect_identical(
            vapply(ns$journal_load(dir)$segments, function(seg) seg$reason, ""),
            c("checkpoint", "thinking_refused"))
    })
})

# A second refusal in the same turn is raised as it is, and the strip
# made for the first one stays.
with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
    stop(REFUSAL, call. = FALSE)
}, function(calls) {
    s <- thinking_session(earlier)
    expect_error(quiet_turn("two", s, tools = list()),
                 "bound to a different conversation")
    expect_equal(length(calls()), 2L)
    expect_identical(s$history, stripped(earlier))
})

# One recovery a turn, also when the second request made progress first
# and so brought new thinking blocks with it. What it added stays in the
# history as the model wrote it.
with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
    if (call$n == 2L) {
        mirror(c(call$history, list(user(call$prompt)), tool_round(5)))
    }
    stop(REFUSAL, call. = FALSE)
}, function(calls) {
    s <- thinking_session(earlier)
    expect_error(quiet_turn("two", s, tools = list()),
                 "bound to a different conversation")
    expect_equal(length(calls()), 2L)
    expect_identical(s$history, c(stripped(earlier), list(user("two")),
                                  tool_round(5)))
})

# With no thinking block to remove, a second request would be the first
# one again. The refusal is raised at once.
with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
    stop(REFUSAL, call. = FALSE)
}, function(calls) {
    plain <- list(user("one"), list(role = "assistant", content = "a"))
    dir <- journal_tmp()
    s <- thinking_session(plain, dir)
    expect_error(quiet_turn("two", s, tools = list()),
                 "bound to a different conversation")
    expect_equal(length(calls()), 1L)
    expect_identical(s$history, plain)
    expect_equal(length(ns$journal_load(dir)$segments), 0L)
})

# Another error is not recovered.
with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
    stop("API error (400): messages: at least one message is required",
         call. = FALSE)
}, function(calls) {
    s <- thinking_session(earlier)
    expect_error(quiet_turn("two", s, tools = list()), "at least one message")
    expect_equal(length(calls()), 1L)
    expect_identical(s$history, earlier)
})

# A refusal removes the blocks whatever the session's thinking settings:
# the provider has just refused them.
with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
    if (call$n == 1L) {
        stop(REFUSAL, call. = FALSE)
    }
    h <- c(call$history, list(user(call$prompt), answer(9)))
    mirror(h)
    reply(h, "answer 9", 1L, usage_of(5, 2, 0.01))
}, function(calls) {
    s <- thinking_session(earlier, thinking_budget_tokens = 2048L)
    out <- quiet_turn("two", s, tools = list())
    expect_equal(length(calls()), 2L)
    expect_identical(calls()[[2]]$history, stripped(earlier))
    expect_equal(out$result$reply, "answer 9")
})

# A journal that cannot record the strip leaves the history alone, and
# the provider's error is what the caller sees.
if (can_chmod) {
    with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
        stop(REFUSAL, call. = FALSE)
    }, function(calls) {
        dir <- journal_tmp()
        s <- thinking_session(earlier, dir)
        Sys.chmod(dir, mode = "0500")
        on.exit(Sys.chmod(dir, mode = "0700"), add = TRUE)
        expect_error(quiet_turn("two", s, tools = list()),
                     "bound to a different conversation")
        Sys.chmod(dir, mode = "0700")
        expect_equal(length(calls()), 1L)
        expect_identical(s$history, earlier)
    })
}

# A refusal, and then the context window overflows in the same turn. The
# reply that is put together from the 2 runs still reports the recovery.
with_summary(function() {
    with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
        if (call$n == 1L) {
            stop(REFUSAL, call. = FALSE)
        }
        if (call$n == 2L) {
            h <- c(call$history, list(user(call$prompt)), tool_round(5))
            mirror(h)
            out <- reply(h, "", 1L, usage_of(10, 2, 0.1))
            out$truncated <- TRUE
            out$truncation_reason <- "model_context_window_exceeded"
            return(out)
        }
        h <- c(call$history, list(answer(9)))
        mirror(h)
        reply(h, "answer 9", 1L, usage_of(20, 4, 0.2))
    }, function(calls) {
        s <- thinking_session(earlier)
        s$config <- list(
            context_compact_pct = 1000,
            subagents = list(context_compaction = list(
                mode = "inherit_strict", compact_pct = 1000,
                keep_recent_turns = 1L, keep_recent_tokens = 1L,
                min_messages = 2L, timeout_seconds = 1L)))
        s$context_window <- 1e9
        out <- quiet_turn("two", s, tools = list())
        made <- calls()
        expect_equal(length(made), 3L)
        expect_identical(made[[2]]$prompt, "two")
        expect_null(made[[3]]$prompt)
        expect_equal(count_thinking(made[[3]]$history), 0L)
        expect_equal(out$result$reply, "answer 9")
        expect_equal(out$result$raw$corteza_thinking_removed, 3L)
        expect_true(out$result$raw$corteza_overflow_retry)
        expect_equal(out$result$raw$turns, 2L)
    })
})

# The request-buffer recovery shares the loop. When the first request of
# a turn overflows the buffer, llm.api has mirrored nothing and the
# compacted history holds no prompt, so the prompt goes out again.
with_summary(function() {
    plain <- list(user("one"), list(role = "assistant", content = "a"),
                  user("two"), list(role = "assistant", content = "b"))
    with_agent(function(call, tool_handler, mirror, checkpoint, reply) {
        if (call$n == 1L) {
            stop(paste("API error (507): exceeded request buffer limit",
                       "while retrying upstream"), call. = FALSE)
        }
        h <- c(call$history, list(user(call$prompt),
                                  list(role = "assistant", content = "c")))
        mirror(h)
        reply(h, "c", 1L, usage_of(5, 2, 0.01))
    }, function(calls) {
        s <- thinking_session(plain)
        s$config <- list(
            context_compact_pct = 1000, context_request_buffer_retries = 3L,
            subagents = list(context_compaction = list(
                mode = "inherit_strict", compact_pct = 1000,
                keep_recent_turns = 1L, keep_recent_tokens = 1L,
                min_messages = 2L, timeout_seconds = 1L)))
        s$context_window <- 1e9
        out <- quiet_turn("three", s, tools = list())
        made <- calls()
        expect_equal(length(made), 2L)
        expect_identical(made[[2]]$prompt, "three")
        expect_true(length(made[[2]]$history) < length(plain))
        expect_equal(out$result$raw$corteza_request_buffer_retries, 1L)
        expect_equal(prompts_in(s$history, "three"), 1L)
    })
})

# ---- Each place that replaces a history --------
#
# Two of them only empty one (a room's /clear, a monitor's change of
# job) and so have nothing to remove. test_context_clm.R and
# test_journal.R test what they record, and the table at the end of this
# file holds them to history_replace().
#
# The others run with a journal and, where a session can reach them
# without one, without. Context editing gives a session a journal when it
# has none, and the 2 restores run before theirs is installed.

# A compaction keeps the last turn, whose thinking was signed with the
# summarized turns in front of it.
with_summary(function() {
    history <- c(list(user("one")), tool_round(1),
                 list(answer(2), user("two")), tool_round(3),
                 list(answer(4)))
    dir <- journal_tmp()
    s <- thinking_session(history, dir)
    s$config <- compact_now
    s$context_window <- 1000L
    seen <- NULL
    s$on_compaction <- function(event) seen <<- event
    expect_true(ns$maybe_compact_turn_session(
        s, compact_now, tools = list(), system = "sys",
        reason = "checkpoint"))
    # The summary, then the turn that was kept: its prompt, a tool round
    # and the answer, text and tool call intact.
    expect_equal(length(s$history), 5L)
    expect_equal(count_thinking(s$history), 0L)
    expect_identical(s$history[-1], stripped(history[5:8]))
    # The host's hook is told the history the session will hold.
    expect_identical(seen$history_before, history)
    expect_identical(seen$history_after, s$history)
    expect_equal(seen$thinking_removed, 3L)
    j <- ns$journal_load(dir)
    expect_identical(j$segments[[1]]$history, history)
    expect_identical(j$segments[[1]]$after, s$history)
    expect_identical(j$live$history, s$history)
    expect_identical(ns$journal_history(dir), history)

    # A session with no journal comes out holding the same history.
    bare <- thinking_session(history)
    bare$config <- compact_now
    bare$context_window <- 1000L
    expect_true(ns$maybe_compact_turn_session(
        bare, compact_now, tools = list(), system = "sys",
        reason = "checkpoint"))
    expect_identical(bare$history, s$history)

    # On a named budget the kept turn stays as the model wrote it.
    named <- thinking_session(history, thinking_budget_tokens = 2048L)
    named$config <- compact_now
    named$context_window <- 1000L
    named$on_compaction <- function(event) seen <<- event
    expect_true(ns$maybe_compact_turn_session(
        named, compact_now, tools = list(), system = "sys",
        reason = "checkpoint"))
    expect_identical(named$history[-1], history[5:8])
    expect_identical(seen$history_after, named$history)
    expect_equal(seen$thinking_removed, 0L)
})

# Context editing labels a plain reply when a turn starts. The label is
# new text in front of every later thinking block, so they come out.
local({
    labelled <- list(user("one"), list(role = "assistant", content = "a"),
                     user("two"), answer(2))
    dir <- journal_tmp()
    s <- thinking_session(labelled, dir,
                          context_edit = list(enabled = TRUE,
                                              cadence = "always"))
    ns$.context_edit_prepare(s)
    expect_true(grepl("^\\[m[0-9]+\\]\na$", s$history[[2]]$content))
    expect_equal(count_thinking(s$history), 0L)
    j <- ns$journal_load(dir)
    expect_identical(vapply(j$segments, function(seg) seg$reason, ""),
                     "context_index")
    expect_identical(j$segments[[1]]$history, labelled)
    expect_identical(j$segments[[1]]$after, s$history)

    # With nothing to label, nothing is replaced and the blocks stay.
    steady <- list(user("one"), answer(1))
    dir <- journal_tmp()
    s <- thinking_session(steady, dir,
                          context_edit = list(enabled = TRUE,
                                              cadence = "always"))
    ns$.context_edit_prepare(s)
    expect_identical(s$history, steady)
    expect_equal(length(ns$journal_load(dir)$segments), 0L)
})

# An edit applied inside the agent loop. llm.api's own loop runs, with
# only its provider request replaced. No budget is named, so the request
# after the edit holds no thinking block: not in the round whose results
# just came in, and not before it. (test_context_edit_turn.R runs the
# same loop on a budget, where that round keeps its blocks.)
local({
    llm <- asNamespace("llm.api")
    names_replaced <- c(".agent_anthropic", ".get_provider_config")
    originals <- lapply(names_replaced, get, envir = llm)
    names(originals) <- names_replaced
    on.exit({
        for (name in names_replaced) {
            assignInNamespace(name, originals[[name]], "llm.api")
        }
    }, add = TRUE)
    assignInNamespace(".get_provider_config", function(...) list(),
                      "llm.api")
    round <- 0L
    sent <- list()
    fake <- function(messages, tools, system, model, config, ...) {
        round <<- round + 1L
        sent[[round]] <<- messages
        calls <- llm.api::history_tool_calls(messages)
        if (round == 1L) {
            requested <- list(list(id = "read-a", name = "read_file",
                                   arguments = list(path = "same-file")))
        } else if (round == 2L) {
            label <- sub("^\\[([^]]+)\\].*", "\\1", calls[[1L]]$result)
            requested <- list(list(id = "drop-a", name = "edit_context",
                                   arguments = list(op = "drop",
                                                    ids = label)))
        } else {
            return(list(text = "done", tool_calls = list(),
                        assistant_message = list(role = "assistant",
                                                 content = "done")))
        }
        assistant <- list(role = "assistant", content = c(
            list(thinking(round), redacted(round)),
            lapply(requested, function(call) {
                list(type = "tool_use", id = call$id, name = call$name,
                     input = call$arguments)
            })))
        list(text = "", assistant_message = assistant,
             tool_calls = requested)
    }
    assignInNamespace(".agent_anthropic", fake, "llm.api")
    dir <- journal_tmp()
    s <- thinking_session(list(), dir, system = "instructions",
                          web_search = FALSE,
                          context_edit = list(enabled = TRUE,
                                              cadence = "always"))
    s$context_window <- 100000L
    s$approval_cb <- function(...) TRUE
    result <- corteza::turn("Inspect the file.", s,
        tools = list(list(name = "read_file", description = "Read",
                          input_schema = list(type = "object"))),
        tool_executor = function(name, args) {
            list(content = list(list(type = "text", text = "output")))
        })
    expect_equal(round, 3L)
    expect_equal(result$reply, "done")
    # The second request follows an append: the first round goes back
    # with its thinking.
    expect_equal(count_thinking(sent[[2]]), 2L)
    # The third follows the edit.
    expect_equal(count_thinking(sent[[3]]), 0L)
    after_edit <- llm.api::history_tool_calls(sent[[3]])
    expect_identical(vapply(after_edit, `[[`, "", "id"), "drop-a")
    expect_true(grepl("applied", after_edit[[1]]$result))
    expect_true(all(vapply(after_edit, `[[`, logical(1), "completed")))
    expect_equal(count_thinking(s$history), 0L)
    # The journal holds both rounds as the model wrote them.
    j <- ns$journal_load(dir)
    reasons <- vapply(j$segments, function(seg) seg$reason, "")
    expect_true("context_edit" %in% reasons)
    edit <- j$segments[[which(reasons == "context_edit")[[1]]]]
    expect_equal(count_thinking(edit$history), 4L)
    expect_equal(count_thinking(edit$after), 0L)
    expect_equal(count_thinking(ns$journal_history(dir)), 4L)
})

# Archival collapses a turn into one entry. What stays in front of that
# entry loses its thinking. The holder is handed the turn as the model
# wrote it, and the journal keeps it that way.
local({
    original <- get("archival_archive_turn", envir = ns)
    on.exit(assignInNamespace("archival_archive_turn", original,
                              ns = "corteza"), add = TRUE)
    received <- NULL
    assignInNamespace("archival_archive_turn", function(history_slice, ...) {
        received <<- history_slice
        list(subagent_id = "fixture-holder", summary = "Archived fixture.")
    }, ns = "corteza")
    kept <- list(user("earlier prompt"), answer(1))
    added <- c(list(user("next prompt")), tool_round(2), list(answer(3)))
    dir <- journal_tmp()
    s <- thinking_session(kept, dir)
    mark <- ns$journal_mark(s)
    s$history <- c(s$history, added)
    ns$journal_snapshot(s)
    cfg <- list(archival = list(enabled = TRUE,
                                trigger = list(token_threshold = 1L)))
    ns$maybe_archive_turn(s, "next prompt", length(kept), list(), cfg,
                          "fixture-session", mark = mark)
    expect_identical(received, added)
    expect_equal(length(s$history), 4L)
    expect_identical(s$history[1:3],
                     c(stripped(kept), list(user("next prompt"))))
    expect_true(startsWith(s$history[[4]]$content, "[archived turn]"))
    j <- ns$journal_load(dir)
    expect_identical(vapply(j$segments, function(seg) seg$reason, ""),
                     "archival")
    expect_identical(j$segments[[1]]$history, c(kept, added))
    expect_identical(j$segments[[1]]$after, s$history)
    expect_identical(ns$journal_history(dir), c(kept, added))

    # A session with no journal is collapsed the same way.
    received <- NULL
    bare <- thinking_session(c(kept, added))
    ns$maybe_archive_turn(bare, "next prompt", length(kept), list(), cfg,
                          "fixture-session")
    expect_identical(received, added)
    expect_identical(bare$history, s$history)
})

# A limit sends a turn to another provider. A reply in text blocks, all
# that is left of one that came with thinking, is still Anthropic's
# shape: the Responses wire gets it as plain text, and the session holds
# what was sent.
local({
    ns$.fallback_reset()
    on.exit(ns$.fallback_reset(), add = TRUE)
    held <- stripped(list(user("play"), answer(1)))
    dir <- journal_tmp()
    s <- thinking_session(held, dir)
    s$provider <- "anthropic_claude"
    s$fallback <- "gpt-5.6-sol openai_codex"
    s$fallback_cooldown <- 30
    seen <- list()
    out <- suppressMessages(ns$.agent_with_fallback(
        list(prompt = "again", model = "claude-fable-5-2",
             provider = "anthropic_claude", history = s$history),
        s, .call = function(args) {
            seen[[length(seen) + 1L]] <<- args
            if (identical(args$provider, "anthropic_claude")) {
                stop("API error (429): rate limit", call. = FALSE)
            }
            list(content = "codex answered")
        }))
    expect_equal(out$content, "codex answered")
    expect_identical(vapply(seen, function(a) a$provider, ""),
                     c("anthropic_claude", "openai_codex"))
    expect_identical(seen[[1]]$history, held)
    expect_true(all(vapply(seen[[2]]$history,
                           function(m) is.character(m$content),
                           logical(1))))
    expect_true(grepl("answer 1", seen[[2]]$history[[2]]$content,
                      fixed = TRUE))
    expect_identical(s$history, seen[[2]]$history)
    j <- ns$journal_load(dir)
    expect_identical(vapply(j$segments, function(seg) seg$reason, ""),
                     "provider_bridge")
    expect_identical(j$segments[[1]]$history, held)

    # A session with no journal is handed the same history.
    ns$.fallback_reset()
    bare <- thinking_session(held)
    bare$provider <- "anthropic_claude"
    bare$fallback <- "gpt-5.6-sol openai_codex"
    bare$fallback_cooldown <- 30
    suppressMessages(ns$.agent_with_fallback(
        list(prompt = "again", model = "claude-fable-5-2",
             provider = "anthropic_claude", history = bare$history),
        bare, .call = function(args) {
            if (identical(args$provider, "anthropic_claude")) {
                stop("API error (429): rate limit", call. = FALSE)
            }
            list(content = "codex answered")
        }))
    expect_identical(bare$history, s$history)
})

# A room's conversation restored after a restart. The history is put in
# before the journal is installed, and the journal then keeps the saved
# one as the segment it rolls over.
local({
    dir <- journal_tmp()
    before_restart <- thinking_session(earlier, dir)
    before_restart$context_conversation <- TRUE
    before_restart$context_archive_vault <- "fixture-vault"
    before_restart$seen_event_ids <- c("$a", "$b")
    ns$.context_room_snapshot(before_restart)

    s <- thinking_session()
    expect_null(ns$journal_dir(s))
    expect_true(ns$.context_room_restore(
        s, dir, list(context_vault = "fixture-vault")))
    expect_identical(s$history, stripped(earlier))
    expect_identical(s$seen_event_ids, c("$a", "$b"))
    # Nothing was written for a session that has no journal yet.
    expect_null(ns$journal_dir(s))
    expect_equal(length(ns$journal_load(dir)$segments), 0L)
    ns$journal_install(s, dir = dir)
    j <- ns$journal_load(dir)
    expect_identical(vapply(j$segments, function(seg) seg$reason, ""),
                     "rollover")
    expect_identical(j$segments[[1]]$history, earlier)
    expect_identical(j$segments[[1]]$after, s$history)
    expect_identical(j$live$history, s$history)
    expect_identical(ns$journal_history(dir), earlier)
})

# An incomplete job resumed in a fresh worker.
local({
    state <- tempfile("history-replace-state-")
    dir.create(state)
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = state)
    on.exit({
        if (is.na(old)) {
            Sys.unsetenv("CORTEZA_STATE_DIR")
        } else {
            Sys.setenv(CORTEZA_STATE_DIR = old)
        }
        unlink(state, recursive = TRUE)
    }, add = TRUE)
    id <- ns$job_new_id()
    dir.create(ns$job_dir(id), recursive = TRUE)
    identity <- list(owner = "fixture-bot", key = "fixture-room",
                     workspace = state, role = "doer")
    left <- c(list(user("the job")), tool_round(1), tool_round(2))
    workspace <- new.env()
    assign("counter", 1L, envir = workspace)
    ns$.job_resume_save(id, identity, thinking_session(left),
                        env = workspace)

    dir <- journal_tmp()
    s <- thinking_session(list(), dir)
    target <- new.env()
    prompt <- ns$.job_resume_restore(id, list(identity = identity), s,
                                     env = target)
    expect_true(startsWith(prompt, "Continue the incomplete job"))
    expect_identical(s$history, stripped(left))
    expect_identical(get("counter", envir = target), 1L)
    j <- ns$journal_load(dir)
    expect_identical(vapply(j$segments, function(seg) seg$reason, ""),
                     "job_resume")
    expect_identical(j$segments[[1]]$after, s$history)
    # The snapshot the job left is not rewritten.
    expect_identical(
        readRDS(file.path(ns$job_resume_path(id), "history.rds")), left)

    # A session with no journal takes the same history.
    bare <- thinking_session(list())
    ns$.job_resume_restore(id, list(identity = identity), bare,
                           env = new.env())
    expect_identical(bare$history, s$history)

    # A journal that cannot record the resume stops it.
    if (can_chmod) {
        dir <- journal_tmp()
        s <- thinking_session(list(), dir)
        target <- new.env()
        Sys.chmod(dir, mode = "0500")
        on.exit(Sys.chmod(dir, mode = "0700"), add = TRUE)
        expect_error(ns$.job_resume_restore(id, list(identity = identity),
                                            s, env = target), "journal")
        Sys.chmod(dir, mode = "0700")
        expect_identical(s$history, list())
        expect_false(exists("counter", envir = target, inherits = FALSE))
    }
})

# A folded thread's archive goes in front of what the session holds,
# which changes what every entry it holds follows.
local({
    archive <- tempfile("history-replace-archive-", fileext = ".md")
    writeLines(c("# Earlier", "user: hello", "assistant: hi"), archive)
    on.exit(unlink(archive), add = TRUE)
    hook <- function(chat, room_id, thread_root) {
        list(vault = archive, segment = 1L)
    }
    held <- list(user("backfilled"), answer(1))
    dir <- journal_tmp()
    s <- thinking_session(held, dir)
    expect_true(suppressMessages(
        ns$bot_maybe_rehydrate(s, NULL, "!room:ex", "$root", hook)))
    expect_equal(length(s$history), 4L)
    expect_true(grepl("user: hello", s$history[[1]]$content, fixed = TRUE))
    expect_identical(s$history[3:4], stripped(held))
    j <- ns$journal_load(dir)
    expect_identical(vapply(j$segments, function(seg) seg$reason, ""),
                     "rehydrate")
    expect_identical(j$segments[[1]]$history, held)
    expect_identical(j$live$history, s$history)

    # A session with no journal is seeded the same way.
    bare <- thinking_session(held)
    expect_true(suppressMessages(
        ns$bot_maybe_rehydrate(bare, NULL, "!room:ex", "$root", hook)))
    expect_identical(bare$history, s$history)

    # A journal that cannot record it leaves the session as it was, and
    # the thread is not asked about again.
    if (can_chmod) {
        dir <- journal_tmp()
        s <- thinking_session(held, dir)
        Sys.chmod(dir, mode = "0500")
        on.exit(Sys.chmod(dir, mode = "0700"), add = TRUE)
        expect_false(suppressMessages(
            ns$bot_maybe_rehydrate(s, NULL, "!room:ex", "$root", hook)))
        Sys.chmod(dir, mode = "0700")
        expect_identical(s$history, held)
        expect_true(s$rehydrate_checked)
    }
})

# The sites that run in a worker's own process: a checkpoint restored
# into a new worker, archival inside the worker, and a holder's seed.
# Slow (a process is started). Skipped on CRAN for check time; CI sets
# NOT_CRAN.
if (tinytest::at_home() || identical(Sys.getenv("NOT_CRAN"), "true")) {
    local({
        root <- tempfile("history-replace-child-")
        dir.create(root)
        root <- normalizePath(root, winslash = "/")
        on.exit(unlink(root, recursive = TRUE), add = TRUE)
        left <- c(list(user("first job")), tool_round(1), list(answer(2)))
        slice <- c(list(user("archived prompt")), tool_round(3),
                   list(answer(4)))
        child <- callr::r(function(root, left, slice, reply) {
            library(corteza)
            ns <- asNamespace("corteza")
            # The user's config, and nothing from a project: archival on.
            Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config"),
                       R_USER_DATA_DIR = file.path(root, "data"),
                       R_USER_CACHE_DIR = file.path(root, "cache"),
                       CORTEZA_STATE_DIR = file.path(root, "state"),
                       ANTHROPIC_API_KEY = "offline-fixture")
            config_dir <- tools::R_user_dir("corteza", "config")
            dir.create(config_dir, recursive = TRUE)
            writeLines('{"archival":{"enabled":true}}',
                       file.path(config_dir, "config.json"))
            cwd <- file.path(root, "project")
            dir.create(cwd)
            identity <- list(owner = "fixture-bot", key = "fixture-room",
                             workspace = cwd, role = "doer")
            spec <- list(identity = identity, init_fn = function(spec) {
                corteza::subagent_turn_init(
                    provider = "anthropic", model = "claude-fable-5-2",
                    tools_filter = character(), system = "fixture",
                    max_turns = 2L, web_search = FALSE)
            })
            state_dir <- file.path(root, "worker")

            # A worker's first life: no checkpoint. It works, and leaves
            # one at the job boundary.
            first <- ns$.job_worker_child_init(cwd, spec, state_dir)
            s <- ns$.subagent_state$session
            s$history <- left
            ns$journal_snapshot(s)
            ns$.job_worker_child_checkpoint(state_dir, identity,
                                            "20261116T000000-0000abcd")

            # Its replacement restores the checkpoint.
            second <- ns$.job_worker_child_init(cwd, spec, state_dir)
            s <- ns$.subagent_state$session
            restored <- s$history
            journal_dir <- ns$job_worker_journal_dir(state_dir)
            after_restore <- ns$journal_load(journal_dir)

            sent <- list()
            archive <- FALSE
            utils::assignInNamespace("agent", function(prompt, history,
                                                       history_callback,
                                                       ...) {
                sent[[length(sent) + 1L]] <<- history
                h <- c(history, list(list(role = "user", content = prompt),
                                     reply))
                history_callback(h)
                list(content = "done", history = h, turns = 1L,
                     usage = list())
            }, "llm.api")
            utils::assignInNamespace("archival_should_trigger",
                                     function(...) archive, "corteza")
            utils::assignInNamespace("archival_archive_turn",
                                     function(history_slice, ...) {
                list(subagent_id = "fixture-holder",
                     summary = "Archived fixture.")
            }, "corteza")
            # The first request after the restore, then a turn that is
            # archived with the one before it still in the history.
            corteza::subagent_turn_prompt("second job")
            before_archival <- s$history
            archive <- TRUE
            corteza::subagent_turn_prompt("third job")
            after_archival <- s$history
            archived <- ns$journal_load(journal_dir)

            # A holder is seeded with a turn from another conversation.
            corteza::subagent_seed_history(slice)
            list(first = first$restored, second = second$restored,
                 restored = restored, after_restore = after_restore,
                 sent = sent, before_archival = before_archival,
                 after_archival = after_archival, archived = archived,
                 seeded = s$history, kind = ns$.subagent_state$kind,
                 protected = ns$.subagent_state$protected_history_len)
        }, list(root = root, left = left, slice = slice, reply = answer(9)))

        expect_false(child$first)
        expect_true(child$second)
        # The restored history holds no thinking block, and the journal
        # holds the one the worker left as the segment it rolled over.
        expect_identical(child$restored, stripped(left))
        segments <- child$after_restore$segments
        expect_identical(vapply(segments, function(seg) seg$reason, ""),
                         "rollover")
        expect_identical(segments[[1]]$history, left)
        expect_identical(segments[[1]]$after, stripped(left))
        # So does the first request the restored worker sends.
        expect_identical(child$sent[[1]], stripped(left))
        # That turn's own thinking stays while the history only grows.
        expect_equal(count_thinking(child$before_archival), 2L)
        expect_equal(count_thinking(child$sent[[2]]), 2L)
        # Archival inside the worker replaces the history.
        expect_equal(count_thinking(child$after_archival), 0L)
        expect_true(startsWith(
            child$after_archival[[length(child$after_archival)]]$content,
            "[archived turn]"))
        reasons <- vapply(child$archived$segments,
                          function(seg) seg$reason, "")
        expect_identical(reasons, c("rollover", "archival"))
        expect_equal(count_thinking(child$archived$segments[[2]]$history),
                     4L)
        # The seed.
        expect_identical(child$seeded, stripped(slice))
        expect_identical(child$kind, "archive_holder")
        expect_equal(child$protected, length(slice))
    })
}

# ---- Every assignment to a history is known --------
#
# Each function of the loaded namespace is read, nested functions
# included, for an assignment to `<something>$history`. One that the
# table below does not list fails this test. Whoever adds one says here
# which kind it is, and a replacement is none of these kinds: it goes
# through history_replace().

# The targets of the assignments in `expr` that name a history: the
# left side of `<-`, `<<-` or `=` wherever it reaches `x$history` or
# `x[["history"]]` (an element or an attribute of one included), and
# assign("history", ...).
history_assignments <- function(expr) {
    found <- character()
    walk <- function(e) {
        if (is.call(e)) {
            op <- e[[1L]]
            if (is.symbol(op) && length(e) == 3L &&
                as.character(op) %in% c("<-", "<<-", "=")) {
                target <- e[[2L]]
                while (is.call(target) && length(target) >= 2L) {
                    head <- target[[1L]]
                    named <- is.symbol(head) && length(target) == 3L &&
                        ((identical(as.character(head), "$") &&
                              identical(as.character(target[[3L]]),
                                        "history")) ||
                             (identical(as.character(head), "[[") &&
                                  identical(target[[3L]], "history")))
                    if (named) {
                        found <<- c(found,
                                    paste(deparse(target), collapse = ""))
                        break
                    }
                    target <- target[[2L]]
                }
            }
            if (is.symbol(op) && identical(as.character(op), "assign") &&
                length(e) >= 2L && identical(e[[2L]], "history")) {
                found <<- c(found, "assign(\"history\")")
            }
        }
        if (is.call(e) || is.pairlist(e) || is.expression(e)) {
            for (i in seq_along(e)) {
                # An argument left empty, as in x[, 1].
                if (!identical(e[[i]], quote(expr = ))) {
                    walk(e[[i]])
                }
            }
        }
    }
    walk(expr)
    found
}

# The reader finds each form, and nothing that only reads a history.
local({
    probe <- function(x, later = function() x$history <- 0) {
        x$history <- 1
        nested <- function() x$history[[2]] <<- 3
        names(x[["history"]]) = "a"
        assign("history", 5, envir = x)
        x$other <- 6
        local_copy <- x$history
        if (identical(x$history, 7)) x[, 1]
    }
    expect_identical(
        c(history_assignments(formals(probe)),
          history_assignments(body(probe))),
        c("x$history", "x$history", "x$history", "x[[\"history\"]]",
          "assign(\"history\")"))
})

local({
    known <- c(
        # The replacement.
        "history_replace :: session$history" = 1L,
        # A history handed to a session that has made no request of its
        # own. Its thinking blocks are removed where it is assigned.
        "new_session :: s$history" = 1L,
        "subagent_seed_history :: .subagent_state$session$history" = 1L,
        # The end of a conversation in the terminal. A new session id,
        # and with it a new journal, follows.
        ".repl_clear :: ctx$session$history" = 1L,
        # What llm.api handed back: the history a request was made from,
        # with what the model and the tools added to it.
        ".turn_impl :: session$history" = 4L,
        ".recover_output :: session$history" = 1L,
        # Entries added at the end.
        ".output_keep_reply :: session$history" = 1L,
        "apply_exit_marker :: session$history" = 1L,
        "bot_poll :: session$history" = 3L,
        "bot_backfill_room :: session$history" = 1L,
        "bot_job_settled :: s$history" = 1L,
        "bot_job_settled :: recipient$history" = 1L,
        "bot_job_report_back :: asker$history" = 1L,
        "bot_job_handoff :: target$history" = 1L,
        "bot_sidebar :: target$history" = 1L,
        ".delegation_receive_request :: s$history" = 1L,
        ".delegation_pump :: s$history" = 1L,
        ".repl_present_job_event :: ctx$session$history" = 1L,
        ".room_consult_record :: s$history" = 1L,
        ".voice_backfill :: s$history" = 1L,
        # Not a session: the arguments of a request, a reply, a record
        # in the journal.
        ".turn_impl :: args$history" = 1L,
        ".turn_impl :: retry_args$history" = 1L,
        ".turn_impl :: response$history" = 1L,
        ".recover_output :: retry$history" = 1L,
        ".agent_with_fallback :: args$history" = 2L,
        ".prepare_route_context :: args$history" = 1L,
        ".fallback_resume_args :: args$history" = 1L,
        ".journal_write_segment :: envelope$history" = 1L,
        "journal_load :: seg$history" = 1L)

    seen <- character()
    for (name in ls(ns, all.names = TRUE)) {
        f <- get(name, envir = ns, inherits = FALSE)
        if (!is.function(f) || is.primitive(f)) {
            next
        }
        targets <- c(history_assignments(formals(f)),
                     history_assignments(body(f)))
        seen <- c(seen, paste(rep(name, length(targets)), targets,
                              sep = " :: "))
    }
    counted <- table(seen)
    found <- stats::setNames(as.integer(counted), names(counted))
    expect_identical(sort(setdiff(names(found), names(known))), character(),
                     info = paste("an assignment to a history that the table",
                                  "in test_history_replace.R does not list"))
    expect_identical(sort(setdiff(names(known), names(found))), character(),
                     info = "the table lists an assignment that is gone")
    expect_identical(found[names(known)], known)
})

# The command's own script assigns no history: it hands one to
# new_session().
local({
    script <- system.file("bin", "corteza", package = "corteza")
    expect_true(nzchar(script))
    lines <- readLines(script, warn = FALSE)
    expect_true(length(lines) > 100L)
    expect_equal(
        sum(grepl("\\$history[[:space:]]*(<-|<<-|=[^=])", lines)), 0L)
})
