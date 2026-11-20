# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# Tool calls left without a result (R/turn-repair.R). A provider refuses
# every request made from a history that holds one, so turn() closes
# them when an error ends it and again at the start of each turn.
# Nothing here reaches a model.

ns <- asNamespace("corteza")
close_calls <- ns$.history_close_open_calls
ph <- ns$.open_call_placeholder

use <- function(id, name = "grep_files") {
    list(type = "tool_use", id = id, name = name, input = list())
}
res <- function(id, content = "out") {
    list(type = "tool_result", tool_use_id = id, content = content)
}
asst <- function(...) list(role = "assistant", content = list(...))
user <- function(...) list(role = "user", content = list(...))
# Element `i` of `x`, or NULL where there is none, so a broken history
# fails the assertion that reads it instead of stopping the file.
at <- function(x, i) {
    if (is.list(x) && i >= 1L && i <= length(x)) x[[i]]
}
# The blocks of message `m`: none where its content is not a list.
blocks <- function(m) {
    if (is.list(m) && is.list(m$content)) Filter(is.list, m$content) else list()
}
types <- function(m) {
    vapply(blocks(m), function(b) as.character(c(b$type, "")[[1L]]), character(1))
}
# The ids of the results in message `m`, in order.
result_ids <- function(m) {
    vapply(Filter(function(b) identical(b$type, "tool_result"), blocks(m)),
           function(b) as.character(b$tool_use_id), character(1))
}
# Every call answered in the message right after the one that made it,
# as Anthropic requires.
answered_next <- function(h) {
    for (i in seq_along(h)) {
        ids <- vapply(Filter(function(b) identical(b$type, "tool_use"), blocks(h[[i]])),
                      function(b) as.character(b$id), character(1))
        if (!length(ids)) next
        if (!all(ids %in% result_ids(at(h, i + 1L)))) {
            return(FALSE)
        }
    }
    TRUE
}

# ---- Closing the calls a history leaves open -------------------------------
local({
    # A history with no open call comes back as it was.
    whole <- list(list(role = "user", content = "look"), asst(use("a")),
                  user(res("a")), list(role = "assistant", content = "done"))
    out <- close_calls(whole)
    expect_identical(out$history, whole)
    expect_identical(out$closed, character())
    expect_false(out$ends_with_results)
    expect_identical(close_calls(list())$history, list())
    expect_identical(close_calls(NULL)$closed, character())

    # A call at the end: its results follow it, and the history ends
    # with them.
    out <- close_calls(list(list(role = "user", content = "look"),
                            asst(use("a"), use("b"))))
    expect_identical(out$closed, c("a", "b"))
    expect_identical(length(out$history), 3L)
    expect_identical(result_ids(at(out$history, 3L)), c("a", "b"))
    expect_identical(at(blocks(at(out$history, 3L)), 1L)$content, ph)
    expect_true(out$ends_with_results)
    expect_true(answered_next(out$history))

    # A batch cut short: the results that came in stay, first, and the
    # missing one joins them in the same message.
    out <- close_calls(list(list(role = "user", content = "look"),
                            asst(use("a"), use("b")), user(res("a", "real"))))
    expect_identical(out$closed, "b")
    expect_identical(length(out$history), 3L)
    expect_identical(result_ids(at(out$history, 3L)), c("a", "b"))
    expect_identical(at(blocks(at(out$history, 3L)), 1L)$content, "real")
    expect_true(out$ends_with_results)

    # A call in the middle, followed by a prompt a room added as text:
    # the result goes first in that message and the text stays after it.
    out <- close_calls(list(asst(use("a")),
                            list(role = "user", content = "next question"),
                            list(role = "assistant", content = "an answer")))
    expect_identical(out$closed, "a")
    expect_identical(length(out$history), 3L)
    expect_identical(result_ids(at(out$history, 2L)), "a")
    expect_identical(at(blocks(at(out$history, 2L)), 2L),
                     list(type = "text", text = "next question"))
    expect_false(out$ends_with_results)
    expect_true(answered_next(out$history))

    # Results go before the other blocks of a message, as Anthropic
    # reads them only there.
    out <- close_calls(list(asst(use("a"), use("b")),
                            user(res("a"), list(type = "text", text = "and"))))
    expect_identical(types(at(out$history, 2L)),
                     c("tool_result", "tool_result", "text"))
    expect_identical(result_ids(at(out$history, 2L)), c("a", "b"))
    # The history ends with a message that says more than results.
    expect_false(out$ends_with_results)

    # A call followed by another assistant message gets a message of
    # results between them.
    out <- close_calls(list(asst(use("a")),
                            list(role = "assistant", content = "ack")))
    expect_identical(length(out$history), 3L)
    expect_identical(result_ids(at(out$history, 2L)), "a")
    expect_identical(at(out$history, 3L)$content, "ack")
    expect_false(out$ends_with_results)

    # An empty prompt adds no empty text block.
    out <- close_calls(list(asst(use("a")), list(role = "user", content = "")))
    expect_identical(types(at(out$history, 2L)), "tool_result")

    # Every call is closed where llm.api reads it too.
    h <- close_calls(list(asst(use("a")), list(role = "user", content = "x"),
                          asst(use("b"), use("c")), user(res("c"))))$history
    expect_true(all(vapply(llm.api::history_tool_calls(h),
                           function(r) isTRUE(r$completed), logical(1))))
    expect_true(answered_next(h))
})

local({
    # OpenAI's results are messages of their own after the call; the
    # missing one goes after those already there.
    call <- function(id) {
        list(id = id, type = "function",
             `function` = list(name = "grep_files", arguments = "{}"))
    }
    tool <- function(id) {
        list(role = "tool", tool_call_id = id, name = "grep_files",
             content = "out")
    }
    h <- list(list(role = "user", content = "look"),
              list(role = "assistant", content = "", tool_calls = list(call("a"),
                                                                        call("b"))),
              tool("a"), list(role = "user", content = "next"))
    out <- close_calls(h)
    expect_identical(out$closed, "b")
    expect_identical(length(out$history), 5L)
    expect_identical(at(out$history, 3L), tool("a"))
    expect_identical(at(out$history, 4L),
                     list(role = "tool", tool_call_id = "b", name = "grep_files",
                          content = ph))
    expect_identical(at(out$history, 5L)$content, "next")
    expect_false(out$ends_with_results)
    # At the end, the history ends with the results.
    out <- close_calls(h[1:2])
    expect_identical(c(at(out$history, 3L)$tool_call_id,
                       at(out$history, 4L)$tool_call_id), c("a", "b"))
    expect_true(out$ends_with_results)

    # The Responses wire: a function_call_output after the output that
    # made the call.
    made <- list(type = ".openai_codex_output",
                 output = list(list(type = "function_call", call_id = "r1",
                                    name = "grep_files", arguments = "{}")))
    out <- close_calls(list(list(role = "user", content = "look"), made,
                            list(role = "user", content = "next")))
    expect_identical(out$closed, "r1")
    expect_identical(at(out$history, 3L),
                     list(type = "function_call_output", call_id = "r1",
                          output = ph))
    expect_identical(at(out$history, 4L)$content, "next")
    expect_true(all(vapply(llm.api::history_tool_calls(out$history),
                           function(r) isTRUE(r$completed), logical(1))))
})

# ---- On a session --------------------------------------------------------
fresh <- function(history) {
    s <- corteza::new_session("cli", provider = "anthropic", verbose = FALSE,
                              model_map = list(cloud = "fixture", local = NULL))
    s$config <- list(approval_mode = "allow")
    s$history <- history
    s
}

local({
    # Closed at the end: the marker follows the results.
    s <- fresh(list(list(role = "user", content = "look"), asst(use("a"))))
    expect_identical(ns$.turn_close_open_calls(s, "[marker]", "test"), 1L)
    expect_identical(length(s$history), 4L)
    expect_identical(result_ids(at(s$history, 3L)), "a")
    expect_identical(at(s$history, 4L), list(role = "assistant", content = "[marker]"))

    # Closed in the middle: nothing is added at the end.
    s <- fresh(list(asst(use("a")), list(role = "user", content = "x")))
    expect_identical(ns$.turn_close_open_calls(s, "[marker]", "test"), 1L)
    expect_identical(length(s$history), 2L)
    expect_identical(result_ids(at(s$history, 2L)), "a")

    # Nothing open: the history is left alone.
    whole <- list(list(role = "user", content = "look"),
                  list(role = "assistant", content = "done"))
    s <- fresh(whole)
    expect_identical(ns$.turn_close_open_calls(s, "[marker]", "test"), 0L)
    expect_identical(s$history, whole)

    # It never throws, which matters in an error handler.
    s <- fresh(list(asst(use("a"))))
    real <- get("history_replace", envir = ns)
    assignInNamespace("history_replace", function(...) stop("disk full"),
                      ns = "corteza")
    closed <- tryCatch(ns$.turn_close_open_calls(s, "[marker]", "test"),
                       error = function(e) "threw")
    assignInNamespace("history_replace", real, ns = "corteza")
    expect_identical(closed, 0L)
    expect_identical(length(s$history), 1L)
    expect_identical(ns$.turn_close_open_calls("not a session", "[m]", "test"), 0L)

    expect_identical(ns$.turn_error_marker(simpleError("reached elapsed time limit")),
                     "[Stopped by an error before completing: reached elapsed time limit]")
    expect_identical(ns$.turn_error_marker(simpleError("two\nlines")),
                     "[Stopped by an error before completing: two lines]")
    expect_identical(ns$.turn_error_marker(simpleError("")),
                     "[Stopped by an error before completing: simpleError]")
})

# ---- A turn ---------------------------------------------------------------
# A stand-in for llm.api::agent() that mirrors the history as agent()
# does, makes one tool call, and then does what `then` says. `seen`
# keeps the history it was handed.
fake_agent <- function(then, seen) {
    function(prompt, tools = list(), tool_handler = NULL, history = NULL,
             history_callback = NULL, ...) {
        seen$history <- history
        messages <- c(history, list(list(role = "user", content = prompt)))
        mirror <- function() {
            if (is.function(history_callback)) history_callback(messages)
        }
        if (identical(then, "fail_first")) {
            stop("the request failed")
        }
        messages <- c(messages, list(asst(use("toolu_1"))))
        mirror()
        if (identical(then, "escape")) {
            # What a time limit that fires outside the tool's handlers
            # does: the error leaves agent() with the call unanswered.
            stop("reached elapsed time limit")
        }
        tool_handler("grep_files", list(pattern = "x"))
        messages <- c(messages, list(user(res("toolu_1"))),
                      list(list(role = "assistant", content = "done")))
        mirror()
        list(content = "done", history = messages, turns = 2L, model = "fixture",
             provider = "anthropic", usage = list(input_tokens = 1, output_tokens = 1))
    }
}
run_turn <- function(session, then) {
    real <- get("agent", envir = asNamespace("llm.api"))
    on.exit(assignInNamespace("agent", real, ns = "llm.api"), add = TRUE)
    seen <- new.env()
    assignInNamespace("agent", fake_agent(then, seen), ns = "llm.api")
    executor <- function(name, args) {
        list(content = list(list(type = "text", text = "found")))
    }
    result <- tryCatch(suppressMessages(corteza::turn("look", session,
        tool_executor = executor, tools = list())),
        error = function(e) e)
    list(result = result, seen = seen$history)
}
earlier <- list(list(role = "user", content = "earlier"),
                list(role = "assistant", content = "earlier answer"))

local({
    # An error that leaves agent() with a call open still ends the turn,
    # as itself, and the history the next request is made from has the
    # call closed and says why.
    s <- fresh(earlier)
    out <- run_turn(s, "escape")
    expect_true(inherits(out$result, "simpleError"))
    expect_identical(conditionMessage(out$result), "reached elapsed time limit")
    h <- s$history
    expect_identical(length(h), 6L)
    expect_identical(at(h, 3L)$content, "look")
    expect_identical(result_ids(at(h, 5L)), "toolu_1")
    expect_identical(at(h, 6L)$content,
                     "[Stopped by an error before completing: reached elapsed time limit]")
    expect_true(answered_next(h))

    # The next turn starts from it and runs.
    out <- run_turn(s, "run")
    expect_identical(out$result$reply, "done")
    expect_identical(out$seen, h)
})

local({
    # An error before anything was mirrored leaves the history as it was.
    s <- fresh(earlier)
    out <- run_turn(s, "fail_first")
    expect_identical(conditionMessage(out$result), "the request failed")
    expect_identical(s$history, earlier)
})

local({
    # A history that comes to a turn with a call open, here followed by
    # what a room added after it, is closed before the first request.
    broken <- c(earlier, list(asst(use("toolu_0")),
                              list(role = "user", content = "are you there?"),
                              list(role = "assistant", content = "[ack]")))
    s <- fresh(broken)
    out <- run_turn(s, "run")
    expect_identical(out$result$reply, "done")
    expect_true(answered_next(out$seen))
    expect_identical(length(out$seen), 5L)
    expect_identical(result_ids(at(out$seen, 4L)), "toolu_0")
    expect_identical(at(blocks(at(out$seen, 4L)), 2L)$text, "are you there?")

    # Open at the end, the marker says an earlier turn left it.
    s <- fresh(c(earlier, list(asst(use("toolu_0")))))
    out <- run_turn(s, "run")
    expect_identical(out$result$reply, "done")
    expect_identical(length(out$seen), 5L)
    expect_identical(at(out$seen, 5L)$content, ns$.open_calls_marker())
    expect_true(answered_next(out$seen))
})
