library(tinytest)

ev <- function(tool = "bash", outcome = "ran", success = TRUE,
               added = NULL, removed = NULL) {
    e <- list(call = list(tool = tool), outcome = outcome, success = success)
    if (!is.null(added) || !is.null(removed)) {
        e$diff <- list(added = added %||% 0L, removed = removed %||% 0L)
    }
    e
}
`%||%` <- function(a, b) if (is.null(a)) b else a

feed <- function(...) {
    acc <- corteza:::rooms_activity_new()
    obs <- corteza:::rooms_activity_observer(acc)
    for (e in list(...)) obs(e)
    acc
}

# ---- Categories ----
expect_equal(corteza:::rooms_activity_category("bash"), "ran")
expect_equal(corteza:::rooms_activity_category("read_file"), "read")
expect_equal(corteza:::rooms_activity_category("replace_in_file"), "edited")
expect_equal(corteza:::rooms_activity_category("grep_files"), "searched")
# An unknown tool is still work. A summary that silently omits what the
# agent did is worse than a vague one.
expect_equal(corteza:::rooms_activity_category("some_new_tool"), "used")
expect_equal(corteza:::rooms_activity_category(NULL), "used")

# ---- The summary line ----
local({
    acc <- feed(ev("bash"), ev("bash"), ev("bash"),
                ev("read_file"), ev("read_file"))
    expect_equal(corteza:::rooms_activity_summary(acc$events),
                 "Ran 3 commands, read 2 files")
})
local({
    # Edits lead, whatever order they happened in. A turn that edited
    # six files and read one should not lead with the read.
    acc <- feed(ev("read_file"), ev("bash"), ev("write_file"),
                ev("write_file"))
    expect_equal(corteza:::rooms_activity_summary(acc$events),
                 "Edited 2 files, ran a command, read a file")
})
# "a command", not "1 command" -- the digit reads like a machine
# counting and this line is for a human glancing at a room.
local({
    acc <- feed(ev("bash"))
    expect_equal(corteza:::rooms_activity_summary(acc$events), "Ran a command")
})
expect_equal(corteza:::rooms_activity_summary(list()), "")

# ---- start events are not work ----
# "start" fires before the call runs. Counting it doubles every tool.
local({
    acc <- feed(ev("bash", outcome = "start"), ev("bash", outcome = "ran"))
    expect_equal(length(acc$events), 1L)
    expect_equal(corteza:::rooms_activity_summary(acc$events), "Ran a command")
})

# A refused call is still something that happened, and the reader should
# be told: silence would read as the agent never trying.
local({
    acc <- feed(ev("bash", outcome = "deny", success = FALSE))
    expect_equal(length(acc$events), 1L)
    expect_true(grepl("1 refused", corteza:::rooms_activity_text(acc$events),
                      fixed = TRUE))
})

# ---- Diff stats ----
local({
    acc <- feed(ev("replace_in_file", added = 100L, removed = 30L),
                ev("write_file", added = 2L, removed = 3L))
    s <- corteza:::rooms_activity_stats(acc$events)
    expect_equal(s$added, 102L)
    expect_equal(s$removed, 33L)
    expect_equal(corteza:::rooms_activity_text(acc$events),
                 "Edited 2 files  +102 -33")
})
# No edits, no stats clause -- "+0 -0" is noise on a read-only turn.
local({
    acc <- feed(ev("bash"))
    expect_equal(corteza:::rooms_activity_text(acc$events), "Ran a command")
})

# ---- HTML ----
local({
    acc <- feed(ev("bash"), ev("read_file"))
    html <- corteza:::rooms_activity_html(acc$events)
    # A real disclosure widget: both tags are in Matrix's allowed subset
    # and Element renders them natively.
    expect_true(grepl("^<details><summary>", html))
    expect_true(grepl("</details>$", html))
    expect_true(grepl("Ran a command, read a file", html, fixed = TRUE))
    # The trail itself, not just the count.
    expect_true(grepl("<li>Bash</li>", html, fixed = TRUE))
    expect_true(grepl("<li>Read File</li>", html, fixed = TRUE))
})
local({
    acc <- feed(ev("write_file", added = 5L, removed = 1L))
    html <- corteza:::rooms_activity_html(acc$events)
    # font/color is what the spec allows; a style attribute is stripped
    # by every client that follows it.
    expect_true(grepl('<font color="#2da44e">+5</font>', html, fixed = TRUE))
    expect_true(grepl('<font color="#cf222e">-1</font>', html, fixed = TRUE))
})
expect_equal(corteza:::rooms_activity_html(list()), "")

# A tool name is model-supplied. Unescaped, one could close the summary
# element and write its own markup into the room.
local({
    acc <- feed(ev("<img src=x onerror=alert(1)>"))
    html <- corteza:::rooms_activity_html(acc$events)
    expect_false(grepl("<img", html, fixed = TRUE))
    expect_true(grepl("&lt;img", html, fixed = TRUE))
})
expect_equal(corteza:::rooms_escape_html("a<b>&\"'"),
             "a&lt;b&gt;&amp;&quot;&#39;")

# ---- When to push ----
local({
    acc <- feed(ev("bash"))
    t0 <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
    # Nothing sent yet, so the first frame is always due.
    expect_true(corteza:::rooms_activity_due(acc, now = t0))
    acc$last_text <- corteza:::rooms_activity_text(acc$events)
    acc$last_at <- t0
    # Same text: not due, however long has passed. A frame that says
    # what the last one said spends a permanent event on nothing.
    expect_false(corteza:::rooms_activity_due(acc, now = t0 + 3600))
    # Changed text, but inside the floor: not yet. Synapse allows a
    # burst of ten and then one event every five seconds, and the frames
    # an over-eager trail loses are the ones at the end.
    obs <- corteza:::rooms_activity_observer(acc)
    obs(ev("read_file"))
    expect_false(corteza:::rooms_activity_due(acc, now = t0 + 1))
    expect_true(corteza:::rooms_activity_due(acc, now = t0 + 5))
})
# Nothing to say is never due.
expect_false(corteza:::rooms_activity_due(corteza:::rooms_activity_new()))

# ---- Sending and editing ----
local({
    sent <- list()
    edited <- list()
    send <- function(chat, channel, text, ..., rich = NULL) {
        sent[[length(sent) + 1L]] <<- list(text = text, rich = rich,
                                           channel = channel)
        "$activity1"
    }
    edit <- function(chat, channel, message_id, text, ..., rich = NULL) {
        edited[[length(edited) + 1L]] <<- list(id = message_id, text = text)
        message_id
    }

    acc <- feed(ev("bash"))
    expect_true(corteza:::rooms_activity_flush(acc, NULL, "!r:ex",
                                               send = send, edit = edit))
    # First flush posts; the id is kept so everything after edits it.
    expect_equal(length(sent), 1L)
    expect_equal(acc$message_id, "$activity1")
    expect_equal(sent[[1L]]$text, "Ran a command")
    expect_equal(sent[[1L]]$channel, "!r:ex")
    # rich carries the collapsible block, text the one-line fallback.
    expect_true(grepl("<details>", sent[[1L]]$rich, fixed = TRUE))

    obs <- corteza:::rooms_activity_observer(acc)
    obs(ev("read_file"))
    corteza:::rooms_activity_flush(acc, NULL, "!r:ex", send = send,
                                   edit = edit)
    # The second one edits rather than posting again. Posting would turn
    # a long turn into a column of near-identical messages.
    expect_equal(length(sent), 1L)
    expect_equal(length(edited), 1L)
    expect_equal(edited[[1L]]$id, "$activity1")
    expect_equal(edited[[1L]]$text, "Ran a command, read a file")
})

# A room that cannot show progress is a cosmetic failure. Letting it
# take the reply with it would turn one into an outage.
local({
    boom <- function(...) stop("M_LIMIT_EXCEEDED")
    acc <- feed(ev("bash"))
    expect_false(corteza:::rooms_activity_flush(acc, NULL, "!r:ex",
                                                send = boom))
    # And nothing is recorded as shown, so the next attempt retries
    # rather than believing the room is up to date.
    expect_null(acc$message_id)
    expect_null(acc$last_text)
    expect_true(corteza:::rooms_activity_due(acc))
})

# ---- The observer's lifetime ----
# A session outlives the turn. A leftover observer would keep appending
# to an accumulator whose message was already finalized, so the next
# turn's first tool call would silently edit the previous turn's trail.
local({
    session <- new.env(parent = emptyenv())
    session$on_tool <- list()
    before <- session$on_tool
    got <- corteza:::rooms_with_activity(session, NULL, "!r:ex", function() {
        expect_equal(length(session$on_tool), 1L)
        "the reply"
    })
    expect_equal(got, "the reply")
    expect_identical(session$on_tool, before)
})
# Restored even when the turn throws, or one failed turn silently
# doubles the trail on every turn after it.
local({
    session <- new.env(parent = emptyenv())
    session$on_tool <- list()
    expect_error(corteza:::rooms_with_activity(session, NULL, "!r:ex",
        function() stop("turn blew up")), "turn blew up")
    expect_equal(length(session$on_tool), 0L)
})

# A frame identical to the last one is skipped, wherever it comes from.
# The final flush bypasses the interval floor to get the last state out,
# and on a turn whose final tool call was a while ago that state is
# already on screen -- without this every turn ends by spending a
# permanent event re-sending its own text.
local({
    calls <- 0L
    send <- function(...) {
        calls <<- calls + 1L
        "$a1"
    }
    edit <- function(...) {
        calls <<- calls + 1L
        "$a1"
    }
    acc <- feed(ev("bash"))
    expect_true(corteza:::rooms_activity_flush(acc, NULL, "!r:ex",
                                               send = send, edit = edit))
    expect_equal(calls, 1L)
    # Nothing has happened since, so there is nothing to say.
    expect_false(corteza:::rooms_activity_flush(acc, NULL, "!r:ex",
                                                send = send, edit = edit))
    expect_equal(calls, 1L)
    # One more tool call and there is.
    obs <- corteza:::rooms_activity_observer(acc)
    obs(ev("read_file"))
    expect_true(corteza:::rooms_activity_flush(acc, NULL, "!r:ex",
                                               send = send, edit = edit))
    expect_equal(calls, 2L)
})

# ---- A room that cannot be kept current gets one message, not a lie ----
# An encrypted Matrix room refuses edits: the replacement text would
# ride in an ordinary event. A trail posted there would put up "Ran a
# command", have every update rejected, and leave that first frame on
# screen for the rest of the turn. The observer swallows those errors,
# so nothing is logged and nothing looks wrong -- the room just says
# something untrue.
if (requireNamespace("chat.api", quietly = TRUE)) {
    # Loopback edits, so a trail there is live.
    expect_true(corteza:::rooms_activity_live(chat.api::chat_loopback()))
    # An adapter whose capabilities cannot even be read is treated as
    # "cannot": posting frames that may never be updatable is the
    # failure this check exists to prevent.
    expect_false(corteza:::rooms_activity_live(
        structure(list(), class = c("chat_nothing", "chat_client"))))
    expect_false(corteza:::rooms_activity_live(NULL))
}

# No live client, no frames during the turn -- only the final one, which
# is accurate because it is written after everything happened.
local({
    session <- new.env(parent = emptyenv())
    session$on_tool <- list()
    flushes <- list()
    orig_live <- corteza:::rooms_activity_live
    orig_flush <- corteza:::rooms_activity_flush
    assignInNamespace("rooms_activity_live", function(chat) FALSE,
                      ns = "corteza")
    assignInNamespace("rooms_activity_flush",
                      function(acc, chat, channel, ...) {
        flushes[[length(flushes) + 1L]] <<-
            corteza:::rooms_activity_text(acc$events)
        invisible(TRUE)
    }, ns = "corteza")
    on.exit({
        assignInNamespace("rooms_activity_live", orig_live, ns = "corteza")
        assignInNamespace("rooms_activity_flush", orig_flush, ns = "corteza")
    }, add = TRUE)

    corteza:::rooms_with_activity(session, "chat", "!r:ex", function() {
        obs <- session$on_tool[[1L]]
        obs(ev("bash"))
        obs(ev("read_file"))
        expect_equal(length(flushes), 0L)
        "reply"
    })
    # One message, written once, saying what actually happened.
    expect_equal(length(flushes), 1L)
    expect_equal(flushes[[1L]], "Ran a command, read a file")
})

# With edits available the first tool call posts straight away, which is
# the whole point of the live path.
local({
    session <- new.env(parent = emptyenv())
    session$on_tool <- list()
    flushes <- 0L
    orig_live <- corteza:::rooms_activity_live
    orig_flush <- corteza:::rooms_activity_flush
    assignInNamespace("rooms_activity_live", function(chat) TRUE,
                      ns = "corteza")
    assignInNamespace("rooms_activity_flush",
                      function(acc, chat, channel, ...) {
        flushes <<- flushes + 1L
        acc$last_text <- corteza:::rooms_activity_text(acc$events)
        acc$last_at <- Sys.time()
        acc$message_id <- "$a1"
        invisible(TRUE)
    }, ns = "corteza")
    on.exit({
        assignInNamespace("rooms_activity_live", orig_live, ns = "corteza")
        assignInNamespace("rooms_activity_flush", orig_flush, ns = "corteza")
    }, add = TRUE)

    corteza:::rooms_with_activity(session, "chat", "!r:ex", function() {
        session$on_tool[[1L]](ev("bash"))
        expect_equal(flushes, 1L)
        "reply"
    })
})
