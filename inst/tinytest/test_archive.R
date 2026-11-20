# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# The archive (R/archive.R): each new range of a session's ledger goes to
# the host's on_archive hook, a range it took is never offered again,
# and one it did not take waits on disk, through /clear and a restart,
# until it does. Nothing here reaches a homeserver or a model.

cfg <- list(user_id = "@bot:ex")

# Each block keeps its archive state in a folder of its own.
with_state <- function(code) {
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    dir <- tempfile("archive-state-")
    Sys.setenv(CORTEZA_STATE_DIR = dir)
    on.exit({
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(dir, recursive = TRUE)
    })
    code
}

# A session whose ledger holds `ids`, said in `room`.
mk <- function(ids, room = "!a:ex") {
    s <- new.env(parent = emptyenv())
    s$room_id <- room
    for (i in ids) {
        corteza:::bot_transcript_add(s, i, "user", paste("turn", i))
    }
    s
}

# A host that records what it was offered. `take` is TRUE, FALSE (the
# range is not taken), "error", or a function of the range.
host <- function(take = TRUE) {
    h <- new.env()
    h$offered <- list()
    h$take <- take
    h$hook <- function(chat, cfg, room_id, archive) {
        h$offered[[length(h$offered) + 1L]] <- list(room_id = room_id,
                                                    archive = archive)
        take <- h$take
        if (is.function(take)) {
            take <- take(archive)
        }
        if (identical(take, "error")) {
            stop("summary model down")
        }
        if (isTRUE(take)) {
            return(list(archived = TRUE,
                        ref = paste0("notes/", substr(archive$id, 1, 8), ".md")))
        }
        NULL
    }
    h
}

offered_ids <- function(h) {
    lapply(h$offered, function(x) {
        vapply(x$archive$entries, function(e) e$event_id, "")
    })
}

pending_path <- function(key, cfg_ = cfg) {
    corteza:::bot_archive_pending_path(corteza:::bot_session_id(cfg_, key))
}

archive <- function(s, key, h, reason = "flush") {
    corteza:::bot_archive_session(s, key, NULL, h$hook, cfg, reason)
}

# ---- No hook: nothing filed, nothing kept, the ledger untouched ----
with_state(local({
    s <- mk(c("$1", "$2"))
    out <- corteza:::bot_archive_session(s, "!a:ex")
    expect_identical(out, list(answer = NULL, filed = 0L, durable = TRUE))
    expect_identical(length(s$transcript), 2L)
    reg <- new.env(parent = emptyenv())
    assign("!a:ex", s, envir = reg)
    # Not even the config is read.
    expect_identical(corteza::bot_archive_all(reg), 0L)
    expect_false(dir.exists(file.path(Sys.getenv("CORTEZA_STATE_DIR"),
                                      "archive")))
}))

# ---- What the host is handed ----
with_state(local({
    h <- host()
    key <- corteza:::bot_session_key("!a:ex", "$root")
    s <- mk(c("$1", "$2"))
    corteza:::bot_transcript_add(s, "$3", "assistant", c("two", "lines"))
    out <- archive(s, key, h)
    expect_identical(length(h$offered), 1L)
    got <- h$offered[[1L]]$archive
    expect_identical(h$offered[[1L]]$room_id, "!a:ex")
    expect_identical(names(got),
                     c("id", "bot", "room_id", "thread", "session",
                       "first_event", "last_event", "entries", "reason"))
    expect_true(grepl("^[0-9a-f]{64}$", got$id))
    expect_identical(got$bot, "@bot:ex")
    expect_identical(got$room_id, "!a:ex")
    expect_identical(got$thread, "$root")
    expect_identical(got$session, corteza:::bot_session_id(cfg, key))
    expect_identical(got$first_event, "$1")
    expect_identical(got$last_event, "$3")
    expect_identical(got$entries[[3L]],
                     list(event_id = "$3", role = "assistant",
                          content = "two\nlines"))
    expect_identical(got$reason, "flush")
    # Taken: the answer comes back, the ledger is consumed, the event
    # ids are kept, and nothing waits.
    expect_identical(out$filed, 1L)
    expect_identical(out$answer,
                     list(archived = TRUE,
                          ref = paste0("notes/", substr(got$id, 1, 8), ".md")))
    expect_identical(length(s$transcript), 0L)
    expect_identical(corteza:::bot_archive_state_read("!a:ex"),
                     c("$1", "$2", "$3"))
    expect_false(file.exists(pending_path(key)))
    # The main timeline's range has no thread, and another session id.
    m <- mk("$4")
    archive(m, "!a:ex", h)
    expect_null(h$offered[[2L]]$archive$thread)
    expect_false(identical(h$offered[[2L]]$archive$session, got$session))
}))

# Tool turns live in history for the provider's benefit and never reach
# the host, whatever role the provider gives them (Anthropic returns
# tool results as role = "user"). Only the ledger is read.
with_state(local({
    h <- host()
    s <- mk(c("$v1", "$v2"))
    s$history <- list(
        list(role = "user", content = "visible ask"),
        list(role = "tool", content = "TOOLRESULT-SENTINEL"),
        list(role = "user", content = "TOOLRESULT-ANTHROPIC-SHAPE"))
    archive(s, "!tools:ex", h)
    text <- vapply(h$offered[[1L]]$archive$entries, function(e) e$content, "")
    expect_identical(text, c("turn $v1", "turn $v2"))
}))

# ---- A range the host took is never offered again ----
with_state(local({
    h <- host()
    archive(mk(c("$1", "$2")), "!a:ex", h)
    expect_identical(length(h$offered), 1L)
    # The same events again, and a restart whose backfill rebuilt them:
    # nothing to offer, and the ledger is drained.
    again <- mk(c("$1", "$2"))
    expect_identical(archive(again, "!a:ex", h)$filed, 0L)
    expect_identical(length(again$transcript), 0L)
    expect_identical(length(h$offered), 1L)
    # A new event is offered alone, and the known ones leave the queue
    # with it.
    mixed <- mk(c("$1", "$2", "$3"))
    archive(mixed, "!a:ex", h)
    expect_identical(offered_ids(h)[[2L]], "$3")
    expect_identical(length(mixed$transcript), 0L)
    # One between kept ones, too. Slicing from the first new index to
    # the end used to offer everything after it.
    archive(mk(c("$1", "$new", "$2", "$3")), "!a:ex", h)
    expect_identical(offered_ids(h)[[3L]], "$new")
    # The /clear shape: an acknowledgement that a restart's backfill
    # puts back among kept events.
    archive(mk(c("$1", "$new", "$2", "$3", "$clear")), "!a:ex", h)
    expect_identical(offered_ids(h)[[4L]], "$clear")
    # More events than the kept tail holds: the ledger is consumed when
    # the host takes it, so no event can age out of the tail while still
    # queued and come back as new.
    big <- mk(sprintf("$big%04d", 1:513))
    archive(big, "!big:ex", h)
    expect_identical(length(h$offered[[5L]]$archive$entries), 513L)
    expect_identical(length(big$transcript), 0L)
    archive(big, "!big:ex", h)
    expect_identical(length(h$offered), 5L)
    # Rooms are kept apart: one room's events cannot suppress the same
    # ids in another, including ids that differ only in punctuation.
    archive(mk(c("$1", "$2"), room = "!c:ex"), "!c:ex", h)
    expect_identical(offered_ids(h)[[6L]], c("$1", "$2"))
    expect_false(identical(corteza:::bot_archive_state_path("!a-b:ex"),
                           corteza:::bot_archive_state_path("!a_b:ex")))
}))

# ---- A range the host did not take waits, through /clear and restart ----
with_state(local({
    down <- host(FALSE)
    key <- "!p:ex"
    s <- mk(c("$1", "$2"), room = key)
    expect_message(out <- archive(s, key, down, "clear"),
                   "on_archive hook: not archived (!p:ex, kept to retry)",
                   fixed = TRUE)
    expect_identical(out, list(answer = NULL, filed = 0L, durable = TRUE))
    first_id <- down$offered[[1L]]$archive$id
    # On disk, not in the session, which /clear is about to drop.
    expect_identical(length(s$transcript), 0L)
    expect_true(file.exists(pending_path(key)))
    expect_identical(corteza:::bot_archive_state_read(key), character())
    kept <- jsonlite::read_json(pending_path(key), simplifyVector = FALSE)
    expect_identical(kept$key, key)
    expect_identical(kept$ranges[[1L]]$reason, "clear")
    # An error is reported the same way.
    down$take <- "error"
    expect_message(archive(NULL, key, down),
                   "on_archive hook: summary model down (!p:ex, kept to retry)",
                   fixed = TRUE)
    # A new process: a registry without that session, and a host that
    # works. The flush offers the range once, as it was, and keeps it.
    up <- host()
    reg <- new.env(parent = emptyenv())
    expect_identical(corteza::bot_archive_all(reg, on_archive = up$hook,
                                              cfg = cfg), 1L)
    expect_identical(length(up$offered), 1L)
    got <- up$offered[[1L]]$archive
    expect_identical(got$id, first_id)
    expect_identical(got$reason, "clear")
    expect_identical(offered_ids(up)[[1L]], c("$1", "$2"))
    expect_false(file.exists(pending_path(key)))
    expect_identical(corteza:::bot_archive_state_read(key), c("$1", "$2"))
    # And only once.
    expect_identical(corteza::bot_archive_all(reg, on_archive = up$hook,
                                              cfg = cfg), 0L)
    expect_identical(length(up$offered), 1L)
}))

# Only list(archived = TRUE) takes a range. A bare reference, TRUE, or
# a list that says anything else leaves it waiting, so a host that
# returns too early never loses a conversation.
with_state(local({
    for (answer in list(TRUE, "notes/x.md", list(archived = FALSE),
                        list(archived = "yes"), list(archived = c(TRUE, TRUE)),
                        list(handled = TRUE))) {
        h <- host(function(archive) "unused")
        h$hook <- function(chat, cfg, room_id, archive) answer
        out <- suppressMessages(archive(mk("$1"), "!m:ex", h))
        expect_identical(out, list(answer = NULL, filed = 0L, durable = TRUE))
        expect_true(file.exists(pending_path("!m:ex")))
        unlink(pending_path("!m:ex"))
    }
    # A reference that is not one string is dropped, and the range is
    # still taken.
    h <- host()
    h$hook <- function(chat, cfg, room_id, archive) {
        list(archived = TRUE, ref = c("a", "b"))
    }
    out <- archive(mk("$2"), "!m:ex", h)
    expect_identical(out, list(answer = list(archived = TRUE, ref = NULL),
                               filed = 1L, durable = TRUE))
}))

# A range from a flush stays open: what the session says next joins it,
# under the same id, until the host takes it.
with_state(local({
    h <- host(FALSE)
    s <- mk(c("$1", "$2"))
    suppressMessages(archive(s, "!a:ex", h))
    open_id <- h$offered[[1L]]$archive$id
    corteza:::bot_transcript_add(s, "$3", "user", "turn $3")
    suppressMessages(archive(s, "!a:ex", h))
    expect_identical(offered_ids(h)[[2L]], c("$1", "$2", "$3"))
    expect_identical(h$offered[[2L]]$archive$id, open_id)
    kept <- jsonlite::read_json(pending_path("!a:ex"), simplifyVector = FALSE)
    expect_identical(length(kept$ranges), 1L)
    # The /clear that ends the conversation closes it and hands it over.
    h$take <- TRUE
    corteza:::bot_transcript_add(s, "$4", "user", "turn $4")
    out <- archive(s, "!a:ex", h, "clear")
    expect_identical(offered_ids(h)[[3L]], c("$1", "$2", "$3", "$4"))
    expect_identical(h$offered[[3L]]$archive$id, open_id)
    expect_identical(h$offered[[3L]]$archive$reason, "clear")
    expect_identical(out$answer$archived, TRUE)
    expect_false(file.exists(pending_path("!a:ex")))
}))

# A range from /clear is closed: it goes on its own, ahead of what the
# room says after it, and each range is tried whatever became of the
# others.
with_state(local({
    h <- host(FALSE)
    suppressMessages(archive(mk(c("$1", "$2")), "!a:ex", h, "clear"))
    closed_id <- h$offered[[1L]]$archive$id
    # The host now refuses that range alone.
    h$take <- function(archive) !identical(archive$id, closed_id)
    fresh <- mk("$3")
    expect_message(out <- archive(fresh, "!a:ex", h),
                   "kept to retry", fixed = TRUE)
    expect_identical(offered_ids(h)[[2L]], c("$1", "$2"))
    expect_identical(offered_ids(h)[[3L]], "$3")
    expect_identical(h$offered[[3L]]$archive$reason, "flush")
    expect_false(identical(h$offered[[3L]]$archive$id, closed_id))
    expect_identical(out$filed, 1L)
    expect_identical(out$answer$archived, TRUE)
    kept <- jsonlite::read_json(pending_path("!a:ex"), simplifyVector = FALSE)
    expect_identical(length(kept$ranges), 1L)
    expect_identical(kept$ranges[[1L]]$id, closed_id)
    # Taken at last.
    h$take <- TRUE
    expect_identical(archive(NULL, "!a:ex", h)$filed, 1L)
    expect_identical(offered_ids(h)[[4L]], c("$1", "$2"))
    expect_false(file.exists(pending_path("!a:ex")))
}))

# What waits is only what the host has not taken: a known event in the
# same pass stays out of the range.
with_state(local({
    h <- host()
    archive(mk("$1"), "!a:ex", h)
    h$take <- FALSE
    suppressMessages(archive(mk(c("$1", "$fresh")), "!a:ex", h))
    expect_identical(offered_ids(h)[[2L]], "$fresh")
    kept <- jsonlite::read_json(pending_path("!a:ex"), simplifyVector = FALSE)
    ids <- vapply(kept$ranges[[1L]]$entries, function(e) e$event_id, "")
    expect_identical(ids, "$fresh")
}))

# A host that filed a range before corteza could record it sees the
# same id again and replaces its note; a range whose events were all
# recorded is dropped without being offered.
with_state(local({
    h <- host(FALSE)
    suppressMessages(archive(mk(c("$1", "$2")), "!a:ex", h, "clear"))
    corteza:::bot_archive_state_write("!a:ex", c("$1", "$2"))
    h$take <- TRUE
    expect_identical(archive(NULL, "!a:ex", h)$filed, 0L)
    expect_identical(length(h$offered), 1L)
    expect_false(file.exists(pending_path("!a:ex")))
}))

# ---- What the flush leaves alone ----
# Another bot's ranges, in a state folder 2 bots share.
with_state(local({
    other <- list(user_id = "@other:ex")
    h <- host(FALSE)
    suppressMessages(corteza:::bot_archive_session(mk("$1"), "!a:ex", NULL,
                                                   h$hook, other, "clear"))
    path <- pending_path("!a:ex", other)
    expect_true(file.exists(path))
    up <- host()
    reg <- new.env(parent = emptyenv())
    expect_identical(corteza::bot_archive_all(reg, on_archive = up$hook,
                                              cfg = cfg), 0L)
    expect_identical(length(up$offered), 0L)
    expect_true(file.exists(path))
    # This bot has a range of the same room waiting too. A flush offers
    # it once, not once for every file that names the room.
    suppressMessages(archive(mk("$2"), "!a:ex", h, "clear"))
    down <- host(FALSE)
    suppressMessages(corteza::bot_archive_all(reg, on_archive = down$hook,
                                              cfg = cfg))
    expect_identical(offered_ids(down), list("$2"))
    expect_true(file.exists(path))
}))

# A pending file that cannot be read is set aside, never overwritten,
# and the session's own new entries still go.
with_state(local({
    path <- pending_path("!a:ex")
    dir.create(dirname(path), recursive = TRUE)
    writeLines("{not json", path)
    h <- host()
    expect_message(archive(mk("$1"), "!a:ex", h), "set aside", fixed = TRUE)
    expect_true(file.exists(paste0(path, ".unreadable")))
    expect_identical(readLines(paste0(path, ".unreadable")), "{not json")
    expect_identical(offered_ids(h)[[1L]], "$1")
}))

# Nowhere to keep it on disk: the session keeps what the host did not
# take, as it always did, and says it may not go (/clear keeps it).
with_state(local({
    state <- Sys.getenv("CORTEZA_STATE_DIR")
    dir.create(state)
    file.create(file.path(state, "archive"))
    h <- host(FALSE)
    s <- mk(c("$1", "$2"))
    expect_message(out <- archive(s, "!a:ex", h, "clear"),
                   "could not keep a pending archive", fixed = TRUE)
    expect_false(out$durable)
    expect_identical(corteza:::bot_transcript_ids(s$transcript), c("$1", "$2"))
}))

# Only the pending file cannot be written: a folder stands where it is
# written first. A range the host took is safe, and so is one that
# waits in the file from before; only what the session said since, not
# taken, keeps the session.
with_state(local({
    key <- "!w:ex"
    path <- pending_path(key)
    s <- mk("$1", room = key)
    h <- host(FALSE)
    suppressMessages(archive(s, key, h))
    expect_true(file.exists(path))
    expect_identical(length(s$transcript), 0L)
    dir.create(paste0(path, ".tmp"))
    # The open range from that flush, offered again: it is on disk.
    out <- suppressMessages(archive(s, key, h, "clear"))
    expect_true(out$durable)
    expect_identical(offered_ids(h)[[2L]], "$1")
    # Something new, not taken: it is in the session alone.
    corteza:::bot_transcript_add(s, "$2", "user", "turn $2")
    out <- suppressMessages(archive(s, key, h, "clear"))
    expect_false(out$durable)
    expect_true("$2" %in% corteza:::bot_transcript_ids(s$transcript))
    # Taken, though still not written: safe.
    h$take <- TRUE
    out <- suppressMessages(archive(s, key, h, "clear"))
    expect_true(out$durable)
    expect_identical(out$filed, 1L)
    expect_identical(offered_ids(h)[[4L]], c("$1", "$2"))
    expect_identical(corteza:::bot_archive_state_read(key), c("$1", "$2"))
}))

# ---- What waits, and for how long ----
# A range keeps the time it was first kept through every retry, and the
# host is not handed it (the names checked above). The log after a
# flush counts this bot's ranges, their size, and the oldest's age.
with_state(local({
    expect_null(corteza:::bot_archive_waiting_line(
        corteza:::bot_archive_waiting(cfg)))
    key <- "!q:ex"
    path <- pending_path(key)
    s <- mk("$1", room = key)
    h <- host(FALSE)
    suppressMessages(archive(s, key, h))
    first <- jsonlite::read_json(path)$ranges[[1L]]$queued
    expect_true(is.numeric(first))
    expect_true(abs(first - as.numeric(Sys.time())) < 60)
    expect_null(h$offered[[1L]]$archive$queued)
    # Two days back: the open range takes a new entry and keeps its time.
    kept <- jsonlite::read_json(path, simplifyVector = FALSE)
    kept$ranges[[1L]]$queued <- first - 2 * 86400
    jsonlite::write_json(kept, path, auto_unbox = TRUE, null = "null",
                         digits = NA)
    corteza:::bot_transcript_add(s, "$2", "user", "turn $2")
    suppressMessages(archive(s, key, h))
    expect_identical(offered_ids(h)[[2L]], c("$1", "$2"))
    expect_equal(jsonlite::read_json(path)$ranges[[1L]]$queued,
                 first - 2 * 86400)
    # A thread's closed range waits in a file of its own; another bot's
    # is not this bot's to count.
    thread <- corteza:::bot_session_key(key, "$root")
    suppressMessages(archive(mk("$5", room = key), thread, h, "clear"))
    other <- list(user_id = "@other:ex")
    suppressMessages(corteza:::bot_archive_session(mk("$7", room = key), key,
                                                   NULL, h$hook, other))
    w <- corteza:::bot_archive_waiting(cfg)
    expect_identical(w$ranges, 2L)
    expect_equal(w$bytes, sum(file.size(c(path, pending_path(thread)))))
    expect_equal(w$oldest, first - 2 * 86400)
    expect_identical(corteza:::bot_archive_waiting(other)$ranges, 1L)
    line <- corteza:::bot_archive_waiting_line(w, now = first)
    expect_true(grepl("bot_run: 2 range(s) wait for the host in ", line,
                      fixed = TRUE))
    expect_true(grepl("the oldest kept 2 day(s) ago)", line, fixed = TRUE))
    line <- corteza:::bot_archive_waiting_line(w,
        now = first - 2 * 86400 + 3 * 3600)
    expect_true(grepl("the oldest kept 3 hour(s) ago)", line, fixed = TRUE))
    # The host takes them: nothing waits, and the log says nothing more.
    h$take <- TRUE
    reg <- new.env(parent = emptyenv())
    expect_identical(corteza::bot_archive_all(reg, on_archive = h$hook,
                                              cfg = cfg), 2L)
    expect_null(corteza:::bot_archive_waiting_line(
        corteza:::bot_archive_waiting(cfg)))
}))

# ---- The flush signal ----
with_state(local({
    sig <- tempfile("sig-")
    file.create(sig)
    reg <- new.env(parent = emptyenv())
    assign("!r:ex", mk("$h1", room = "!r:ex"), envir = reg)
    # Without a hook the signal is consumed, and the log says why
    # nothing was filed.
    expect_message(n <- corteza:::bot_handle_flush_signal(sig, reg),
                   "no on_archive hook", fixed = TRUE)
    expect_identical(n, 0L)
    expect_false(file.exists(sig))
    expect_identical(length(get("!r:ex", envir = reg)$transcript), 1L)
    # With one, the session goes to the host.
    file.create(sig)
    h <- host()
    expect_message(n <- corteza:::bot_handle_flush_signal(sig, reg, NULL,
                                                          h$hook, cfg),
                   "the host archived 1 range(s)", fixed = TRUE)
    expect_identical(n, 1L)
    expect_false(file.exists(sig))
    expect_identical(h$offered[[1L]]$archive$reason, "flush")
    expect_identical(length(get("!r:ex", envir = reg)$transcript), 0L)
    # A host that takes nothing: the log says what waits for it.
    file.create(sig)
    assign("!r:ex", mk("$h2", room = "!r:ex"), envir = reg)
    expect_message(corteza:::bot_handle_flush_signal(sig, reg, NULL,
                                                     host(FALSE)$hook, cfg),
                   "bot_run: 1 range(s) wait for the host", fixed = TRUE)
}))
