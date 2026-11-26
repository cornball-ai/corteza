# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# A room's conversation, kept for its monitor (R/room-record.R). Each
# conversation keeps a record beside the room's journal: who said what,
# and the bot's own posts, and whether it reaches back to the
# conversation's start. The monitor judges a call only by a whole record,
# and only the lines of someone who may approve are instructions.
# Nothing here reaches a homeserver or a model.

ns <- asNamespace("corteza")

# The same folder, its separators made alike: on Windows dirname() gives
# forward slashes, and a temporary folder backslashes.
same_dir <- function(a, b) {
    identical(gsub("\\", "/", a, fixed = TRUE), gsub("\\", "/", b, fixed = TRUE))
}

# Each block keeps the room records in a state folder of its own.
with_state <- function() {
    state <- tempfile("room-record-state-")
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = state)
    function() {
        if (is.na(old)) {
            Sys.unsetenv("CORTEZA_STATE_DIR")
        } else {
            Sys.setenv(CORTEZA_STATE_DIR = old)
        }
        unlink(state, recursive = TRUE)
    }
}
room <- "!room:example.invalid"
person <- "@human:example.invalid"
room_cfg <- function() {
    list(user = "bot", user_id = "@bot:example.invalid", operators = person,
         provider = "ollama", model = "fixture")
}
# Replace namespace functions for one block; the returned function puts
# them back.
patching <- function() {
    saved <- list()
    list(set = function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, envir = asNamespace(package)))
        assignInNamespace(name, fn, package)
    }, undo = function() {
        for (x in rev(saved)) {
            assignInNamespace(x$name, x$fn, x$package)
        }
    })
}
# A session holding a conversation of `key`, its record attached.
session_for <- function(cfg, key = room, start = "conversation") {
    s <- new.env()
    s$conversation_id <- ns$job_new_id()
    ns$bot_record_attach(s, cfg, key, start)
    s
}
msg <- function(id, body, sender = person, ts = 1) {
    list(id = id, sender = sender, body = body, ts = ts)
}
# What the person said, as the room ledgers it.
says <- function(s, id, body, sender = person, ...) {
    ns$bot_transcript_add(s, id, "user", paste0("[", sender, "] ", body),
                          event = msg(id, body, sender), ...)
}
lines_of <- function(s) {
    ns$bot_record_read(s$review_record$path)$lines
}
types_of <- function(s) {
    vapply(lines_of(s), function(x) x$type, "")
}
judged <- function(s, approvers = person, ...) {
    ns$bot_record_conversation(s$review_record$path, approvers, ...)
}
# Element `i` of a list a fake filled, or an empty list when it holds
# fewer: a call that never came fails its assertion, not the file.
at <- function(x, i) {
    if (length(x) >= i) {
        x[[i]]
    } else {
        list()
    }
}

# ---- What a line holds, and how the monitor reads it --------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    s <- session_for(cfg)
    path <- s$review_record$path
    # In the room's journal folder, a file for each conversation, and
    # nothing written until there is something to write.
    expect_true(same_dir(dirname(path), ns$bot_journal_dir(cfg, room)))
    expect_identical(basename(path),
                     paste0("conversation-", s$conversation_id, ".jsonl"))
    expect_false(file.exists(path))
    says(s, "$1", "please tidy R/")
    ns$bot_transcript_add(s, "$2", "assistant", "Shall I remove R/old too?")
    ns$bot_transcript_add(s, "$3", "user", "[human] yes",
                          event = msg("$3", "yes", ts = 3),
                          note = "[Attachment receipts] {\"name\":\"plan.png\"}")
    expect_identical(types_of(s), c("start", "event", "event", "event"))
    held <- lines_of(s)
    expect_identical(held[[1L]]$start, "conversation")
    expect_identical(held[[1L]]$conversation, s$conversation_id)
    # The sender and the words as sent come from the event, not from
    # the body the model is given.
    expect_identical(held[[2L]]$sender, person)
    expect_identical(held[[2L]]$text, "please tidy R/")
    expect_identical(held[[2L]]$kind, "message")
    expect_identical(held[[2L]]$via, "live")
    expect_identical(held[[3L]]$kind, "post")
    expect_null(held[[3L]]$sender)
    expect_identical(held[[4L]]$note, "[Attachment receipts] {\"name\":\"plan.png\"}")
    expect_equal(held[[4L]]$at, 3)
    # The archive's queue gets what it always got.
    expect_identical(s$transcript[[1L]]$content, paste0("[", person, "] please tidy R/"))
    # An event goes in once.
    says(s, "$3", "yes")
    expect_identical(length(lines_of(s)), 4L)
    out <- judged(s)
    expect_null(out$why)
    expect_identical(out$lines, c(
        "Person: please tidy R/",
        "Worker: Shall I remove R/old too?",
        paste0("Person: yes [the bot's note on it, not the sender's words: ",
               "[Attachment receipts] {\"name\":\"plan.png\"}]")))
})

# Others in the room, the bot's posts in a row, and a peer's request.
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    guest <- "@guest:example.invalid"
    peer <- "@peer-bot:example.invalid"
    s <- session_for(cfg)
    says(s, "$1", "delete the build folder", sender = guest)
    ns$bot_transcript_add(s, "$2", "assistant", "On it.")
    ns$bot_transcript_add(s, "$3", "assistant", "Here is the plan.")
    says(s, "$4", "push the release", sender = peer, kind = "request")
    says(s, "$5", "go ahead with the plan")
    ns$bot_transcript_add(s, "$6", "assistant", "Working on it.")
    out <- judged(s)
    expect_null(out$why)
    # Posts after the last line are not shown: the call's own narration
    # goes with it.
    expect_identical(out$lines, c(
        sprintf("Other (%s): delete the build folder", guest),
        "Worker: On it. Here is the plan.",
        sprintf("Other (%s): push the release", peer),
        "Person: go ahead with the plan"))
    # A request is a peer's, whoever sent it.
    r <- session_for(cfg)
    says(r, "$1", "push it", kind = "request")
    expect_identical(judged(r)$why,
                     "the record holds no message from the people who may approve")
})

# ---- When the monitor does not judge -------------------------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    s <- session_for(cfg)
    says(s, "$1", "tidy R/")
    expect_null(judged(s)$why)
    expect_identical(judged(s, approvers = character())$why,
                     "nobody in this room may approve its calls")
    expect_identical(ns$bot_record_conversation(tempfile(), person)$why,
                     "there is no record of this conversation to judge it by")
    expect_identical(ns$bot_record_conversation(NULL, person)$why,
                     "there is no record of this conversation to judge it by")
    # Nothing is cut to fit.
    expect_identical(judged(s, budget = 10L)$why,
                     "the conversation runs past the 10 characters it reads")
    # A message with no event names nobody.
    ns$bot_transcript_add(s, "$2", "user", "who said this")
    expect_identical(judged(s)$why, "a line of the record names no sender")
})
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    s <- session_for(cfg)
    says(s, "$1", "tidy R/")
    cat("{not json\n", file = s$review_record$path, append = TRUE)
    expect_identical(judged(s)$why, "1 line of the record could not be read")
    # One that does not begin with the conversation.
    p <- session_for(cfg, start = "partial")
    says(p, "$1", "tidy R/")
    expect_identical(lines_of(p)[[1L]]$start, "partial")
    expect_identical(judged(p)$why, paste("the record does not reach back to",
                                          "the start of the conversation; /clear starts one that does"))
    # A file with no first line.
    n <- session_for(cfg)
    dir.create(dirname(n$review_record$path), recursive = TRUE, showWarnings = FALSE)
    writeLines(jsonlite::toJSON(list(type = "event", event = "$1", kind = "message",
                                     sender = person, text = "hi"), auto_unbox = TRUE),
               n$review_record$path)
    expect_identical(judged(n)$why, "the record does not say how it begins")
})

# A break: a gap line, or, before the first line, a `partial` start.
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    s <- session_for(cfg)
    says(s, "$1", "tidy R/")
    ns$bot_record_break(s, "the thread went on from an archive")
    expect_identical(types_of(s), c("start", "event", "gap"))
    expect_identical(judged(s)$why,
                     "the record has a break: the thread went on from an archive")
    early <- session_for(cfg)
    ns$bot_record_break(early, "the thread went on from an archive")
    says(early, "$1", "tidy R/")
    expect_identical(types_of(early), c("start", "event"))
    expect_identical(lines_of(early)[[1L]]$start, "partial")
})

# A line that could not be written: the record is not judged by until
# the next one puts a gap ahead of it.
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    s <- session_for(cfg)
    says(s, "$1", "tidy R/")
    p <- patching()
    p$set("bot_record_append", function(path, lines) stop("disk full"))
    says(s, "$2", "stop, leave R/old")
    p$undo()
    expect_identical(types_of(s), c("start", "event"))
    expect_identical(judged(s)$why,
                     "a line of this conversation could not be written to its record")
    says(s, "$3", "go on")
    expect_identical(types_of(s), c("start", "event", "gap", "event"))
    expect_identical(judged(s)$why,
                     "the record has a break: a line could not be written")
})

# ---- A session's record ------------------------------------------------------
# A room's sessions are built for real (ollama: no key to refuse), on a
# loopback client, working in `root`.
have_chat <- requireNamespace("chat.api", quietly = TRUE)
patch_room <- function(lo, root) {
    p <- patching()
    p$set("bot_chat_client", function(...) lo)
    p$set("bot_room_cwd", function(...) root)
    p$undo
}
if (have_chat) local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    root <- tempfile("room-root-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    lo <- chat.api::chat_loopback()
    restore <- patch_room(lo, root)
    on.exit(restore(), add = TRUE)
    cfg <- room_cfg()
    reg <- ns$bot_new_session_registry()
    # A new conversation in a session that holds nothing: whole.
    s <- ns$bot_get_or_create_session(reg, room, cfg)
    expect_identical(s$review_record$start, "conversation")
    expect_false(s$review_record$started)
    # Built again before a line was written: the conversation is on
    # record and its record is not, so it begins partial.
    again <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    expect_identical(again$review_record$start, "partial")
    # Written, a restart goes on with it.
    says(s, "$1", "tidy R/")
    third <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    expect_true(third$review_record$started)
    expect_identical(third$review_record$ids, "$1")
    expect_identical(third$review_record$last, "$1")
    # /clear begins a whole one, and the old one keeps its lines.
    fresh <- ns$bot_reset_session(reg, room, cfg, "$ack",
                                  "Cleared. Starting a fresh session.",
                                  ns$bot_conversation_begin(cfg, room, room,
                                                            msg("$c", "/clear")))
    expect_false(identical(fresh$review_record$path, s$review_record$path))
    expect_identical(types_of(fresh), c("start", "event"))
    expect_identical(lines_of(fresh)[[1L]]$start, "conversation")
    expect_identical(lines_of(fresh)[[2L]]$kind, "post")
    expect_identical(length(lines_of(s)), 2L)
    # A thread keeps its own, in its own journal folder.
    key <- ns$bot_session_key(room, "$t")
    th <- ns$bot_get_or_create_session(reg, key, cfg, room_id = room)
    expect_true(same_dir(dirname(th$review_record$path),
                         ns$bot_journal_dir(cfg, key)))
    # A session that holds a history from before begins partial: a room's
    # context restored from its journal.
    p <- patching()
    p$set(".context_room_restore", function(session, dir, cfg, ...) {
        session$history <- list(list(role = "user", content = "from before"))
        invisible(TRUE)
    })
    restored <- tryCatch(ns$bot_get_or_create_session(reg, "!other:example.invalid",
                                                      cfg), finally = p$undo())
    expect_identical(restored$review_record$start, "partial")
})

# A thread that goes on from its archive has a break.
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    s <- session_for(cfg, key = ns$bot_session_key(room, "$t"))
    says(s, "$1", "tidy R/")
    p <- patching()
    on.exit(p$undo(), add = TRUE)
    p$set("bot_thread_context", function(...) list(vault = "raw/x.md", kind = "transcript"))
    p$set("bot_archive_excerpt", function(...) "an earlier conversation")
    expect_true(ns$bot_rehydrate_session(s, NULL, room, "$t", function(...) NULL))
    expect_identical(types_of(s), c("start", "event", "gap"))
})

# ---- /clear through the bot's loop -------------------------------------------
if (have_chat) local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    root <- tempfile("room-root-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    lo <- chat.api::chat_loopback()
    restore <- patch_room(lo, root)
    on.exit(restore(), add = TRUE)
    cfg <- room_cfg()
    sessions <- ns$bot_new_session_registry()
    main <- ns$bot_get_or_create_session(sessions, room, cfg)
    id <- chat.api::chat_send(lo, room, "/clear", identity = list(name = person))
    clear <- chat.api::chat_history(lo, room, limit = 10L)$messages[[1L]]
    p <- patching()
    p$set("bot_load_config", function(...) cfg)
    p$set("chat_whoami", function(...) {
        chat.api::chat_identity("@bot:example.invalid")
    }, "chat.api")
    p$set("chat_poll", function(...) {
        list(first_run = FALSE, messages = list(clear), reactions = list(),
             invites = list())
    }, "chat.api")
    p$set("bot_should_respond", function(...) TRUE)
    p$set("bot_run_turn_in_cwd", function(...) stop("the poll reached a model"))
    p$set("bot_update_displayname", function(cfg, ...) cfg)
    p$set("bot_reply_send", function(chat, room, text, ...) {
        chat.api::chat_send(lo, room, text)
    })
    polled <- tryCatch(corteza::bot_poll(sessions = sessions, timeout = 0,
                                         model = "fixture", provider = "ollama"),
                       finally = p$undo())
    expect_equal(polled, 1L)
    # The /clear is the old conversation's last line, from its sender.
    old <- ns$bot_record_read(main$review_record$path)$lines
    expect_identical(old[[2L]]$text, "/clear")
    expect_identical(old[[2L]]$sender, person)
    after <- get(room, envir = sessions)
    new <- lines_of(after)
    expect_identical(new[[1L]]$start, "conversation")
    expect_identical(new[[2L]]$kind, "post")
    expect_identical(new[[2L]]$text, "Cleared. Starting a fresh session.")
})

# ---- A restart's backfill ----------------------------------------------------
backfill <- function(lo, cfg, limit = 100L) {
    reg <- ns$bot_new_session_registry()
    ns$bot_backfill_sessions(lo, reg, cfg, model = "fixture", provider = "ollama",
                             limit = limit)
    get(room, envir = reg)
}
send <- function(lo, body) {
    chat.api::chat_send(lo, room, body, identity = list(name = person))
}
with_room <- function(code) {
    if (!have_chat) return(invisible(NULL))
    done <- with_state()
    on.exit(done(), add = TRUE)
    root <- tempfile("room-root-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    lo <- chat.api::chat_loopback()
    restore <- patch_room(lo, root)
    on.exit(restore(), add = TRUE)
    code(lo, room_cfg())
}
# A conversation the bot first meets in a backfill may reach back past
# the window: its record begins partial.
with_room(function(lo, cfg) {
    send(lo, "an old instruction")
    send(lo, "more")
    s <- backfill(lo, cfg)
    held <- lines_of(s)
    expect_identical(held[[1L]]$start, "partial")
    expect_identical(vapply(held[-1L], function(x) x$via, ""),
                     c("backfill", "backfill"))
    expect_identical(vapply(held[-1L], function(x) x$text, ""),
                     c("an old instruction", "more"))
    expect_identical(held[[2L]]$sender, person)
})
# A restart whose window reaches the last event on record adds only what
# came after it, and nothing it skipped before it.
with_room(function(lo, cfg) {
    s <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    a <- send(lo, "tidy R/")
    says(s, a, "tidy R/")
    send(lo, "a message the bot did not record")
    b <- send(lo, "and the docs")
    says(s, b, "and the docs")
    send(lo, "stop: leave the docs")
    r <- backfill(lo, cfg)
    expect_identical(types_of(r), c("start", "event", "event", "event"))
    expect_identical(lines_of(r)[[4L]]$via, "backfill")
    out <- judged(r)
    expect_null(out$why)
    expect_identical(out$lines, c("Person: tidy R/", "Person: and the docs",
                                  "Person: stop: leave the docs"))
})
# One whose window starts after it may have missed what came between.
with_room(function(lo, cfg) {
    s <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    a <- send(lo, "tidy R/")
    says(s, a, "tidy R/")
    send(lo, "one")
    send(lo, "two")
    send(lo, "three")
    r <- backfill(lo, cfg, limit = 2L)
    expect_identical(types_of(r), c("start", "event", "gap", "event", "event"))
    expect_identical(judged(r)$why, paste("the record has a break: the bot was not",
                                          "running, and the backfill did not reach back to the last event on record"))
})

# A restart checks each record from before it when a session takes it
# (Codex, #286): one whose room it could not read, or whose last event is
# not in its room's window, gets a gap first, and the monitor does not
# judge by it. Messages the backfill says, kept.
backfill_saying <- function(lo, reg, cfg, limit = 100L) {
    said <- character()
    n <- withCallingHandlers(tryCatch(
        ns$bot_backfill_sessions(lo, reg, cfg, model = "fixture",
                                 provider = "ollama", limit = limit),
        error = function(e) conditionMessage(e)),
        message = function(m) {
        said <<- c(said, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    list(n = n, said = said)
}
unread_room <- paste("the record has a break: the bot restarted, and could",
                     "not read the room's history")
# A thread quiet for longer than the window: nothing of it is in the
# window, which may have missed what was said in it.
with_room(function(lo, cfg) {
    reg <- ns$bot_new_session_registry()
    root_id <- send(lo, "a thread's root")
    tkey <- ns$bot_session_key(room, root_id)
    t <- ns$bot_get_or_create_session(reg, tkey, cfg, room_id = room)
    a <- chat.api::chat_send(lo, room, "in the thread",
                             identity = list(name = person), thread = root_id)
    says(t, a, "in the thread")
    for (i in 1:3) {
        send(lo, paste("main", i))
    }
    restarted <- ns$bot_new_session_registry()
    backfill_saying(lo, restarted, cfg, limit = 2L)
    expect_false(exists(tkey, envir = restarted, inherits = FALSE))
    later <- ns$bot_get_or_create_session(restarted, tkey, cfg, room_id = room)
    expect_identical(types_of(later), c("start", "event", "gap"))
    expect_true(grepl("did not reach back", judged(later)$why, fixed = TRUE))
})
# The rooms could not be listed. A registry no restart made checks
# nothing; the restart's gives the record a gap, once.
with_room(function(lo, cfg) {
    s <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    a <- send(lo, "tidy R/")
    says(s, a, "tidy R/")
    again <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room,
                                          cfg)
    expect_identical(types_of(again), c("start", "event"))
    p <- patching()
    on.exit(p$undo(), add = TRUE)
    p$set("chat_channels", function(...) stop("fixture: no rooms"), "chat.api")
    restarted <- ns$bot_new_session_registry()
    out <- backfill_saying(lo, restarted, cfg)
    expect_equal(out$n, 0L)
    expect_true(any(grepl("could not list the rooms", out$said, fixed = TRUE)))
    r <- ns$bot_get_or_create_session(restarted, room, cfg)
    expect_identical(types_of(r), c("start", "event", "gap"))
    expect_identical(judged(r)$why, unread_room)
    rm(list = room, envir = restarted)
    r <- ns$bot_get_or_create_session(restarted, room, cfg)
    expect_identical(types_of(r), c("start", "event", "gap"))
})
# The room's history could not be read.
with_room(function(lo, cfg) {
    s <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    a <- send(lo, "tidy R/")
    says(s, a, "tidy R/")
    p <- patching()
    on.exit(p$undo(), add = TRUE)
    p$set("chat_history", function(...) stop("fixture: unreadable"),
          "chat.api")
    restarted <- ns$bot_new_session_registry()
    out <- backfill_saying(lo, restarted, cfg)
    expect_true(any(grepl(paste("could not read", room), out$said,
                          fixed = TRUE)))
    r <- ns$bot_get_or_create_session(restarted, room, cfg)
    expect_identical(types_of(r), c("start", "event", "gap"))
    expect_identical(judged(r)$why, unread_room)
})
# The backfill failed partway through a room: what it took says so, and
# the next room is still backfilled.
with_room(function(lo, cfg) {
    s <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    a <- send(lo, "tidy R/")
    says(s, a, "tidy R/")
    send(lo, "and the docs")
    other <- "!other:example.invalid"
    chat.api::chat_send(lo, other, "elsewhere", identity = list(name = person))
    p <- patching()
    on.exit(p$undo(), add = TRUE)
    p$set("bot_maybe_rehydrate", function(session, chat, rid, ...) {
        if (identical(rid, room)) {
            stop("fixture: rehydrate failed")
        }
        invisible(FALSE)
    })
    restarted <- ns$bot_new_session_registry()
    out <- backfill_saying(lo, restarted, cfg)
    expect_equal(out$n, 1L)
    expect_true(any(grepl(paste("failed partway through", room), out$said,
                          fixed = TRUE)))
    r <- get(room, envir = restarted)
    expect_identical(types_of(r), c("start", "event", "gap"))
    expect_identical(judged(r)$why, paste("the record has a break: the backfill",
                                          "failed partway through the room's history"))
    expect_true(exists(other, envir = restarted, inherits = FALSE))
})
# bot_run_init() makes its registry a restart's before it reads anything:
# with no config it reads nothing, and its records get the gap.
if (requireNamespace("mx.client", quietly = TRUE)) with_room(function(lo, cfg) {
    s <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    a <- send(lo, "tidy R/")
    says(s, a, "tidy R/")
    old <- Sys.getenv("CORTEZA_MATRIX_CONFIG", unset = NA)
    Sys.setenv(CORTEZA_MATRIX_CONFIG = file.path(tempdir(), "absent-room.json"))
    on.exit(if (is.na(old)) {
        Sys.unsetenv("CORTEZA_MATRIX_CONFIG")
    } else {
        Sys.setenv(CORTEZA_MATRIX_CONFIG = old)
    }, add = TRUE)
    state <- suppressMessages(corteza::bot_run_init())
    r <- ns$bot_get_or_create_session(state$sessions, room, cfg)
    expect_identical(types_of(r), c("start", "event", "gap"))
    expect_identical(judged(r)$why, unread_room)
})

# ---- The room's own calls ----------------------------------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    proj <- tempfile("room-proj-")
    dir.create(file.path(proj, ".git"), recursive = TRUE)
    on.exit(unlink(proj, recursive = TRUE), add = TRUE)
    cfg <- c(room_cfg(), list(auto_approve_asks = TRUE))
    approvers <- person
    verdict <- "approve"
    asked <- list()
    prompted <- list()
    p <- patching()
    on.exit(p$undo(), add = TRUE)
    p$set("bot_room_approvers", function(cfg, room_id) approvers)
    p$set("bot_load_config", function(...) cfg)
    p$set("job_monitor_ask_wait", function(session, q, ...) {
        asked[[length(asked) + 1L]] <<- q
        list(verdict = verdict, reason = "fine", q = q)
    })
    p$set("bot_reaction_approval", function(cfg, call, decision, room_id) {
        prompted[[length(prompted) + 1L]] <<- decision
        FALSE
    })
    s <- session_for(cfg)
    s$cwd <- proj
    says(s, "$1", "tidy R/")
    ns$bot_supervise_session(s, cfg, room)
    call <- list(tool = "bash", args = list(command = "git add R/x.R"),
                 model_context = list(assistant_text = "Staging the tidy file."))
    decision <- list(approval = "ask", reason = "config: bash requires approval")
    r <- s$auto_gate(call, decision)
    expect_identical(r$action, "proceed")
    q <- at(asked, 1L)
    expect_identical(q$conversation, "Person: tidy R/")
    expect_true(isTRUE(q$room))
    expect_identical(q$scope, "review:t000001")
    expect_identical(q$narration, "Staging the tidy file.")
    expect_null(q$goal)
    # Each request is a scope of its own, and says how the earlier went.
    s$auto_gate(call, decision)
    expect_identical(at(asked, 2L)$scope, "review:t000002")
    expect_identical(at(asked, 2L)$earlier,
                     "bash: git add R/x.R -> approved (by you)")
    # Read when the call comes: what was said since is in it.
    says(s, "$2", "and stop after that")
    s$auto_gate(call, decision)
    expect_identical(at(asked, 3L)$conversation,
                     c("Person: tidy R/", "Person: and stop after that"))
    # Passed on, the call goes to the person with the monitor's reason.
    verdict <- "escalate"
    r <- s$auto_gate(call, decision)
    expect_identical(r$action, "declined")
    expect_identical(at(prompted, 1L)$reason, "the monitor passed this to you: fine")
    # Nobody may approve: the monitor is not asked.
    verdict <- "approve"
    approvers <- character()
    n <- length(asked)
    r <- s$auto_gate(call, decision)
    expect_identical(length(asked), n)
    expect_identical(r$action, "declined")
    expect_identical(at(prompted, 2L)$reason, paste(
        "the monitor was not asked (nobody in this room may approve its calls);",
        "policy said: config: bash requires approval"))
    # Nor on a record it cannot judge by.
    approvers <- person
    ns$bot_record_break(s, "lines are missing")
    s$auto_gate(call, decision)
    expect_identical(length(asked), n)
    expect_true(isTRUE(startsWith(as.character(at(prompted, 3L)$reason),
                                  "the monitor was not asked (the record has a break")))
})

# ---- A job's calls -----------------------------------------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- c(room_cfg(), list(auto_approve_asks = TRUE))
    asked <- list()
    persons <- list()
    p <- patching()
    on.exit(p$undo(), add = TRUE)
    p$set("bot_room_approvers", function(cfg, room_id) person)
    p$set("bot_job_auto_answer", function(...) FALSE)
    p$set("job_monitor_ask", function(session, q) {
        asked[[length(asked) + 1L]] <<- q
        invisible(TRUE)
    })
    p$set("bot_job_ask_person", function(chat, cfg, s, job, req) {
        persons[[length(persons) + 1L]] <<- req
        invisible(NULL)
    })
    s <- session_for(cfg)
    says(s, "$1", "tidy R/")
    req <- list(id = "q1", tool = "bash", args = list(command = "rm -r R/old"),
                reason = "config: bash requires approval", route = "monitor")
    job <- list(id = "20261119T000000-0000abcd",
                task = "Remove R/old and tidy the rest.",
                origin = list(room = room, session_key = room,
                              conversation = s$conversation_id))
    ns$bot_job_approval(NULL, cfg, s, list(job = job, request = req))
    q <- at(asked, 1L)
    expect_identical(q$scope, "review:20261119T000000-0000abcd:q1")
    expect_identical(q$conversation, "Person: tidy R/")
    expect_identical(q$goal, job$task)
    expect_true(isTRUE(q$room))
    # After /clear the job reads its own conversation: the new one's
    # lines are not its instructions.
    later <- session_for(cfg)
    says(later, "$9", "do something else")
    ns$bot_job_approval(NULL, cfg, later, list(job = job, request = req))
    expect_identical(at(asked, 2L)$conversation, "Person: tidy R/")
    # A hand-off is judged by the asking room's.
    away <- "!away:example.invalid"
    handed <- list(id = "20261119T000000-0000abce", task = "x",
                   origin = list(room = away, session_key = away,
                                 conversation = later$conversation_id,
                                 from = list(session_key = room, room = room,
                                             conversation = s$conversation_id)))
    ns$bot_job_approval(NULL, cfg, s, list(job = handed, request = req))
    expect_identical(at(asked, 3L)$conversation, "Person: tidy R/")
    # A job that does not say which conversation asked goes to the person.
    old <- list(id = "20261119T000000-0000abcf", task = "x",
                origin = list(room = room, session_key = room))
    ns$bot_job_approval(NULL, cfg, s, list(job = old, request = req))
    expect_identical(length(asked), 3L)
    expect_identical(at(persons, 1L)$reason, paste(
        "the monitor was not asked (the job does not say which conversation",
        "asked for it); policy said: config: bash requires approval"))
})

# ---- What the monitor is sent ------------------------------------------------
local({
    q <- list(request_id = "r1", conversation = c("Person: tidy R/", "Worker: ok"),
              room = TRUE, goal = "Remove R/old.", tool = "bash",
              args = list(command = "rm -r R/old"), reason = "ask")
    text <- ns$job_monitor_question(q)
    expect_true(grepl("THE CONVERSATION IN THE ROOM SO FAR", text, fixed = TRUE))
    expect_true(grepl("- Person: tidy R/\n- Worker: ok", text, fixed = TRUE))
    expect_true(grepl(paste0("DELEGATED IT WROTE IT, NOT THE PERSON:\n",
                             "Remove R/old."), text, fixed = TRUE))
    # The terminal's reviewer is sent what it was.
    q$room <- NULL
    q$goal <- NULL
    text <- ns$job_monitor_question(q)
    expect_true(grepl("THE PERSON'S FIRST LINE GAVE THE TASK", text, fixed = TRUE))
    expect_false(grepl("IN THE ROOM", text, fixed = TRUE))
})

# ---- Retention takes a record with its journal --------------------------------
local({
    j <- tempfile("journal-")
    dir.create(j)
    on.exit(unlink(j, recursive = TRUE), add = TRUE)
    file.create(file.path(j, c("live.json", "conversation-20261119T000000-0000abcd.jsonl",
                               "conversation-notes.txt")))
    got <- basename(ns$retention_journal_files(j))
    expect_true("conversation-20261119T000000-0000abcd.jsonl" %in% got)
    expect_false("conversation-notes.txt" %in% got)
})

# ---- An attachment's receipt, kept apart -------------------------------------
if (have_chat) local({
    p <- patching()
    on.exit(p$undo(), add = TRUE)
    p$set("chat_capabilities", function(...) list(attachments = FALSE), "chat.api")
    m <- list(id = "$file", sender = person,
              attachments = list(list(name = "notes.pdf", mime = "application/pdf",
                                      bytes = 10)))
    media <- ns$bot_message_media(chat.api::chat_loopback(), m, "read this",
                                  "anthropic")
    expect_true(startsWith(media$receipt, "[Attachment receipts;"))
    expect_identical(media$text, paste("read this", media$receipt, sep = "\n\n"))
    expect_null(ns$bot_message_media(NULL, list(id = "$t"), "plain", "anthropic")$receipt)
})
