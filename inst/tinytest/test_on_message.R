# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# The on_message hook: a host takes a room's messages for itself. What
# the loop offers it, when, and what it does with each answer. Nothing
# here reaches a homeserver or a model.

# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}
if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}

ns <- asNamespace("corteza")

# ---- The slot ----
# Known at init, like the other two.
local({
    f <- function(...) NULL
    expect_identical(names(corteza:::bot_hooks(list(on_message = f))),
                     "on_message")
})

# ---- One harness ----
# A loopback client, the messages one poll returns, a hook that records
# each call and answers as told, and the side effects counted: turns,
# sends and read receipts. Sessions are built for real (ollama: no API
# key to refuse).
cfg <- list(user_id = "@bot:example", operators = "@human:example",
            model = "qwen3:8b", provider = "ollama")

run_poll <- function(messages, answer = NULL, sessions = NULL, hook = NULL) {
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        assignInNamespace(name, fn, package)
    }
    on.exit({
        for (x in rev(saved)) assignInNamespace(x$name, x$fn, x$package)
    }, add = TRUE)
    chat <- chat.api::chat_loopback()
    if (is.null(sessions)) {
        sessions <- ns$bot_new_session_registry()
    }
    calls <- list()
    if (is.null(hook)) {
        hook <- function(chat, cfg, room_id, message, session, backfill) {
            calls[[length(calls) + 1L]] <<- list(chat = chat, cfg = cfg,
                room_id = room_id, message = message, session = session,
                backfill = backfill)
            answer
        }
    }
    patch("bot_load_config", function(...) cfg)
    patch("bot_chat_client", function(...) chat)
    patch("chat_whoami", function(...) chat.api::chat_identity(cfg$user_id),
          "chat.api")
    patch("chat_poll", function(...) list(first_run = FALSE,
        messages = messages, reactions = list(), invites = list()),
        "chat.api")
    reads <- character()
    patch("chat_mark_read", function(chat, channel, message_id, ...) {
        reads <<- c(reads, message_id)
        invisible(TRUE)
    }, "chat.api")
    patch("bot_should_respond", function(...) TRUE)
    turns <- character()
    patch("bot_run_turn_in_cwd", function(prompt, session) {
        turns <<- c(turns, prompt)
        "ok"
    })
    sent <- character()
    patch("bot_reply_send", function(chat, room, text, ...) {
        sent <<- c(sent, text)
        paste0("$sent", length(sent))
    })
    n <- corteza::bot_poll(sessions = sessions, timeout = 0,
                           hooks = list(on_message = hook),
                           model = "qwen3:8b", provider = "ollama")
    list(n = n, calls = calls, turns = turns, sent = sent, reads = reads,
         sessions = sessions, chat = chat)
}

said <- function(id, body = "list the files", room = "!agent:example",
                 sender = "@human:example", ...) {
    chat.api::chat_message(id = id, channel = room, sender = sender,
                           body = body, ts = Sys.time(), ...)
}

own <- function(id, body, room = "!agent:example", ...) {
    said(id, body = body, room = room, sender = cfg$user_id, self = TRUE,
         ...)
}

# ---- Taken ----
# Offered once, with the documented arguments: the transport's own
# message, raw included, and the session it belongs to. Taken, the
# message makes no turn, no send and no read receipt, stays out of the
# history and the transcript, and is seen.
local({
    msg <- said("$p1", raw = list(content = list(body = "list the files",
        ai.cornball.agent = list(v = 1L, kind = "prompt"))))
    r <- run_poll(list(msg), answer = list(handled = TRUE))
    expect_identical(length(r$calls), 1L)
    call <- r$calls[[1L]]
    expect_identical(call$room_id, "!agent:example")
    expect_identical(call$message, msg)
    expect_identical(call$message$raw$content$ai.cornball.agent$kind, "prompt")
    expect_identical(call$cfg, cfg)
    expect_identical(call$chat, r$chat)
    expect_false(call$backfill)
    s <- get("!agent:example", envir = r$sessions)
    expect_true(identical(call$session, s))
    expect_identical(r$n, 0L)
    expect_identical(r$turns, character())
    expect_identical(r$sent, character())
    expect_identical(r$reads, character())
    expect_identical(length(s$history), 0L)
    expect_identical(length(s$transcript), 0L)
    expect_true("$p1" %in% s$seen_event_ids)
    # Seen, so the same event in a later poll is not offered again.
    again <- run_poll(list(msg), answer = list(handled = TRUE),
                      sessions = r$sessions)
    expect_identical(length(again$calls), 0L)
    expect_identical(again$turns, character())
})

# ---- Refused ----
# Anything but list(handled = TRUE) takes the normal path, exactly as if
# there were no hook: the read receipt, the turn, the reply.
local({
    for (answer in list(NULL, list(handled = FALSE), TRUE,
                        list(handled = NA), "handled")) {
        r <- run_poll(list(said("$p1")), answer = answer)
        expect_identical(length(r$calls), 1L)
        expect_identical(r$turns, "list the files")
        expect_identical(r$sent, "ok")
        expect_identical(r$reads, "$p1")
        s <- get("!agent:example", envir = r$sessions)
        expect_null(s$host_took)
    }
})

# An error is reported and taken as a refusal: a broken hook must not
# silence the bot's own rooms.
local({
    said_msgs <- character()
    r <- withCallingHandlers(
        run_poll(list(said("$p1")), hook = function(...) stop("relay down")),
        message = function(m) {
            said_msgs <<- c(said_msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
        })
    expect_true(any(grepl("on_message hook: relay down", said_msgs,
                          fixed = TRUE)))
    expect_identical(r$turns, "list the files")
    expect_identical(r$sent, "ok")
})

# ---- Not offered ----
# The bot's own messages never reach the hook, and in a room the host
# has not taken they are recorded as before.
local({
    r <- run_poll(list(own("$o1", "Morning briefing", room = "!plain:example")),
                  answer = list(handled = TRUE))
    expect_identical(length(r$calls), 0L)
    s <- get("!plain:example", envir = r$sessions)
    expect_identical(length(s$history), 1L)
})

# Remote control and delegation keep their messages, ahead of the hook.
# Delegation's bare approval from an unverified sender in an encrypted
# room is one: it is swallowed before the hook sees it, which is why a
# host's approvals use "/approve <id>". The same text from a verified
# sender answers no job here, so it reaches the hook; so does the slash
# form.
local({
    orig <- ns$.remote_bot_route
    assignInNamespace(".remote_bot_route", function(m, ...) {
        identical(m$channel, "!remote:example")
    }, "corteza")
    on.exit(assignInNamespace(".remote_bot_route", orig, "corteza"),
            add = TRUE)
    r <- run_poll(list(
        said("$r1", body = "hello", room = "!remote:example"),
        said("$d1", body = "approve abc", encrypted = TRUE,
             sender_verified = FALSE),
        said("$d2", body = "approve abc", encrypted = TRUE,
             sender_verified = TRUE),
        said("$d3", body = "/approve abc", encrypted = TRUE,
             sender_verified = FALSE)),
        answer = list(handled = TRUE))
    expect_identical(vapply(r$calls, function(x) x$message$id, ""),
                     c("$d2", "$d3"))
})

# ---- The host's room ----
# Once the hook takes a message in a room, the bot's own messages there
# are the host's too and recorded nowhere: in the room's timeline and in
# its threads. A room the host has not taken records them as before.
local({
    r <- run_poll(list(said("$p1")), answer = list(handled = TRUE))
    thread_key <- ns$bot_session_key("!agent:example", "$p1")
    r2 <- run_poll(list(
        own("$c1", "[tool] ls (pending)", kind = "notice"),
        own("$c2", "[tool] ls (completed)", kind = "notice", thread = "$p1"),
        own("$c3", "Morning briefing", room = "!plain:example")),
        answer = list(handled = TRUE), sessions = r$sessions)
    expect_identical(length(r2$calls), 0L)
    s <- get("!agent:example", envir = r$sessions)
    expect_identical(length(s$history), 0L)
    expect_identical(length(s$transcript), 0L)
    expect_identical(length(get(thread_key, envir = r$sessions)$history), 0L)
    p <- get("!plain:example", envir = r$sessions)
    expect_identical(length(p$history), 1L)
    # A message the host refuses in its room still takes the normal path.
    r3 <- run_poll(list(said("$p2", body = "and you, bot?")),
                   answer = list(handled = FALSE), sessions = r$sessions)
    expect_identical(r3$turns, "and you, bot?")
})

# ---- A restart's backfill ----
# Offered what backfill would record, with backfill = TRUE: the human
# messages, not the bot's own and not a notice, which backfill skips.
# A taken message stays out of the history and is seen; the bot's own
# messages after it in that room do too. Another room fills as before.
local({
    saved <- list()
    patch <- function(name, fn, package) {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        assignInNamespace(name, fn, package)
    }
    on.exit({
        for (x in rev(saved)) assignInNamespace(x$name, x$fn, x$package)
    }, add = TRUE)
    history <- list(
        said("$h1"),
        own("$h2", "[tool] ls", kind = "notice"),
        own("$h3", "Here they are."),
        said("$h4", body = "hi", room = "!plain:example"))
    patch("chat_channels", function(...) c("!agent:example", "!plain:example"),
          "chat.api")
    patch("chat_history", function(chat, channel, ...) {
        list(messages = Filter(function(m) identical(m$channel, channel),
                               history))
    }, "chat.api")
    patch("chat_whoami", function(...) chat.api::chat_identity(cfg$user_id),
          "chat.api")
    calls <- list()
    hook <- function(chat, cfg, room_id, message, session, backfill) {
        calls[[length(calls) + 1L]] <<- list(id = message$id,
                                             backfill = backfill)
        list(handled = identical(room_id, "!agent:example"))
    }
    sessions <- ns$bot_new_session_registry()
    ns$bot_backfill_sessions(chat.api::chat_loopback(), sessions, cfg,
                             on_message = hook)
    expect_identical(vapply(calls, `[[`, "", "id"), c("$h1", "$h4"))
    expect_true(all(vapply(calls, `[[`, logical(1), "backfill")))
    a <- get("!agent:example", envir = sessions)
    expect_identical(length(a$history), 0L)
    expect_true("$h1" %in% a$seen_event_ids)
    p <- get("!plain:example", envir = sessions)
    expect_identical(length(p$history), 1L)
    # Without a hook the backfill is what it was.
    calls <- list()
    plain <- ns$bot_new_session_registry()
    ns$bot_backfill_sessions(chat.api::chat_loopback(), plain, cfg)
    expect_identical(length(get("!agent:example", envir = plain)$history), 2L)
})

# ---- bot_run_init() hands the hook to the backfill ----
if (requireNamespace("mx.client", quietly = TRUE)) local({
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        assignInNamespace(name, fn, package)
    }
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    dir <- tempfile("corteza-on-message-")
    dir.create(dir)
    Sys.setenv(CORTEZA_STATE_DIR = dir)
    on.exit({
        for (x in rev(saved)) assignInNamespace(x$name, x$fn, x$package)
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old_state)
        unlink(dir, recursive = TRUE)
    }, add = TRUE)
    got <- NULL
    patch("bot_load_config", function(...) cfg)
    patch("bot_chat_client", function(...) chat.api::chat_loopback())
    patch("chat_pending", function(...) list(invites = list()), "chat.api")
    patch("bot_backfill_sessions", function(...) {
        got <<- list(...)
        0L
    })
    patch("bot_update_displayname", function(cfg, ...) cfg)
    patch("bot_recover_jobs", function(...) invisible(NULL))
    f <- function(chat, cfg, room_id, message, session, backfill) NULL
    state <- corteza::bot_run_init(hooks = list(on_message = f))
    expect_identical(got$on_message, f)
    expect_identical(state$opts$hooks$on_message, f)
})
