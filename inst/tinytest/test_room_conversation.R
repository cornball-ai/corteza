# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# A room's conversations (R/room-conversation.R; WP20a, rules 1, 4 and
# 9). A room or a thread keeps a record of the conversation it holds: an
# id, and the event that started it. /clear replaces it, through the
# bot's own loop; a restart goes on with it and seeds nothing from
# before it. Nothing here reaches a homeserver or a model.

ns <- asNamespace("corteza")

# Each block keeps the room records in a state folder of its own.
with_state <- function() {
    state <- tempfile("room-conversation-state-")
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
room_cfg <- function() {
    list(user = "bot", user_id = "@bot:example.invalid",
         operators = "@human:example.invalid", provider = "ollama",
         model = "fixture")
}
# Messages it caught, and what it says.
messages_of <- function(expr) {
    said <- character()
    value <- withCallingHandlers(expr, message = function(m) {
        said <<- c(said, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    list(value = value, said = said)
}

# ---- The record --------------------------------------------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- room_cfg()
    other <- utils::modifyList(cfg, list(user_id = "@other:example.invalid"))
    # 2 bots in 1 room keep 2 files, each named as the room's journal is
    # (the vignette's "Session records").
    expect_false(identical(ns$bot_room_state_path(cfg, room),
                           ns$bot_room_state_path(other, room)))
    expect_identical(basename(ns$bot_room_state_path(cfg, room)),
                     sub("[.]journal$", ".json",
                         basename(ns$bot_journal_dir(cfg, room))))
    first <- ns$bot_conversation_open(cfg, room, room)
    expect_true(file.exists(ns$bot_room_state_path(cfg, room)))
    expect_null(first$since)
    # Read back, not made again: a restart goes on with it.
    expect_identical(ns$bot_conversation_open(cfg, room, room)$id, first$id)
    expect_null(ns$bot_conversation_get(other, room, room))
    # A thread is a conversation of its own, kept in its room's file.
    thread <- ns$bot_session_key(room, "$thread")
    second <- ns$bot_conversation_open(cfg, thread, room)
    expect_false(identical(second$id, first$id))
    expect_identical(ns$bot_conversation_get(cfg, room, room)$id, first$id)
    expect_identical(ns$bot_conversation_get(cfg, thread, room)$id, second$id)
    expect_identical(length(list.files(dirname(
        ns$bot_room_state_path(cfg, room)))), 1L)

    # A /clear's record: the message that began it, saved before anything
    # acts on it, and then the ids of the bot's answer.
    start <- list(id = "$clear", ts = as.POSIXct(1763560000.5,
                                                 origin = "1970-01-01"))
    record <- ns$bot_conversation_begin(cfg, room, room, start)
    expect_identical(record$since, "$clear")
    expect_equal(record$at, 1763560000.5)
    expect_null(record$ack)
    got <- ns$bot_conversation_get(cfg, room, room)
    expect_identical(got$id, record$id)
    expect_null(got$ack)
    record <- ns$bot_conversation_acked(cfg, room, room, record,
                                        c("$ack", NA, ""))
    expect_identical(record$ack, "$ack")
    got <- ns$bot_conversation_get(cfg, room, room)
    expect_identical(got$id, record$id)
    expect_identical(got$since, "$clear")
    expect_identical(got$ack, "$ack")
    expect_equal(got$at, 1763560000.5)
    # An answer that was not sent adds nothing.
    expect_identical(ns$bot_conversation_acked(cfg, room, room, record, NULL),
                     record)
    expect_identical(ns$bot_conversation_get(cfg, thread, room)$id, second$id)
    # Made with a session: no starting event, nothing left out.
    bare <- ns$bot_conversation_new()
    expect_identical(names(bare), "id")

    # An unreadable file is set aside, not written over: it holds every
    # thread's record.
    path <- ns$bot_room_state_path(cfg, room)
    writeLines("{ not json", path)
    out <- messages_of(ns$bot_conversation_open(cfg, room, room))
    expect_true(any(grepl("set aside", out$said, fixed = TRUE)))
    expect_false(identical(out$value$id, record$id))
    aside <- list.files(dirname(path), pattern = "unreadable", full.names = TRUE)
    expect_true(length(aside) == 1L &&
                identical(readLines(aside), "{ not json"))
    # A record that could not be saved is said so: a file stands where
    # the folder goes.
    blocked <- tempfile("blocked-state-")
    dir.create(blocked)
    writeLines("a file", file.path(blocked, "rooms"))
    Sys.setenv(CORTEZA_STATE_DIR = blocked)
    out <- messages_of(ns$bot_conversation_save(cfg, room, room, bare))
    expect_false(out$value)
    expect_true(any(grepl("could not save", out$said, fixed = TRUE)))
    # A /clear's record that could not be saved begins nothing.
    out <- messages_of(ns$bot_conversation_begin(cfg, room, room, start))
    expect_null(out$value)
    # The answer's ids stay in hand when they cannot be saved.
    out <- messages_of(ns$bot_conversation_acked(cfg, room, room, bare, "$a"))
    expect_identical(out$value$ack, "$a")
    expect_true(any(grepl("could not save", out$said, fixed = TRUE)))
    unlink(blocked, recursive = TRUE)
})

# ---- What a restart's backfill leaves out ------------------------------------
local({
    s <- new.env()
    ids <- c("$a", "$clear", "$ack", "$b")
    ts <- c(1, 2, 3, 4)
    before <- function(record) {
        s$conversation_record <- record
        vapply(seq_along(ids), function(i) {
            ns$bot_conversation_before(s, ids, i, ts[[i]])
        }, logical(1))
    }
    # A conversation made with its session leaves nothing out.
    expect_identical(before(list(id = "c1")), rep(FALSE, 4L))
    # A /clear's: everything up to the /clear, and the answer to it.
    expect_identical(before(list(id = "c2", since = "$clear", at = 2,
                                 ack = "$ack")), c(TRUE, TRUE, TRUE, FALSE))
    # Its starting event not in the window: what is older than it.
    expect_identical(before(list(id = "c3", since = "$gone", at = 2.5)),
                     c(TRUE, TRUE, FALSE, FALSE))
    # A message with no time is kept.
    expect_false(ns$bot_conversation_before(s, ids, 1L, NA))
    expect_false(ns$bot_conversation_before(s, ids, 1L, NULL))
    # Neither the event nor a time: nothing is left out.
    expect_identical(before(list(id = "c4", since = "$gone")), rep(FALSE, 4L))
})

if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}

# A room's sessions are built for real (ollama: no key to refuse), on a
# loopback client, working in `root`.
patch_room <- function(lo, root) {
    names <- c("bot_chat_client", "bot_room_cwd")
    saved <- lapply(names, function(n) get(n, envir = ns))
    names(saved) <- names
    assignInNamespace("bot_chat_client", function(...) lo, "corteza")
    assignInNamespace("bot_room_cwd", function(...) root, "corteza")
    function() {
        for (n in names) assignInNamespace(n, saved[[n]], "corteza")
    }
}
# A person's message, sent to the loopback so a restart reads it back,
# and handed to the loop with the id and time the loopback gave it.
said <- function(lo, body, thread = NULL) {
    id <- chat.api::chat_send(lo, room, body, thread = thread)
    msgs <- chat.api::chat_history(lo, room, limit = 100L)$messages
    m <- msgs[[match(id, vapply(msgs, function(x) x$id, ""))]]
    chat.api::chat_message(id = m$id, channel = room,
                           sender = "@human:example.invalid", body = body,
                           ts = m$ts, thread = thread)
}
# One pass of the bot's loop with one incoming message, as test_hooks.R
# drives it. Replies go to the loopback, each through `on_reply` first.
poll <- function(lo, cfg, sessions, incoming, on_reply = NULL) {
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, envir = asNamespace(package)))
        assignInNamespace(name, fn, package)
    }
    on.exit(for (x in rev(saved)) {
        assignInNamespace(x$name, x$fn, x$package)
    }, add = TRUE)
    patch("bot_load_config", function(...) cfg)
    patch("chat_whoami", function(...) {
        chat.api::chat_identity("@bot:example.invalid")
    }, "chat.api")
    patch("chat_poll", function(...) {
        list(first_run = FALSE, messages = list(incoming),
             reactions = list(), invites = list())
    }, "chat.api")
    patch("bot_should_respond", function(...) TRUE)
    patch("bot_run_turn_in_cwd", function(...) stop("the poll reached a model"))
    patch("bot_update_displayname", function(cfg, ...) cfg)
    patch("bot_reply_send", function(chat, room, text, ...) {
        if (is.function(on_reply)) {
            on_reply(text)
        }
        chat.api::chat_send(lo, room, text, thread = list(...)$thread)
    })
    corteza::bot_poll(sessions = sessions, timeout = 0, model = "fixture",
                      provider = "ollama")
}
bodies <- function(lo) {
    vapply(chat.api::chat_history(lo, room, limit = 100L)$messages,
           function(m) m$body %||% "", "")
}
rendered <- function(s) {
    paste(vapply(s$history, function(m) {
        paste(unlist(m$content), collapse = " ")
    }, ""), collapse = "\n")
}

# ---- A session holds its room's conversation ---------------------------------
local({
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
    s <- ns$bot_get_or_create_session(reg, room, cfg)
    expect_true(is.character(s$conversation_id) && nzchar(s$conversation_id))
    expect_identical(ns$session_conversation(s), s$conversation_id)
    expect_identical(ns$bot_conversation_get(cfg, room, room)$id,
                     s$conversation_id)
    # A restart: a new registry, the same conversation.
    again <- ns$bot_get_or_create_session(ns$bot_new_session_registry(),
                                          room, cfg)
    expect_identical(again$conversation_id, s$conversation_id)
    # A thread holds its own.
    th <- ns$bot_get_or_create_session(reg, ns$bot_session_key(room, "$t"),
                                       cfg, room_id = room)
    expect_false(identical(th$conversation_id, s$conversation_id))
    # The reset takes the record the /clear saved: without one, the
    # session stays.
    expect_error(ns$bot_reset_session(reg, room, cfg, NULL, "ack", NULL),
                 "saved record")
    expect_identical(get(room, envir = reg), s)
    record <- ns$bot_conversation_begin(cfg, room, room,
                                        list(id = "$c", ts = 1))
    fresh <- ns$bot_reset_session(reg, room, cfg, NULL, "ack", record)
    expect_false(identical(fresh, s))
    expect_identical(fresh$conversation_id, record$id)
    expect_identical(get(room, envir = reg), fresh)
    expect_identical(ns$bot_conversation_get(cfg, room, room)$id, record$id)
})

# ---- /clear through the bot's loop, and a restart after it -------------------
# The room's timeline, in order. The main timeline and thread T are each
# cleared; thread U never is. A restart seeds each conversation with
# what came after its own /clear, and the answer to the /clear stays out.
local({
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
    t_root <- chat.api::chat_send(lo, room, "MAIN-OLD first")
    u_root <- chat.api::chat_send(lo, room, "MAIN-OLD second")
    chat.api::chat_send(lo, room, "T-OLD", thread = t_root)
    chat.api::chat_send(lo, room, "U-OLD", thread = u_root)
    main <- ns$bot_get_or_create_session(sessions, room, cfg)
    first <- main$conversation_id

    clear <- said(lo, "/clear")
    expect_equal(poll(lo, cfg, sessions, clear), 1L)
    after <- get(room, envir = sessions)
    expect_false(identical(after, main))
    expect_false(identical(after$conversation_id, first))
    record <- ns$bot_conversation_get(cfg, room, room)
    expect_identical(record$id, after$conversation_id)
    expect_identical(record$since, clear$id)
    expect_equal(record$at, as.numeric(clear$ts))
    msgs <- chat.api::chat_history(lo, room, limit = 100L)$messages
    ack <- msgs[[length(msgs)]]
    expect_identical(ack$body, "Cleared. Starting a fresh session.")
    expect_identical(record$ack, ack$id)

    chat.api::chat_send(lo, room, "MAIN-NEW")
    chat.api::chat_send(lo, room, "T-MID", thread = t_root)
    t_clear <- said(lo, "/clear", thread = t_root)
    expect_equal(poll(lo, cfg, sessions, t_clear), 1L)
    t_key <- ns$bot_session_key(room, t_root)
    expect_identical(ns$bot_conversation_get(cfg, t_key, room)$since,
                     t_clear$id)
    # The thread's /clear left the room's record alone.
    expect_identical(ns$bot_conversation_get(cfg, room, room)$id,
                     after$conversation_id)
    chat.api::chat_send(lo, room, "T-NEW", thread = t_root)
    chat.api::chat_send(lo, room, "U-NEW", thread = u_root)

    # The restart.
    restarted <- ns$bot_new_session_registry()
    ns$bot_backfill_sessions(lo, restarted, cfg, model = "fixture",
                             provider = "ollama", limit = 100L)
    m <- get(room, envir = restarted)
    expect_identical(m$conversation_id, after$conversation_id)
    expect_true(grepl("MAIN-NEW", rendered(m), fixed = TRUE))
    expect_false(grepl("MAIN-OLD", rendered(m), fixed = TRUE))
    expect_false(grepl("/clear", rendered(m), fixed = TRUE))
    expect_false(grepl("Cleared", rendered(m), fixed = TRUE))
    t <- get(t_key, envir = restarted)
    expect_true(grepl("T-NEW", rendered(t), fixed = TRUE))
    expect_false(grepl("T-OLD", rendered(t), fixed = TRUE))
    expect_false(grepl("T-MID", rendered(t), fixed = TRUE))
    expect_false(grepl("Cleared", rendered(t), fixed = TRUE))
    u <- get(ns$bot_session_key(room, u_root), envir = restarted)
    expect_true(grepl("U-OLD", rendered(u), fixed = TRUE))
    expect_true(grepl("U-NEW", rendered(u), fixed = TRUE))
})

# ---- A /clear whose record cannot be saved -----------------------------------
# Codex's finding on #282: the reset went on when the new record was not
# saved, so a restart brought the old conversation back. The record is
# saved before the room is answered and before the session goes. One
# that cannot be saved clears nothing, and the room is told.
local({
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
    s <- ns$bot_get_or_create_session(sessions, room, cfg)
    first <- s$conversation_id
    save <- ns$bot_conversation_save
    assignInNamespace("bot_conversation_save", function(...) FALSE, "corteza")
    n <- tryCatch(poll(lo, cfg, sessions, said(lo, "/clear")),
                  error = function(e) conditionMessage(e),
                  finally = assignInNamespace("bot_conversation_save", save,
                                              "corteza"))
    expect_equal(n, 1L)
    expect_identical(get(room, envir = sessions), s)
    expect_identical(s$conversation_id, first)
    expect_identical(ns$bot_conversation_get(cfg, room, room)$id, first)
    posted <- bodies(lo)
    expect_true(startsWith(posted[length(posted)],
                           "Not cleared: the new conversation could not be"))
    expect_false(any(startsWith(posted, "Cleared")))
    # Saved, the next /clear goes through, and the room's record names
    # the new conversation by the time the room is told.
    named <- NULL
    n <- poll(lo, cfg, sessions, said(lo, "/clear"), on_reply = function(text) {
        if (startsWith(text, "Cleared")) {
            named <<- ns$bot_conversation_get(cfg, room, room)$id
        }
    })
    expect_equal(n, 1L)
    after <- get(room, envir = sessions)
    expect_false(identical(after, s))
    expect_false(identical(named, first))
    expect_identical(named, after$conversation_id)
})

# ---- /clear in a room with a context vault -----------------------------------
# The command used to drop the session before the reset could clear its
# context, and the new session restored the cleared history from the
# journal's last snapshot.
if (requireNamespace("pensar", quietly = TRUE)) local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    root <- tempfile("room-root-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    vault <- file.path(root, "vault")
    pensar::init_vault(vault, rproj = FALSE, agent_instructions = FALSE)
    lo <- chat.api::chat_loopback()
    restore <- patch_room(lo, root)
    on.exit(restore(), add = TRUE)
    cfg <- room_cfg()
    cfg$context_vault <- vault
    sessions <- ns$bot_new_session_registry()
    s <- ns$bot_get_or_create_session(sessions, room, cfg)
    expect_true(ns$.context_edit_archives(s))
    s$history <- list(list(role = "user", content = "VAULT-OLD question"),
                      list(role = "assistant", content = "VAULT-OLD answer"))
    ns$.context_room_snapshot(s)
    expect_equal(poll(lo, cfg, sessions, said(lo, "/clear")), 1L)
    after <- get(room, envir = sessions)
    expect_false(identical(after, s))
    expect_identical(length(after$history), 0L)
    again <- ns$bot_get_or_create_session(ns$bot_new_session_registry(),
                                          room, cfg)
    expect_false(grepl("VAULT-OLD", rendered(again), fixed = TRUE))
})

# ---- The terminal's /clear and a room's reach one function -------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    calls <- list()
    real <- ns$job_conversation_clear
    assignInNamespace("job_conversation_clear",
                      function(session, conversation, checkpoint_key = NULL) {
        calls[[length(calls) + 1L]] <<- list(conversation = conversation,
                                              key = checkpoint_key)
        invisible(NULL)
    }, ns = "corteza")
    on.exit(assignInNamespace("job_conversation_clear", real, ns = "corteza"),
            add = TRUE)
    # Each first holds the queued jobs of the conversation that ends,
    # while the session is still that conversation's.
    real_hold <- ns$job_conversation_hold
    assignInNamespace("job_conversation_hold", function(session) {
        calls[[length(calls) + 1L]] <<- list(
            held = ns$session_conversation(session))
        invisible(0L)
    }, ns = "corteza")
    on.exit(assignInNamespace("job_conversation_hold", real_hold,
                              ns = "corteza"), add = TRUE)
    # The terminal's (R/repl-clear.R).
    ctx <- new.env(parent = emptyenv())
    ctx$palette <- list(dim = "", reset = "", cyan = "", yellow = "")
    ctx$session <- new.env(parent = emptyenv())
    ctx$session$on_tool <- list()
    ctx$session$.repl_jobs_ready <- TRUE
    ctx$session$sessionId <- "old-sess"
    ctx$disk_session <- list(session = list(sessionId = "old-sess"),
                             sessionId = "old-sess")
    ctx$new_session_fn <- function() {
        list(session = list(sessionId = "new-sess"), sessionId = "new-sess",
             resumed = FALSE)
    }
    invisible(capture.output(ns$.repl_clear(ctx)))
    expect_identical(calls, list(list(held = "old-sess"),
                                 list(conversation = "new-sess",
                                      key = "repl:new-sess")))
    # A terminal session without jobs is left alone.
    ctx$session$.repl_jobs_ready <- NULL
    invisible(capture.output(ns$.repl_clear(ctx)))
    expect_identical(length(calls), 2L)
    # A room's, through the bot's loop: the new conversation's id, and
    # no key, since a room keeps one checkpoint for each session key.
    root <- tempfile("room-root-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    lo <- chat.api::chat_loopback()
    restore <- patch_room(lo, root)
    on.exit(restore(), add = TRUE)
    cfg <- room_cfg()
    sessions <- ns$bot_new_session_registry()
    first <- ns$bot_get_or_create_session(sessions, room, cfg)$conversation_id
    poll(lo, cfg, sessions, said(lo, "/clear"))
    expect_identical(length(calls), 4L)
    expect_identical(calls[3:4], list(list(held = first), list(
        conversation = get(room, envir = sessions)$conversation_id, key = NULL)))
})

# ---- A job in flight at a room's /clear --------------------------------------
# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}
# The worker's job reports the conversation the worker holds when it
# runs, so a reply shows whether it built its session again.
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    root <- tempfile("room-root-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    lo <- chat.api::chat_loopback()
    restore <- patch_room(lo, root)
    on.exit(restore(), add = TRUE)
    cfg <- room_cfg()
    sessions <- ns$bot_new_session_registry()
    s <- ns$bot_get_or_create_session(sessions, room, cfg)
    gate <- file.path(root, "gate")
    Sys.setenv(ROOM_CONVERSATION_GATE = gate)
    on.exit(Sys.unsetenv("ROOM_CONVERSATION_GATE"), add = TRUE)
    s$job_worker_spec <- list(
        init_fn = function(spec) invisible(TRUE),
        run_fn = function(task) {
            if (grepl("wait", task, fixed = TRUE)) {
                gate <- Sys.getenv("ROOM_CONVERSATION_GATE")
                deadline <- Sys.time() + 30
                while (!file.exists(gate) && Sys.time() < deadline) {
                    Sys.sleep(0.05)
                }
            }
            held <- get(".job_worker_state", envir = asNamespace("corteza"))
            list(reply = sprintf("%s, held %s", task,
                                 held$conversation %||% "none"),
                 usage = list(input_tokens = 1L))
        })
    # The job as it stands once it settles, or when the wait runs out.
    settle <- function(session, id) {
        deadline <- Sys.time() + 60
        repeat {
            ns$bot_pump_jobs(sessions, lo, cfg)
            j <- ns$job_read(id)
            if (j$status %in% ns$JOB_STATUSES_FINAL || Sys.time() > deadline) {
                return(j)
            }
            Sys.sleep(0.05)
        }
    }
    pid_of <- function(session) {
        if (is.null(session$.job_worker)) NA else session$.job_worker$get_pid()
    }
    on.exit({
        ns$job_worker_close_all(s)
        ns$job_worker_close_all(get(room, envir = sessions))
    }, add = TRUE, after = FALSE)
    first <- s$conversation_id
    id <- ns$job_submit(s, "wait for the gate")
    deadline <- Sys.time() + 30
    while (!identical(ns$job_read(id)$status, "running") &&
           Sys.time() < deadline) {
        ns$job_pump(s)
        Sys.sleep(0.05)
    }
    expect_identical(ns$job_read(id)$status, "running")
    pid <- pid_of(s)
    # Codex's case on #281: a job queued behind it, and plan mode turned
    # on before the /clear. The new conversation starts with it off.
    queued <- ns$job_submit(s, "queued behind")
    expect_identical(ns$job_read(queued)$status, "queued")
    s$plan_mode <- TRUE

    expect_equal(poll(lo, cfg, sessions, said(lo, "/clear")), 1L)
    after <- get(room, envir = sessions)
    # The worker, the job in flight and what was held for the queued one
    # went to the replacement.
    expect_false(identical(after, s))
    expect_null(s$.job_worker)
    expect_null(s$.job_current)
    expect_identical(after$.job_current, id)
    expect_identical(pid_of(after), pid)
    expect_true(isTRUE(after$.job_held[[queued]]$spec$plan_mode))
    expect_false(isTRUE(after$plan_mode))
    # It finishes in the conversation that asked for it, and tells the
    # room, saying so.
    file.create(gate)
    j <- settle(after, id)
    expect_identical(j$status, "done")
    expect_identical(j$origin$conversation, first)
    expect_identical(j$outcome$result,
                     sprintf("wait for the gate, held %s", first))
    posted <- bodies(lo)
    hit <- grepl("wait for the gate, held", posted, fixed = TRUE)
    expect_true(any(hit))
    expect_true(any(grepl("Asked before /clear", posted[hit], fixed = TRUE)))
    # The queued job runs next, in its own conversation, under what that
    # conversation allowed as it ended.
    q <- settle(after, queued)
    expect_identical(q$status, "done")
    expect_identical(q$outcome$result, sprintf("queued behind, held %s", first))
    record <- ns$job_read_file(ns$job_authority_path(queued))
    expect_true(isTRUE(record$plan_mode))
    expect_identical(length(after$.job_held), 0L)
    expect_identical(length(after$history), 2L)
    expect_true(grepl("Asked before /clear", rendered(after), fixed = TRUE))
    # The next job runs in the same process, in the new conversation, and
    # its result carries no note. The stand-in for a model is the old
    # session's, and the replacement was built new.
    after$job_worker_spec <- s$job_worker_spec
    id2 <- ns$job_submit(after, "next")
    j2 <- settle(after, id2)
    expect_identical(pid_of(after), pid)
    expect_identical(j2$outcome$result,
                     sprintf("next, held %s", after$conversation_id))
    last <- list()
    if (length(after$history)) {
        last <- after$history[[length(after$history)]]
    }
    expect_true(grepl("next, held", paste(unlist(last$content), collapse = " "),
                      fixed = TRUE))
    expect_false(grepl("Asked before", paste(unlist(last$content),
                                             collapse = " "), fixed = TRUE))
})
