library(tinytest)

# The bot process's side of a call (R/call-bot.R): the /call and
# /hangup commands, starting the worker from what the signaling join
# returned, forwarding keys, posting and editing for the worker, and
# ending. chat.api and the worker are faked.

# ---- commands ----
expect_identical(corteza:::bot_parse_call_command("/call"), "join")
expect_identical(corteza:::bot_parse_call_command("@bot: /call"), "join")
expect_identical(corteza:::bot_parse_call_command("/hangup"), "hangup")
expect_identical(corteza:::bot_parse_call_command("/hang up"), "hangup")
expect_identical(corteza:::bot_parse_call_command("/leave call"), "hangup")
expect_null(corteza:::bot_parse_call_command("/call me maybe"))
expect_null(corteza:::bot_parse_call_command("call"))
expect_null(corteza:::bot_parse_call_command(""))
expect_null(corteza:::bot_parse_call_command(NULL))

# ---- the child's config carries no Matrix credential ----
local({
    cfg <- list(server = "https://ex", user = "bot", user_id = "@bot:ex",
                token = "secret-token", password = "pw", device_id = "DEV",
                sync_token = "s1", model = "m", provider = "anthropic",
                display_name = "Cornelius",
                voice = list(allocator_token = "svc", call = list(names = "corny"),
                             stt = list(url = "u", model = "m"),
                             tts = list(url = "u", model = "m")))
    child <- corteza:::bot_call_child_cfg(cfg)
    expect_false(any(c("token", "password", "sync_token", "device_id") %in% names(child)))
    expect_null(child$voice$allocator_token)
    expect_identical(child$model, "m")
    expect_identical(child$voice$stt$url, "u")
    expect_identical(corteza:::bot_call_names(cfg), c("corny", "Cornelius", "bot"))
    expect_identical(corteza:::bot_call_names(list(user_id = "@tiny:ex")), "tiny")
})

# ---- a fake chat.api and a fake worker ----
# chat.api's call surface is replaced where corteza calls it: the
# functions are looked up in the chat.api namespace, so the fakes are
# installed there for the test and restored after.
with_fake_chat <- function(fn, caps = list(calls = TRUE)) {
    ns <- asNamespace("chat.api")
    log <- new.env(parent = emptyenv())
    log$sent <- list()
    log$edits <- list()
    log$left <- 0L
    call <- new.env(parent = emptyenv())
    call$channel <- "!r:ex"
    call$identity <- "@bot:ex:DEV"
    call$changes <- list(keys = list(), own = NULL, members = NULL, ended = FALSE)
    class(call) <- "chat_call"
    log$call <- call
    fakes <- list(
        chat_call_join = function(client, channel, ...) call,
        chat_call_media = function(call) {
            list(url = "wss://sfu", jwt = "jwt-1", identity = call$identity,
                 key = list(key = as.raw(1:16), index = 0L),
                 peers = list(list(identity = "@ann:ex:PHONE", key = as.raw(21:36),
                                   index = 2L)))
        },
        chat_call_updates = function(call) {
            out <- call$changes
            call$changes <- list(keys = list(), own = NULL, members = NULL,
                                 ended = isTRUE(out$ended))
            out
        },
        chat_call_leave = function(client, call) {
            log$left <- log$left + 1L
            call$changes$ended <- TRUE
            invisible(call)
        },
        chat_capabilities = function(client, ...) caps,
        chat_history = function(client, channel, limit = 50L, ...) {
            list(messages = list(list(kind = "message", body = "earlier", self = FALSE)))
        },
        chat_send = function(client, channel, text, ...) {
            log$sent[[length(log$sent) + 1L]] <- list(channel = channel, text = text)
            sprintf("$e%d", length(log$sent))
        },
        chat_edit = function(client, channel, message_id, text, ...) {
            log$edits[[length(log$edits) + 1L]] <- list(id = message_id, text = text)
            invisible(message_id)
        })
    saved <- list()
    for (nm in names(fakes)) {
        saved[[nm]] <- get(nm, envir = ns)
        unlockBinding(nm, ns)
        assign(nm, fakes[[nm]], envir = ns)
    }
    on.exit({
        for (nm in names(saved)) {
            assign(nm, saved[[nm]], envir = ns)
            lockBinding(nm, ns)
        }
    }, add = TRUE)
    fn(log)
    log
}

# A worker that records what it is started with and told, and whose
# requests the test writes into the mailbox.
fake_worker <- function() {
    w <- new.env(parent = emptyenv())
    w$alive <- TRUE
    w$result <- NULL
    w$commands <- list()
    w$dir <- tempfile("fake-call-")
    dir.create(file.path(w$dir, "answers"), recursive = TRUE)
    file.create(file.path(w$dir, "requests.jsonl"))
    file.create(file.path(w$dir, "commands.jsonl"))
    w$read_from <- 0L
    w$room_id <- "!r:ex"
    w
}

if (requireNamespace("chat.api", quietly = TRUE)) {
    ns <- asNamespace("corteza")
    # corteza's worker verbs, redirected at the fake.
    worker <- fake_worker()
    redirect <- list(
        call_worker_command = function(w, cmd) {
            w$commands[[length(w$commands) + 1L]] <- cmd
            invisible(w)
        },
        call_worker_alive = function(w) isTRUE(w$alive),
        call_worker_close = function(w, grace = 5) {
            w$alive <- FALSE
            invisible(w$result)
        })
    saved <- list()
    for (nm in names(redirect)) {
        saved[[nm]] <- get(nm, envir = ns)
        unlockBinding(nm, ns)
        assign(nm, redirect[[nm]], envir = ns)
    }
    restore <- function() {
        for (nm in names(saved)) {
            assign(nm, saved[[nm]], envir = ns)
            lockBinding(nm, ns)
        }
    }
    started <- list()
    start <- function(args, dir) {
        started[[length(started) + 1L]] <<- list(args = args, dir = dir)
        worker$room_id <- args$room_id
        worker
    }
    cfg <- list(user = "bot", user_id = "@bot:ex", token = "secret", model = "m",
                provider = "anthropic",
                voice = list(stt = list(url = "u", model = "m"),
                             tts = list(url = "u", model = "m")))
    sessions <- new.env(parent = emptyenv())
    s <- corteza::new_session("matrix")
    assign("!r:ex", s, envir = sessions)
    expect_false(corteza:::bot_calls_active(sessions))

    # Joining: the signaling join, then the worker, with the token, the
    # keys (own first), the history, the names, and a config without
    # credentials.
    log <- with_fake_chat(function(log) {
        expect_identical(corteza:::bot_call_command(s, cfg, "chat", "!r:ex", "hangup"),
                         "Not in a call here.")
        corteza:::bot_call_start(s, cfg, "chat", "!r:ex", start = start)
        expect_true(corteza:::bot_calls_active(sessions))
        expect_identical(length(started), 1L)
        a <- started[[1L]]$args
        expect_identical(a$url, "wss://sfu")
        expect_identical(a$jwt, "jwt-1")
        expect_identical(a$identity, "@bot:ex:DEV")
        expect_identical(length(a$keys), 2L)
        expect_identical(a$keys[[1L]]$identity, "@bot:ex:DEV")
        expect_identical(jsonlite::base64_dec(a$keys[[1L]]$key), as.raw(1:16))
        expect_identical(a$keys[[2L]]$identity, "@ann:ex:PHONE")
        expect_identical(a$keys[[2L]]$index, 2L)
        expect_identical(a$history[[1L]]$body, "earlier")
        expect_identical(a$names, "bot")
        expect_null(a$cfg$token)
        expect_true(grepl("_r_ex$", started[[1L]]$dir))
        # A second /call is refused while one is on.
        expect_identical(corteza:::bot_call_command(s, cfg, "chat", "!r:ex", "join"),
                         "Already in this room's call. /hangup to leave it.")

        # A poll's updates reach the worker as key commands: a peer's,
        # then our own rotated key under our identity.
        log_call <- get("chat_call_join", envir = asNamespace("chat.api"))("x", "!r:ex")
        log_call$changes$keys <- list(list(identity = "@bob:ex:LAPTOP", key = as.raw(41:56),
                                           index = 7L))
        log_call$changes$own <- list(key = as.raw(61:76), index = 1L)
        expect_true(corteza:::bot_call_pump(s, "chat"))
        expect_identical(length(worker$commands), 2L)
        expect_identical(worker$commands[[1L]]$identity, "@bob:ex:LAPTOP")
        expect_identical(worker$commands[[1L]]$index, 7L)
        expect_identical(worker$commands[[2L]]$identity, "@bot:ex:DEV")
        expect_identical(jsonlite::base64_dec(worker$commands[[2L]]$key), as.raw(61:76))

        # The worker's requests are answered through the room.
        box <- new.env(parent = emptyenv())
        box$dir <- worker$dir
        box$n <- 0L
        box$read_from <- 0L
        box$wait_s <- 1
        corteza:::.call_request(box, list(type = "note", text = "in the call"))
        corteza:::.call_request(box, list(type = "post", text = "Hello there."))
        expect_message(corteza:::bot_call_pump(s, "chat"), "in the call")
        ans <- jsonlite::fromJSON(file.path(worker$dir, "answers", "000002"))
        expect_identical(ans$event_id, "$e1")
        corteza:::.call_request(box, list(type = "edit", event_id = "$e1", text = "Hello"))
        corteza:::bot_call_pump(s, "chat")
        expect_identical(length(log$edits), 1L)

        # The worker ending ends the call: signaling left, the room told.
        worker$alive <- FALSE
        worker$result <- list(value = list(turns = 2L), error = NULL)
        expect_message(corteza:::bot_call_pump(s, "chat"), "the call ended")
        expect_null(s$call)
        expect_false(corteza:::bot_calls_active(sessions))
        expect_identical(log$left, 1L)
        expect_true(grepl("^Left the call", log$sent[[length(log$sent)]]$text))

        # /hangup stops a running call.
        worker$alive <- TRUE
        corteza:::bot_call_start(s, cfg, "chat", "!r:ex", start = start)
        expect_identical(corteza:::bot_call_command(s, cfg, "chat", "!r:ex", "hangup"),
                         "Left the call.")
        expect_null(s$call)
        expect_false(worker$alive)
        expect_identical(log$left, 2L)
    })
    expect_identical(length(log$sent), 2L)
    expect_identical(log$sent[[1L]]$text, "Hello there.")

    # Refusals, by reason.
    with_fake_chat(function(log) {
        bad <- cfg
        bad$voice <- NULL
        expect_true(grepl("voice.stt and voice.tts",
                          corteza:::bot_call_command(s, bad, "chat", "!r:ex", "join")))
    })
    with_fake_chat(function(log) {
        expect_true(grepl("cannot join calls",
                          corteza:::bot_call_command(s, cfg, "chat", "!r:ex", "join")))
    }, caps = list(calls = FALSE))
    restore()
    unlink(worker$dir, recursive = TRUE)
}
