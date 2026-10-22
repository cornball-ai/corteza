library(tinytest)

# Host hooks: what the loop asks the host and when. The /clear handler
# asks on_clear whether the ended conversation becomes a room of its
# own, under corteza's own gates; a bare bot (no hooks) archives and
# resets. Nothing here reaches a homeserver or a model.

if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}

# ---- Validation at init ----
# A misspelled slot is an error at startup, not a hook that never fires.
expect_identical(corteza:::bot_hooks(NULL), list())
expect_identical(corteza:::bot_hooks(list()), list())
local({
    f <- function(...) NULL
    h <- corteza:::bot_hooks(list(on_clear = f, thread_context = f))
    expect_identical(names(h), c("on_clear", "thread_context"))
})
expect_error(corteza:::bot_hooks(list(on_cleared = function(...) NULL)),
             "unknown hook")
expect_error(corteza:::bot_hooks(list(on_clear = "segment_open")),
             "must be functions")
expect_error(corteza:::bot_hooks(list(function(...) NULL)), "named list")
expect_error(corteza:::bot_hooks("on_clear"), "named list")

# ---- The /clear handler ----
# One harness: a loopback client, one scripted incoming message, the
# archive seamed to answer a path, the reply captured. Sessions are
# built for real (ollama: no API key to refuse), because the reset at
# the end of /clear is part of what is under test.
run_clear <- function(hooks, body = "/clear", thread = NULL,
                      transcript = list(list(role = "user",
                                             content = "fold formula?")),
                      archived = file.path(tempdir(), "seg.md")) {
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        assignInNamespace(name, fn, package)
    }
    on.exit({
        for (x in rev(saved)) assignInNamespace(x$name, x$fn, x$package)
    }, add = TRUE)
    cfg <- list(user_id = "fixture-bot", operators = "fixture-human",
                model = "qwen3:8b", provider = "ollama",
                segment_rooms = "fixture-room")
    chat <- chat.api::chat_loopback()
    sessions <- corteza:::bot_new_session_registry()
    key <- corteza:::bot_session_key("fixture-room", thread)
    s <- corteza:::bot_get_or_create_session(sessions, key, cfg,
                                             room_id = "fixture-room")
    s$transcript <- transcript
    s$seen_event_ids <- character()
    s$cwd <- tempdir()
    incoming <- chat.api::chat_message(id = "clear-1", channel = "fixture-room",
        sender = "fixture-human", body = body, ts = Sys.time(),
        thread = thread)
    patch("bot_load_config", function(...) cfg)
    patch("bot_chat_client", function(...) chat)
    patch("chat_whoami", function(...) chat.api::chat_identity("fixture-bot"),
          "chat.api")
    patch("chat_poll", function(...) list(first_run = FALSE,
        messages = list(incoming), reactions = list(), invites = list()),
        "chat.api")
    patch("bot_should_respond", function(...) TRUE)
    patch("bot_run_turn_in_cwd", function(...) stop("/clear reached the LLM"))
    patch("bot_archive_session", function(session, room_id, chat = NULL) archived)
    patch("bot_update_displayname", function(cfg, ...) cfg)
    replies <- character()
    patch("bot_reply_send", function(chat, room, text, ...) {
        replies <<- c(replies, text)
        paste0("receipt-", length(replies))
    })
    n <- corteza::bot_poll(sessions = sessions, timeout = 0, hooks = hooks,
                           model = "qwen3:8b", provider = "ollama")
    list(n = n, replies = replies, chat = chat, cfg = cfg, before = s,
         after = get(key, envir = sessions))
}

# A bare bot: archived, reset, acknowledged, and no one asked.
local({
    r <- run_clear(hooks = NULL)
    expect_equal(r$n, 1L)
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_false(identical(r$before, r$after))
})

# The hook is called once, with the documented arguments, and what it
# files is what the acknowledgement names.
local({
    calls <- list()
    hooks <- list(on_clear = function(chat, cfg, room_id, title, vault_ref) {
        calls[[length(calls) + 1L]] <<- list(chat = chat, cfg = cfg,
            room_id = room_id, title = title, vault_ref = vault_ref)
        list(id = "!seg:ex", name = title)
    })
    r <- run_clear(hooks)
    expect_identical(length(calls), 1L)
    expect_identical(calls[[1L]]$chat, r$chat)
    expect_identical(calls[[1L]]$cfg, r$cfg)
    expect_identical(calls[[1L]]$room_id, "fixture-room")
    expect_true(startsWith(calls[[1L]]$title, "fold formula? ("))
    expect_identical(calls[[1L]]$vault_ref,
                     corteza:::bot_vault_ref(file.path(tempdir(), "seg.md")))
    expect_identical(r$replies, sprintf("Cleared. Filed as \"%s\".",
                                        calls[[1L]]$title))
    expect_false(identical(r$before, r$after))
})

# The hook declining (NULL), answering nonsense, or failing all leave
# the plain acknowledgement; a failure is reported.
local({
    r <- run_clear(list(on_clear = function(...) NULL))
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    r <- run_clear(list(on_clear = function(...) "not a segment"))
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_message(
        r <- run_clear(list(on_clear = function(...) stop("room create refused"))),
        "on_clear hook: room create refused")
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_false(identical(r$before, r$after))
})

# corteza's gates stay in front of the hook: nothing archived, a
# conversation in which the user said nothing, and a clear inside a
# thread all reset without asking.
local({
    asked <- 0L
    hooks <- list(on_clear = function(...) {
        asked <<- asked + 1L
        list(id = "!seg:ex", name = "never")
    })
    r <- run_clear(hooks, archived = NULL)
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    r <- run_clear(hooks, transcript = list(list(role = "user",
                                                 content = "/clear")))
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    r <- run_clear(hooks, thread = "$root")
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_identical(asked, 0L)
})

# ---- bot_run_init() keeps the hooks for the loop ----
# No config at the explicit path, so no client is built and nothing is
# contacted; the state still carries the validated hooks for
# bot_run_step() to hand to bot_poll().
if (requireNamespace("mx.client", quietly = TRUE)) local({
    old_cfg <- Sys.getenv("CORTEZA_MATRIX_CONFIG", unset = NA)
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    dir <- tempfile("corteza-hooks-")
    dir.create(dir)
    Sys.setenv(CORTEZA_MATRIX_CONFIG = file.path(dir, "absent.json"),
               CORTEZA_STATE_DIR = dir)
    on.exit({
        if (is.na(old_cfg)) Sys.unsetenv("CORTEZA_MATRIX_CONFIG") else
            Sys.setenv(CORTEZA_MATRIX_CONFIG = old_cfg)
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old_state)
        unlink(dir, recursive = TRUE)
    }, add = TRUE)
    f <- function(chat, cfg, room_id, title, vault_ref) NULL
    state <- corteza::bot_run_init(hooks = list(on_clear = f))
    expect_identical(state$opts$hooks$on_clear, f)
    expect_null(state$opts$hooks$thread_context)
    expect_identical(corteza::bot_run_init()$opts$hooks, list())
    expect_error(corteza::bot_run_init(hooks = list(nope = f)), "unknown hook")
})
