# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# Host hooks: what the loop asks the host and when. The /clear handler
# hands the ended conversation to on_archive, then asks on_clear whether
# it becomes a room of its own, under corteza's own gates; a bare bot
# (no hooks) resets and files nothing. Nothing here reaches a
# homeserver or a model.

# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}
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
    h <- corteza:::bot_hooks(list(on_archive = f))
    expect_identical(names(h), "on_archive")
})
expect_error(corteza:::bot_hooks(list(on_cleared = function(...) NULL)),
             "unknown hook")
expect_error(corteza:::bot_hooks(list(on_clear = "segment_open")),
             "must be functions")
expect_error(corteza:::bot_hooks(list(function(...) NULL)), "named list")
expect_error(corteza:::bot_hooks("on_clear"), "named list")

# ---- The /clear handler ----
# One harness: a loopback client, one scripted incoming message, the
# reply captured. Unless told otherwise, a host with hooks has an
# on_archive that records what it was offered and answers `archived`.
# Each run keeps its archive state in a folder of its own, returned as
# `state`. Sessions are built for real (ollama: no API key to refuse),
# because the reset at the end of /clear is part of what is under test.
run_clear <- function(hooks, body = "/clear", thread = NULL,
                      transcript = list(list(event_id = "$said", role = "user",
                                             content = "fold formula?")),
                      archived = list(archived = TRUE, ref = "notes/seg.md"),
                      archive_hook = TRUE, cwd = NULL, block_pending = FALSE,
                      archive_fails = FALSE) {
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        assignInNamespace(name, fn, package)
    }
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    state <- tempfile("hooks-state-")
    Sys.setenv(CORTEZA_STATE_DIR = state)
    on.exit({
        for (x in rev(saved)) assignInNamespace(x$name, x$fn, x$package)
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old_state)
    }, add = TRUE)
    offered <- list()
    if (!is.null(hooks) && archive_hook) {
        hooks$on_archive <- function(chat, cfg, room_id, archive) {
            offered[[length(offered) + 1L]] <<- archive
            archived
        }
    }
    cfg <- list(user_id = "fixture-bot", operators = "fixture-human",
                model = "qwen3:8b", provider = "ollama",
                segment_rooms = "fixture-room")
    chat <- chat.api::chat_loopback()
    sessions <- corteza:::bot_new_session_registry()
    key <- corteza:::bot_session_key("fixture-room", thread)
    if (block_pending) {
        # The pending file cannot be written: a folder stands where it
        # is written first. Everything else in the state folder works.
        dir.create(paste0(corteza:::bot_archive_pending_path(
            corteza:::bot_session_id(cfg, key)), ".tmp"), recursive = TRUE)
    }
    if (archive_fails) {
        patch("bot_archive_session", function(...) stop("disk on fire"))
    }
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
    patch("bot_update_displayname", function(cfg, ...) cfg)
    replies <- character()
    patch("bot_reply_send", function(chat, room, text, ...) {
        replies <<- c(replies, text)
        paste0("receipt-", length(replies))
    })
    n <- corteza::bot_poll(sessions = sessions, timeout = 0, hooks = hooks,
                           model = "qwen3:8b", provider = "ollama",
                           cwd = cwd)
    list(n = n, replies = replies, chat = chat, cfg = cfg, before = s,
         after = get(key, envir = sessions), offered = offered,
         state = state)
}
# What a run left waiting for a retry.
pending_files <- function(state) {
    list.files(file.path(state, "archive"), pattern = "[.]pending[.]json$")
}

# A bare bot: reset, acknowledged, nothing filed or kept, no one asked.
local({
    r <- run_clear(hooks = NULL)
    expect_equal(r$n, 1L)
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_false(identical(r$before, r$after))
    expect_false(dir.exists(file.path(r$state, "archive")))
})

# The session a poll builds works where the poll was told to: the
# fixture room's topic names no directory. Told nothing, it works in the
# scratch folder, where it used to make ~/fixture-bot.
local({
    d <- tempfile("poll-cwd-")
    dir.create(d)
    on.exit({
        corteza:::bot_run_cwd_set(NULL)
        unlink(d, recursive = TRUE)
    })
    r <- run_clear(hooks = NULL, cwd = d)
    expect_identical(normalizePath(r$after$cwd), normalizePath(d))
    expect_true(grepl(paste("Working directory:", r$after$cwd), r$after$system,
                      fixed = TRUE))
    expect_false(isTRUE(r$after$unbound))
    r <- run_clear(hooks = NULL)
    expect_true(corteza:::bot_is_scratch(r$after$cwd))
    expect_true(grepl("no project directory", r$after$system, fixed = TRUE))
    # Marked, so that its tools read and talk only (WP10).
    expect_true(isTRUE(r$after$unbound))
    # The session's own spelling of the path still reads as the scratch
    # folder once the folder is gone, which is when a turn has to make
    # it again. A path that does not exist normalizes to itself, so a
    # session that stored a resolved path would fail this where the
    # temp root is a symlink (macOS).
    unlink(r$after$cwd, recursive = TRUE)
    expect_false(dir.exists(r$after$cwd))
    expect_true(corteza:::bot_is_scratch(r$after$cwd))
})

# The hook is called once, with the documented arguments, and what it
# files is what the acknowledgement names. The archive hook went first,
# with the conversation /clear ended, and its reference is the one
# on_clear receives.
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
    expect_identical(calls[[1L]]$vault_ref, "notes/seg.md")
    expect_identical(r$replies, sprintf("Cleared. Filed as \"%s\".",
                                        calls[[1L]]$title))
    expect_false(identical(r$before, r$after))
    expect_identical(length(r$offered), 1L)
    expect_identical(r$offered[[1L]]$reason, "clear")
    expect_identical(r$offered[[1L]]$room_id, "fixture-room")
    expect_identical(r$offered[[1L]]$entries[[1L]]$content, "fold formula?")
    expect_identical(pending_files(r$state), character())
})

# Filed with no reference: on_clear is still asked, and given none.
local({
    got <- "unset"
    hooks <- list(on_clear = function(chat, cfg, room_id, title, vault_ref) {
        got <<- vault_ref
        NULL
    })
    r <- run_clear(hooks, archived = list(archived = TRUE))
    expect_null(got)
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
})

# An archive the host did not take opens no segment and waits on disk
# for a retry; the clear itself goes ahead, as before.
local({
    asked <- 0L
    hooks <- list(on_clear = function(...) {
        asked <<- asked + 1L
        list(id = "!seg:ex", name = "never")
    })
    expect_message(r <- run_clear(hooks, archived = NULL),
                   "on_archive hook: not archived (fixture-room, kept to retry)",
                   fixed = TRUE)
    expect_identical(asked, 0L)
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_false(identical(r$before, r$after))
    expect_identical(length(pending_files(r$state)), 1L)
    expect_message(r <- run_clear(list(on_archive = function(...) {
        stop("summary model down")
    }, on_clear = hooks$on_clear), archive_hook = FALSE),
    "on_archive hook: summary model down (fixture-room, kept to retry)",
    fixed = TRUE)
    expect_identical(asked, 0L)
    expect_identical(length(pending_files(r$state)), 1L)
})

# Nothing may be cleared that is neither with the host nor on disk. When
# the pending file cannot be written and the host declines, the session
# is all that holds the conversation: /clear keeps it, says so, and asks
# no segment. A later /clear tries again.
local({
    asked <- 0L
    hooks <- list(on_clear = function(...) {
        asked <<- asked + 1L
        list(id = "!seg:ex", name = "never")
    })
    expect_message(r <- run_clear(hooks, archived = NULL, block_pending = TRUE),
                   "/clear kept the session in fixture-room", fixed = TRUE)
    expect_true(identical(r$before, r$after))
    expect_true("$said" %in% corteza:::bot_transcript_ids(r$after$transcript))
    expect_true(startsWith(r$replies[[1L]], "Not cleared:"))
    expect_identical(length(r$offered), 1L)
    expect_identical(asked, 0L)
    expect_identical(pending_files(r$state), character())
    # The host takes it: nothing is left in memory alone, so it clears.
    r <- suppressMessages(run_clear(hooks, block_pending = TRUE))
    expect_false(identical(r$before, r$after))
    expect_identical(r$replies, "Cleared. Filed as \"never\".")
    # The archive step fails outright: the session stays as well.
    expect_message(r <- run_clear(hooks, archive_fails = TRUE),
                   "archive failed: disk on fire", fixed = TRUE)
    expect_true(identical(r$before, r$after))
    expect_true(startsWith(r$replies[[1L]], "Not cleared:"))
    expect_identical(asked, 1L)
})

# A host with on_clear and no on_archive, such as one written for an
# older corteza: nothing is filed, so no segment opens, and nothing is
# kept.
local({
    asked <- 0L
    hooks <- list(on_clear = function(...) {
        asked <<- asked + 1L
        list(id = "!seg:ex", name = "never")
    })
    r <- run_clear(hooks, archive_hook = FALSE)
    expect_identical(asked, 0L)
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_false(dir.exists(file.path(r$state, "archive")))
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
# thread all reset without asking. The thread's conversation is still
# offered to the archive, as the thread's.
local({
    asked <- 0L
    hooks <- list(on_clear = function(...) {
        asked <<- asked + 1L
        list(id = "!seg:ex", name = "never")
    })
    r <- suppressMessages(run_clear(hooks, archived = NULL))
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    r <- run_clear(hooks, transcript = list(list(event_id = "$cmd",
                                                 role = "user",
                                                 content = "/clear")))
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    r <- run_clear(hooks, thread = "$root")
    expect_identical(r$replies, "Cleared. Starting a fresh session.")
    expect_identical(asked, 0L)
    expect_identical(length(r$offered), 1L)
    expect_identical(r$offered[[1L]]$thread, "$root")
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
