library(tinytest)

# Threaded sessions and rehydration: a reply in a folded conversation's
# thread continues that conversation instead of starting a blank one.
# Which archive a thread stands for is the host's knowledge, supplied as
# the thread_context hook; corteza resolves the path, bounds the
# transcript, and seeds the session.

if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}
if (!("chat_get_state" %in% getNamespaceExports("chat.api"))) {
    exit_file("chat.api too old: no chat_get_state")
}

# Sessions are built for real here rather than hand-assembled, because
# what is under test is how bot_get_or_create_session() keys and
# populates them. That means session_setup() runs, and it refuses a
# provider whose API key is absent -- so the provider is ollama, for
# which there is no key to check. Nothing here reaches a model.
test_cfg <- function() {
    list(room_id = "!r:ex", model = "qwen3:8b", provider = "ollama")
}

# A host's lookup, shaped like cerebro's: the fold index is state in the
# topic room, keyed by the thread root.
fold_lookup <- function(chat, room_id, thread_root) {
    st <- tryCatch(chat.api::chat_get_state(chat, room_id, "ai.cornball.fold",
                                            state_key = thread_root),
                   error = function(e) NULL)
    if (!is.list(st)) {
        return(NULL)
    }
    list(segment = st$segment, vault = st$vault)
}

# ---- Session keys ----
# Without a thread the key is the room id exactly, which is what keeps
# every existing room behaving as it did.
expect_identical(corteza:::bot_session_key("!r:ex"), "!r:ex")
expect_identical(corteza:::bot_session_key("!r:ex", NULL), "!r:ex")
expect_identical(corteza:::bot_session_key("!r:ex", ""), "!r:ex")
# With one, room and root together -- and separated by a character
# neither a room id nor an event id can contain, so no thread's key can
# collide with a bare room id.
expect_identical(corteza:::bot_session_key("!r:ex", "$root"),
                 "!r:ex\r$root")
expect_false(identical(corteza:::bot_session_key("!r:ex", "$a"),
                       corteza:::bot_session_key("!r:ex", "$b")))

# ---- The registry keeps them apart ----
# One room, two threads, and the main timeline: three conversations,
# three histories. Before this they shared one, so a reply in a topic
# room's thread answered with another topic's context.
local({
    reg <- corteza:::bot_new_session_registry()
    cfg <- test_cfg()
    main <- corteza:::bot_get_or_create_session(reg, "!r:ex", cfg)
    t1 <- corteza:::bot_get_or_create_session(
        reg, corteza:::bot_session_key("!r:ex", "$a"), cfg,
        room_id = "!r:ex")
    t2 <- corteza:::bot_get_or_create_session(
        reg, corteza:::bot_session_key("!r:ex", "$b"), cfg,
        room_id = "!r:ex")
    expect_identical(length(ls(reg, all.names = TRUE)), 3L)
    main$history <- list(list(role = "user", content = "main"))
    t1$history <- list(list(role = "user", content = "thread a"))
    expect_identical(length(t2$history %||% list()), 0L)
    expect_identical(t1$history[[1L]]$content, "thread a")
    # Every one of them still knows the room it speaks into, which is
    # what the send target and the archive's source are taken from.
    expect_identical(main$room_id, "!r:ex")
    expect_identical(t1$room_id, "!r:ex")
    expect_identical(t2$room_id, "!r:ex")
    # Re-fetching a key returns the same session, not a fresh one.
    expect_identical(
        corteza:::bot_get_or_create_session(
            reg, corteza:::bot_session_key("!r:ex", "$a"), cfg,
            room_id = "!r:ex")$history[[1L]]$content, "thread a")
})

# ---- Archival files under the room, not the key ----
# The registry key of a thread is not a room id, and archiving under it
# would file the transcript against a room no homeserver has heard of.
local({
    seen <- list()
    orig <- corteza:::bot_archive_session
    assignInNamespace("bot_archive_session",
                      function(session, room_id, chat = NULL) {
        seen[[length(seen) + 1L]] <<- room_id
        "ok"
    }, ns = "corteza")
    on.exit(assignInNamespace("bot_archive_session", orig, ns = "corteza"),
            add = TRUE)
    reg <- corteza:::bot_new_session_registry()
    cfg <- test_cfg()
    corteza:::bot_get_or_create_session(reg, "!r:ex", cfg)
    corteza:::bot_get_or_create_session(
        reg, corteza:::bot_session_key("!r:ex", "$a"), cfg,
        room_id = "!r:ex")
    corteza::bot_archive_all(reg)
    expect_identical(sort(unique(unlist(seen))), "!r:ex")
    expect_false(any(grepl("\r", unlist(seen), fixed = TRUE)))
})

# ---- Asking the host which archive a thread stands for ----
# No hook, no root, a hook that answers nothing, or an answer without a
# usable pointer: all NULL, never an error.
local({
    lo <- chat.api::chat_loopback()
    expect_null(corteza:::bot_thread_context(NULL, lo, "topic", "$root"))
    expect_null(corteza:::bot_thread_context(fold_lookup, lo, "topic", NULL))
    expect_null(corteza:::bot_thread_context(fold_lookup, lo, "topic", "$root"))
    expect_null(corteza:::bot_thread_context(function(...) "raw/x.md",
                                             lo, "topic", "$root"))
    expect_null(corteza:::bot_thread_context(function(...) list(segment = "!s:ex"),
                                             lo, "topic", "$root"))
    # A usable answer comes back as the documented pair.
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex",
                                  vault = "raw/matrix/seg.md"),
                             state_key = "$root")
    got <- corteza:::bot_thread_context(fold_lookup, lo, "topic", "$root")
    expect_identical(got, list(segment = "!seg:ex", vault = "raw/matrix/seg.md"))
})

# ---- Reading the archive ----
local({
    expect_null(corteza:::bot_archive_excerpt(NULL))
    expect_null(corteza:::bot_archive_excerpt(file.path(tempdir(), "nope.md")))
    f <- tempfile(fileext = ".md")
    writeLines(c("# room", "", "## user", "", "hello"), f)
    expect_true(grepl("hello", corteza:::bot_archive_excerpt(f), fixed = TRUE))
    # Bounded, and from the tail: resuming a conversation, the last
    # exchanges are the ones the next message answers.
    big <- tempfile(fileext = ".md")
    writeLines(c(strrep("a", 5000L), strrep("z", 5000L)), big)
    ex <- corteza:::bot_archive_excerpt(big, max_chars = 1000L)
    expect_true(nchar(ex) < 1200L)
    expect_true(grepl("zzz", ex, fixed = TRUE))
    expect_false(grepl("aaa", ex, fixed = TRUE))
    expect_true(grepl("earlier turns omitted", ex, fixed = TRUE))
})

# An absolute reference resolves to itself; a relative one needs the
# vault root and answers NULL without one.
expect_identical(corteza:::bot_vault_path("/tmp/x.md"), "/tmp/x.md")
expect_null(corteza:::bot_vault_path(NULL))
expect_null(corteza:::bot_vault_path(""))

# ---- Seeding a session ----
local({
    lo <- chat.api::chat_loopback()
    f <- tempfile(fileext = ".md")
    writeLines(c("## user", "", "what about the fold formula",
                 "## assistant", "", "seven days"), f)
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex", vault = f),
                             state_key = "$root")
    s <- new.env(parent = emptyenv())
    s$history <- list(list(role = "user", content = "and now?"))
    expect_message(ok <- corteza:::bot_rehydrate_session(s, lo, "topic",
                                                         "$root", fold_lookup),
                   "rehydrated thread")
    expect_true(ok)
    # The archive goes in front of what was already there, so the new
    # message still reads as the latest turn.
    expect_identical(length(s$history), 3L)
    expect_identical(s$history[[3L]]$content, "and now?")
    expect_true(grepl("seven days", s$history[[1L]]$content, fixed = TRUE))
    # As one framing exchange, not replayed turn by turn: parsing the
    # archive's markdown back into roles could put words in the user's
    # mouth, which is the one error a resumed conversation cannot
    # survive.
    expect_identical(s$history[[1L]]$role, "user")
    expect_identical(s$history[[2L]]$role, "assistant")

    # A thread that is not a fold leaves the session alone.
    s2 <- new.env(parent = emptyenv())
    s2$history <- list()
    expect_false(corteza:::bot_rehydrate_session(s2, lo, "topic", "$nope",
                                                 fold_lookup))
    expect_identical(length(s2$history), 0L)

    # An index pointing at an archive that is gone says so and leaves
    # the session unseeded, rather than failing the turn.
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex",
                                  vault = "/nonexistent/gone.md"),
                             state_key = "$missing")
    s3 <- new.env(parent = emptyenv())
    s3$history <- list()
    expect_message(res <- corteza:::bot_rehydrate_session(s3, lo, "topic",
                                                          "$missing",
                                                          fold_lookup),
                   "unreadable")
    expect_false(res)
    expect_identical(length(s3$history), 0L)

    # A bare bot has no hook, and a thread with a perfectly good index
    # behind it stays unseeded: the session is not even marked, since
    # a hook cannot appear later in the process.
    s4 <- new.env(parent = emptyenv())
    s4$history <- list()
    expect_false(corteza:::bot_maybe_rehydrate(s4, lo, "topic", "$root"))
    expect_identical(length(s4$history), 0L)
    expect_null(s4$rehydrate_checked)
})

# ---- Rehydration is considered once per session ----
# The gate is a flag on the session, not "was it just created": a
# session can reach its first live message already populated, because
# startup backfill built one for every thread in its window.
local({
    lo <- chat.api::chat_loopback()
    f <- tempfile(fileext = ".md")
    writeLines(c("## user", "", "earlier turn"), f)
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex", vault = f),
                             state_key = "$root")
    # A session backfill already filled still gets seeded on its first
    # live message. Keying on freshness is what left a restart
    # answering an active thread from the backfilled tail alone.
    s <- new.env(parent = emptyenv())
    s$history <- list(list(role = "user", content = "from backfill"))
    expect_true(corteza:::bot_maybe_rehydrate(s, lo, "topic", "$root",
                                              fold_lookup))
    expect_identical(length(s$history), 3L)
    expect_true(grepl("earlier turn", s$history[[1L]]$content, fixed = TRUE))
    # And not a second time, however many messages follow.
    expect_false(corteza:::bot_maybe_rehydrate(s, lo, "topic", "$root",
                                               fold_lookup))
    expect_identical(length(s$history), 3L)

    # A thread nobody folded into is asked about once too, or every
    # message in an ordinary thread would cost the host a lookup for
    # the life of the process.
    asked <- 0L
    counting <- function(chat, room_id, thread_root) {
        asked <<- asked + 1L
        NULL
    }
    s2 <- new.env(parent = emptyenv())
    s2$history <- list()
    for (i in 1:3) corteza:::bot_maybe_rehydrate(s2, lo, "topic", "$plain",
                                                 counting)
    expect_identical(asked, 1L)

    # An unthreaded message is never a rehydration candidate, and does
    # not spend a lookup finding that out.
    s3 <- new.env(parent = emptyenv())
    expect_false(corteza:::bot_maybe_rehydrate(s3, lo, "topic", NULL, counting))
    expect_identical(asked, 1L)
})

# A rehydration that throws is contained and still marks the session,
# so a failing hook cannot take the turn with it or retry forever.
local({
    lo <- chat.api::chat_loopback()
    s <- new.env(parent = emptyenv())
    s$history <- list()
    expect_message(res <- corteza:::bot_maybe_rehydrate(s, lo, "topic",
                                                        "$root",
                                                        function(...) {
        stop("homeserver down")
    }),
                   "rehydrate failed")
    expect_false(res)
    expect_true(isTRUE(s$rehydrate_checked))
})

# ---- Backfill routes threads into their own sessions ----
# The defect this pins: backfill keyed every message by room, so a
# restart put each topic's history into the main timeline's context and
# left the threads themselves empty.
local({
    lo <- chat.api::chat_loopback()
    hist <- list(
        chat.api::chat_message(id = "$m1", channel = "!r:ex",
                               sender = "@troy:ex", body = "main timeline",
                               ts = Sys.time()),
        chat.api::chat_message(id = "$m2", channel = "!r:ex",
                               sender = "@troy:ex", body = "in thread a",
                               ts = Sys.time(), thread = "$a"),
        chat.api::chat_message(id = "$m3", channel = "!r:ex",
                               sender = "@troy:ex", body = "also thread a",
                               ts = Sys.time(), thread = "$a"),
        chat.api::chat_message(id = "$m4", channel = "!r:ex",
                               sender = "@troy:ex", body = "in thread b",
                               ts = Sys.time(), thread = "$b"))
    orig_hist <- chat.api::chat_history
    orig_chan <- chat.api::chat_channels
    assignInNamespace("chat_history",
                      function(client, channel, limit = 50L, cursor = NULL,
                               ...) list(messages = hist, cursor = NULL),
                      ns = "chat.api")
    assignInNamespace("chat_channels", function(client, ...) "!r:ex",
                      ns = "chat.api")
    on.exit({
        assignInNamespace("chat_history", orig_hist, ns = "chat.api")
        assignInNamespace("chat_channels", orig_chan, ns = "chat.api")
    }, add = TRUE)

    reg <- corteza:::bot_new_session_registry()
    corteza:::bot_backfill_sessions(lo, reg, test_cfg())
    keys <- sort(ls(reg, all.names = TRUE))
    expect_identical(keys, sort(c("!r:ex",
                                  corteza:::bot_session_key("!r:ex", "$a"),
                                  corteza:::bot_session_key("!r:ex", "$b"))))
    body_of <- function(key) {
        vapply(get(key, envir = reg)$history,
               function(h) h$content, character(1))
    }
    # The room's own session holds only what was on its main timeline.
    expect_true(any(grepl("main timeline", body_of("!r:ex"), fixed = TRUE)))
    expect_false(any(grepl("in thread a", body_of("!r:ex"), fixed = TRUE)))
    expect_false(any(grepl("in thread b", body_of("!r:ex"), fixed = TRUE)))
    # And each thread holds its own, both messages of it.
    a <- body_of(corteza:::bot_session_key("!r:ex", "$a"))
    expect_identical(length(a), 2L)
    expect_true(any(grepl("also thread a", a, fixed = TRUE)))
    expect_false(any(grepl("in thread b", a, fixed = TRUE)))
    # Every session knows the room it speaks into, so archival and
    # sends still target a real room id rather than a composite key.
    for (k in keys) {
        expect_identical(get(k, envir = reg)$room_id, "!r:ex")
    }
})

# Backfilled thread sessions are seeded from the archive too, and the
# archive lands in front of the backfilled window -- it is the older
# context of the two.
local({
    lo <- chat.api::chat_loopback()
    f <- tempfile(fileext = ".md")
    writeLines(c("## user", "", "the archived conversation"), f)
    chat.api::chat_set_state(lo, "!r:ex", "ai.cornball.fold",
                             list(segment = "!seg:ex", vault = f),
                             state_key = "$a")
    hist <- list(chat.api::chat_message(id = "$m1", channel = "!r:ex",
                                        sender = "@troy:ex",
                                        body = "recent thread turn",
                                        ts = Sys.time(), thread = "$a"))
    orig_hist <- chat.api::chat_history
    orig_chan <- chat.api::chat_channels
    assignInNamespace("chat_history",
                      function(client, channel, limit = 50L, cursor = NULL,
                               ...) list(messages = hist, cursor = NULL),
                      ns = "chat.api")
    assignInNamespace("chat_channels", function(client, ...) "!r:ex",
                      ns = "chat.api")
    on.exit({
        assignInNamespace("chat_history", orig_hist, ns = "chat.api")
        assignInNamespace("chat_channels", orig_chan, ns = "chat.api")
    }, add = TRUE)

    reg <- corteza:::bot_new_session_registry()
    corteza:::bot_backfill_sessions(lo, reg, test_cfg(),
                                    thread_context = fold_lookup)
    s <- get(corteza:::bot_session_key("!r:ex", "$a"), envir = reg)
    bodies <- vapply(s$history, function(h) h$content, character(1))
    expect_true(grepl("the archived conversation", bodies[[1L]], fixed = TRUE))
    expect_true(grepl("recent thread turn", bodies[[length(bodies)]],
                      fixed = TRUE))
    # Considered once: the live path must not seed it a second time.
    expect_true(isTRUE(s$rehydrate_checked))
    expect_false(corteza:::bot_maybe_rehydrate(s, lo, "!r:ex", "$a",
                                               fold_lookup))

    # Without the hook, backfill leaves the thread's window as it is.
    reg2 <- corteza:::bot_new_session_registry()
    corteza:::bot_backfill_sessions(lo, reg2, test_cfg())
    s2 <- get(corteza:::bot_session_key("!r:ex", "$a"), envir = reg2)
    expect_identical(length(s2$history), 1L)
})
