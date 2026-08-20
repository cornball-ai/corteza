library(tinytest)

# Threaded sessions and rehydration: a reply in a folded conversation's
# thread continues that conversation instead of starting a blank one.

if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}
if (!("chat_get_state" %in% getNamespaceExports("chat.api"))) {
    exit_file("chat.api too old: no chat_get_state")
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
    cfg <- list(room_id = "!r:ex", model = "claude-sonnet-4-6",
                provider = "anthropic")
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
    cfg <- list(room_id = "!r:ex")
    corteza:::bot_get_or_create_session(reg, "!r:ex", cfg)
    corteza:::bot_get_or_create_session(
        reg, corteza:::bot_session_key("!r:ex", "$a"), cfg,
        room_id = "!r:ex")
    corteza::bot_archive_all(reg)
    expect_identical(sort(unique(unlist(seen))), "!r:ex")
    expect_false(any(grepl("\r", unlist(seen), fixed = TRUE)))
})

# ---- Finding the archive a thread stands for ----
local({
    lo <- chat.api::chat_loopback()
    # A thread nobody folded into is not a fold, and answers NULL
    # rather than erroring.
    expect_null(corteza:::bot_thread_archive(lo, "topic", "$root"))
    expect_null(corteza:::bot_thread_archive(lo, "topic", NULL))
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex",
                                  vault = "raw/matrix/seg.md"),
                             state_key = "$root")
    got <- corteza:::bot_thread_archive(lo, "topic", "$root")
    expect_identical(got$vault, "raw/matrix/seg.md")
    expect_identical(got$segment, "!seg:ex")
    # Keyed by the root: a different thread in the same room is a
    # different fold, and an unindexed one is not a fold at all.
    expect_null(corteza:::bot_thread_archive(lo, "topic", "$other"))
    # An index without a usable pointer is no archive.
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex"), state_key = "$bare")
    expect_null(corteza:::bot_thread_archive(lo, "topic", "$bare"))
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
                                                         "$root"),
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
    expect_false(corteza:::bot_rehydrate_session(s2, lo, "topic", "$nope"))
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
                                                          "$missing"),
                   "unreadable")
    expect_false(res)
    expect_identical(length(s3$history), 0L)
})

# ---- A transport without durable state ----
# rehydration is a no-op rather than an error, so a bot on a transport
# that has no state events keeps answering.
local({
    irc <- structure(list(env = new.env(parent = emptyenv()), nick = "bot"),
                     class = c("chat_irc", "chat_client"))
    expect_null(corteza:::bot_thread_archive(irc, "#lab", "$root"))
})
