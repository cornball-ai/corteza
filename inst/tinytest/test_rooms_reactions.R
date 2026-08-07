library(tinytest)

# The approve/deny vocabulary is corteza's, and the reaction records it
# reads come from chat.api. mx.client ships its own verdict extractor
# with these keys baked in; corteza no longer calls it, because which
# emoji means yes belongs next to the prompt that teaches a user to tap
# it.
if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not available")
}
if (!"chat_reaction" %in% getNamespaceExports("chat.api")) {
    exit_file("chat.api predates chat_reaction()")
}

rx <- function(key, target = "$target", room = "!r:ex", self = FALSE,
               sender = "@alice:ex", id = "$r1") {
    chat.api::chat_reaction(id = id, channel = room, sender = sender,
                            target = target, key = key,
                            ts = as.POSIXct(NA), self = self)
}
approve <- corteza:::bot_approve_keys(list())
deny <- corteza:::bot_deny_keys(list())
verdict <- function(reactions, room = "!r:ex", target = "$target") {
    corteza:::bot_reaction_verdict(reactions, room, target, approve, deny)
}

# --- The key vocabulary lives here ---

expect_true(intToUtf8(0x1F44D) %in% approve)
expect_true(intToUtf8(0x2705) %in% approve)
expect_true(all(c("y", "yes", "ok") %in% approve))
expect_true(intToUtf8(0x1F44E) %in% deny)
expect_true(intToUtf8(0x274C) %in% deny)
expect_true(all(c("n", "no", "nope") %in% deny))
# No key means both things.
expect_equal(length(intersect(approve, deny)), 0L)
# A config can override either: which emoji a team uses is theirs.
expect_identical(corteza:::bot_approve_keys(list(approve_keys = "si")), "si")
expect_identical(corteza:::bot_deny_keys(list(deny_keys = "nein")), "nein")

# --- Reading a verdict ---

# Nothing to read.
expect_null(verdict(list()))
# Thumbs up from a human.
expect_true(verdict(list(rx(intToUtf8(0x1F44D)))))
expect_true(verdict(list(rx("yes"))))
# Thumbs down.
expect_false(verdict(list(rx(intToUtf8(0x1F44E)))))
expect_false(verdict(list(rx("no"))))
# A key in neither list is not a verdict, and does not block a later one.
expect_null(verdict(list(rx(intToUtf8(0x1F440)))))
expect_true(verdict(list(rx("shrug"), rx("yes"))))

# The loop seeds both reactions itself so the user can tap rather than
# type. Counting them would approve every request the instant it was
# asked, which is the most important thing this filter does.
expect_null(verdict(list(rx(intToUtf8(0x1F44D), self = TRUE),
                         rx(intToUtf8(0x1F44E), self = TRUE))))
# ... and a human tapping after the seeds still registers.
expect_true(verdict(list(rx(intToUtf8(0x1F44D), self = TRUE),
                         rx(intToUtf8(0x1F44E), self = TRUE),
                         rx(intToUtf8(0x1F44D), self = FALSE))))

# A reaction on some other message is not an answer to this prompt.
expect_null(verdict(list(rx("yes", target = "$other"))))
# Nor is one in another room. The prompt goes to the session's room,
# which is not the config's default room in any room but one -- reading
# the wrong room meant approvals outside the default room could only
# ever time out.
expect_null(verdict(list(rx("yes", room = "!elsewhere:ex"))))
expect_true(verdict(list(rx("yes", room = "!other:ex"), rx("yes")),
                    room = "!r:ex"))

# First verdict wins, in the order the homeserver reported them: a
# thumbs-down after a thumbs-up does not overturn it, and the reverse
# holds too.
expect_true(verdict(list(rx(intToUtf8(0x1F44D)), rx(intToUtf8(0x1F44E)))))
expect_false(verdict(list(rx(intToUtf8(0x1F44E)), rx(intToUtf8(0x1F44D)))))

# --- The mx.client passthrough is gone ---
# It delegated the approve/deny vocabulary to the transport package,
# which is exactly where it should not live.
expect_false("bot_extract_reaction_verdict" %in%
             ls(asNamespace("corteza"), all.names = TRUE))

# --- The approval loop ---
# Driven through chat.api's seams, so nothing here reaches a homeserver.
# bot_chat_client() is swapped for one that layers them on, the same
# way test_rooms_transport.R does.

if (requireNamespace("mx.client", quietly = TRUE)) {

    # A scripted sequence of poll results. Each call returns the next
    # entry; the last repeats, so a loop that keeps polling keeps seeing
    # an unhelpful answer rather than falling off the end.
    scripted <- function(..., record = NULL) {
        steps <- list(...)
        i <- 0L
        function(client, timeout = 0L, save = TRUE, ...) {
            i <<- i + 1L
            step <- steps[[min(i, length(steps))]]
            if (!is.null(record)) {
                record$polls[[length(record$polls) + 1L]] <- list(
                    since = client$sync_token, save = save)
            }
            client$sync_token <- step$cursor
            list(sync = step$sync %||% list(rooms = list(join = list())),
                 client = client, first_run = isTRUE(step$first_run))
        }
    }
    rx_event <- function(key, target = "$prompt", sender = "@alice:ex",
                         room = "!session:ex", id = "$r1") {
        join <- list(list(timeline = list(events = list(list(
            type = "m.reaction", event_id = id, sender = sender,
            origin_server_ts = 1700000000000,
            content = list(`m.relates_to` = list(rel_type = "m.annotation",
                                                 event_id = target,
                                                 key = key)))))))
        names(join) <- room
        list(rooms = list(join = join))
    }

    approval_client <- function(sync_fn, record = NULL, sent = NULL,
                                reacted = NULL) {
        orig <- corteza:::bot_chat_client
        stub <- function(cfg, save_cursor = TRUE, ...) {
            if (!is.null(record)) {
                record$save_cursor <- c(record$save_cursor, save_cursor)
            }
            orig(cfg, save_cursor = save_cursor,
                 .sync = sync_fn,
                 .send = function(client, text, room = NULL, ...) {
                     if (!is.null(sent)) {
                         sent$args[[length(sent$args) + 1L]] <- list(
                             room = room, text = text)
                     }
                     "$prompt"
                 },
                 .media = function(...) NULL,
                 .react = function(session, room_id, event_id, key) {
                     if (!is.null(reacted)) {
                         reacted$args[[length(reacted$args) + 1L]] <- list(
                             room_id = room_id, event_id = event_id, key = key)
                     }
                     paste0("$seed-", key)
                 },
                 ...)
        }
        assignInNamespace("bot_chat_client", stub, ns = "corteza")
        stub
    }
    cfg <- list(server = "https://ex.invalid", user = "bot", token = "tok",
                user_id = "@bot:ex", device_id = "DEV",
                room_id = "!default:ex")
    a_call <- list(name = "bash", args = list(cmd = "ls"))
    a_dec <- list(reason = "ask")

    # A thumbs-up from a human approves.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1",
                                      sync = rx_event(intToUtf8(0x1F44D)))))
        expect_true(corteza:::bot_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 5L))
    })

    # A thumbs-down declines.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1",
                                      sync = rx_event(intToUtf8(0x1F44E)))))
        expect_false(corteza:::bot_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 5L))
    })

    # The loop's own cursor never touches the bot's. A client built
    # save_cursor = TRUE here would persist the approval's position and
    # the next bot_poll() would resume past everything that arrived
    # while the user was deciding.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        rec <- new.env(); rec$polls <- list(); rec$save_cursor <- logical()
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1",
                                      sync = rx_event("yes")),
                                 record = rec),
                        record = rec)
        corteza:::bot_reaction_approval(cfg, a_call, a_dec,
                                           room_id = "!session:ex",
                                           timeout_sec = 5L)
        expect_true(all(!rec$save_cursor))
        # ... and the seam is told not to save, on every poll.
        expect_true(all(!vapply(rec$polls, function(p) isTRUE(p$save),
                                logical(1))))
    })

    # The baseline is taken before the prompt is sent, so a reaction
    # placed the instant the prompt lands cannot fall into the discarded
    # baseline sync. The first poll happens with no prompt sent yet.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        order <- character()
        rec <- new.env(); rec$polls <- list()
        sent <- new.env(); sent$args <- list()
        steps <- list(list(cursor = "s0"),
                      list(cursor = "s1", sync = rx_event("yes")))
        i <- 0L
        stub_sync <- function(client, timeout = 0L, save = TRUE, ...) {
            i <<- i + 1L
            order <<- c(order, "poll")
            step <- steps[[min(i, length(steps))]]
            client$sync_token <- step$cursor
            list(sync = step$sync %||% list(rooms = list(join = list())),
                 client = client, first_run = FALSE)
        }
        o <- corteza:::bot_chat_client
        assignInNamespace("bot_chat_client", function(cfg,
                                                         save_cursor = TRUE,
                                                         ...) {
            o(cfg, save_cursor = save_cursor, .sync = stub_sync,
              .send = function(client, text, room = NULL, ...) {
                  order <<- c(order, "send")
                  "$prompt"
              },
              .media = function(...) NULL,
              .react = function(...) { order <<- c(order, "react"); "$s" },
              ...)
        }, ns = "corteza")
        corteza:::bot_reaction_approval(cfg, a_call, a_dec,
                                           room_id = "!session:ex",
                                           timeout_sec = 5L)
        expect_identical(order[1:2], c("poll", "send"))
        # Both seeds go out, after the prompt and before any further poll.
        expect_identical(order[3:4], c("react", "react"))
    })

    # Both seed reactions are placed, on the prompt, in the session's
    # room -- not the config's default room.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        reacted <- new.env(); reacted$args <- list()
        sent <- new.env(); sent$args <- list()
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1", sync = rx_event("yes"))),
                        sent = sent, reacted = reacted)
        corteza:::bot_reaction_approval(cfg, a_call, a_dec,
                                           room_id = "!session:ex",
                                           timeout_sec = 5L)
        expect_identical(length(reacted$args), 2L)
        expect_identical(reacted$args[[1L]]$key, intToUtf8(0x1F44D))
        expect_identical(reacted$args[[2L]]$key, intToUtf8(0x1F44E))
        expect_identical(reacted$args[[1L]]$event_id, "$prompt")
        expect_identical(reacted$args[[1L]]$room_id, "!session:ex")
        expect_identical(sent$args[[1L]]$room, "!session:ex")
    })

    # Unrelated traffic advances the cursor. A poll that returns someone
    # else's reaction, or none at all, still has to move `since` -- or
    # the next poll asks for the same batch and the loop spins to the
    # deadline without seeing the verdict that arrives after it.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        rec <- new.env(); rec$polls <- list()
        approval_client(scripted(
            list(cursor = "s0"),
            # unrelated: a reaction on another message
            list(cursor = "s1", sync = rx_event("yes", target = "$other")),
            # unrelated: nothing at all
            list(cursor = "s2"),
            # the answer
            list(cursor = "s3", sync = rx_event("yes")),
            record = rec), record = rec)
        expect_true(corteza:::bot_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 10L))
        # Each poll asked from where the previous one ended.
        since <- vapply(rec$polls, function(p) p$since %||% NA_character_,
                        character(1))
        expect_identical(since[2:4], c("s0", "s1", "s2"))
    })

    # A first_run means the cursor was lost and the homeserver sent a
    # backfill window. Its reactions are history, not an answer to a
    # prompt sent seconds ago -- but the cursor still advances, so the
    # next poll is live and can answer.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        rec <- new.env(); rec$polls <- list()
        approval_client(scripted(
            list(cursor = "s0"),
            list(cursor = "s1", first_run = TRUE,
                 sync = rx_event(intToUtf8(0x1F44E))),
            list(cursor = "s2", sync = rx_event(intToUtf8(0x1F44D))),
            record = rec), record = rec)
        # The backfilled thumbs-down is not read; the live thumbs-up is.
        expect_true(corteza:::bot_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 10L))
        since <- vapply(rec$polls, function(p) p$since %||% NA_character_,
                        character(1))
        expect_identical(since[3L], "s1")
    })

    # The bot's own seeded reactions never answer their own prompt.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        approval_client(scripted(
            list(cursor = "s0"),
            list(cursor = "s1", sync = rx_event(intToUtf8(0x1F44D),
                                                sender = "@bot:ex"))))
        # Times out to FALSE rather than approving on its own seed.
        expect_false(corteza:::bot_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 1L))
    })

    # A send that fails declines rather than waiting for a reaction to a
    # message nobody received.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        o <- corteza:::bot_chat_client
        assignInNamespace("bot_chat_client", function(cfg,
                                                         save_cursor = TRUE,
                                                         ...) {
            o(cfg, save_cursor = save_cursor,
              .sync = scripted(list(cursor = "s0")),
              .send = function(...) NULL, .media = function(...) NULL,
              .react = function(...) "$s", ...)
        }, ns = "corteza")
        expect_false(corteza:::bot_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 5L))
    })

    # No verdict before the deadline declines.
    local({
        orig <- corteza:::bot_chat_client
        on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"))
        approval_client(scripted(list(cursor = "s0"), list(cursor = "s1")))
        expect_false(corteza:::bot_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 1L))
    })

    # corteza no longer runs its own sync loop or calls mx_react.
    src <- paste(deparse(body(corteza:::bot_reaction_approval)),
                 collapse = " ")
    expect_false(grepl("mx_sync", src, fixed = TRUE))
    expect_false(grepl("mx_react", src, fixed = TRUE))
    expect_true(grepl("chat_poll", src, fixed = TRUE))
    expect_true(grepl("chat_react", src, fixed = TRUE))
    expect_true(grepl("save_cursor = FALSE", src, fixed = TRUE))
}

# --- Channel metadata through the contract ---
# Four call sites used to make five state lookups between them, with the
# topic fetched twice for one session. They now share one verb.

if (requireNamespace("chat.api", quietly = TRUE) &&
    "chat_channel_info" %in% getNamespaceExports("chat.api")) {

    info_of <- corteza:::bot_channel_info

    # No client is a real path: archiving from a registry with no live
    # transport. It answers empty rather than erroring.
    expect_identical(info_of(NULL, "!r:ex"),
                     list(id = "!r:ex", name = NULL, topic = NULL))
    # So is no room.
    expect_null(info_of(structure(list(), class = "chat_nothing"), NULL)$name)

    # "Cannot ask" is absorbed here, once, rather than at four call sites
    # that would each have to remember. Every caller wants the same
    # answer from it -- fall back to a default -- so the distinction the
    # contract draws between an error and a NULL field is collapsed
    # deliberately, in one place.
    local({
        cl <- structure(list(), class = c("chat_boom", "chat_client"))
        registerS3method("chat_channel_info", "chat_boom",
                         function(client, channel, ...) stop("403"),
                         envir = asNamespace("chat.api"))
        expect_identical(info_of(cl, "!r:ex"),
                         list(id = "!r:ex", name = NULL, topic = NULL))
    })

    # A real answer comes through untouched.
    local({
        cl <- structure(list(), class = c("chat_ok", "chat_client"))
        registerS3method("chat_channel_info", "chat_ok",
                         function(client, channel, ...) {
                             list(id = channel, name = "The Lab",
                                  topic = "~/lab | a place")
                         }, envir = asNamespace("chat.api"))
        expect_identical(corteza:::bot_room_name(cl, "!r:ex"), "The Lab")
        expect_identical(info_of(cl, "!r:ex")$topic, "~/lab | a place")
    })

    # --- bot_room_cwd takes a topic, not a session ---
    # It used to fetch one itself, which was the third state read for a
    # single session and the second read of the same topic.
    local({
        cfg <- list(user_id = "@bot:ex", user = "bot")
        default <- corteza:::bot_default_cwd(cfg)
        # No topic is the default.
        expect_identical(corteza:::bot_room_cwd(cfg, NULL), default)
        # A topic with no cwd part is the default too.
        expect_identical(corteza:::bot_room_cwd(cfg, "just a description"),
                         default)
        # A cwd that does not exist falls back, and says so.
        expect_message(
            got <- corteza:::bot_room_cwd(cfg, "/nonexistent/xyzzy | d"),
            "does not exist")
        expect_identical(got, default)
        # A cwd that does exist is used.
        d <- tempfile("roomcwd"); dir.create(d)
        on.exit(unlink(d, recursive = TRUE))
        expect_identical(corteza:::bot_room_cwd(cfg, paste0(d, " | desc")),
                         d)
        # The signature no longer takes a session.
        expect_false("mx_sess" %in% names(formals(corteza:::bot_room_cwd)))
        expect_true("topic" %in% names(formals(corteza:::bot_room_cwd)))
    })

    # --- The members cache reads chat_members() ---
    local({
        cl <- structure(list(), class = c("chat_mem", "chat_client"))
        asked <- character()
        registerS3method("chat_members", "chat_mem",
                         function(client, channel, ...) {
                             asked <<- c(asked, channel)
                             c("@a:ex", "@b:ex")
                         }, envir = asNamespace("chat.api"))
        s <- new.env(parent = emptyenv())
        got <- corteza:::bot_room_members_cached(s, "!r:ex", chat = cl)
        expect_identical(got, c("@a:ex", "@b:ex"))
        expect_identical(asked, "!r:ex")
        # Cached on the second call, not re-fetched.
        corteza:::bot_room_members_cached(s, "!r:ex", chat = cl)
        expect_identical(length(asked), 1L)
    })

    # A failed lookup keeps the previous cache rather than emptying it.
    # chat_members() raises rather than returning character() precisely
    # so this can tell the two apart.
    local({
        cl <- structure(list(), class = c("chat_memfail", "chat_client"))
        registerS3method("chat_members", "chat_memfail",
                         function(client, channel, ...) stop("403"),
                         envir = asNamespace("chat.api"))
        s <- new.env(parent = emptyenv())
        s$members <- c("@a:ex")
        s$members_at <- Sys.time() - 10000
        expect_identical(
            corteza:::bot_room_members_cached(s, "!r:ex", chat = cl),
            "@a:ex")
    })

    # No client at all yields no members rather than an error.
    local({
        s <- new.env(parent = emptyenv())
        expect_identical(
            corteza:::bot_room_members_cached(s, "!r:ex", chat = NULL),
            character())
    })

    # --- corteza no longer reads room metadata off mx.api ---
    src <- paste(vapply(c("bot_archive_session", "bot_room_cwd",
                          "bot_room_members_cached", "bot_new_session"),
                        function(f) paste(deparse(body(get(f,
                            envir = asNamespace("corteza")))), collapse = " "),
                        character(1)), collapse = " ")
    for (gone in c("mx_room_name", "mx_room_topic", "mx_room_members")) {
        expect_false(grepl(gone, src, fixed = TRUE), info = gone)
    }
    # And bot_new_session asks once, not three times.
    ns <- paste(deparse(body(corteza:::bot_new_session)), collapse = " ")
    expect_identical(lengths(regmatches(ns,
                                        gregexpr("bot_channel_info", ns)))[[1]],
                     1L)
    # The topic it got is the one the cwd is derived from. Asserted on
    # the source because reaching bot_new_session() for real runs
    # session_setup(), which wants a provider API key -- and the wiring
    # is the whole point: passing NULL here would silently send every
    # room back to the default directory while the single lookup above
    # still looked correct.
    expect_true(grepl("bot_room_cwd(cfg, info$topic)", ns, fixed = TRUE))
    expect_true(grepl("room_name = info$name", ns, fixed = TRUE))
    expect_true(grepl("bot_parse_topic(info$topic)", ns, fixed = TRUE))
}
