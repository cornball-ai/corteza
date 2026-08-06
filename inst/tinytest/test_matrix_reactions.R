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
approve <- corteza:::matrix_approve_keys(list())
deny <- corteza:::matrix_deny_keys(list())
verdict <- function(reactions, room = "!r:ex", target = "$target") {
    corteza:::matrix_reaction_verdict(reactions, room, target, approve, deny)
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
expect_identical(corteza:::matrix_approve_keys(list(approve_keys = "si")), "si")
expect_identical(corteza:::matrix_deny_keys(list(deny_keys = "nein")), "nein")

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
expect_false("matrix_extract_reaction_verdict" %in%
             ls(asNamespace("corteza"), all.names = TRUE))

# --- The approval loop ---
# Driven through chat.api's seams, so nothing here reaches a homeserver.
# matrix_chat_client() is swapped for one that layers them on, the same
# way test_matrix_transport.R does.

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
        orig <- corteza:::matrix_chat_client
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
        assignInNamespace("matrix_chat_client", stub, ns = "corteza")
        stub
    }
    restore_client <- function() {
        assignInNamespace("matrix_chat_client",
                          get("matrix_chat_client",
                              envir = asNamespace("corteza")), ns = "corteza")
    }
    cfg <- list(server = "https://ex.invalid", user = "bot", token = "tok",
                user_id = "@bot:ex", device_id = "DEV",
                room_id = "!default:ex")
    a_call <- list(name = "bash", args = list(cmd = "ls"))
    a_dec <- list(reason = "ask")

    # A thumbs-up from a human approves.
    local({
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1",
                                      sync = rx_event(intToUtf8(0x1F44D)))))
        expect_true(corteza:::matrix_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 5L))
    })

    # A thumbs-down declines.
    local({
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1",
                                      sync = rx_event(intToUtf8(0x1F44E)))))
        expect_false(corteza:::matrix_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 5L))
    })

    # The loop's own cursor never touches the bot's. A client built
    # save_cursor = TRUE here would persist the approval's position and
    # the next matrix_poll() would resume past everything that arrived
    # while the user was deciding.
    local({
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        rec <- new.env(); rec$polls <- list(); rec$save_cursor <- logical()
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1",
                                      sync = rx_event("yes")),
                                 record = rec),
                        record = rec)
        corteza:::matrix_reaction_approval(cfg, a_call, a_dec,
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
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
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
        o <- corteza:::matrix_chat_client
        assignInNamespace("matrix_chat_client", function(cfg,
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
        corteza:::matrix_reaction_approval(cfg, a_call, a_dec,
                                           room_id = "!session:ex",
                                           timeout_sec = 5L)
        expect_identical(order[1:2], c("poll", "send"))
        # Both seeds go out, after the prompt and before any further poll.
        expect_identical(order[3:4], c("react", "react"))
    })

    # Both seed reactions are placed, on the prompt, in the session's
    # room -- not the config's default room.
    local({
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        reacted <- new.env(); reacted$args <- list()
        sent <- new.env(); sent$args <- list()
        approval_client(scripted(list(cursor = "s0"),
                                 list(cursor = "s1", sync = rx_event("yes"))),
                        sent = sent, reacted = reacted)
        corteza:::matrix_reaction_approval(cfg, a_call, a_dec,
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
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
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
        expect_true(corteza:::matrix_reaction_approval(
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
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        rec <- new.env(); rec$polls <- list()
        approval_client(scripted(
            list(cursor = "s0"),
            list(cursor = "s1", first_run = TRUE,
                 sync = rx_event(intToUtf8(0x1F44E))),
            list(cursor = "s2", sync = rx_event(intToUtf8(0x1F44D))),
            record = rec), record = rec)
        # The backfilled thumbs-down is not read; the live thumbs-up is.
        expect_true(corteza:::matrix_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 10L))
        since <- vapply(rec$polls, function(p) p$since %||% NA_character_,
                        character(1))
        expect_identical(since[3L], "s1")
    })

    # The bot's own seeded reactions never answer their own prompt.
    local({
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        approval_client(scripted(
            list(cursor = "s0"),
            list(cursor = "s1", sync = rx_event(intToUtf8(0x1F44D),
                                                sender = "@bot:ex"))))
        # Times out to FALSE rather than approving on its own seed.
        expect_false(corteza:::matrix_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 1L))
    })

    # A send that fails declines rather than waiting for a reaction to a
    # message nobody received.
    local({
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        o <- corteza:::matrix_chat_client
        assignInNamespace("matrix_chat_client", function(cfg,
                                                         save_cursor = TRUE,
                                                         ...) {
            o(cfg, save_cursor = save_cursor,
              .sync = scripted(list(cursor = "s0")),
              .send = function(...) NULL, .media = function(...) NULL,
              .react = function(...) "$s", ...)
        }, ns = "corteza")
        expect_false(corteza:::matrix_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 5L))
    })

    # No verdict before the deadline declines.
    local({
        orig <- corteza:::matrix_chat_client
        on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"))
        approval_client(scripted(list(cursor = "s0"), list(cursor = "s1")))
        expect_false(corteza:::matrix_reaction_approval(
            cfg, a_call, a_dec, room_id = "!session:ex", timeout_sec = 1L))
    })

    # corteza no longer runs its own sync loop or calls mx_react.
    src <- paste(deparse(body(corteza:::matrix_reaction_approval)),
                 collapse = " ")
    expect_false(grepl("mx_sync", src, fixed = TRUE))
    expect_false(grepl("mx_react", src, fixed = TRUE))
    expect_true(grepl("chat_poll", src, fixed = TRUE))
    expect_true(grepl("chat_react", src, fixed = TRUE))
    expect_true(grepl("save_cursor = FALSE", src, fixed = TRUE))
}
