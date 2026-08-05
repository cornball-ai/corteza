library(tinytest)

# Regression tests for the four invariants that had to survive moving
# corteza's Matrix message plane onto the chat.api transport contract:
#
#   1. token-expiry relogin      -- the sync still runs inside
#                                   mx.client::mx_with_relogin()
#   2. sync cursor persistence   -- the client is built save_cursor =
#                                   TRUE, so mx.client still writes the
#                                   advanced cursor inside the sync
#                                   call, before anything parses it
#   3. initial-sync suppression  -- first_run survives the trip out of
#                                   the adapter and still short-circuits
#                                   matrix_poll()
#   4. E2EE goes through the      -- the adapter owns the Olm account and
#      adapter                      both directions of encrypted traffic.
#                                   corteza builds no crypto state and
#                                   has no second send path. It used to
#                                   run mx.crypto itself off $raw.
#
# Everything here runs offline. Nothing hits a homeserver: the sync is
# supplied through chat_matrix()'s .sync/.extract seams, reached via
# matrix_chat_client()'s `...`.

if (!requireNamespace("mx.client", quietly = TRUE)) {
    exit_file("mx.client not available")
}
if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not available")
}
# chat.api 0.0.1.1 bumped its version across the change that added
# first_run, the post-sync client, and the seams, and
# matrix_require_mx() now refuses anything older. This check stays
# because it is more specific than a version comparison: a build that
# reports a new version without carrying the change -- which is exactly
# what happened to mx.client on this host -- passes the version gate and
# fails here. An installed-but-too-old chat.api is a broken install, not
# an unsupported environment: every matrix_poll() on that host dies in
# the guard at R/matrix.R. Report it as the failure it is, then stop --
# continuing would bury the one useful result under a cascade of
# identical ones.
adapter_ok <- all(c(".sync", "relogin") %in%
    names(formals(chat.api::chat_matrix)))
expect_true(adapter_ok,
            info = "installed chat.api predates the seamed Matrix adapter")
if (!adapter_ok) {
    exit_file("chat.api predates the seamed Matrix adapter")
}

# Point every config-path input at a tempdir. matrix_config_path()
# consults CORTEZA_MATRIX_CONFIG first, then tools::R_user_dir(), which
# reads R_USER_CONFIG_DIR and XDG_CONFIG_HOME before HOME. Miss one and
# a test writes over a running bot's live credentials.
isolate_config <- function(cfg) {
    tmp_home <- tempfile("home-")
    dir.create(tmp_home)
    vars <- c(HOME = tmp_home,
              CORTEZA_MATRIX_CONFIG = file.path(tmp_home, "matrix.json"),
              R_USER_CONFIG_DIR = file.path(tmp_home, "config"),
              XDG_CONFIG_HOME = file.path(tmp_home, "config"))
    orig <- Sys.getenv(names(vars), unset = NA)
    do.call(Sys.setenv, as.list(vars))
    corteza:::matrix_save_config(cfg)
    list(home = tmp_home, orig = orig)
}

restore_config <- function(iso) {
    keep <- iso$orig[!is.na(iso$orig)]
    if (length(keep)) {
        do.call(Sys.setenv, as.list(keep))
    }
    drop <- names(iso$orig)[is.na(iso$orig)]
    if (length(drop)) {
        Sys.unsetenv(drop)
    }
    unlink(iso$home, recursive = TRUE)
}

# sync_token = NULL drops the key rather than storing a NULL. That is
# not cosmetic: jsonlite writes a NULL member as `{}` and reads it back
# as an empty list, and first_run is decided by is.null() on that field.
# A config carrying the key at all is therefore never a first run.
#
# user_id is a parameter because chat.api interns one crypto context per
# identity, and the identity is the store path, which corteza derives
# from the user id. Two e2ee tests sharing a user id share a context: the
# second gets the first's encrypted-room set and routes its sends the
# wrong way. Under R CMD check that is not hypothetical -- tests/tinytest.R
# pins R_USER_DATA_DIR for the whole run, so the store path no longer
# varies with the per-test HOME.
base_cfg <- function(sync_token = "s1", e2ee = FALSE,
                     user_id = "@bot:example") {
    cfg <- list(server = "https://example", user = "bot", password = "pw",
                token = "tok", user_id = user_id, device_id = "DEV",
                room_id = "!room:example")
    if (!is.null(sync_token)) {
        cfg$sync_token <- sync_token
    }
    if (isTRUE(e2ee)) {
        cfg$e2ee <- TRUE
    }
    cfg
}

# A .crypto seam for chat.api's adapter, so the E2EE paths run here
# without mx.crypto or a Rust toolchain. `decrypted` is what the adapter
# will report as having come out of an encrypted room.
crypto_seam <- function(decrypted = list(), encrypted_rooms = "!secret:example",
                        event_id = "$enc:example") {
    log <- new.env(parent = emptyenv())
    log$sent <- list()
    log$decrypts <- 0L
    log$inits <- 0L
    ctx <- new.env(parent = emptyenv())
    ctx$encrypted <- encrypted_rooms
    ops <- list(
        init = function(mx, store = NULL, app = NULL) {
            log$inits <- log$inits + 1L
            log$store <- store
            ctx
        },
        encrypted = function(crypto, mx, room_id) room_id %in% crypto$encrypted,
        send = function(crypto, mx, room_id, text, ...) {
            log$sent[[length(log$sent) + 1L]] <- list(room_id = room_id,
                                                      text = text, mx = mx,
                                                      extra = list(...))
            event_id
        },
        decrypt = function(crypto, sync, mx) {
            log$decrypts <- log$decrypts + 1L
            decrypted
        })
    list(ops = ops, log = log)
}

# A session registry with the named rooms already in it. Any test that
# lets a message reach the loop needs one: matrix_get_or_create_session()
# builds a session by asking the homeserver for the room name, which is
# one of the direct mx.api calls the contract does not model yet.
seeded_sessions <- function(rooms) {
    sessions <- corteza:::matrix_new_session_registry()
    for (rid in rooms) {
        s <- new.env(parent = emptyenv())
        s$model <- "qwen3:8b"
        s$provider <- "ollama"
        s$default_model <- "qwen3:8b"
        s$default_provider <- "ollama"
        s$history <- list()
        s$transcript <- list()
        s$seen_event_ids <- character()
        assign(rid, s, envir = sessions)
    }
    sessions
}

# A sync response with one message from a human, so a test that means to
# prove suppression is proving it against traffic that would otherwise
# be processed.
sync_with_message <- function(next_batch = "s2") {
    list(next_batch = next_batch,
         rooms = list(join = list(`!room:example` = list(timeline = list(
             events = list(list(type = "m.room.message",
                                event_id = "$ev1:example",
                                sender = "@ann:example",
                                origin_server_ts = 1700000000000,
                                content = list(msgtype = "m.text",
                                               body = "hello")))
         )))))
}

# Swap matrix_chat_client() for one that layers seams onto the real
# wrapper. Calling the captured original is what keeps save_cursor =
# FALSE and relogin = TRUE under test instead of re-declaring them here.
with_seamed_client <- function(seams, expr) {
    orig <- corteza:::matrix_chat_client
    stub <- function(cfg, ...) {
        do.call(orig, c(list(cfg), seams, list(...)))
    }
    assignInNamespace("matrix_chat_client", stub, ns = "corteza")
    on.exit(assignInNamespace("matrix_chat_client", orig, ns = "corteza"),
            add = TRUE)
    force(expr)
}

# mx.api signals HTTP errors as conditions classed
# c("mx_error_<CODE>", "mx_error", "error", "condition") -- see
# mx_raise() in mx.api. mx_with_relogin() catches the UNKNOWN_TOKEN one
# by class, so the class vector is the whole contract here.
token_rejected <- function() {
    stop(structure(class = c("mx_error_M_UNKNOWN_TOKEN", "mx_error",
                             "error", "condition"),
                   list(message = "Matrix error [M_UNKNOWN_TOKEN]: bad token",
                        call = NULL, errcode = "M_UNKNOWN_TOKEN")))
}

read_cursor <- function() {
    corteza:::matrix_load_config()$sync_token
}

# mx.client::mx_sync_update() writes the advanced cursor itself, inside
# the sync call, when save = TRUE. Where that write happens relative to
# the parse is the whole of invariant 2, so every .sync stand-in below
# puts it in the same place rather than leaving the cursor to a later
# step that a throw could skip.
sync_saved <- function(client, save) {
    if (isTRUE(save)) {
        corteza:::matrix_save_config(corteza:::matrix_plain_cfg(client))
    }
    invisible(client)
}


# ---------------------------------------------------------------
# Transport client shape: where the invariants are declared
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg())
    on.exit(restore_config(iso), add = TRUE)
    cli <- corteza:::matrix_chat_client(corteza:::matrix_load_config())

    expect_inherits(cli, "chat_matrix")
    expect_inherits(cli, "chat_client")

    # Invariant 2, the declaration half: the cursor is written inside
    # the sync call, as it was before the rewire. Flip this to FALSE and
    # the write moves after chat_poll()'s parse, where a throw skips it.
    expect_true(cli$save_cursor)

    # Invariant 1, the declaration half: chat_poll() only reaches
    # mx_with_relogin() when this is TRUE.
    expect_true(cli$relogin)

    # app = NULL leaves the wrapped config's own attributes in charge of
    # where mx.client would persist. Naming "chat.api" here would file
    # corteza's credentials under chat.api's namespace.
    expect_null(cli$app)
    expect_equal(attr(cli$env$mx, "app"), "corteza")
    expect_equal(attr(cli$env$mx, "path"), corteza:::matrix_config_path())

    # Invariant 4, the declaration half. This config does not set e2ee,
    # so the client holds no crypto context and says so. The flag answers
    # for the client, not for whether mx.crypto is installed: a TRUE here
    # would invite a send into an encrypted room that goes out in the
    # clear.
    caps <- chat.api::chat_capabilities(cli)
    expect_false(caps$e2ee)
    expect_true(caps$typing)
    expect_null(cli$env$crypto)
})


# ---------------------------------------------------------------
# Invariant 2: the advanced cursor reaches disk, inside the sync
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))
    on.exit(restore_config(iso), add = TRUE)
    seen_since <- NULL
    seen_save <- NULL
    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            seen_since <<- client$sync_token
            seen_save <<- save
            client$sync_token <- "s2"
            sync_saved(client, save)
            list(sync = list(next_batch = "s2", rooms = list(join = list())),
                 client = client, first_run = FALSE)
        },
        .extract = function(sync_resp, self_id, ...) list())

    expect_equal(read_cursor(), "s1")
    replied <- with_seamed_client(seams, corteza::matrix_poll(timeout = 0L))
    expect_equal(replied, 0L)

    # The sync resumed from the stored cursor...
    expect_equal(seen_since, "s1")
    # ...the adapter asked mx.client to persist the new one...
    expect_true(seen_save)
    # ...and it reached disk.
    expect_equal(read_cursor(), "s2")
})


# The cursor is durable across a crash, which is the property that makes
# matrix_run()'s "crash and let systemd restart" recovery work at all.
# chat_poll() parses the sync into chat_message records before it
# returns, so anything that throws in there -- a timeline event with no
# event_id is enough -- happens after the sync and before corteza sees a
# result. Persist from the returned cursor instead of inside the sync
# and this batch is re-fetched on every restart, forever.
local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))
    on.exit(restore_config(iso), add = TRUE)
    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            client$sync_token <- "s2"
            sync_saved(client, save)
            list(sync = list(next_batch = "s2", rooms = list(join = list())),
                 client = client, first_run = FALSE)
        },
        # A record the adapter cannot finish building: no ts and no
        # event_id, so its timestamp lookup subscripts by character(0).
        # mx_extract_text_events() copies event_id straight off the
        # event, so a timeline entry without one produces exactly this.
        .extract = function(sync_resp, self_id, ...) {
            list(list(room_id = "!room:example", sender = "@ann:example",
                      body = "hello", msgtype = "m.text", event_id = NULL,
                      ts = NULL))
        })

    expect_error(with_seamed_client(seams, corteza::matrix_poll(timeout = 0L)))
    # The poll died, and the restart will resume past the event that
    # killed it rather than fetching it again.
    expect_equal(read_cursor(), "s2")
})

# matrix_poll() names the short dependency instead of failing obscurely
# three lines later.
local({
    iso <- isolate_config(base_cfg())
    on.exit(restore_config(iso), add = TRUE)
    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            client$sync_token <- "s2"
            # An adapter that reports neither client nor first_run is
            # simulated by a sync whose result carries neither. Such an
            # adapter also predates `save`, so nothing is written.
            list(sync = list(next_batch = "s2", rooms = list(join = list())),
                 client = NULL, first_run = NULL)
        },
        .extract = function(sync_resp, self_id, ...) list())

    expect_error(with_seamed_client(seams, corteza::matrix_poll(timeout = 0L)),
                 "no client/first_run")
})


# ---------------------------------------------------------------
# Invariant 3: first_run suppresses the backfill, but still saves
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg(sync_token = NULL))
    on.exit(restore_config(iso), add = TRUE)
    # matrix_msg_record() is corteza's first look at a message, and it
    # runs past the first_run gate. Counting it is what makes suppression
    # observable: the adapter extracts either way, so counting its
    # extractor would prove nothing.
    mapped <- 0L
    orig_record <- corteza:::matrix_msg_record
    assignInNamespace("matrix_msg_record", function(m) {
        mapped <<- mapped + 1L
        orig_record(m)
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_msg_record", orig_record,
                              ns = "corteza"), add = TRUE)

    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            first <- is.null(client$sync_token)
            client$sync_token <- "s2"
            sync_saved(client, save)
            list(sync = sync_with_message(), client = client,
                 first_run = first)
        },
        .extract = function(sync_resp, self_id, ...) {
            list(list(event_id = "$ev1:example", room_id = "!room:example",
                      sender = "@ann:example", body = "hello",
                      msgtype = "m.text", is_self = FALSE))
        })

    msg <- capture.output(
        replied <- with_seamed_client(seams, corteza::matrix_poll(timeout = 0L)),
        type = "message")

    # The sync carried a real human message and the adapter handed it
    # over. Suppression is the only reason it went unprocessed.
    expect_equal(replied, 0L)
    expect_equal(mapped, 0L)
    expect_true(any(grepl("baseline established", msg)))

    # Ordering guard: the baseline cursor still has to be written.
    # Skip it on a first run and the token stays NULL forever, so every
    # restart re-establishes a baseline and the bot never reads a
    # message.
    expect_equal(read_cursor(), "s2")
})

# The second poll, now that a cursor exists, is not a first run and does
# process what it is given.
local({
    iso <- isolate_config(base_cfg(sync_token = "s2"))
    on.exit(restore_config(iso), add = TRUE)
    mapped <- 0L
    orig_record <- corteza:::matrix_msg_record
    assignInNamespace("matrix_msg_record", function(m) {
        mapped <<- mapped + 1L
        orig_record(m)
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_msg_record", orig_record,
                              ns = "corteza"), add = TRUE)

    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            client$sync_token <- "s3"
            sync_saved(client, save)
            list(sync = sync_with_message("s3"), client = client,
                 first_run = FALSE)
        },
        # is_self so the loop records it and moves on: this test is about
        # whether the message was taken up at all, not about answering it.
        .extract = function(sync_resp, self_id, ...) {
            list(list(event_id = "$ev1:example", room_id = "!room:example",
                      sender = "@bot:example", body = "hello",
                      msgtype = "m.text", is_self = TRUE))
        })

    replied <- with_seamed_client(
        seams,
        corteza::matrix_poll(timeout = 0L,
                             sessions = seeded_sessions("!room:example")))
    expect_equal(replied, 0L)
    expect_equal(mapped, 1L)
    expect_equal(read_cursor(), "s3")
})


# ---------------------------------------------------------------
# Invariant 1: an invalidated access token still self-heals
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))
    on.exit(restore_config(iso), add = TRUE)

    # mx_with_relogin() calls mx_client_relogin() inside mx.client's
    # namespace. Stubbing it there keeps the retry offline while leaving
    # the wrapper itself -- the thing under test -- untouched.
    orig_relogin <- mx.client::mx_client_relogin
    assignInNamespace("mx_client_relogin", function(client, save = TRUE, ...) {
        client$token <- "fresh-token"
        client
    }, ns = "mx.client")
    on.exit(assignInNamespace("mx_client_relogin", orig_relogin,
                              ns = "mx.client"), add = TRUE)

    tokens <- character()
    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            tokens <<- c(tokens, client$token)
            if (length(tokens) == 1L) {
                token_rejected()
            }
            client$sync_token <- "s2"
            sync_saved(client, save)
            list(sync = list(next_batch = "s2", rooms = list(join = list())),
                 client = client, first_run = FALSE)
        },
        .extract = function(sync_resp, self_id, ...) list())

    msg <- capture.output(
        replied <- with_seamed_client(seams, corteza::matrix_poll(timeout = 0L)),
        type = "message")

    # Two attempts: the rejected one and the retry.
    expect_equal(length(tokens), 2L)
    expect_equal(tokens[[1]], "tok")
    # The retry ran on the re-authenticated config, not the token the
    # homeserver had just rejected. Handing the retry the stale client is
    # the shape that looks like it works and never recovers.
    expect_equal(tokens[[2]], "fresh-token")
    # The recovery message comes from mx.client, which pins the wrapper
    # as the thing that handled it rather than something corteza-local.
    expect_true(any(grepl("token rejected", msg)))

    # The poll completed, and the refreshed token plus the new cursor
    # both reached corteza's config file.
    expect_equal(replied, 0L)
    expect_equal(read_cursor(), "s2")
    expect_equal(corteza:::matrix_load_config()$token, "fresh-token")
})

# Errors that are not a rejected token still propagate.
local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))
    on.exit(restore_config(iso), add = TRUE)
    seams <- list(
        .sync = function(client, timeout = 0L, ...) {
            stop("homeserver on fire")
        },
        .extract = function(sync_resp, self_id, ...) list())

    expect_error(with_seamed_client(seams, corteza::matrix_poll(timeout = 0L)),
                 "homeserver on fire")
    # A failed sync must not move the cursor.
    expect_equal(read_cursor(), "s1")
})


# ---------------------------------------------------------------
# Invariant 4: E2EE goes through the adapter
# ---------------------------------------------------------------

# corteza manages no crypto state. The old architecture is gone, and
# these assertions are what stop it coming back: a reintroduced
# R/matrix_crypto.R, or a matrix_run_init() that builds a context and
# threads it into the poll, fails here.
expect_false("matrix_crypto_init" %in% ls(asNamespace("corteza"),
                                          all.names = TRUE))
expect_false("matrix_crypto_decrypt" %in% ls(asNamespace("corteza"),
                                             all.names = TRUE))
expect_false("matrix_send_maybe_encrypted" %in% ls(asNamespace("corteza"),
                                                   all.names = TRUE))
# ... and matrix_poll() no longer takes a context to be handed one.
expect_false("crypto" %in% names(formals(corteza::matrix_poll)))
# matrix_run_init() no longer builds one. Read off the source rather than
# from a call: the real function syncs, catches up on invites, and
# backfills, so calling it here would reach a homeserver.
expect_false(any(grepl("crypto", deparse(body(corteza::matrix_run_init)))))
expect_false(any(grepl("crypto", deparse(body(corteza::matrix_run_step)))))

# e2ee off is the default and stays exactly as it was: no crypto context
# on the client, nothing asked of the store.
local({
    iso <- isolate_config(base_cfg())
    on.exit(restore_config(iso), add = TRUE)
    cs <- crypto_seam()
    cli <- corteza:::matrix_chat_client(corteza:::matrix_load_config(),
                                        .crypto = cs$ops)
    expect_null(cli$env$crypto)
    expect_false(chat.api::chat_capabilities(cli)$e2ee)
    expect_null(cs$log$store)
})

# e2ee on: the config flag reaches the adapter, and nothing else does.
# corteza names no store. The adapter derives one per Matrix device off
# this same config and records that identity in it, so the store is
# unique to (user_id, device_id) without corteza having a second place to
# get that wrong.
local({
    iso <- isolate_config(base_cfg(e2ee = TRUE))
    on.exit(restore_config(iso), add = TRUE)
    cs <- crypto_seam()
    cli <- corteza:::matrix_chat_client(corteza:::matrix_load_config(),
                                        .crypto = cs$ops)
    expect_true(chat.api::chat_capabilities(cli)$e2ee)
    expect_null(cli$crypto_store)
    # Built on first use, not at construction: key publication is an
    # authenticated request, and the stored token may already be rejected
    # at startup.
    expect_null(cli$env$crypto)
    expect_identical(cs$log$inits, 0L)
})

# corteza no longer computes a store path at all -- that briefly lived
# here, keyed on user_id alone, which is the right shape for telling
# cornelius and tiny apart and the wrong one for an Olm account, since an
# account belongs to a device.
expect_false("matrix_crypto_store" %in% ls(asNamespace("corteza"),
                                           all.names = TRUE))
expect_false(any(grepl("crypto_store",
                       deparse(body(corteza:::matrix_chat_client)))))

# Decrypted traffic reaches the poll loop as ordinary messages, with
# encrypted carried through. corteza used to run its own decrypt off
# chat_poll()$raw and concatenate the results itself.
local({
    iso <- isolate_config(base_cfg(sync_token = "s1", e2ee = TRUE, user_id = "@decrypt:example"))
    on.exit(restore_config(iso), add = TRUE)
    seen <- list()
    orig_record <- corteza:::matrix_msg_record
    assignInNamespace("matrix_msg_record", function(m) {
        rec <- orig_record(m)
        seen[[length(seen) + 1L]] <<- rec
        rec
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_msg_record", orig_record,
                              ns = "corteza"), add = TRUE)

    cs <- crypto_seam(decrypted = list(
        list(room_id = "!secret:example", event_id = "$enc1:example",
             sender = "@bot:example", body = "shh", msgtype = "m.text",
             sender_verified = TRUE, is_self = TRUE)))
    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            client$sync_token <- "s2"
            sync_saved(client, save)
            list(sync = sync_with_message(), client = client,
                 first_run = FALSE)
        },
        # is_self on both, so the loop takes each one up and moves on.
        .extract = function(sync_resp, self_id, ...) {
            list(list(event_id = "$ev1:example", room_id = "!room:example",
                      sender = "@bot:example", body = "hello",
                      msgtype = "m.text", is_self = TRUE))
        },
        .crypto = cs$ops)

    replied <- with_seamed_client(
        seams,
        corteza::matrix_poll(timeout = 0L,
                             sessions = seeded_sessions(c("!room:example",
                                                          "!secret:example"))))
    expect_equal(replied, 0L)
    expect_identical(cs$log$decrypts, 1L)
    # Both the cleartext and the decrypted message arrived, in that order.
    expect_identical(length(seen), 2L)
    expect_identical(seen[[1L]]$event_id, "$ev1:example")
    expect_false(seen[[1L]]$encrypted)
    expect_identical(seen[[2L]]$event_id, "$enc1:example")
    expect_identical(seen[[2L]]$room_id, "!secret:example")
    expect_identical(seen[[2L]]$body, "shh")
    expect_true(seen[[2L]]$encrypted)
    expect_true(seen[[2L]]$sender_verified)
})

# Encrypted rooms go out through the transport contract now, and the
# adapter routes them to Megolm. corteza has no second send path to keep
# in step -- which is what used to make a cleartext leak possible, since
# a room the adapter thought was plain got a plaintext PUT.
local({
    iso <- isolate_config(base_cfg(e2ee = TRUE, user_id = "@routing:example"))
    on.exit(restore_config(iso), add = TRUE)
    cfg <- corteza:::matrix_load_config()

    sent <- character()
    cs <- crypto_seam()
    seams <- list(
        .send = function(client, text, room = NULL, ...) {
            sent <<- c(sent, text)
            "$plain:example"
        },
        .sync = function(...) stop("unused"),
        .extract = function(...) stop("unused"),
        .crypto = cs$ops)

    with_seamed_client(seams, {
        # Encrypted room: the contract call goes through, and the adapter
        # sends it encrypted rather than in the clear.
        eid <- corteza:::matrix_reply_send(cfg, "!secret:example", "shh")
        expect_equal(eid, "$enc:example")
        expect_identical(length(cs$log$sent), 1L)
        expect_identical(cs$log$sent[[1L]]$text, "shh")
        expect_equal(length(sent), 0L)

        # Plaintext room on the same client: cleartext, and no Megolm.
        pid <- corteza:::matrix_reply_send(cfg, "!room:example", "hi")
        expect_equal(pid, "$plain:example")
        expect_identical(length(cs$log$sent), 1L)
        expect_equal(sent, "hi")
    })
})


# ---------------------------------------------------------------
# The exported send contract survives the rewire
# ---------------------------------------------------------------

# chat.api's `kind` vocabulary is message/notice/emote, which maps onto
# exactly three msgtypes. matrix_send() is exported with msgtype as a
# documented argument, so anything outside those three has to keep going
# out verbatim instead of being laundered through kind and coming back
# as m.text.
local({
    iso <- isolate_config(base_cfg())
    on.exit(restore_config(iso), add = TRUE)
    seen <- list()

    orig_send <- mx.client::mx_send_text
    assignInNamespace("mx_send_text", function(client, text, room = NULL,
                                               msgtype = "m.text",
                                               markdown = FALSE, ...) {
        formatted <- if (isTRUE(markdown)) {
            mx.client::mx_markdown_to_html(text)
        } else {
            NULL
        }
        seen[[length(seen) + 1L]] <<- list(msgtype = msgtype,
                                           markdown = markdown,
                                           text = text,
                                           formatted = formatted)
        "$direct:example"
    }, ns = "mx.client")
    on.exit(assignInNamespace("mx_send_text", orig_send, ns = "mx.client"),
            add = TRUE)

    seams <- list(.sync = function(...) stop("unused"),
                  .extract = function(...) stop("unused"))
    with_seamed_client(seams, {
        # The three the contract models ride it, and arrive intact.
        for (mt in c("m.text", "m.notice", "m.emote")) {
            corteza::matrix_send("x", room_id = "!room:example", msgtype = mt)
            expect_equal(seen[[length(seen)]]$msgtype, mt)
        }
        # The ones it does not model go direct, still intact.
        for (mt in c("m.image", "m.file", "m.audio")) {
            corteza::matrix_send("x", room_id = "!room:example", msgtype = mt)
            expect_equal(seen[[length(seen)]]$msgtype, mt)
        }
        # markdown survives both routes, including pipe tables. This is the
        # executable guard for the Cornelius table-rendering regression: text
        # m.room.message goes corteza -> chat.api -> mx.client, while msgtypes
        # outside chat.api's vocabulary still go corteza -> mx.client direct.
        table_md <- paste(c(
            "| package | days per submission | latest |",
            "|---|---:|---:|",
            "| `llm.api` | 20.8 | 2026-06-26 |",
            "| `tinyrox` | 52.5 | 2026-06-24 |"
        ), collapse = "\n")
        corteza::matrix_send(table_md, room_id = "!room:example",
                             markdown = TRUE)
        chat_route <- seen[[length(seen)]]
        expect_true(chat_route$markdown)
        expect_true(grepl("<table>", chat_route$formatted, fixed = TRUE))
        expect_true(grepl("<td><code>llm.api</code></td>",
                          chat_route$formatted, fixed = TRUE))
        corteza::matrix_send(table_md, room_id = "!room:example",
                             msgtype = "m.image", markdown = TRUE)
        direct_route <- seen[[length(seen)]]
        expect_true(direct_route$markdown)
        expect_true(grepl("<table>", direct_route$formatted, fixed = TRUE))
    })
})

# The event id comes back, and it autoprints. chat_send() returns it
# invisibly; a user typing matrix_send() at the console saw the id
# before the rewire and has to keep seeing it.
local({
    iso <- isolate_config(base_cfg())
    on.exit(restore_config(iso), add = TRUE)
    seams <- list(.send = function(client, text, room = NULL, ...) {
                      "$ev:example"
                  },
                  .sync = function(...) stop("unused"),
                  .extract = function(...) stop("unused"))
    with_seamed_client(seams, {
        out <- withVisible(corteza::matrix_send("hi", room_id = "!room:example"))
        expect_equal(out$value, "$ev:example")
        expect_true(out$visible)
    })
})

# A 200 with no event_id in it. mx.client answers NULL, chat_send()
# as.character()s that into character(0), and every caller here tests
# the result with is.null() -- which character(0) passes, taking
# matrix_remember_event() down with it mid-batch. Both send paths hand
# back a real NULL.
local({
    iso <- isolate_config(base_cfg())
    on.exit(restore_config(iso), add = TRUE)
    cfg <- corteza:::matrix_load_config()
    seams <- list(.send = function(client, text, room = NULL, ...) NULL,
                  .sync = function(...) stop("unused"),
                  .extract = function(...) stop("unused"))
    with_seamed_client(seams, {
        expect_null(corteza::matrix_send("hi", room_id = "!room:example"))
        expect_null(corteza:::matrix_reply_send(cfg, "!room:example", "hi"))
    })
    # The guard the poll loop actually uses, and the call it guards.
    expect_equal(corteza:::matrix_remember_event(character(), character(0)),
                 character())
})


# ---------------------------------------------------------------
# Typing indicator: the other rewired site
# ---------------------------------------------------------------

# Not one of the four invariants, but the unit changed with the rewire.
# mx.api counts milliseconds and the contract counts seconds, so a
# timeout passed through unconverted is a 120000-second indicator (or a
# 120ms one) and nobody notices until a bot looks dead mid-turn.
local({
    iso <- isolate_config(base_cfg())
    on.exit(restore_config(iso), add = TRUE)
    cfg <- corteza:::matrix_load_config()
    seen <- list()
    cli <- corteza:::matrix_chat_client(cfg, .typing = function(session, room_id,
                                                               typing = TRUE,
                                                               timeout = 30000L) {
        seen[[length(seen) + 1L]] <<- list(room = room_id, on = typing,
                                           timeout = timeout)
        TRUE
    })

    chat.api::chat_typing(cli, "!room:example", TRUE, timeout = 120)
    chat.api::chat_typing(cli, "!room:example", FALSE)

    expect_equal(length(seen), 2L)
    expect_true(seen[[1]]$on)
    expect_equal(seen[[1]]$timeout, 120000L)
    expect_equal(seen[[1]]$room, "!room:example")
    expect_false(seen[[2]]$on)
    # The off-call keeps mx.api's own 30s default, as the pre-rewire
    # bare mx_typing() call did.
    expect_equal(seen[[2]]$timeout, 30000L)

    # A dead indicator is swallowed, never propagated: the pre-rewire
    # code wrapped each call in tryCatch, and dropping that wrapper is
    # only safe because the adapter absorbs the failure itself.
    boom <- corteza:::matrix_chat_client(cfg, .typing = function(...) {
        stop("no typing for you")
    })
    expect_silent(ok <- chat.api::chat_typing(boom, "!room:example", TRUE))
    expect_false(ok)
})

# matrix_poll() drives typing through the contract, at the 120s cap, and
# no longer calls mx.api::mx_typing() itself.
local({
    # width.cutoff at the maximum so deparse does not split an asserted
    # call across elements and quietly turn a grep into a false pass.
    src <- paste(deparse(body(corteza::matrix_poll), width.cutoff = 500L),
                 collapse = "\n")
    expect_true(grepl("chat_typing(chat_now(), m$room_id, TRUE, timeout = 120)", src,
                      fixed = TRUE))
    expect_true(grepl("chat_typing(chat_now(), m$room_id, FALSE)", src, fixed = TRUE))
    expect_false(grepl("mx_typing", src, fixed = TRUE))
    # ...and the sync itself is the contract's, not a direct mx.client
    # call. Both invariant-1 and invariant-2 tests above would still pass
    # against a hand-rolled mx_with_relogin here, so pin the route.
    expect_true(grepl("chat_poll(chat, timeout = timeout/1000)", src,
                      fixed = TRUE))
    expect_false(grepl("mx_sync_update", src, fixed = TRUE))
})

# matrix_require_mx() is version-aware, not just presence-aware.
# requireNamespace() returns TRUE for any installed build, and a
# Suggests floor is a resolution hint rather than a runtime guarantee,
# so a host carrying an older chat.api would otherwise sync -- spending
# the cursor -- and only then die on the missing first_run.
expect_true(exists(".CHAT_API_MIN", envir = asNamespace("corteza")))
expect_identical(corteza:::.CHAT_API_MIN, "0.0.1.9")
# The comparison is a version comparison, not a string one: "0.0.1.10"
# sorts before "0.0.1.9" as text and after it as a version.
expect_true(package_version("0.0.1.10") > package_version("0.0.1.9"))
expect_false(package_version("0.0.1.8") >= package_version(corteza:::.CHAT_API_MIN))
# The installed build satisfies it, so the guard passes rather than
# being vacuously untested.
expect_true(utils::packageVersion("chat.api") >=
            package_version(corteza:::.CHAT_API_MIN))
expect_silent(corteza:::matrix_require_mx())


# ---------------------------------------------------------------
# A /model switch renames the bot, and that rename can relogin. The
# refreshed token has to reach the acknowledgement send.
#
# This drives matrix_poll() rather than calling the helpers, because the
# defect was never in matrix_update_displayname() itself -- it was in the
# call site not adopting what it returned. Remove the `cfg <-` at the
# /model branch and this goes red; a test that only calls the helper
# stays green, which is how the bug survived its first fix.
# ---------------------------------------------------------------

local({
    cfg0 <- base_cfg(sync_token = "s1")
    cfg0$model_badge <- "always"
    cfg0$model <- "qwen3:8b"
    cfg0$provider <- "ollama"
    iso <- isolate_config(cfg0)
    on.exit(restore_config(iso), add = TRUE)

    # The rename relogins: mx.client persists a refreshed token and hands
    # back only TRUE, discarding the client it refreshed.
    orig_set <- mx.client::mx_set_displayname
    assignInNamespace("mx_set_displayname",
                      function(client, name, save = TRUE) {
        c <- corteza:::matrix_load_config()
        c$token <- "rotated"
        corteza:::matrix_save_config(c)
        invisible(TRUE)
    }, ns = "mx.client")
    on.exit(assignInNamespace("mx_set_displayname", orig_set, ns = "mx.client"),
            add = TRUE)

    # Reply gating is a separate concern with its own tests; force the
    # message through so this asserts token propagation and nothing else.
    orig_resp <- corteza:::matrix_should_respond
    assignInNamespace("matrix_should_respond",
                      function(...) TRUE, ns = "corteza")
    on.exit(assignInNamespace("matrix_should_respond", orig_resp,
                              ns = "corteza"), add = TRUE)

    # Capture the config the send is handed.
    seen_cfg <- NULL
    orig_send <- corteza:::matrix_reply_send
    assignInNamespace("matrix_reply_send",
                      function(cfg, room_id, text, markdown = FALSE) {
        seen_cfg <<- cfg
        "$ack"
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_reply_send", orig_send,
                              ns = "corteza"), add = TRUE)

    # Pre-seeded registry: no session_setup, no provider calls.
    sessions <- corteza:::matrix_new_session_registry()
    s <- new.env(parent = emptyenv())
    s$model <- "qwen3:8b"
    s$provider <- "ollama"
    s$default_model <- "qwen3:8b"
    s$default_provider <- "ollama"
    s$history <- list()
    s$transcript <- list()
    s$seen_event_ids <- character()
    assign("!room:example", s, envir = sessions)

    # The adapter extracts the messages matrix_poll() reads, so the
    # message still has to be in the sync the .sync seam returns.
    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            ev <- list(type = "m.room.message", event_id = "$m1",
                       sender = "@human:example",
                       origin_server_ts = 1700000000000,
                       content = list(msgtype = "m.text",
                           body = "/model claude-sonnet-4-6 anthropic_claude"))
            list(sync = list(next_batch = "s2", rooms = list(join = list(
                     "!room:example" = list(timeline = list(events = list(ev)))))),
                 client = client, first_run = FALSE)
        })

    with_seamed_client(seams,
        corteza::matrix_poll(timeout = 0L, sessions = sessions))

    # The send ran with the token the rename produced, not the one the
    # homeserver had just rejected.
    expect_equal(seen_cfg$token, "rotated")
})


# ---------------------------------------------------------------
# The encrypted send takes the caller's live cfg, not a copy cached at
# crypto init. Reintroduce a cached client and this goes red.
# ---------------------------------------------------------------

# The Olm account is interned per identity and outlives any one client,
# so the danger is a config interned along with it. The crypto context
# holds no client for exactly this reason: it is built once, and the token
# rotates under it. Every send has to read the cfg it was called with.
local({
    iso <- isolate_config(base_cfg(e2ee = TRUE, user_id = "@livecfg:example"))
    on.exit(restore_config(iso), add = TRUE)
    cs <- crypto_seam(encrypted_rooms = "!room:example")

    stale <- corteza:::matrix_load_config()
    live <- stale
    live$token <- "rotated"

    seams <- list(.sync = function(...) stop("unused"),
                  .extract = function(...) stop("unused"),
                  .crypto = cs$ops)
    with_seamed_client(seams, {
        # Build once against the pre-rotation config, which is what
        # interns the context, then send against the rotated one.
        corteza:::matrix_reply_send(stale, "!room:example", "before")
        corteza:::matrix_reply_send(live, "!room:example", "after")
    })
    expect_identical(length(cs$log$sent), 2L)
    expect_identical(cs$log$sent[[1L]]$mx$token, "tok")
    expect_identical(cs$log$sent[[2L]]$mx$token, "rotated")
    # corteza names no store, so the adapter derives one from the config's
    # own device identity.
    expect_null(cs$log$store)
    # Interning is what makes per-use client building affordable with e2ee
    # on. Two sends built two clients above, and the account was loaded
    # once; without it every send would unpickle it and republish 50
    # one-time keys. It survives the token rotation, because the identity
    # is (user_id, device_id) and neither of those rotates.
    expect_identical(cs$log$inits, 1L)
    with_seamed_client(seams, corteza:::matrix_reply_send(live, "!room:example",
                                                          "again"))
    expect_identical(cs$log$inits, 1L)
})


# ---------------------------------------------------------------
# /clear renames too, so it has the same relogin exposure as /model.
# ---------------------------------------------------------------

local({
    cfg0 <- base_cfg(sync_token = "s1")
    cfg0$model_badge <- "always"
    cfg0$model <- "qwen3:8b"
    iso <- isolate_config(cfg0)
    on.exit(restore_config(iso), add = TRUE)

    orig_set <- mx.client::mx_set_displayname
    assignInNamespace("mx_set_displayname",
                      function(client, name, save = TRUE) {
        c <- corteza:::matrix_load_config()
        c$token <- "rotated"
        corteza:::matrix_save_config(c)
        invisible(TRUE)
    }, ns = "mx.client")
    on.exit(assignInNamespace("mx_set_displayname", orig_set, ns = "mx.client"),
            add = TRUE)

    orig_resp <- corteza:::matrix_should_respond
    assignInNamespace("matrix_should_respond", function(...) TRUE,
                      ns = "corteza")
    on.exit(assignInNamespace("matrix_should_respond", orig_resp,
                              ns = "corteza"), add = TRUE)

    seen_cfg <- NULL
    orig_send <- corteza:::matrix_reply_send
    assignInNamespace("matrix_reply_send",
                      function(cfg, room_id, text, markdown = FALSE) {
        seen_cfg <<- cfg
        "$ack"
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_reply_send", orig_send,
                              ns = "corteza"), add = TRUE)

    # /clear deletes the room's session and builds a fresh one, and
    # building one calls session_setup(), which wants a provider API key.
    # This test is about which config the acknowledgement is sent with,
    # not about session construction, so the rebuild is stubbed. Without
    # it the file passes only on a machine that happens to have
    # credentials in its environment -- which is why CI never saw it: the
    # whole file used to exit at its requireNamespace guard.
    orig_new <- corteza:::matrix_new_session
    assignInNamespace("matrix_new_session", function(cfg, ...) {
        e <- new.env(parent = emptyenv())
        e$model <- "qwen3:8b"
        e$provider <- "ollama"
        e$history <- list()
        e$transcript <- list()
        e$seen_event_ids <- character()
        e
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_new_session", orig_new, ns = "corteza"),
            add = TRUE)

    sessions <- corteza:::matrix_new_session_registry()
    s <- new.env(parent = emptyenv())
    s$model <- "qwen3:8b"
    s$provider <- "ollama"
    s$history <- list()
    s$transcript <- list()
    s$seen_event_ids <- character()
    assign("!room:example", s, envir = sessions)

    seams <- list(
        .sync = function(client, timeout = 0L, save = TRUE, ...) {
            ev <- list(type = "m.room.message", event_id = "$c1",
                       sender = "@human:example",
                       origin_server_ts = 1700000000000,
                       content = list(msgtype = "m.text", body = "/clear"))
            list(sync = list(next_batch = "s2", rooms = list(join = list(
                     "!room:example" = list(timeline = list(events = list(ev)))))),
                 client = client, first_run = FALSE)
        })

    with_seamed_client(seams,
        corteza::matrix_poll(timeout = 0L, sessions = sessions))
    expect_equal(seen_cfg$token, "rotated")
})


# ---------------------------------------------------------------
# Relogin succeeds, the retry then fails. mx_with_relogin() persists the
# refreshed client BEFORE retrying, so the live token is on disk even
# though the call threw. Returning the caller's cfg on the error path
# discarded it and sent the acknowledgement on the rejected token.
#
# The earlier guards only modelled "persist, then succeed", so this path
# stayed green through two rounds of fixes.
# ---------------------------------------------------------------

local({
    cfg0 <- base_cfg(sync_token = "s1")
    cfg0$model_badge <- "always"
    cfg0$model <- "qwen3:8b"
    iso <- isolate_config(cfg0)
    on.exit(restore_config(iso), add = TRUE)

    orig_set <- mx.client::mx_set_displayname
    assignInNamespace("mx_set_displayname",
                      function(client, name, save = TRUE) {
        # The relogin half: refreshed token reaches disk.
        c <- corteza:::matrix_load_config()
        c$token <- "rotated"
        corteza:::matrix_save_config(c)
        # The retry half: rate-limited, so the call throws anyway.
        stop("M_LIMIT_EXCEEDED: slow down")
    }, ns = "mx.client")
    on.exit(assignInNamespace("mx_set_displayname", orig_set, ns = "mx.client"),
            add = TRUE)

    got <- corteza:::matrix_update_displayname(corteza:::matrix_load_config())
    expect_equal(got$token, "rotated")
})
