library(tinytest)

# Regression tests for the four invariants that had to survive moving
# corteza's Matrix message plane onto the chat.api transport contract:
#
#   1. token-expiry relogin      -- the sync still runs inside
#                                   mx.client::mx_with_relogin()
#   2. sync cursor persistence   -- the client is built save_cursor =
#                                   FALSE, so corteza writes the cursor
#                                   returned by chat_poll() itself
#   3. initial-sync suppression  -- first_run survives the trip out of
#                                   the adapter and still short-circuits
#                                   matrix_poll()
#   4. E2EE behaviour unchanged  -- encrypted rooms stay on the
#                                   mx.crypto path, and the crypto
#                                   decrypt step still gets the raw sync
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
# The adapter grew first_run, the post-sync client, and the seams
# together. An older chat.api would make every test below fail for the
# same uninformative reason, so say so once instead.
if (!all(c(".sync", "relogin") %in% names(formals(chat.api::chat_matrix)))) {
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
base_cfg <- function(sync_token = "s1") {
    cfg <- list(server = "https://example", user = "bot", password = "pw",
                token = "tok", user_id = "@bot:example", device_id = "DEV",
                room_id = "!room:example")
    if (!is.null(sync_token)) {
        cfg$sync_token <- sync_token
    }
    cfg
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


# ---------------------------------------------------------------
# Transport client shape: where the invariants are declared
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg())
    cli <- corteza:::matrix_chat_client(corteza:::matrix_load_config())

    expect_inherits(cli, "chat_matrix")
    expect_inherits(cli, "chat_client")

    # Invariant 2, the declaration half: chat.api must not write
    # corteza's config file. matrix_persist_cursor() is the only writer,
    # and the poll test below proves it runs.
    expect_false(cli$save_cursor)

    # Invariant 1, the declaration half: chat_poll() only reaches
    # mx_with_relogin() when this is TRUE.
    expect_true(cli$relogin)

    # app = NULL leaves the wrapped config's own attributes in charge of
    # where mx.client would persist. Naming "chat.api" here would file
    # corteza's credentials under chat.api's namespace.
    expect_null(cli$app)
    expect_equal(attr(cli$env$mx, "app"), "corteza")
    expect_equal(attr(cli$env$mx, "path"), corteza:::matrix_config_path())

    # Invariant 4, the declaration half: the adapter does not claim
    # encryption, which is why matrix_send_maybe_encrypted() keeps its
    # own mx.crypto branch.
    caps <- chat.api::chat_capabilities(cli)
    expect_false(caps$e2ee)
    expect_true(caps$typing)

    restore_config(iso)
})


# ---------------------------------------------------------------
# Invariant 2: corteza persists the cursor chat_poll() returned
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))
    seen_since <- NULL
    seams <- list(
        .sync = function(client, timeout = 0L, ...) {
            seen_since <<- client$sync_token
            client$sync_token <- "s2"
            list(sync = list(next_batch = "s2", rooms = list(join = list())),
                 client = client, first_run = FALSE)
        },
        .extract = function(sync_resp, self_id, ...) list())

    expect_equal(read_cursor(), "s1")
    replied <- with_seamed_client(seams, corteza::matrix_poll(timeout = 0L))
    expect_equal(replied, 0L)

    # The sync resumed from the stored cursor...
    expect_equal(seen_since, "s1")
    # ...and the new one reached disk. Delete matrix_persist_cursor() and
    # this stays "s1": nothing under chat.api writes with save_cursor =
    # FALSE, so the next process would replay from the stale token.
    expect_equal(read_cursor(), "s2")

    restore_config(iso)
})


# ---------------------------------------------------------------
# Invariant 3: first_run suppresses the backfill, but still saves
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg(sync_token = NULL))
    extracted <- 0L
    orig_extract <- corteza:::matrix_extract_messages
    assignInNamespace("matrix_extract_messages", function(sync_resp, self_id) {
        extracted <<- extracted + 1L
        orig_extract(sync_resp, self_id)
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_extract_messages", orig_extract,
                              ns = "corteza"), add = TRUE)

    seams <- list(
        .sync = function(client, timeout = 0L, ...) {
            first <- is.null(client$sync_token)
            client$sync_token <- "s2"
            list(sync = sync_with_message(), client = client,
                 first_run = first)
        },
        .extract = function(sync_resp, self_id, ...) list())

    msg <- capture.output(
        replied <- with_seamed_client(seams, corteza::matrix_poll(timeout = 0L)),
        type = "message")

    # The sync carried a real human message. Suppression is the only
    # reason it went unprocessed.
    expect_equal(replied, 0L)
    expect_equal(extracted, 0L)
    expect_true(any(grepl("baseline established", msg)))

    # Ordering guard: the baseline cursor still has to be written.
    # Persisting after the first_run early return leaves the token NULL
    # forever, so every restart re-establishes a baseline and the bot
    # never reads a message.
    expect_equal(read_cursor(), "s2")

    restore_config(iso)
})

# The second poll, now that a cursor exists, is not a first run and does
# process what it is given.
local({
    iso <- isolate_config(base_cfg(sync_token = "s2"))
    extracted <- 0L
    orig_extract <- corteza:::matrix_extract_messages
    assignInNamespace("matrix_extract_messages", function(sync_resp, self_id) {
        extracted <<- extracted + 1L
        list()
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_extract_messages", orig_extract,
                              ns = "corteza"), add = TRUE)

    seams <- list(
        .sync = function(client, timeout = 0L, ...) {
            client$sync_token <- "s3"
            list(sync = sync_with_message("s3"), client = client,
                 first_run = FALSE)
        },
        .extract = function(sync_resp, self_id, ...) list())

    replied <- with_seamed_client(seams, corteza::matrix_poll(timeout = 0L))
    expect_equal(replied, 0L)
    expect_equal(extracted, 1L)
    expect_equal(read_cursor(), "s3")

    restore_config(iso)
})


# ---------------------------------------------------------------
# Invariant 1: an invalidated access token still self-heals
# ---------------------------------------------------------------

local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))

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
        .sync = function(client, timeout = 0L, ...) {
            tokens <<- c(tokens, client$token)
            if (length(tokens) == 1L) {
                token_rejected()
            }
            client$sync_token <- "s2"
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

    restore_config(iso)
})

# Errors that are not a rejected token still propagate.
local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))
    seams <- list(
        .sync = function(client, timeout = 0L, ...) {
            stop("homeserver on fire")
        },
        .extract = function(sync_resp, self_id, ...) list())

    expect_error(with_seamed_client(seams, corteza::matrix_poll(timeout = 0L)),
                 "homeserver on fire")
    # A failed sync must not move the cursor.
    expect_equal(read_cursor(), "s1")

    restore_config(iso)
})


# ---------------------------------------------------------------
# Invariant 4: E2EE behaviour unchanged
# ---------------------------------------------------------------

# The decrypt step still receives the untouched sync response. chat.api
# models neither m.room.encrypted nor to-device traffic, so anything
# less than the raw payload loses the room keys.
local({
    iso <- isolate_config(base_cfg(sync_token = "s1"))
    payload <- sync_with_message()
    captured <- NULL
    orig_decrypt <- corteza:::matrix_crypto_decrypt
    assignInNamespace("matrix_crypto_decrypt", function(crypto, sync, cfg) {
        captured <<- sync
        list()
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_crypto_decrypt", orig_decrypt,
                              ns = "corteza"), add = TRUE)
    # Stop short of the turn machinery: this test is about what the
    # decrypt step is handed, not about answering the message.
    orig_extract <- corteza:::matrix_extract_messages
    assignInNamespace("matrix_extract_messages", function(sync_resp, self_id) {
        list()
    }, ns = "corteza")
    on.exit(assignInNamespace("matrix_extract_messages", orig_extract,
                              ns = "corteza"), add = TRUE)

    seams <- list(
        .sync = function(client, timeout = 0L, ...) {
            client$sync_token <- "s2"
            list(sync = payload, client = client, first_run = FALSE)
        },
        # A non-empty extractor result would still be discarded: corteza
        # reads events off the raw sync itself.
        .extract = function(sync_resp, self_id, ...) {
            list(list(event_id = "$ev1:example", room_id = "!room:example",
                      sender = "@ann:example", body = "hello",
                      msgtype = "m.text", is_self = FALSE))
        })

    replied <- with_seamed_client(
        seams,
        corteza::matrix_poll(timeout = 0L,
                             crypto = list(encrypted = character(),
                                           sessions = list())))
    expect_equal(replied, 0L)
    expect_identical(captured, payload)

    restore_config(iso)
})

# Encrypted rooms never reach the transport contract. The adapter's
# chat_send() PUTs a cleartext m.room.message whatever the room's
# encryption state says, so routing an encrypted room through it would
# put plaintext on the homeserver.
local({
    iso <- isolate_config(base_cfg())
    cfg <- corteza:::matrix_load_config()

    enc_calls <- 0L
    orig_enc <- mx.client::mx_send_encrypted
    assignInNamespace("mx_send_encrypted", function(...) {
        enc_calls <<- enc_calls + 1L
        list(event_id = "$enc:example", sessions = list())
    }, ns = "mx.client")
    on.exit(assignInNamespace("mx_send_encrypted", orig_enc, ns = "mx.client"),
            add = TRUE)

    orig_members <- mx.api::mx_room_members
    assignInNamespace("mx_room_members", function(...) "@ann:example",
                      ns = "mx.api")
    on.exit(assignInNamespace("mx_room_members", orig_members, ns = "mx.api"),
            add = TRUE)

    sent <- character()
    seams <- list(
        .send = function(client, text, room = NULL, ...) {
            sent <<- c(sent, text)
            "$plain:example"
        },
        .sync = function(...) stop("unused"),
        .extract = function(...) stop("unused"))

    crypto <- list(encrypted = "!secret:example", sessions = list(),
                   account = NULL, self_curve = NULL, store = NULL,
                   client = NULL)

    with_seamed_client(seams, {
        # Encrypted room: mx.crypto, and chat.api is never called.
        eid <- corteza:::matrix_send_maybe_encrypted(crypto, cfg,
                                                     "!secret:example", "shh")
        expect_equal(eid, "$enc:example")
        expect_equal(enc_calls, 1L)
        expect_equal(length(sent), 0L)

        # Plaintext room: the transport contract, and mx.crypto is not
        # touched.
        pid <- corteza:::matrix_send_maybe_encrypted(crypto, cfg,
                                                     "!room:example", "hi")
        expect_equal(pid, "$plain:example")
        expect_equal(enc_calls, 1L)
        expect_equal(sent, "hi")
    })

    restore_config(iso)
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

    restore_config(iso)
})

# matrix_poll() drives typing through the contract, at the 120s cap, and
# no longer calls mx.api::mx_typing() itself.
local({
    src <- paste(deparse(body(corteza::matrix_poll)), collapse = "\n")
    expect_true(grepl("chat_typing(chat, m$room_id, TRUE, timeout = 120)", src,
                      fixed = TRUE))
    expect_true(grepl("chat_typing(chat, m$room_id, FALSE)", src, fixed = TRUE))
    expect_false(grepl("mx_typing", src, fixed = TRUE))
    # ...and the sync itself is the contract's, not a direct mx.client
    # call. Both invariant-1 and invariant-2 tests above would still pass
    # against a hand-rolled mx_with_relogin here, so pin the route.
    expect_true(grepl("chat_poll(chat, timeout = timeout/1000)", src,
                      fixed = TRUE))
    expect_false(grepl("mx_sync_update", src, fixed = TRUE))
})
