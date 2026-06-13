# Opt-in end-to-end encryption for the Matrix bot loop.
#
# Off by default: matrix_poll(crypto = NULL) behaves exactly as before.
# When config sets `e2ee: true`, matrix_run() builds a crypto context
# (account + sessions + known-encrypted rooms) and threads it through the
# loop, so the bot decrypts incoming m.room.encrypted events and encrypts
# its replies in rooms that advertise m.room.encryption. Plaintext rooms
# are untouched. All work is delegated to mx.client / mx.crypto, both
# Suggests, both behind requireNamespace.

# Per-identity crypto store: lives beside the JSON config so cornelius and
# tiny (different config paths) keep separate stores.
matrix_crypto_store <- function() {
    file.path(dirname(matrix_config_path()), "crypto")
}

matrix_crypto_available <- function() {
    requireNamespace("mx.client", quietly = TRUE) &&
        requireNamespace("mx.crypto", quietly = TRUE)
}

# Build the crypto context: load/create the account, publish keys, and
# restore sessions + the known-encrypted-room set. Returns an environment
# (mutable, so the poll loop's session updates persist in place).
matrix_crypto_init <- function(cfg) {
    if (!matrix_crypto_available()) {
        stop("E2EE requires the 'mx.client' and 'mx.crypto' packages.",
             call. = FALSE)
    }
    store <- matrix_crypto_store()
    acct <- mx.client::mx_crypto_account(store)
    mx.client::mx_crypto_publish_keys(cfg, acct, store, n_otks = 50L)
    crypto <- new.env(parent = emptyenv())
    crypto$account <- acct
    crypto$store <- store
    crypto$client <- cfg
    crypto$sessions <- mx.client::mx_crypto_sessions_load(store)
    crypto$encrypted <- matrix_crypto_load_encrypted(store)
    crypto$self_curve <-
        mx.crypto::mxc_account_identity_keys(acct)$curve25519
    # Ask each joined room for its m.room.encryption state up front,
    # instead of waiting for a sync to happen to mention it. Best-effort
    # per room; a transient failure just defers that room to sync-time
    # detection.
    found <- matrix_crypto_scan_rooms(cfg)
    new_rooms <- setdiff(found, crypto$encrypted)
    if (length(new_rooms)) {
        crypto$encrypted <- c(crypto$encrypted, new_rooms)
        matrix_crypto_save_encrypted(crypto)
    }
    message("matrix_run: E2EE enabled (", length(crypto$encrypted),
            " known encrypted room(s))")
    crypto
}

# All joined rooms that advertise m.room.encryption, by direct state
# query (startup path; sync-time detection still runs in the poll loop).
matrix_crypto_scan_rooms <- function(cfg) {
    s <- tryCatch(matrix_mx_session(cfg), error = function(e) NULL)
    if (is.null(s)) {
        return(character())
    }
    rooms <- tryCatch(mx.api::mx_rooms(s), error = function(e) character())
    out <- character()
    for (rid in rooms) {
        if (isTRUE(tryCatch(mx.client::mx_room_encrypted(cfg, rid),
                            error = function(e) FALSE))) {
            out <- c(out, rid)
        }
    }
    out
}

matrix_crypto_load_encrypted <- function(store) {
    f <- file.path(store, "encrypted_rooms.json")
    if (!file.exists(f)) {
        return(character())
    }
    unlist(jsonlite::fromJSON(paste(readLines(f, warn = FALSE),
                                    collapse = "\n")),
           use.names = FALSE) %||% character()
}

matrix_crypto_save_encrypted <- function(crypto) {
    dir.create(crypto$store, showWarnings = FALSE, recursive = TRUE)
    writeLines(jsonlite::toJSON(as.list(crypto$encrypted)),
               file.path(crypto$store, "encrypted_rooms.json"))
}

# Rooms that advertise m.room.encryption in this sync (state or timeline).
matrix_detect_encrypted_rooms <- function(sync) {
    out <- character()
    joined <- sync$rooms$join %||% list()
    for (rid in names(joined)) {
        evs <- c(joined[[rid]]$state$events %||% list(),
                 joined[[rid]]$timeline$events %||% list())
        for (ev in evs) {
            if (isTRUE(ev$type == "m.room.encryption")) {
                out <- c(out, rid)
                break
            }
        }
    }
    out
}

# Decrypt a sync: recover room keys from to-device, decrypt encrypted
# timeline events, refresh the encrypted-room set. Mutates `crypto`,
# returns the decrypted messages (same shape as matrix_extract_messages).
matrix_crypto_decrypt <- function(crypto, sync, cfg) {
    found <- matrix_detect_encrypted_rooms(sync)
    new_rooms <- setdiff(found, crypto$encrypted)
    if (length(new_rooms)) {
        crypto$encrypted <- c(crypto$encrypted, new_rooms)
        matrix_crypto_save_encrypted(crypto)
    }
    res <- mx.client::mx_crypto_process_sync(
        crypto$account, crypto$sessions, sync, crypto$self_curve,
        self_id = cfg$user_id)
    crypto$sessions <- res$sessions
    mx.client::mx_crypto_sessions_save(crypto$sessions, crypto$store)
    res$events
}

# Send text to a room, encrypting when crypto is on and the room is
# known-encrypted; otherwise mx.client's plaintext path (with optional
# markdown). Returns the event id, or NULL on failure.
matrix_send_maybe_encrypted <- function(crypto, cfg, room_id, text,
                                        markdown = FALSE) {
    if (!is.null(crypto) && room_id %in% crypto$encrypted) {
        mx_sess <- matrix_mx_session(cfg)
        members <- tryCatch(mx.api::mx_room_members(mx_sess, room_id),
                            error = function(e) character())
        res <- tryCatch(
            mx.client::mx_send_encrypted(
                crypto$client, crypto$account, crypto$sessions, room_id,
                list(msgtype = "m.text", body = text), crypto$store,
                member_ids = members),
            error = function(e) {
                message("matrix: encrypted send failed: ",
                        conditionMessage(e))
                NULL
            })
        if (is.null(res)) {
            return(NULL)
        }
        crypto$sessions <- res$sessions
        return(res$event_id)
    }
    mx.client::mx_send_text(cfg, text, room = room_id, markdown = markdown)
}
