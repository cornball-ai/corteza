# The call worker: the call loop in a process of its own, and the
# mailbox between it and the bot process.
#
# A bot process long-polls Matrix and owns the room's crypto store; a
# call needs audio polled every few tens of milliseconds and blocks on
# transcription and synthesis. So the call runs in a child R process
# (callr::r_session, as subagents and job workers do) and the Matrix
# side stays in the parent. What crosses between them is small and
# goes through files in the call's directory:
#
#   requests.jsonl   child -> parent: post this reply (text), edit that
#                    post (event_id, text), a note for the log. Each
#                    carries an id; a post waits for its answer.
#   answers/<id>     parent -> child: the event id of a post, or an
#                    error, as JSON.
#   commands.jsonl   parent -> child: a media key for a participant
#                    (identity, key, index), or leave.
#
# The child never holds a Matrix credential or the crypto store: the
# parent posts, edits, and exchanges call keys, and tells the child
# the keys. The child holds the LiveKit token (the SFU leg) and the
# model credentials, as a job worker does.

CALL_POST_WAIT_S <- 15

# ---- parent side -------------------------------------------------------------

# Start a call worker. `args`: `url` and `jwt` for the SFU, `room_id`,
# `cfg` (the bot config: model, provider, voice.stt, voice.tts,
# voice.call), `history` (the room's recent messages, as chat.api's
# chat_history gives them, for the brain's first turn), `names` (what
# the agent is called), `keys` (list of list(identity, key, index) to
# set before connecting; NULL for a room without encryption),
# `identity` (the LiveKit identity expected). Returns the worker handle.
call_worker_start <- function(args, dir = tempfile("call-")) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(dir, "answers"), showWarnings = FALSE)
    file.create(file.path(dir, "requests.jsonl"))
    file.create(file.path(dir, "commands.jsonl"))
    args$dir <- dir
    rs <- callr::r_session$new(wait = TRUE)
    rs$call(function(args) {
        corteza:::call_worker_main(args)
    }, list(args = args))
    w <- new.env(parent = emptyenv())
    w$rs <- rs
    w$dir <- dir
    w$read_from <- 0L
    w$room_id <- args$room_id
    w$result <- NULL
    w
}

# Alive until the loop returns or the process dies. Once it has
# returned, `w$result` holds list(value, error): the loop's summary, or
# the condition that ended it.
call_worker_alive <- function(w) {
    if (!is.null(w$result)) {
        return(FALSE)
    }
    alive <- tryCatch(w$rs$is_alive(), error = function(e) FALSE)
    if (!alive) {
        w$result <- list(value = NULL, error = "the call process died")
        return(FALSE)
    }
    state <- w$rs$get_state()
    if (identical(state, "idle") || identical(w$rs$poll_process(0), "ready")) {
        msg <- tryCatch(w$rs$read(), error = function(e) list(error = e))
        w$result <- list(value = msg$result,
                         error = if (!is.null(msg$error)) conditionMessage(msg$error))
        return(FALSE)
    }
    TRUE
}

# Read the child's new requests and act on them. `post(room_id, text)`
# returns an event id; `edit(room_id, event_id, text)`; `note(text)`
# takes a log line. Each answerable request gets its answer written,
# so the child stops waiting. Returns the requests handled.
call_worker_pump <- function(w, post, edit,
                             note = function(text) message(text)) {
    lines <- .call_lines_after(file.path(w$dir, "requests.jsonl"), w$read_from)
    w$read_from <- w$read_from + length(lines$lines)
    handled <- list()
    for (line in lines$lines) {
        req <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                        error = function(e) NULL)
        if (!is.list(req)) {
            next
        }
        handled[[length(handled) + 1L]] <- req
        type <- req$type %||% ""
        if (identical(type, "post")) {
            ans <- tryCatch(list(event_id = post(w$room_id, req$text)),
                            error = function(e) list(error = conditionMessage(e)))
            .call_answer_write(w$dir, req$id, ans)
        } else if (identical(type, "edit")) {
            ans <- tryCatch({
                edit(w$room_id, req$event_id, req$text)
                list(ok = TRUE)
            }, error = function(e) list(error = conditionMessage(e)))
            .call_answer_write(w$dir, req$id, ans)
        } else if (identical(type, "note")) {
            note(req$text %||% "")
        }
    }
    handled
}

# Tell the child something: a key (`list(type = "key", identity, key,
# index)`, key as base64) or `list(type = "leave")`.
call_worker_command <- function(w, cmd) {
    cat(jsonlite::toJSON(cmd, auto_unbox = TRUE, null = "null"), "\n",
        sep = "", file = file.path(w$dir, "commands.jsonl"), append = TRUE)
    invisible(w)
}

# End the call: ask the child to leave, give it a moment, then close
# the process either way.
call_worker_close <- function(w, grace = 5) {
    if (call_worker_alive(w)) {
        call_worker_command(w, list(type = "leave"))
        deadline <- Sys.time() + grace
        while (call_worker_alive(w) && Sys.time() < deadline) {
            Sys.sleep(0.1)
        }
    }
    tryCatch(w$rs$close(), error = function(e) NULL)
    invisible(w$result)
}

# ---- the mailbox ----------------------------------------------------------------

.call_lines_after <- function(path, n) {
    if (!file.exists(path)) {
        return(list(lines = character()))
    }
    all <- readLines(path, warn = FALSE)
    if (length(all) <= n) {
        return(list(lines = character()))
    }
    list(lines = all[(n + 1L):length(all)])
}

.call_answer_write <- function(dir, id, ans) {
    path <- file.path(dir, "answers", id)
    tmp <- paste0(path, ".tmp")
    writeLines(jsonlite::toJSON(ans, auto_unbox = TRUE, null = "null"), tmp)
    file.rename(tmp, path)
    invisible(path)
}

# Child side: append a request; for a post, wait for its answer.
.call_request <- function(box, req, wait = FALSE) {
    box$n <- box$n + 1L
    req$id <- sprintf("%06d", box$n)
    cat(jsonlite::toJSON(req, auto_unbox = TRUE, null = "null"), "\n",
        sep = "", file = file.path(box$dir, "requests.jsonl"), append = TRUE)
    if (!wait) {
        return(invisible(NULL))
    }
    path <- file.path(box$dir, "answers", req$id)
    deadline <- Sys.time() + box$wait_s
    while (!file.exists(path)) {
        if (Sys.time() > deadline) {
            stop("no answer from the bot process within ", box$wait_s, " s",
                 call. = FALSE)
        }
        # Keep the media flowing while waiting: frames would otherwise
        # pile up in the native queue.
        if (is.function(box$tick)) {
            box$tick()
        }
        Sys.sleep(0.05)
    }
    ans <- jsonlite::fromJSON(readLines(path, warn = FALSE), simplifyVector = FALSE)
    if (!is.null(ans$error)) {
        stop(ans$error, call. = FALSE)
    }
    ans
}

# Child side: new commands since last read.
.call_commands <- function(box) {
    lines <- .call_lines_after(file.path(box$dir, "commands.jsonl"),
                               box$read_from)
    box$read_from <- box$read_from + length(lines$lines)
    Filter(is.list, lapply(lines$lines, function(l) {
        tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
    }))
}

# ---- child side ---------------------------------------------------------------

# The child's whole life: connect, run the loop with the parent's
# mailbox as the brain's room, leave. Returns a summary for the parent.
call_worker_main <- function(args) {
    box <- new.env(parent = emptyenv())
    box$dir <- args$dir
    box$n <- 0L
    box$read_from <- 0L
    box$wait_s <- args$post_wait_s %||% CALL_POST_WAIT_S
    note <- function(...) {
        .call_request(box, list(type = "note", text = paste0(...)))
    }
    call_opts <- args$cfg$voice$call %||% list()
    # livekitr reports through message(); in a child nobody reads those,
    # so every message becomes a note the bot process logs. The level is
    # the config's (`voice.call.livekit_log`: warn, info, debug, trace).
    if (is.character(call_opts$livekit_log)) {
        options(livekitr.log = call_opts$livekit_log)
    }
    withCallingHandlers(.call_worker_run(args, box, note, call_opts),
                        message = function(m) {
        note(trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
    })
}

.call_worker_run <- function(args, box, note, call_opts) {
    if (!requireNamespace("livekitr", quietly = TRUE)) {
        stop("a call needs livekitr installed", call. = FALSE)
    }
    e2ee <- if (length(args$keys)) call_e2ee_options()
    # `voice.call.ice_transport = "relay"` makes a node that only serves
    # media through TURN fail fast and say why, instead of waiting out
    # host candidates that can never pair.
    lk_opts <- list()
    if (is.character(call_opts$ice_transport)) {
        lk_opts$ice_transport <- call_opts$ice_transport
    }
    # `voice.call.connect_delay_s`: wait this long after the membership
    # was posted before joining the media room. A client that only
    # notices participants who connect after it does (FluffyChat 2.10's
    # ring) sees the agent only when the agent arrives second.
    delay <- suppressWarnings(as.numeric(call_opts$connect_delay_s %||% 0))
    if (length(delay) == 1L && is.finite(delay) && delay > 0) {
        note("waiting ", delay, " s before connecting")
        Sys.sleep(delay)
    }
    note("connecting to ", args$url,
        if (length(lk_opts)) paste0(" (ice_transport = ",
                                    lk_opts$ice_transport, ")"))
    session <- livekitr::lk_connect(args$url, args$jwt, e2ee = e2ee, opts = lk_opts)
    on.exit(try(livekitr::lk_disconnect(session), silent = TRUE), add = TRUE)
    for (k in args$keys) {
        .call_set_key(session, k, note)
    }
    if (!is.null(args$identity) &&
        !identical(session$identity, args$identity)) {
        note("joined as ", session$identity, ", expected ", args$identity)
    }
    media <- call_media_livekit(session)
    box$tick <- function() media$poll(0)

    cfg <- args$cfg
    history <- args$history %||% list()
    state <- voice_state(function() cfg, list(
            post = function(room_id, text) {
        .call_request(box, list(type = "post", text = text), wait = TRUE)$event_id
    },
            edit = function(room_id, event_id, text) {
        .call_request(box,
                      list(type = "edit", event_id = event_id, text = text),
                      wait = TRUE)
        invisible(TRUE)
    },
            history = function(room_id) history,
            members = function(room_id) character()))
    brain <- call_brain(state, args$room_id)
    speech <- call_speech(call_media_config(cfg))
    cl <- call_loop_new(media, speech$stt, speech$tts, brain,
                        list(answer = call_opts$answer %||% "addressed",
                             names = args$names, vad = call_opts$vad,
                             log = note))
    note("in the call as ", session$identity)
    # `voice.call.greeting`: said once on joining. It tells the people in
    # the call the agent is there, and it exercises the agent's own
    # track and key before anyone has spoken.
    if (is.character(call_opts$greeting) && nzchar(call_opts$greeting)) {
        call_loop_step(cl, 0.5)
        call_loop_say(cl, call_opts$greeting)
    }
    call_loop_run(cl, until = function() {
        for (cmd in .call_commands(box)) {
            if (identical(cmd$type, "leave")) {
                return(TRUE)
            }
            if (identical(cmd$type, "key")) {
                .call_set_key(session, cmd, note)
            }
        }
        FALSE
    })
    list(turns = cl$turns, humans = call_humans(cl), stopped = cl$stopped)
}

# A participant's media key, as mx.client hands it on: base64 bytes and
# the key index both sides use.
.call_set_key <- function(session, k, note = NULL) {
    key <- jsonlite::base64_dec(k$key)
    index <- as.integer(k$index %||% 0L)
    # The identity and index, never the key: an undecryptable track is
    # almost always a key filed under another identity or index.
    if (is.function(note)) {
        note("key for ", k$identity, " at index ", index, " (",
             length(key), " bytes)")
    }
    livekitr::lk_set_e2ee_key(session, key, identity = k$identity,
                              key_index = index)
}

# How a MatrixRTC call is encrypted, as Element Call and FluffyChat
# configure the frame cryptor: keys used as given (HKDF), one slot per
# key index, a few failed frames tolerated while keys are in flight.
# The same values mx.client's mx_call_connect() uses; a key set here by
# identity and index is what those clients expect to match.
call_e2ee_options <- function() {
    list(kdf = "hkdf", key_ring_size = 256L, ratchet_window_size = 10L,
         failure_tolerance = 10L)
}
