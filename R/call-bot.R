# A room's call, from the bot process's side.
#
# The bot process owns the Matrix account, so it is the one that joins
# the call's signaling (chat.api::chat_call_join: membership, the media
# token, the per-member keys over Olm) and the one that posts and edits
# in the room. The hearing and talking happen in a call worker
# (R/call-worker.R) in a process of its own. This file connects the two:
# start the worker from what the join returned, hand it every key that
# arrives on later polls, post and edit on its behalf, and leave when it
# ends or is told to.
#
# `/call` in a room joins that room's call; `/hangup` leaves it. One
# call per room session, kept on the session as `session$call`.

# While a call is on, the long poll is cut short so the worker's
# requests (a reply to post) are answered within this many milliseconds
# plus whatever the homeserver holds an empty sync for.
BOT_CALL_POLL_MS <- 500L

bot_parse_call_command <- function(body) {
    cmd <- bot_command_text(body)
    if (!nzchar(cmd)) {
        return(NULL)
    }
    if (grepl("^/+call\\s*$", cmd, perl = TRUE, ignore.case = TRUE)) {
        return("join")
    }
    if (grepl("^/+(hangup|hang\\s*up|leave\\s+call)\\s*$", cmd, perl = TRUE,
              ignore.case = TRUE)) {
        return("hangup")
    }
    NULL
}

# Act on a call command. Returns the acknowledgement to post.
bot_call_command <- function(session, cfg, chat, room_id, cmd) {
    if (identical(cmd, "hangup")) {
        if (is.null(session$call)) {
            return("Not in a call here.")
        }
        bot_call_stop(session, chat)
        return("Left the call.")
    }
    if (!is.null(session$call)) {
        return("Already in this room's call. /hangup to leave it.")
    }
    why <- bot_call_unavailable(cfg, chat)
    if (!is.null(why)) {
        return(paste("Cannot join a call:", why))
    }
    tryCatch({
        bot_call_start(session, cfg, chat, room_id)
        "Joining the call."
    }, error = function(e) {
        paste("Could not join the call:", conditionMessage(e))
    })
}

# NULL when this bot can take part in a call, else the reason it cannot.
bot_call_unavailable <- function(cfg, chat) {
    if (!requireNamespace("livekitr", quietly = TRUE)) {
        return("livekitr is not installed")
    }
    caps <- tryCatch(chat.api::chat_capabilities(chat),
                     error = function(e) list())
    if (!isTRUE(caps$calls)) {
        return(paste("the chat transport cannot join calls (it needs an",
                     "encrypted Matrix client and a chat.api that joins calls)"))
    }
    if (!is.list(cfg$voice$stt) || !is.list(cfg$voice$tts)) {
        return("voice.stt and voice.tts are not configured")
    }
    NULL
}

# Join the call's signaling and start the worker from what it returned.
bot_call_start <- function(session, cfg, chat, room_id,
                           start = call_worker_start) {
    call <- chat.api::chat_call_join(chat, room_id)
    media <- chat.api::chat_call_media(call)
    keys <- c(list(bot_call_key(media$identity, media$key)),
              lapply(media$peers, function(p) bot_call_key(p$identity, p)))
    history <- tryCatch(chat.api::chat_history(chat, room_id, limit = 30L)$messages,
                        error = function(e) list())
    dir <- file.path(bot_call_dir(), gsub("[^A-Za-z0-9]", "_", room_id))
    w <- start(list(url = media$url, jwt = media$jwt, identity = media$identity,
                    keys = keys, room_id = room_id,
                    cfg = bot_call_child_cfg(cfg), history = history,
                    names = bot_call_names(cfg)),
               dir = dir)
    session$call <- list(call = call, worker = w, room_id = room_id,
                         started = Sys.time())
    invisible(session$call)
}

# The worker's copy of the config: what the room session and the speech
# routes need, and none of the Matrix credentials.
bot_call_child_cfg <- function(cfg) {
    drop <- c("token", "password", "sync_token", "device_id", "bots",
              "operators", "talker")
    out <- cfg[setdiff(names(cfg), drop)]
    if (is.list(out$voice)) {
        out$voice$allocator_token <- NULL
    }
    out
}

# What the agent answers to in a call with several people: its display
# name, its Matrix localpart, and any `voice.call.names`.
bot_call_names <- function(cfg) {
    # `[[`: `cfg$user` would answer with user_id when user is absent.
    own <- c(cfg[["display_name"]], cfg[["user"]],
             sub("^@([^:]+):.*$", "\\1", cfg[["user_id"]] %||% ""))
    unique(c(as.character(cfg$voice$call$names %||% character()),
             own[nzchar(own)]))
}

bot_call_key <- function(identity, k) {
    list(type = "key", identity = identity,
         key = jsonlite::base64_enc(as.raw(k$key)),
         index = as.integer(k$index))
}

bot_call_dir <- function() {
    file.path(corteza_data_path("calls"))
}

# Each poll: hand the worker the keys that arrived, answer its requests,
# and notice when it has ended. Returns TRUE while the call is on.
bot_call_pump <- function(session, chat) {
    c0 <- session$call
    if (is.null(c0)) {
        return(FALSE)
    }
    upd <- chat.api::chat_call_updates(c0$call)
    for (k in upd$keys) {
        call_worker_command(c0$worker, bot_call_key(k$identity, k))
    }
    if (!is.null(upd$own)) {
        call_worker_command(c0$worker, bot_call_key(c0$call$identity, upd$own))
    }
    call_worker_pump(c0$worker,
                     post = function(room_id, text) {
        bot_event_id(bot_reply_send(chat, room_id, text))
    },
                     edit = function(room_id, event_id, text) {
        chat.api::chat_edit(chat, room_id, event_id, text)
    },
                     note = function(text) message("corteza call ", c0$room_id, ": ", text))
    if (!call_worker_alive(c0$worker)) {
        result <- c0$worker$result
        why <- if (!is.null(result$error)) {
            paste("the call ended with an error:", result$error)
        } else {
            "the call ended"
        }
        message("corteza call ", c0$room_id, ": ", why)
        bot_call_stop(session, chat, note = why)
        return(FALSE)
    }
    TRUE
}

# End a call: stop the worker, leave the signaling, say so in the room.
bot_call_stop <- function(session, chat, note = NULL) {
    c0 <- session$call
    if (is.null(c0)) {
        return(invisible(FALSE))
    }
    session$call <- NULL
    tryCatch(call_worker_close(c0$worker), error = function(e) NULL)
    tryCatch(chat.api::chat_call_leave(chat, c0$call), error = function(e) {
        message("corteza call ", c0$room_id, ": could not leave: ",
                conditionMessage(e))
    })
    if (!is.null(note)) {
        tryCatch(bot_reply_send(chat, c0$room_id, paste0("Left the call: ", note)),
                 error = function(e) NULL)
    }
    invisible(TRUE)
}

# Pump every room session that is in a call. Called once per poll.
bot_calls_pump <- function(sessions, chat) {
    if (is.null(chat)) {
        return(invisible(0L))
    }
    n <- 0L
    for (key in ls(sessions, all.names = TRUE)) {
        s <- get(key, envir = sessions)
        if (is.environment(s) && !is.null(s$call)) {
            n <- n + 1L
            tryCatch(bot_call_pump(s, chat), error = function(e) {
                message("corteza call: ", conditionMessage(e))
            })
        }
    }
    invisible(n)
}

# Any session in a call?
bot_calls_active <- function(sessions) {
    for (key in ls(sessions, all.names = TRUE)) {
        s <- get(key, envir = sessions)
        if (is.environment(s) && !is.null(s$call)) {
            return(TRUE)
        }
    }
    FALSE
}
