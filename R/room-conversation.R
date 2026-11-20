# A room's conversations (WP20a, rule 1).
#
# At the terminal a conversation's id is its session id. A room or a
# thread has no session id on disk, so the bot keeps a record for each:
# an id, and the event that started it. The record is made with the
# session (bot_get_or_create_session()), so a restart goes on with the
# same conversation, and replaced at /clear (bot_conversation_begin(),
# before the room is answered or the session replaced). The
# job system reads the id (session_conversation(), R/job-conversation.R),
# and a restart's backfill seeds nothing from before the starting event
# (bot_backfill_sessions(), rule 9).
#
# One file per room and bot, in the state folder beside the archive's
# keys, named the way the room's journal is (bot_session_id()): 2 bots in
# 1 room keep 2 files. The room's own timeline is `main`, and each thread
# is under `threads` by its root event. Retention never removes it: it
# removes journals, traces and the job, worker and remote-control
# stores, and this folder is none of those.

bot_room_state_path <- function(cfg, room_id) {
    file.path(bot_signal_dir(), "rooms",
              paste0(bot_session_id(cfg, room_id), ".json"))
}

# The room's state, or an empty list when there is none.
#
# A file that is there and cannot be read is copied aside, named for
# when, since it holds every thread's record. In its place goes a state
# whose binding is unknown, which holds the room to no directory until
# an operator runs /cd (bot_room_binding()). Read as no binding at all,
# it would give the bot's default directory to a room an operator had
# bound to none or to another directory. The marker replaces the file
# in one rename, so until it is written the unreadable file stays, and
# the next read finds it the same way.
bot_room_state_read <- function(cfg, room_id) {
    path <- bot_room_state_path(cfg, room_id)
    if (!file.exists(path)) {
        return(list())
    }
    state <- tryCatch(job_read_file(path), error = function(e) e)
    if (is.list(state) && !inherits(state, "error")) {
        return(state)
    }
    marker <- list(binding = list(state = "unknown"))
    aside <- paste0(path, ".unreadable-", format(Sys.time(), "%Y%m%dT%H%M%S"))
    if (!file.exists(aside) && !isTRUE(file.copy(path, aside))) {
        message("bot: could not copy aside a room state file it could not read: ",
                path, "; the room reads and talks only")
        log_event("room_state_unreadable", level = "warn", path = path,
                  replaced = FALSE)
        return(marker)
    }
    replaced <- tryCatch({
        bot_room_state_write(cfg, room_id, marker)
        TRUE
    }, error = function(e) FALSE)
    message("bot: set aside a room state file it could not read: ", aside,
            "; the room reads and talks only until an operator runs /cd")
    log_event("room_state_unreadable", level = "warn", path = aside,
              replaced = replaced)
    marker
}

bot_room_state_write <- function(cfg, room_id, state) {
    path <- bot_room_state_path(cfg, room_id)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    job_write_file(path, state)
}

# The thread of a session key (bot_session_key()), or NULL for the room.
bot_key_thread <- function(key) {
    parts <- strsplit(key, "\r", fixed = TRUE)[[1L]]
    if (length(parts) > 1L) {
        return(parts[[2L]])
    }
    NULL
}

# A new conversation's record. `start` is the message that began it (a
# /clear). The ids of the bot's answer to it, which belongs to the
# conversation that ended, are added once it is sent
# (bot_conversation_acked()). A conversation made with a session has
# neither: nothing before it is left out.
bot_conversation_new <- function(start = NULL) {
    record <- list(id = job_new_id())
    if (!is.null(start$id)) {
        record$since <- as.character(start$id)
        at <- suppressWarnings(as.numeric(start$ts))
        if (length(at) == 1L && is.finite(at)) {
            record$at <- at
        }
    }
    record
}

# The record of the conversation `key` holds, or NULL.
bot_conversation_get <- function(cfg, key, room_id) {
    state <- bot_room_state_read(cfg, room_id)
    thread <- bot_key_thread(key)
    record <- if (is.null(thread)) {
        state$main
    } else if (is.list(state$threads)) {
        state$threads[[thread]]
    }
    id <- record$id
    if (!is.list(record) || !is.character(id) || length(id) != 1L ||
        is.na(id) || !nzchar(id)) {
        return(NULL)
    }
    record
}

# Record `record` as the conversation `key` holds. Failing to write it
# costs the room no session: the session holds the record, and only a
# restart loses it, which the log says.
bot_conversation_save <- function(cfg, key, room_id, record) {
    tryCatch({
        state <- bot_room_state_read(cfg, room_id)
        state$room <- room_id
        thread <- bot_key_thread(key)
        if (is.null(thread)) {
            state$main <- record
        } else {
            threads <- if (is.list(state$threads)) state$threads else list()
            threads[[thread]] <- record
            state$threads <- threads
        }
        bot_room_state_write(cfg, room_id, state)
        TRUE
    }, error = function(e) {
        message("bot: could not save the conversation record for ", room_id,
                "; a restart will not find it: ", conditionMessage(e))
        log_event("room_conversation_unsaved", level = "warn",
                  room = room_id, error = conditionMessage(e))
        FALSE
    })
}

# The conversation a /clear begins (`start`), saved before anything acts
# on it: a restart opens the conversation the record names, so a /clear
# whose record could not be written would come undone at the next
# restart. NULL when it could not be saved, and then the room's
# conversation goes on.
bot_conversation_begin <- function(cfg, key, room_id, start) {
    record <- bot_conversation_new(start)
    if (!isTRUE(bot_conversation_save(cfg, key, room_id, record))) {
        return(NULL)
    }
    record
}

# The record with the ids of the bot's answer to the /clear that began
# it, saved again once the answer is sent, so a restart's backfill leaves
# the answer out. A save that fails leaves the record without them: the
# conversation stands, and the answer would only be seeded into it.
bot_conversation_acked <- function(cfg, key, room_id, record, ack) {
    ack <- as.character(ack %||% character())
    ack <- ack[!is.na(ack) & nzchar(ack)]
    if (!length(ack)) {
        return(record)
    }
    record$ack <- ack
    bot_conversation_save(cfg, key, room_id, record)
    record
}

# The conversation a new session for `key` takes: the one on record, so
# a restart goes on with it, or a new one, recorded now. A new one in a
# room that was moved (/cd, R/room-binding.R) starts at the move, so a
# thread first seen after it is seeded with nothing from before.
bot_conversation_open <- function(cfg, key, room_id) {
    record <- bot_conversation_get(cfg, key, room_id)
    if (is.null(record)) {
        record <- bot_conversation_new(bot_room_state_read(cfg, room_id)$moved)
        bot_conversation_save(cfg, key, room_id, record)
        # Made now, so its record for the monitor begins now too
        # (bot_get_or_create_session()). Not saved: it describes this
        # process's call.
        attr(record, "made") <- TRUE
    }
    record
}

bot_conversation_attach <- function(session, record) {
    session$conversation_id <- record$id
    session$conversation_record <- record
    invisible(session)
}

# Whether message `i` of a backfill window (event ids `ids`, in order)
# comes from before its session's conversation began (WP20a, rule 9): at
# or before the event that started it, by the window's order, or, when
# that event is not in the window, older than it. The bot's answer to a
# /clear goes with the conversation that ended.
bot_conversation_before <- function(session, ids, i, ts) {
    record <- session$conversation_record
    if (is.null(record$since)) {
        return(FALSE)
    }
    if (ids[[i]] %in% record$ack) {
        return(TRUE)
    }
    j <- match(record$since, ids)
    if (!is.na(j)) {
        return(i <= j)
    }
    isTRUE(suppressWarnings(as.numeric(ts)) < record$at)
}
