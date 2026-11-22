# Host hooks: policy the bot loop asks about but does not own.
#
# corteza runs a conversation. What happens to a conversation once it
# ends, where a thread's earlier context comes from, and which rooms'
# messages the host answers itself, are the host's decisions (a host can
# file segments and fold them into topic rooms, or run agent sessions in
# rooms of their own). The loop calls out through named
# hooks and carries on without them: with none supplied, /clear resets
# and nothing is filed, a thread reply starts from the thread's own
# history, and every message is the bot's to answer.

BOT_HOOK_NAMES <- c("on_clear", "thread_context", "on_message", "on_archive")

# Validate the host's hook list once, at init, so a misspelled slot is
# an error at startup rather than a hook that silently never fires.
bot_hooks <- function(hooks) {
    if (is.null(hooks)) {
        return(list())
    }
    if (!is.list(hooks) || (length(hooks) && is.null(names(hooks)))) {
        stop("hooks must be NULL or a named list of functions", call. = FALSE)
    }
    unknown <- setdiff(names(hooks), BOT_HOOK_NAMES)
    if (length(unknown)) {
        stop("unknown hook(s): ", paste(unknown, collapse = ", "),
             "; known: ", paste(BOT_HOOK_NAMES, collapse = ", "),
             call. = FALSE)
    }
    bad <- names(hooks)[!vapply(hooks, is.function, logical(1))]
    if (length(bad)) {
        stop("hook(s) must be functions: ", paste(bad, collapse = ", "),
             call. = FALSE)
    }
    hooks
}

# What on_clear handed back, or NULL when it is not a segment the
# acknowledgement can name. The hook's contract is list(id, name) or
# NULL; anything else is treated as "no segment" rather than letting a
# malformed return break the /clear acknowledgement.
bot_hook_segment <- function(seg) {
    if (!is.list(seg) || !is.character(seg$name) || length(seg$name) != 1L ||
        is.na(seg$name) || !nzchar(seg$name)) {
        return(NULL)
    }
    seg
}

# The host's first refusal on a message (the on_message hook): TRUE when
# the host took it, and the caller does nothing more with it. `incoming`
# is the transport's chat_message, raw included, which corteza's own
# record drops. Offered once per event and never for the bot's own
# account. An echo of that account in a room the host took is the
# host's too, recorded nowhere: the cards a host posts as the bot would
# otherwise pile up in a session no turn reads. An error is reported
# and taken as a refusal, since a broken hook must not silence the
# bot's own rooms.
bot_host_message <- function(hook, chat, cfg, sessions, session, incoming,
                             backfill = FALSE) {
    if (!is.function(hook)) {
        return(FALSE)
    }
    if (isTRUE(incoming$self)) {
        return(bot_host_room(sessions, incoming$channel))
    }
    if (incoming$id %in% session$seen_event_ids) {
        return(FALSE)
    }
    took <- tryCatch({
        room <- incoming$channel
        answer <- hook(chat, cfg, room, incoming, session, backfill)
        is.list(answer) && isTRUE(answer$handled)
    }, error = function(e) {
        message("on_message hook: ", conditionMessage(e))
        FALSE
    })
    if (took) {
        session$host_took <- TRUE
        seen <- bot_remember_event(session$seen_event_ids, incoming$id)
        session$seen_event_ids <- seen
    }
    took
}

# A room the host took: one of its sessions, the room's or a thread's,
# holds a message the on_message hook handled. Room-wide, since a host
# posts its replies where it likes, and for the life of the process.
bot_host_room <- function(sessions, room_id) {
    for (s in bot_sessions_list(sessions)) {
        if (isTRUE(s$host_took) && identical(s$room_id, room_id)) {
            return(TRUE)
        }
    }
    FALSE
}
