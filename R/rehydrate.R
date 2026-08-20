# Rehydrating a folded conversation.
#
# A conversation that ended at /clear is archived to the vault and, once
# stale, folded into a topic room as a thread whose root stands for it.
# Replying to that thread should continue the conversation rather than
# start a blank one in a room full of unrelated topics, so the first
# message in such a thread seeds its session from the archive.
#
# The mapping from thread root to archive is a state event the fold
# wrote into the topic room, keyed by the root's event id:
#
#   ai.cornball.fold / <root event id> -> {segment, vault}
#
# Read rather than derived, and O(1): the alternative was scanning the
# topic room's history for the root event, which grows with the room and
# is exactly the work the index exists to avoid.

# The archive a thread root stands for, or NULL when the root is not a
# fold (an ordinary thread someone started in a room, a topic room
# nobody folded into). Best-effort: a homeserver that cannot answer
# leaves the session unseeded rather than failing the turn, because an
# unseeded reply is a worse conversation and a failed one is no
# conversation at all.
bot_thread_archive <- function(chat, room_id, thread_root) {
    if (is.null(thread_root) || !nzchar(thread_root)) {
        return(NULL)
    }
    caps <- tryCatch(chat.api::chat_capabilities(chat),
                     error = function(e) list())
    if (!isTRUE(caps$set_state)) {
        return(NULL)
    }
    st <- tryCatch(
                   chat.api::chat_get_state(chat, room_id, "ai.cornball.fold",
            state_key = thread_root),
                   error = function(e) {
        message("bot_thread_archive: could not read fold index: ",
                conditionMessage(e))
        NULL
    }
    )
    if (!is.list(st)) {
        return(NULL)
    }
    ref <- st$vault
    if (!is.character(ref) || length(ref) != 1L || !nzchar(ref)) {
        return(NULL)
    }
    list(segment = st$segment, vault = ref)
}

# Absolute path of a vault-relative archive reference. The inverse of
# bot_vault_ref(): the pointer is stored relative to the vault root so
# it survives the vault moving hosts, which means resolving it needs
# the root back.
bot_vault_path <- function(ref) {
    if (is.null(ref) || !length(ref) || !nzchar(ref[[1L]])) {
        return(NULL)
    }
    p <- as.character(ref)[[1L]]
    if (startsWith(p, "/")) {
        return(p)
    }
    root <- tryCatch(getExportedValue("pensar", "default_vault")(),
                     error = function(e) NULL)
    if (is.null(root) || !length(root) || !nzchar(root)) {
        return(NULL)
    }
    file.path(root, p)
}

# The archived transcript, tail-bounded.
#
# The tail rather than the head: resuming a conversation, the last
# exchanges are the ones the next message answers, and the fold's root
# message already carries the topic. Bounded because the whole point is
# to continue a conversation, not to spend the context window
# reconstructing one -- a transcript stuffed in whole crowds out the
# turns that follow it.
bot_archive_excerpt <- function(path, max_chars = 6000L) {
    if (is.null(path) || !file.exists(path)) {
        return(NULL)
    }
    txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL)
    if (is.null(txt) || !nzchar(txt)) {
        return(NULL)
    }
    if (nchar(txt) <= max_chars) {
        return(txt)
    }
    paste0("[earlier turns omitted]\n\n",
           substring(txt, nchar(txt) - max_chars + 1L))
}

# Seed a fresh session with a folded conversation's archive.
#
# The transcript goes in as one framing exchange rather than replayed
# turn by turn. Replaying would mean parsing the archive's markdown back
# into roles, and a mis-parse puts words in the user's mouth -- the one
# error a resumed conversation cannot survive. One turn that says
# plainly what this is keeps the provenance visible to the model and
# costs a single exchange.
#
# Returns TRUE when it seeded, invisibly, so a caller can tell a
# rehydrated session from a blank one.
bot_rehydrate_session <- function(session, chat, room_id, thread_root) {
    archive <- bot_thread_archive(chat, room_id, thread_root)
    if (is.null(archive)) {
        return(invisible(FALSE))
    }
    excerpt <- bot_archive_excerpt(bot_vault_path(archive$vault))
    if (is.null(excerpt)) {
        message("bot_rehydrate_session: archive ", archive$vault,
                " is unreadable; continuing unseeded")
        return(invisible(FALSE))
    }
    framing <- paste0(
                      "This thread continues an earlier conversation that was ",
                      "archived. Its transcript follows; treat it as the ",
                      "conversation so far and carry on from it.\n\n", excerpt)
    session$history <- c(
                         list(list(role = "user", content = framing),
                              list(role = "assistant",
                                   content = paste("I have the earlier conversation in",
                    "mind. Go ahead."))),
                         session$history %||% list())
    message("corteza: rehydrated thread ", thread_root, " from ", archive$vault)
    invisible(TRUE)
}

# The registry key for a session.
#
# A room's main timeline and each of its threads are separate
# conversations, so they get separate sessions: without this, a reply in
# a folded thread would land in the room's own history and every topic
# in that room would share one context. \r cannot appear in a Matrix
# room id or event id, so the composite cannot collide with a bare room
# id.
bot_session_key <- function(room_id, thread = NULL) {
    if (is.null(thread) || !length(thread) || !nzchar(thread[[1L]])) {
        return(room_id)
    }
    paste0(room_id, "\r", as.character(thread)[[1L]])
}
