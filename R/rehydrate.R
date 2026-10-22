# Rehydrating a folded conversation.
#
# A conversation that ended at /clear is archived to the vault and, once
# stale, the host folds it into a topic room as a thread whose root
# stands for it. Replying to that thread should continue the conversation
# rather than start a blank one in a room full of unrelated topics, so
# the first message in such a thread seeds its session from the archive.
#
# Which archive a thread stands for is the host's knowledge (cerebro
# keeps an index in the topic room's state), asked through the
# thread_context hook given to bot_run_init(). This file owns what
# corteza knows: resolving the archive path, bounding the transcript,
# and seeding the session with it.

# The archive a thread root stands for, through the host's hook, or NULL
# when there is no hook, the root is not a fold (an ordinary thread
# someone started in a room), or the answer is not a usable pointer.
bot_thread_context <- function(thread_context, chat, room_id, thread_root) {
    if (!is.function(thread_context) || is.null(thread_root) ||
        !nzchar(thread_root)) {
        return(NULL)
    }
    ctx <- thread_context(chat, room_id, thread_root)
    if (!is.list(ctx)) {
        return(NULL)
    }
    ref <- ctx$vault
    if (!is.character(ref) || length(ref) != 1L || is.na(ref) || !nzchar(ref)) {
        return(NULL)
    }
    list(segment = ctx$segment, vault = ref)
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
bot_rehydrate_session <- function(session, chat, room_id, thread_root,
                                  thread_context = NULL) {
    # Marked before the lookup, not after, and whatever the outcome:
    # the flag means "this session has been considered", so a thread
    # nobody folded into is asked about once rather than on every
    # message for the life of the process.
    session$rehydrate_checked <- TRUE
    archive <- bot_thread_context(thread_context, chat, room_id, thread_root)
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

# Seed a thread's session from its archive, once.
#
# The gate is a flag on the session rather than "was this session just
# created", because a session can reach its first live message already
# populated: startup backfill builds one for every thread in its window.
# Keying on freshness meant a restart left an active thread answering
# from the backfilled tail alone, with the conversation it continues
# lost for good.
#
# Errors are contained here, so a rehydration that fails cannot take
# the turn with it. Without a thread_context hook there is nothing to
# ask, and the session is left as it is without being marked: a hook
# cannot appear later in a process, so the mark would buy nothing.
bot_maybe_rehydrate <- function(session, chat, room_id, thread,
                                thread_context = NULL) {
    if (!is.function(thread_context) || is.null(thread) || !nzchar(thread) ||
        isTRUE(session$rehydrate_checked)) {
        return(invisible(FALSE))
    }
    tryCatch(bot_rehydrate_session(session, chat, room_id, thread,
                                   thread_context),
             error = function(e) {
        session$rehydrate_checked <- TRUE
        message("corteza: rehydrate failed: ", conditionMessage(e))
        invisible(FALSE)
    })
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
