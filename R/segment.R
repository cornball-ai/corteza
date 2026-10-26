# /clear-time conversation segmentation: corteza's half.
#
# When a conversation ends at /clear, the host may give it a room of its
# own: a "segment" the user can reopen from the room list while it is
# fresh (cerebro does this, through the on_clear hook given to
# bot_run_init(), for the rooms its config names). Creating that room is
# the host's; what this file owns is read from the session transcript,
# which is corteza's data: the segment's title and whether the
# conversation is worth a room at all, plus the durable form of the
# archive path the hook is handed.
#
# Segmentation is best-effort from the /clear handler's point of view:
# archive-and-reset is the critical path, and a hook that fails must not
# break it. It also rides on the archive: a clear that archived nothing
# (empty conversation, or pensar absent) asks the hook nothing, because
# the pointer to the transcript is the segment's content.

# A line that drives the tool rather than saying anything: "/clear",
# "/reset", "/model sonnet".
#
# The first token must be a slash followed by a word and then a space or
# the end of the line. The trailing condition is the whole rule: without
# it "/home/troy/cerebro" is a command, and a user whose opening line is
# a bare path loses it -- the title falls back to "Conversation" and the
# segment stops being worth keeping. A false positive here deletes
# content; a false negative just leaves an ugly title.
#
# Worth its own predicate because such a line is neither a title nor
# content, and both of those judgements are made below.
.bot_is_command_line <- function(line) {
    grepl("^/[[:alnum:]][[:alnum:]_-]*([[:space:]]|$)", line)
}

# The user's own lines, commands excluded, in order.
#
# A conversation's substance is what the user actually said. "/clear" is
# not that, and a conversation consisting only of it has nothing to
# archive under any name.
.bot_user_lines <- function(session) {
    out <- character()
    for (e in session$transcript %||% list()) {
        if (!identical(e$role %||% "", "user")) {
            next
        }
        text <- if (is.character(e$content)) {
            paste(e$content, collapse = "\n")
        } else {
            as.character(e$content %||% "")
        }
        for (line in strsplit(text, "\n", fixed = TRUE)[[1L]]) {
            line <- trimws(line)
            if (!nzchar(line) || .bot_is_command_line(line)) {
                next
            }
            out <- c(out, line)
        }
    }
    out
}

# Title for a segment room: the opening line of the conversation it
# holds. The first human line is what the user typed to start the
# topic, which beats a generated label for recognizing it in a room
# list. Must be read before bot_archive_session() drains the
# transcript.
#
# Commands are skipped. The fork auto-prepends a bare "/clear" as its own
# message, so a conversation that opened with one was titled "/clear
# (2026-08-20)" -- the tool's own vocabulary, on a room in the user's
# sidebar, saying nothing about what is inside it.
bot_segment_title <- function(session, max_chars = 60L) {
    lines <- .bot_user_lines(session)
    if (length(lines)) {
        return(sprintf("%s (%s)",
                       .sanitize_inline(lines[[1L]], max_chars = max_chars),
                       format(Sys.Date())))
    }
    sprintf("Conversation (%s)", format(Sys.time(), "%Y-%m-%d %H:%M"))
}

# Whether a cleared conversation is worth a room of its own.
#
# A /clear immediately after a /clear ends a conversation in which the
# user said nothing. It archives fine and used to get a room named after
# the command that ended it. The room is the cost here: it is permanent,
# it sits in the sidebar, and nothing in it will ever be worth reopening.
bot_segment_worth_keeping <- function(session) {
    length(.bot_user_lines(session)) > 0L
}

# The durable form of an archive path: relative to the pensar vault
# root when it lives there, so the pointer survives the vault moving
# hosts. Anything else passes through unchanged.
bot_vault_ref <- function(path) {
    if (is.null(path) || !length(path) || !nzchar(path[[1L]])) {
        return(NULL)
    }
    p <- normalizePath(as.character(path[[1L]]), mustWork = FALSE)
    root <- tryCatch(
                     normalizePath(getExportedValue("pensar", "default_vault")(),
                                   mustWork = FALSE),
                     error = function(e) NULL
    )
    if (!is.null(root) && nzchar(root) && startsWith(p, paste0(root, "/"))) {
        return(substring(p, nchar(root) + 2L))
    }
    p
}
