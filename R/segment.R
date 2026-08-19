# /clear-time conversation segmentation.
#
# When a cleared room is listed in the bot config's `segment_rooms`
# (a character vector of room ids, hand-added to the saved config),
# the conversation that just ended gets a room of its own: a "segment"
# the user can reopen from the room list while it is fresh. The segment
# room holds a pointer to the archived transcript and an
# ai.cornball.lifecycle state event that clients group and demote by.
#
# Segmentation is best-effort from the /clear handler's point of view:
# archive-and-reset is the critical path, and a homeserver that cannot
# create a room must not break it. It also rides on the archive: a
# clear that archived nothing (empty conversation, or pensar absent)
# makes no segment, because the pointer to the transcript is the
# segment's content.

# Title for a segment room: the opening line of the conversation it
# holds. The first human line is what the user typed to start the
# topic, which beats a generated label for recognizing it in a room
# list. Must be read before bot_archive_session() drains the
# transcript.
bot_segment_title <- function(session, max_chars = 60L) {
    entries <- session$transcript %||% list()
    for (e in entries) {
        if (!identical(e$role %||% "", "user")) {
            next
        }
        text <- if (is.character(e$content)) {
            paste(e$content, collapse = "\n")
        } else {
            as.character(e$content %||% "")
        }
        if (!nzchar(trimws(text))) {
            next
        }
        line <- trimws(strsplit(text, "\n", fixed = TRUE)[[1L]][[1L]])
        if (!nzchar(line)) {
            next
        }
        return(sprintf("%s (%s)",
                       .sanitize_inline(line, max_chars = max_chars),
                       format(Sys.Date())))
    }
    sprintf("Conversation (%s)", format(Sys.time(), "%Y-%m-%d %H:%M"))
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
                     normalizePath(getExportedValue("pensar",
                                                    "default_vault")(),
                                   mustWork = FALSE),
                     error = function(e) NULL
    )
    if (!is.null(root) && nzchar(root) && startsWith(p, paste0(root, "/"))) {
        return(substring(p, nchar(root) + 2L))
    }
    p
}

# Create the segment room for a conversation that /clear just ended.
# Returns list(id, name) on success, NULL when the transport cannot do
# it. Errors from the create itself propagate to the caller's tryCatch:
# a segment the user was told about must exist, so the failure has to
# reach the handler that words the acknowledgement.
bot_segment_from_clear <- function(chat, home_id, title, vault_ref) {
    caps <- chat.api::chat_capabilities(chat)
    if (!isTRUE(caps$channel_create) || !isTRUE(caps$set_state)) {
        message("bot_segment_from_clear: transport cannot create rooms ",
                "or set state; skipping")
        return(invisible(NULL))
    }
    # Everyone in the home room except the bot itself. A segment room
    # without its human is a sidebar entry no one sees. Best-effort:
    # a transport without a member list yields an uninvited room, which
    # still archives correctly.
    invite <- tryCatch({
        members <- chat.api::chat_members(chat, home_id)
        setdiff(members, chat.api::chat_whoami(chat)$id)
    }, error = function(e) character())
    seg_id <- chat.api::chat_channel_create(chat, name = title,
                                            preset = "private_chat",
                                            invite = invite)
    home_label <- bot_room_name(chat, home_id) %||% home_id
    if (!length(home_label) || !nzchar(home_label)) {
        home_label <- home_id
    }
    body <- paste0(
        "Continued from ", home_label, ".",
        if (!is.null(vault_ref)) paste0("\nArchived transcript: ", vault_ref),
        "\nReply here to pick the topic back up."
    )
    # The summary is bot bookkeeping, not conversation: a notice renders
    # without a notification ping.
    tryCatch(chat.api::chat_send(chat, seg_id, body, kind = "notice"),
             error = function(e) {
        message("bot_segment_from_clear: summary send failed: ",
                conditionMessage(e))
        NULL
    })
    content <- list(state = "segment", segment_of = home_id,
                    since = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
                                   tz = "UTC"))
    if (!is.null(vault_ref)) {
        content$vault <- vault_ref
    }
    chat.api::chat_set_state(chat, seg_id, "ai.cornball.lifecycle", content)
    invisible(list(id = seg_id, name = title))
}
