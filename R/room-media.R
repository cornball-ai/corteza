# A host-owned attachment sender. The model supplies only a local path;
# identity, destination, encryption, and event bookkeeping stay in the host.
.room_media_tools <- function(tools, session) {
    tools <- Filter(function(t) !identical(t$name, "send_room_file"), tools)
    if (!is.function(session$send_room_file) || isTRUE(session$plan_mode)) {
        return(tools)
    }
    c(tools, list(list(name = "send_room_file", description = paste(
        "Send an existing file requested by the user to this conversation.",
        "Use this for images and attachments instead of loading chat credentials or running Matrix code.",
        "The host uses this room and bot identity, encrypting attachments for encrypted rooms.",
        "Maximum size is 25 MiB. This tool cannot choose another room or send credential files.",
        "Only report success after the tool returns an event ID."),
        input_schema = list(type = "object", properties = list(
            path = list(type = "string", description = "Local path to the requested file.")),
            required = list("path"), additionalProperties = FALSE))))
}

.room_media_bind <- function(session, cfg, chat_now, sender, room, thread = NULL) {
    force(chat_now)
    force(room)
    force(thread)
    session$send_room_file <- if (isTRUE(sender %in% bot_operators(cfg))) {
        function(path) .room_media_send(session, chat_now(), room, thread, path)
    } else NULL
    invisible(session)
}

.room_media_send <- function(session, chat, room, thread, path) {
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        return(err("Name one existing local file."))
    }
    cwd <- session$cwd %||% getwd()
    path <- .resolve_real(path, cwd)
    size <- file.info(path)
    if (nrow(size) != 1L || is.na(size$size) || !file_test("-f", path)) {
        return(err("The attachment is not an existing file."))
    }
    if (scan_sensitive(path) || .path_within(path, file.path(cwd, ".git")) ||
        .path_within(path, file.path(cwd, ".corteza"))) {
        return(err("Credential and runtime configuration files cannot be sent as room attachments."))
    }
    if (size$size > 25 * 1024^2) {
        return(err("The attachment exceeds the 25 MiB limit."))
    }
    caps <- chat.api::chat_capabilities(chat)
    if (!isTRUE(caps$files)) {
        return(err("This chat transport cannot send attachments with its installed dependencies."))
    }
    if (!is.null(thread)) {
        # The current file contract cannot preserve thread relations. Do not
        # quietly move a private thread's file to the main room timeline.
        return(err("File sending is currently available in the room's main conversation only."))
    }
    ids <- chat.api::chat_send(chat, room, "", files = path)
    ids <- as.character(unlist(ids, use.names = FALSE))
    if (!length(ids) || anyNA(ids) || any(!nzchar(ids))) {
        return(err("The transport returned no attachment event ID; delivery is unconfirmed."))
    }
    for (id in ids) {
        session$seen_event_ids <- bot_remember_event(session$seen_event_ids, id)
        bot_transcript_add(session, id, "assistant", paste("[Sent file]", basename(path)))
    }
    .context_room_snapshot(session)
    ok(paste("Sent", basename(path), "to this conversation. Event:", paste(ids, collapse = ", ")))
}
