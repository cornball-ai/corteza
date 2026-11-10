# A room belongs to the durable CLI session, not its checkout or a temporary
# attachment mailbox. Only the courier writes this index. Matrix's immutable
# creation event lets a retry recover a room after an ambiguous create reply.
.remote_room_file <- function(owner, session_id) {
    file.path(.remote_root(), "rooms",
              paste0(digest::digest(list(owner, session_id)), ".json"))
}

.remote_room_marker <- function(request) {
    list(owner = request$owner, session_id = request$session_id)
}

.remote_room_recover <- function(chat, marker) {
    matches <- Filter(function(room) {
        state <- chat.api::chat_get_state(chat, room, "m.room.create")
        saved <- state[["ai.cornball.corteza.remote"]]
        identical(saved$owner, marker$owner) && identical(saved$session_id, marker$session_id)
    }, chat.api::chat_channels(chat))
    if (length(matches) > 1L) {
        stop("Multiple rooms claim this CLI session; choose a room explicitly.")
    }
    if (length(matches)) matches[[1L]] else NULL
}

.remote_room <- function(request, cfg, chat, operators) {
    id <- request$session_id
    if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
        stop("A durable CLI session ID is required to create its room.")
    }
    caps <- chat.api::chat_capabilities(chat)
    if (!all(vapply(c("channel_create", "channel_invite", "set_state", "channels"),
                    function(x) isTRUE(caps[[x]]), TRUE))) {
        stop("Session rooms need chat.api >= 0.2.0 and a transport with room creation, ",
             "invitations and state. Update the courier or specify an existing room.")
    }
    if (isTRUE(cfg$e2ee) && !isTRUE(caps$e2ee)) {
        stop("The courier must support encryption before creating this session room.")
    }
    path <- .remote_room_file(request$owner, id)
    binding <- .remote_read_file(path)
    if (is.null(binding) && file.exists(path)) {
        stop("The session room record is unreadable; refusing to create a duplicate.")
    }
    marker <- .remote_room_marker(request)
    if (!is.null(binding) && is.null(binding$room)) {
        binding$room <- .remote_room_recover(chat, marker)
    }
    binding <- binding %||% c(marker, list(name = paste0("Corteza: ", id)))
    if (isTRUE(binding$encrypted) && !isTRUE(caps$e2ee)) {
        stop("The courier must support encryption to reconnect this session room.")
    }
    if (is.null(binding$room)) {
        .remote_room_write(path, binding)
        topic <- paste("CLI session", id)
        if (!is.null(request$cwd)) topic <- paste(request$cwd, topic, sep = " | ")
        binding$room <- chat.api::chat_channel_create(chat, binding$name,
            topic = topic, visibility = "private", preset = "private_chat",
            invite = character(),
            creation_content = list("ai.cornball.corteza.remote" = marker))
        if (!is.character(binding$room) || length(binding$room) != 1L ||
            is.na(binding$room) || !nzchar(binding$room)) stop("Room creation returned no ID.")
    }
    # Save the ID before encryption or invitations, so a partial setup is
    # resumed in the same room. Invite only after encryption is established.
    .remote_room_write(path, binding)
    if (isTRUE(cfg$e2ee) && !isTRUE(binding$encrypted)) {
        chat.api::chat_set_state(chat, binding$room, "m.room.encryption",
            list(algorithm = "m.megolm.v1.aes-sha2"))
        binding$encrypted <- TRUE
        .remote_room_write(path, binding)
    }
    for (operator in setdiff(operators, unlist(binding$invited))) {
        .remote_room_invite(chat, binding$room, operator)
        binding$invited <- as.list(c(unlist(binding$invited), operator))
        .remote_room_write(path, binding)
    }
    binding$ready <- TRUE
    .remote_room_write(path, binding)
    binding$room
}

# Matrix membership is authoritative when a successful invite was not recorded
# locally. Check again after an error in case the user joined during the call.
.remote_room_invite <- function(chat, room, operator) {
    present <- function() {
        membership <- chat.api::chat_get_state(chat, room, "m.room.member",
                                               state_key = operator)$membership
        isTRUE(membership %in% c("join", "invite"))
    }
    if (!present()) {
        tryCatch(chat.api::chat_channel_invite(chat, room, operator),
            error = function(e) {
                if (!present()) stop(e)
            })
    }
    invisible(NULL)
}

# A disconnected CLI room must not silently start an unrelated bot session.
.remote_room_offline <- function(m, cfg, chat) {
    index <- .remote_room_index(bot_job_owner(cfg))
    path <- index[[m$channel]]
    if (is.null(path)) return(FALSE)
    binding <- .remote_read_file(path)
    if (is.null(binding) || !identical(binding$owner, bot_job_owner(cfg)) ||
        !identical(binding$room, m$channel)) {
        rm(list = m$channel, envir = index)
        return(FALSE)
    }
    seen <- file.path(.remote_root(), "offline", digest::digest(m$id))
    if (!is.null(chat) && isTRUE(binding$ready) && !isTRUE(m$is_self) &&
        m$sender %in% bot_operators(cfg) && !file.exists(seen)) {
        sent <- bot_reply_send(chat, m$channel,
            "This CLI session is disconnected. Resume it in the terminal and run /rc to continue.")
        if (!is.null(sent)) .remote_write(seen, list(event_id = sent))
    }
    TRUE
}
