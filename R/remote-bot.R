# The bot is the sole writer of inboxes and acknowledgements. The CLI is the
# sole writer of outboxes. Keep sent/consumed records so redelivery is inert.
.remote_accept <- function(request, cfg, chat, sessions) {
    path <- file.path(request$dir, "accepted.json")
    old <- .remote_read_file(path)
    if (!is.null(old)) {
        return(old)
    }
    result <- tryCatch({
        operators <- intersect(unlist(request$controllers), bot_operators(cfg))
        if (!length(operators)) stop("No configured operator may control this session.")
        room <- chat.api::chat_resolve(chat, request$room)
        if (!is.character(room) || length(room) != 1L ||
                       !nzchar(room)) stop("Room not found.")
        others <- Filter(function(x) !identical(x$id, request$id),
                         .remote_sessions(request$owner))
        occupied <- any(vapply(others, function(x) identical(
                    .remote_read_file(file.path(x$dir, "accepted.json"))$room, room), TRUE))
        if (occupied) stop("This room already controls another CLI session.")
        s <- if (!is.null(sessions) &&
                       exists(room, sessions, inherits = FALSE)) get(room, sessions)
        if ((is.environment(s) &&
                       (!is.null(s$.job_current) || length(s$.job_events))) ||
                       length(job_list(status = JOB_STATUSES_OPEN, origin_key = room,
                                       owner = request$owner))) {
            stop("The room has delegated work in flight; finish it before attaching.")
        }
        list(room = room, controllers = as.list(operators))
    }, error = function(e) list(error = conditionMessage(e)))
    .remote_write(path, result)
    result
}

.remote_bot_pump <- function(cfg, chat, sessions = NULL) {
    .remote_advertise(cfg)
    for (request in .remote_sessions(bot_job_owner(cfg), active = FALSE)) {
        # Closed/dead sessions can still have final replies waiting to send.
        accepted <- .remote_read_file(file.path(request$dir, "accepted.json"))
        if (is.null(accepted) && .remote_live(request) &&
            !file.exists(file.path(request$dir, "closed.json"))) {
            accepted <- .remote_accept(request, cfg, chat, sessions)
        }
        if (is.null(accepted$room) || !is.null(accepted$error)) {
            next
        }
        records <- .remote_records(request$dir, "out")
        for (record in records) {
            if (is.null(record) || file.exists(paste0(record$path, ".sent"))) {
                next
            }
            # Revocation takes effect before the next send as well as on input.
            if (!length(intersect(unlist(accepted$controllers),
                                  bot_operators(cfg)))) {
                break
            }
            text <- record$text %||% ""
            chunks <- chunk_text(text, limit = 6000L)
            complete <- TRUE
            for (i in seq_along(chunks)) {
                marker <- paste0(record$path, ".", i, ".sent")
                if (file.exists(marker)) {
                    next
                }
                sent <- tryCatch(bot_reply_send(chat, accepted$room, chunks[[i]]),
                                 error = function(e) NULL)
                if (!is.character(sent) || length(sent) != 1L || is.na(sent) ||
                    !nzchar(sent)) {
                    complete <- FALSE
                    break
                }
                .remote_write(marker, list(event_id = sent))
                s <- if (!is.null(sessions) &&
                    exists(accepted$room, sessions, inherits = FALSE))
                get(accepted$room, sessions)
                if (is.environment(s)) {
                    s$seen_event_ids <- bot_remember_event(s$seen_event_ids, sent)
                    bot_transcript_add(s, sent, "assistant", chunks[[i]])
                }
            }
            if (!complete) {
                break
            }
            .remote_write(paste0(record$path, ".sent"), list(sent = TRUE))
        }
    }
    invisible(NULL)
}

.remote_bot_route <- function(m, cfg) {
    if (!is.null(m$thread)) {
        return(FALSE)
    }
    for (request in .remote_sessions(bot_job_owner(cfg))) {
        accepted <- .remote_read_file(file.path(request$dir, "accepted.json"))
        if (!identical(accepted$room, m$channel)) {
            next
        }
        if (!isTRUE(as.numeric(m$ts) >= request$created)) {
            return(TRUE)
        }
        # While attached, this bot must not start a second model for the room.
        if (isTRUE(m$is_self) || !m$sender %in%
            intersect(unlist(accepted$controllers), bot_operators(cfg))) {
            return(TRUE)
        }
        seen <- file.path(request$dir, "seen",
                          paste0(digest::digest(m$id), ".json"))
        if (file.exists(seen)) {
            return(TRUE)
        }
        text <- trimws(m$body %||% "")
        if (identical(text, "/stop") ||
            identical(text, "/remote-control off")) {
            .remote_write(file.path(request$dir, "stop.json"), list(id = m$id))
        }
        parts <- strsplit(text, "\\s+")[[1L]]
        if (length(parts) == 2L && parts[[1L]] %in% c("/approve", "/deny") &&
            grepl("^[0-9]{8}T[0-9]{6}-[0-9a-f]{8}$", parts[[2L]])) {
            path <- file.path(request$dir, "approvals", parts[[2L]])
            pending <- .remote_read_file(paste0(path, ".request"))
            if (!is.null(pending) && !file.exists(paste0(path, ".closed")) &&
                !file.exists(paste0(path, ".answer")) &&
                isTRUE(as.numeric(Sys.time()) < pending$expires)) {
                .remote_write(paste0(path, ".answer"), list(
                        approved = identical(parts[[1L]], "/approve"), by = m$sender))
            }
        } else if (!identical(text, "/stop")) {
            # IDs in the inbox make replay after a crash before 'seen' harmless.
            inbox <- .remote_records(request$dir, "in")
            if (!any(vapply(inbox, function(x) identical(x$id, m$id), TRUE))) {
                .remote_append(request$dir, "in", list(id = m$id, text = text,
                        sender = m$sender, media = length(m$attachments) > 0L))
            }
        }
        .remote_write(seen, list(id = m$id))
        return(TRUE)
    }
    FALSE
}

.remote_bot_safe <- function(cfg, chat, sessions = NULL) {
    tryCatch(.remote_bot_pump(cfg, chat, sessions), error = function(e) {
        message("remote control: ", conditionMessage(e))
    })
}

# Remote control changes who answers, not what belongs in the room archive.
# Reuse the room ledger's event IDs so sync redelivery and outgoing echoes do
# not add a second copy. The CLI alone owns the provider working history.
.remote_bot_record <- function(session, m) {
    if (m$id %in% session$seen_event_ids) {
        return(invisible(NULL))
    }
    if (isTRUE(m$is_self)) {
        role <- "assistant"
    } else {
        role <- "user"
    }
    if (isTRUE(m$is_self)) {
        body <- m$body
    } else {
        body <- bot_ingest_body(m$sender, m$body, attribute_sender = TRUE)
    }

    bot_transcript_add(session, m$id, role, body)
    session$seen_event_ids <- bot_remember_event(session$seen_event_ids, m$id)
    .context_room_snapshot(session)
    invisible(NULL)
}
