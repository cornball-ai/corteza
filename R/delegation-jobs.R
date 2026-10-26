.delegation_state <- function(id) job_read_file(file.path(job_dir(id),
        "remote.json")) %||% list()
.delegation_save <- function(id, state) job_write_file(file.path(job_dir(id),
        "remote.json"),
    state)

.delegation_peers <- function(cfg) {
    entries <- cfg$delegates %||% list()
    if (!is.list(entries)) {
        return(list())
    }
    entries <- Filter(function(x) is.list(x) && is.character(x$bot) &&
                      length(x$bot) == 1L &&
                      !is.na(x$bot) && nzchar(x$bot) && is.character(x$channel) &&
                      length(x$channel) == 1L && !is.na(x$channel) &&
                      nzchar(x$channel), entries)
    if (is.null(names(entries))) names(entries) <- vapply(entries,
        function(x) x$bot, "")
    entries[nzchar(names(entries))]
}

.delegation_allow <- function(cfg, sender) {
    entries <- cfg$delegators %||% list()
    for (entry in entries) {
        if (is.character(entry) && identical(entry, sender)) {
            return(list(bot = sender))
        }
        if (is.list(entry) && identical(entry$bot, sender)) {
            return(entry)
        }
    }
    NULL
}

.delegation_submit <- function(session, cfg, bot, task, review = FALSE) {
    if (!is.character(task) || length(task) != 1L || is.na(task) ||
        !nzchar(trimws(task)) || nchar(task, type = "bytes") > 7000L) {
        stop("Delegated task must be non-empty and at most 7000 UTF-8 bytes",
             call. = FALSE)
    }
    peers <- .delegation_peers(cfg)
    peer <- peers[[bot]]
    if (is.null(peer)) {
        matched <- Filter(function(x) identical(x$bot, bot), peers)
        if (length(matched) == 1L) {
            peer <- matched[[1L]]
        }
    }
    if (is.null(peer)) {
        stop("No configured delegate named ", bot, call. = FALSE)
    }
    if (identical(peer$bot, job_worker_owner(session))) {
        stop("Cannot delegate to this bot itself", call. = FALSE)
    }
    if (!isTRUE(session$job_requester %in% bot_operators(cfg))) {
        stop("Only an operator may request work from another bot", call. = FALSE)
    }
    origin <- session$job_origin %||% list(room = session$room_id)
    origin$session_key <- job_worker_key(session)
    origin$remote <- list(peer = peer$bot, channel = peer$channel,
                          transport = peer$transport %||% "matrix", hop = 1L, hops = 3L)
    id <- job_create(task, workspace = session$cwd %||% getwd(),
                     requester = session$job_requester, owner = job_worker_owner(session),
                     origin = origin, backend = "remote", review = isTRUE(review))
    # The poll loop sends the durable intent, including after a restart.
    id
}

.delegation_find_incoming <- function(owner, sender, channel, transport, id,
                                      message = NULL) {
    Filter(function(j) {
        d <- j$origin$delegation
        !is.null(d) && is.null(j$review_of) && identical(d$peer, sender) &&
        identical(d$transport, transport) && identical(d$channel, channel) &&
        (identical(d$id, id) ||
            (!is.null(message) && identical(d$message, message)))
    }, job_list(owner = owner))
}

.delegation_worker_reply <- function(chat, job, key = NULL) {
    d <- job$origin$delegation
    if (is.null(d)) {
        return(invisible(NULL))
    }
    final <- job$status %in% JOB_STATUSES_FINAL
    .delegation_post(chat, job$owner, d$peer, d$channel,
        if (final) "result" else "accepted",
                     list(id = d$id, job = job$id, status = if (final) job$status,
                          of = job$review_of),
        if (final) bot_job_result_text(job) else paste("Queued as job", job$id),
                     thread = d$thread,
                     key = key %||% paste(job$id, job$status))
}

.delegation_receive_request <- function(m, p, cfg, chat, sessions) {
    owner <- bot_job_owner(cfg)
    transport <- .delegation_transport(chat)
    fields <- p$fields
    refuse <- function(reason) .delegation_post(chat, owner, m$sender,
        m$channel, "refused", list(id = fields$id, reason = reason),
        paste("Request refused:", reason), m$thread,
        key = paste(m$id, "refused"))
    if (!isTRUE(p$version == 1L)) {
        return(refuse("version"))
    }
    if (isTRUE(m$encrypted) && identical(m$sender_verified, FALSE)) {
        return(refuse("unverified"))
    }
    allow <- .delegation_allow(cfg, m$sender)
    if (is.null(allow)) {
        return(refuse("not-allowed"))
    }
    valid_id <- tryCatch({ job_check_id(fields$id) ; TRUE }, error = function(e) FALSE)
    if (!valid_id || !nzchar(trimws(p$text))) {
        return(refuse("invalid"))
    }
    prior <- .delegation_find_incoming(owner, m$sender, m$channel, transport, fields$id, m$id)
    if (length(prior)) {
        return(.delegation_worker_reply(chat, prior[[1L]], key = m$id))
    }
    expires <- suppressWarnings(as.numeric(fields$expires))
    if (length(expires) != 1L || is.na(expires) || !is.finite(expires) ||
        expires < as.numeric(Sys.time())) {
        return(refuse("expired"))
    }
    if (file.exists(.delegation_cancel_path(owner, m$sender, m$channel, transport, fields$id))) {
        return(refuse("cancelled"))
    }
    if (any(vapply(.remote_sessions(owner), function(x) identical(
                    .remote_read_file(file.path(x$dir, "accepted.json"))$room, m$channel), TRUE))) {
        return(refuse("busy"))
    }
    hop <- suppressWarnings(as.numeric(fields$hop))
    hops <- suppressWarnings(as.numeric(fields$hops))
    if (length(hop) != 1L || length(hops) != 1L || is.na(hop) || is.na(hops) ||
        !is.finite(hop) || !is.finite(hops) || hop < 1 || hop != trunc(hop) ||
        hops != trunc(hops) || hop > min(hops, JOB_DEFAULT_LIMITS$hops)) {
        return(refuse("hop-limit"))
    }
    root <- fields$root %||% fields$id
    active_root <- function(j) identical(j$origin$delegation$root %||%
        j$origin$remote$root %||% j$id, root)
    if (any(vapply(job_list(status = JOB_STATUSES_OPEN, owner = owner), active_root, TRUE))) {
        return(refuse("busy"))
    }
    key <- bot_session_key(m$channel, m$thread)
    s <- bot_get_or_create_session(sessions, key, cfg, room_id = m$channel)
    spec <- job_worker_spec(s)
    restricted <- allow$tools
    if (!is.null(restricted)) {
        restricted <- as.character(unlist(restricted, use.names = FALSE))
        if (!is.null(spec$tools)) {
            restricted <- intersect(restricted, spec$tools)
        }
    }
    origin <- list(room = m$channel, thread = m$thread, session_key = key,
                   delegation = list(id = fields$id, peer = m$sender, channel = m$channel,
                                     thread = m$thread, transport = transport, message = m$id,
                                     hop = hop, root = root, tools = restricted))
    id <- job_create(p$text, workspace = s$cwd, requester = m$sender,
                     owner = owner, origin = origin, limits = list(hops = min(hops, 3L)),
                     permissions = list(tools = restricted %||% spec$tools),
                     review = isTRUE(allow$review) || identical(fields$review, "true"))
    request_text <- paste0("Delegated task from ", m$sender, " (job ", id, "):\n", p$text)
    s$history <- c(s$history, list(list(role = "user", content = request_text)))
    bot_transcript_add(s, m$id, "user", request_text)
    .context_room_snapshot(s)
    .delegation_worker_reply(chat, job_read(id))
    invisible(id)
}

.delegation_receive_result <- function(m, p, cfg, chat) {
    j <- tryCatch(job_read(p$fields$id), error = function(e) NULL)
    d <- j$origin$remote
    if (is.null(d) || !identical(j$owner, bot_job_owner(cfg)) ||
        !identical(d$peer, m$sender) || !identical(d$channel, m$channel) ||
        !identical(d$transport, .delegation_transport(chat)) ||
        (isTRUE(m$encrypted) && identical(m$sender_verified, FALSE))) {
        return(invisible(NULL))
    }
    state <- .delegation_state(j$id)
    if (p$kind == "accepted" && j$status %in% JOB_STATUSES_OPEN) {
        if (!is.null(state$job) && !identical(state$job, p$fields$job)) {
            return(invisible(NULL))
        }
        job_check_id(p$fields$job)
        state$job <- p$fields$job
        state$last_seen <- as.numeric(Sys.time())
        state$probe <- NULL
        .delegation_save(j$id, state)
    } else if (p$kind == "refused") {
        status <- if (identical(p$fields$reason, "cancelled")) "cancelled" else
        if (identical(p$fields$reason, "unknown")) {
            "indeterminate"
        } else {
            "failed"
        }
        job_settle(j$id, status,
                   reason = paste("Remote refusal:", p$fields$reason))
    } else if (p$kind == "result" && p$fields$status %in% JOB_STATUSES_FINAL) {
        job_check_id(p$fields$job)
        if (!is.null(p$fields$of)) {
            job_check_id(p$fields$of)
            if (!is.null(state$job) && !identical(p$fields$of, state$job)) {
                return(invisible(NULL))
            }
            # One authenticated review addendum, without replacing the doer result.
            if (is.null(state$review)) {
                state$review <- list(job = p$fields$job, of = p$fields$of, text = p$text)
                .delegation_save(j$id, state)
            }
        } else if (is.null(state$job) || identical(state$job, p$fields$job)) {
            state$job <- p$fields$job
            .delegation_save(j$id, state)
            job_settle(j$id, p$fields$status, result = p$text)
        }
    }
    invisible(NULL)
}

.delegation_pump <- function(chat, cfg, sessions,
                             now = as.numeric(Sys.time())) {
    owner <- bot_job_owner(cfg)
    for (j in job_list(owner = owner)) {
        if (!is.null(j$origin$delegation)) {
            if (j$status %in% JOB_STATUSES_FINAL) {
                .delegation_worker_reply(chat, j)
            }
            next
        }
        if (!identical(j$backend, "remote")) {
            next
        }
        d <- j$origin$remote
        state <- .delegation_state(j$id)
        if (j$status == "queued") {
            if (isTRUE(j$cancel_requested)) {
                job_settle(j$id, "cancelled",
                           reason = "Cancelled before remote dispatch")
                next
            }
            job_mark_dispatched(j$id, worker = list(bot = d$peer))
            state <- list(pid = Sys.getpid(), last_seen = now)
            .delegation_save(j$id, state)
            .delegation_post(chat, owner, d$peer, d$channel, "request",
                             list(id = j$id, hop = d$hop, hops = d$hops, root = d$root %||% j$id,
                                  expires = format(now + 120, scientific = FALSE, trim = TRUE),
                                  review = if (isTRUE(j$review)) "true" else "false"),
                             j$task, key = paste(j$id, "request"))
        } else if (j$status == "running") {
            if (!is.null(state$probe) && now - state$probe > 120) {
                job_settle(j$id, "indeterminate", reason = "Remote worker did not answer the status probe; work may have run. It will not be resubmitted.")
            } else if (is.null(state$probe) &&
                (!identical(state$pid, Sys.getpid()) ||
                    now - (state$last_seen %||% 0) > if (is.null(state$job)) {
                        120
                    } else {
                        300
                    })) {
                state$pid <- Sys.getpid()
                state$probe <- now
                .delegation_save(j$id, state)
                .delegation_post(chat, owner, d$peer, d$channel, "status",
                                 list(id = j$id, job = state$job), key = paste(j$id, "status", now))
            }
            if (isTRUE(j$cancel_requested) && !isTRUE(state$cancel_sent)) {
                .delegation_post(chat, owner, d$peer, d$channel, "cancel",
                                 list(id = j$id, job = state$job), key = paste(j$id, "cancel"))
                state$cancel_sent <- TRUE
                .delegation_save(j$id, state)
            }
        }
        j <- job_read(j$id)
        s <- if (!is.null(j$origin$session_key) &&
            exists(j$origin$session_key, sessions, inherits = FALSE))
        get(j$origin$session_key, sessions)
        if (j$status %in% JOB_STATUSES_FINAL && !isTRUE(state$reported)) {
            if (is.environment(s)) {
                bot_job_settled(chat, s, j, sessions)
            } else
            bot_post_or_queue(chat, owner, j$origin$room, bot_job_result_text(j),
                              thread = j$origin$thread, job = j$id)
            state$reported <- TRUE
            .delegation_save(j$id, state)
        }
        if (!is.null(state$review) && identical(j$status, "done") &&
            identical(state$review$of, state$job) &&
            !isTRUE(state$review_reported)) {
            bot_post_or_queue(chat, owner, j$origin$room, state$review$text,
                              thread = j$origin$thread, session = s, job = j$id)
            if (is.environment(s)) {
                s$history <- c(s$history, list(list(role = "assistant", content = state$review$text)))
                .context_room_snapshot(s)
            }
            state$review_reported <- TRUE
            .delegation_save(j$id, state)
        }
    }
    .delegation_flush(chat, owner)
    invisible(NULL)
}
