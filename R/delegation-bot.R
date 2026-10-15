.delegation_tools <- function(tools, session) {
    tools <- Filter(function(t) !identical(t$name, "delegate_to"), tools)
    if (!is.function(session$delegate_to) || isTRUE(session$plan_mode)) return(tools)
    ensure_skills()
    existing <- vapply(tools, function(t) t$name, "")
    tools <- c(tools, skills_as_api_tools(setdiff(c("job_status", "job_cancel"), existing)))
    c(tools, list(list(name = "delegate_to",
        description = paste("Ask a configured bot to run a self-contained task asynchronously.",
            "The result will return here; do not invent the answer.",
            "Allowed delegates:", paste(session$delegation_names, collapse = ", ")),
        input_schema = list(type = "object", properties = list(
            bot = list(type = "string", enum = as.list(session$delegation_names)),
            task = list(type = "string"), review = list(type = "boolean")),
            required = list("bot", "task"), additionalProperties = FALSE))))
}

.delegation_bind <- function(session, cfg) {
    peers <- .delegation_peers(cfg)
    session$delegation_names <- names(peers)
    session$delegate_to <- if (length(peers)) function(bot, task, review = FALSE) {
        id <- .delegation_submit(session, cfg, bot, task, review)
        ok(paste("Delegated as remote job", id, ". The result will return to this conversation."))
    } else NULL
    invisible(session)
}

.delegation_cancel_path <- function(owner, peer, channel, transport, id) {
    file.path(bot_signal_dir(), "delegation-cancelled",
        paste0(digest::digest(list(owner, peer, channel, transport, id)), ".json"))
}

# A human's text answer has the same sender, room, expiry and first-answer
# checks as a reaction. It is also available on transports without reactions.
.delegation_approval <- function(m, sessions, req, approved) {
    if (isTRUE(m$encrypted) && identical(m$sender_verified, FALSE)) return(TRUE)
    for (s in bot_sessions_list(sessions)) {
        for (eid in names(s$.job_prompts)) {
            p <- s$.job_prompts[[eid]]
            if (!identical(p$req, req) || !identical(p$room, m$channel) ||
                !isTRUE(m$sender %in% p$approvers)) next
            job_answer(s, p$job, p$req, approved, by = m$sender)
            s$.job_prompts[[eid]] <- NULL
            return(TRUE)
        }
    }
    FALSE
}

.delegation_route <- function(m, cfg, chat, sessions) {
    p <- .delegation_parse(m$body)
    if (is.null(p)) {
        match <- regmatches(m$body, regexec("^(approve|deny) ([A-Za-z0-9_-]+)$", trimws(m$body)))[[1L]]
        if (length(match) == 3L) return(.delegation_approval(m, sessions, match[[3L]], match[[2L]] == "approve"))
        return(FALSE)
    }
    if (isTRUE(m$is_self)) {
        key <- bot_session_key(m$channel, m$thread)
        if (exists(key, sessions, inherits = FALSE)) {
            s <- get(key, sessions)
            bot_transcript_add(s, m$id, "assistant", m$body)
        }
        return(TRUE)
    }
    if (!isTRUE(m$addressed)) return(TRUE)
    if (p$kind == "request") {
        .delegation_receive_request(m, p, cfg, chat, sessions)
    } else if (isTRUE(p$version == 1L)) {
        if (p$kind %in% c("accepted", "refused", "result")) {
            .delegation_receive_result(m, p, cfg, chat)
        } else if (p$kind %in% c("approve", "deny")) {
            .delegation_approval(m, sessions, p$fields$req, p$kind == "approve")
        } else if (p$kind %in% c("status", "cancel") &&
            !(isTRUE(m$encrypted) && identical(m$sender_verified, FALSE))) {
            jobs <- .delegation_find_incoming(bot_job_owner(cfg), m$sender,
                m$channel, .delegation_transport(chat), p$fields$id)
            if (!length(jobs) && !is.null(p$fields$job) && m$sender %in% bot_operators(cfg)) {
                j <- tryCatch(job_read(p$fields$job), error = function(e) NULL)
                if (!is.null(j$origin$delegation) && identical(j$owner, bot_job_owner(cfg)) &&
                    identical(j$origin$room, m$channel)) jobs <- list(j)
            }
            if (length(jobs)) {
                j <- jobs[[1L]]
                if (p$kind == "cancel" && job_request_cancel(j$id, by = m$sender) && j$status == "queued") {
                    job_settle(j$id, "cancelled", reason = "Cancelled before dispatch")
                }
                .delegation_worker_reply(chat, job_read(j$id), key = m$id)
            } else if (p$kind == "cancel" && !is.null(.delegation_allow(cfg, m$sender))) {
                job_check_id(p$fields$id)
                path <- .delegation_cancel_path(bot_job_owner(cfg), m$sender,
                    m$channel, .delegation_transport(chat), p$fields$id)
                dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
                job_write_file(path, list(at = job_now()))
                .delegation_post(chat, bot_job_owner(cfg), m$sender, m$channel,
                    "refused", list(id = p$fields$id, reason = "cancelled"),
                    "Request cancelled before receipt.", key = m$id)
            } else if (p$kind == "status" && !is.null(.delegation_allow(cfg, m$sender))) {
                .delegation_post(chat, bot_job_owner(cfg), m$sender, m$channel,
                    "refused", list(id = p$fields$id, reason = "unknown"),
                    "No matching job is recorded; no work was restarted.", key = m$id)
            }
        }
    }
    TRUE
}
