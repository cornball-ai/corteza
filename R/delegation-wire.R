# Bot-to-bot job messages use the existing chat client and its single poll
# cursor. Protocol notices never enter a model's conversation dispatcher.
.delegation_parse <- function(text) {
    if (!is.character(text) || length(text) != 1L || is.na(text) ||
        !startsWith(text, "[corteza-job ")) {
        return(NULL)
    }
    line <- strsplit(text, "\n", fixed = TRUE)[[1L]][[1L]]
    invalid <- list(version = NA_integer_, kind = "invalid", fields = list(),
                    text = "")
    if (nchar(line, type = "bytes") > 2048L ||
        !grepl("^\\[corteza-job v[0-9]+ [a-z]+( [A-Za-z0-9_]+=[A-Za-z0-9_.:@+%/-]+)*\\]$", line)) {
        return(invalid)
    }
    parts <- strsplit(substring(line, 2L, nchar(line) - 1L), " ", fixed = TRUE)[[1L]]
    fields <- list()
    for (part in parts[-(1:3)]) {
        pair <- strsplit(part, "=", fixed = TRUE)[[1L]]
        if (pair[[1L]] %in% names(fields)) {
            return(invalid)
        }
        fields[[pair[[1L]]]] <- utils::URLdecode(pair[[2L]])
    }
    list(version = suppressWarnings(as.integer(substring(parts[[2L]], 2L))),
         kind = parts[[3L]], fields = fields,
         text = substring(text, nchar(line) + 2L))
}

.delegation_body <- function(kind, fields, text = "") {
    fields <- Filter(Negate(is.null), fields)
    values <- vapply(fields,
                     function(x) utils::URLencode(as.character(x), reserved = TRUE), "")
    paste0("[corteza-job v1 ", kind, " ",
           paste(paste(names(values), values, sep = "="), collapse = " "),
           "]\n", text)
}

.delegation_transport <- function(chat) {
    sub("^chat_", "", class(chat)[[1L]])
}

.delegation_outbox <- function() file.path(bot_signal_dir(),
    "delegation-outbox")

# Commit the notice before sending. A crash after delivery may repeat a
# notice; the receiver correlates it to an existing job and never reruns work.
.delegation_post <- function(chat, owner, peer, channel, kind, fields,
                             text = "", thread = NULL, key = NULL) {
    dir <- .delegation_outbox()
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    id <- digest::digest(list(owner, key %||% job_new_id(), peer, channel,
                              kind, fields, text))
    path <- file.path(dir, paste0(id, ".json"))
    if (!file.exists(path) && !file.exists(paste0(path, ".sent"))) {
        job_write_file(path, list(owner = owner, peer = peer, channel = channel,
                                  kind = kind, fields = fields, text = text, thread = thread))
    }
    if (!is.null(chat)) {
        .delegation_deliver(chat, path)
    }
    invisible(id)
}

.delegation_deliver <- function(chat, path) {
    if (!file.exists(path) || file.exists(paste0(path, ".sent"))) {
        return(FALSE)
    }
    n <- job_read_file(path)
    if (identical(n$kind, "request")) {
        job <- tryCatch(job_read(n$fields$id), error = function(e) NULL)
        expired <- as.numeric(n$fields$expires %||% 0) < as.numeric(Sys.time())
        if (is.null(job) || job$status %in% JOB_STATUSES_FINAL || expired) {
            job_write_file(paste0(path, ".sent"),
                           list(discarded = TRUE, at = job_now()))
            unlink(path)
            return(FALSE)
        }
    }
    caps <- chat.api::chat_capabilities(chat)
    limit <- caps$max_message_bytes
    if (!is.numeric(limit) || length(limit) != 1L || is.na(limit)) {
        limit <- 64000
    }
    # Reserve envelope/escaping space as well as the protocol header. The
    # original result remains in the job ledger even on small transports.
    budget <- min(8000L, floor(limit / 8))
    header <- .delegation_body(n$kind, n$fields)
    available <- max(0, budget - nchar(header, type = "bytes") - 40L)
    body <- n$text
    if (nchar(body, type = "bytes") > available) {
        if (identical(n$kind, "request")) {
            job_settle(n$fields$id, "failed", reason = "Delegated task exceeds the transport message limit; shorten the task. Nothing was sent.")
            job_write_file(paste0(path, ".sent"), list(discarded = TRUE, at = job_now()))
            unlink(path)
            return(FALSE)
        }
        n$fields$truncated <- "true"
        while (nchar(body, type = "bytes") > available) {
            body <- substr(body, 1L, max(0L, floor(nchar(body) * 0.8)))
        }
        body <- paste0(body, "\n[Report truncated; full text in worker ledger.]")
    }
    wire <- .delegation_body(n$kind, n$fields, body)
    if (nchar(wire, type = "bytes") > budget) {
        return(FALSE)
    }
    sent <- tryCatch(chat.api::chat_send(chat, n$channel,
            wire, kind = "notice", markup = "plain",
            mentions = n$peer, thread = if (isTRUE(caps$threads)) n$thread),
                     error = function(e) NULL)
    if (!is.character(sent) || length(sent) != 1L || is.na(sent) ||
        !nzchar(sent)) {
        return(FALSE)
    }
    job_write_file(paste0(path, ".sent"), list(event = sent, at = job_now()))
    unlink(path)
    TRUE
}

.delegation_flush <- function(chat, owner) {
    paths <- list.files(.delegation_outbox(), pattern = "[.]json$",
                        full.names = TRUE)
    for (path in paths) {
        n <- job_read_file(path)
        if (identical(n$owner, owner)) {
            .delegation_deliver(chat, path)
        }
    }
    invisible(NULL)
}
