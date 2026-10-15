.room_consult_workers <- new.env(parent = emptyenv())

.room_consult_answer <- function(snapshot, .chat = llm.api::chat) {
    if (!is.null(snapshot$base_url)) {
        old <- llm.api::llm_base(snapshot$base_url)
        on.exit(llm.api::llm_base(old), add = TRUE)
    }
    answer <- do.call(.chat, snapshot$args)
    text <- answer$content %||% ""
    if (identical(answer$finish_reason, "length")) {
        return(list(error = paste("The consultation reached its output limit; this answer is incomplete:", text, sep = "\n\n"), usage = answer$usage))
    }
    if (!is.character(text) || length(text) != 1L || !nzchar(trimws(text))) {
        return(list(error = "The consultation returned no answer", usage = answer$usage))
    }
    list(reply = text, usage = answer$usage)
}

.room_consult_child <- function(id) {
    snapshot <- readRDS(file.path(job_dir(id), "consultation.rds"))
    .room_consult_answer(snapshot)
}

.room_consult_start <- function(id) {
    callr::r_bg(function(id) {
        get(".room_consult_child", envir = asNamespace("corteza"), inherits = FALSE)(id)
    }, args = list(id = id), libpath = .libPaths(), supervise = TRUE,
    stdout = file.path(job_dir(id), "consultation.stdout"),
    stderr = file.path(job_dir(id), "consultation.stderr"))
}

.room_consult_close <- function(id) {
    if (exists(id, .room_consult_workers, inherits = FALSE)) {
        worker <- get(id, .room_consult_workers)$process
        tryCatch(worker$kill(), error = function(e) NULL)
        rm(list = id, envir = .room_consult_workers)
    }
    invisible(NULL)
}

.room_consult_record <- function(chat, sessions, job, text, room, key, thread) {
    s <- if (!is.null(key) && exists(key, sessions, inherits = FALSE)) get(key, sessions)
    bot_post_or_queue(chat, job$owner, room, text, thread = thread,
                      session = s, session_key = key, job = job$id)
    if (is.environment(s)) {
        s$history <- c(s$history %||% list(), list(list(role = "assistant", content = text)))
        .context_room_record(s)
        journal_snapshot(s)
    }
}

.room_consult_notice <- function(chat, sessions, job, final = FALSE) {
    stage <- if (final) "answer" else "question"
    o <- job$origin
    text <- if (final) {
        paste0("**Answer from ", o$label, "** (", job$id, ", ", job$status, ")\n\n",
            if (job$status == "done") job$outcome$result else
                job$outcome$error %||% job$outcome$reason %||% job$status)
    } else paste0("**Question from ", o$from$label, "** (", job$id, ")\n\n", job$task,
        "\n\nThe answer uses this conversation's text context at the time of the request and returns to both conversations.")
    destinations <- list(target = list(room = o$room, key = o$session_key, thread = o$thread),
                         source = list(room = o$from$room, key = o$from$session_key, thread = o$from$thread))
    for (name in names(destinations)) {
        path <- file.path(job_dir(job$id), paste0("consultation-", stage, "-", name, ".json"))
        if (file.exists(path)) next
        d <- destinations[[name]]
        .room_consult_record(chat, sessions, job, text, d$room, d$key, d$thread)
        # A send failure is already durable in the ordinary notice queue.
        job_write_file(path, list(at = job_now()))
    }
    invisible(NULL)
}

.room_consult_pump <- function(chat, cfg, sessions, now = as.numeric(Sys.time()),
                                .start = .room_consult_start) {
    jobs <- Filter(function(j) identical(j$backend, "consultation"),
                   job_list(owner = bot_job_owner(cfg)))
    for (j in jobs) {
        w <- if (exists(j$id, .room_consult_workers, inherits = FALSE)) get(j$id, .room_consult_workers)
        if (j$status %in% JOB_STATUSES_OPEN &&
            (!isTRUE(cfg$room_consultation) || !j$requester %in% bot_operators(cfg) || isTRUE(j$cancel_requested))) {
            .room_consult_close(j$id)
            job_settle(j$id, "cancelled", reason = "Consultation cancelled or permission withdrawn")
        } else if (j$status == "queued") {
            # At most one consultation per destination and four per bot. These
            # processes never use the destination's doer or its checkout lock.
            active <- lapply(ls(.room_consult_workers), function(id) get(id, .room_consult_workers))
            active <- Filter(function(x) identical(x$owner, j$owner), active)
            if (length(active) >= 4L || any(vapply(active, function(x)
                identical(x$key, j$origin$session_key), TRUE))) next
            tryCatch({
                if (!file.exists(file.path(job_dir(j$id), "consultation.rds"))) stop("Conversation snapshot is missing")
                .room_consult_notice(chat, sessions, j)
                job_mark_dispatched(j$id)
                process <- .start(j$id)
                assign(j$id, list(process = process, owner = j$owner,
                    key = j$origin$session_key, started = now), .room_consult_workers)
            }, error = function(e) job_settle(j$id, "failed", error = conditionMessage(e)))
        } else if (j$status == "running") {
            if (is.null(w)) {
                job_settle(j$id, "indeterminate", reason = "The consultation process was lost; ask again to retry.")
            } else if (now - w$started > j$limits$wall_seconds) {
                .room_consult_close(j$id)
                job_settle(j$id, "failed", reason = "The consultation exceeded its 120 second limit")
            } else if (!isTRUE(w$process$is_alive())) {
                answer <- tryCatch(w$process$get_result(), error = function(e) list(error = conditionMessage(e)))
                .room_consult_close(j$id)
                if (!is.null(answer$error)) {
                    job_settle(j$id, "failed", error = answer$error, usage = answer$usage)
                } else job_settle(j$id, "done", result = answer$reply, usage = answer$usage)
            }
        }
        j <- job_read(j$id)
        if (j$status %in% JOB_STATUSES_FINAL) {
            .room_consult_close(j$id)
            .room_consult_notice(chat, sessions, j, final = TRUE)
        }
    }
    invisible(NULL)
}
