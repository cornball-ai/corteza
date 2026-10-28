# Child workers write bounded public progress without holding transport
# clients. Atomic, numbered files let the parent consume complete records while
# the worker is busy; the existing journal retains the full tool traffic.
job_progress_observer <- function(id) {
    dir <- file.path(job_dir(job_check_id(id)), "progress")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    seq <- 0L
    function(event) {
        record <- rooms_activity_record(event)
        if (is.null(record) || (is.null(record$tool) &&
                !nzchar(record$narration %||% ""))) {
            return(invisible(NULL))
        }
        next_seq <- seq + 1L
        job_write_file(file.path(dir, sprintf("%08d.json", next_seq)), record)
        seq <<- next_seq
        invisible(NULL)
    }
}

job_progress_read <- function(session, id) {
    j <- job_read(id)
    if (!job_in_session(j, session)) {
        return(list())
    }
    cursor <- session$.job_progress_cursor %||% list()
    last <- cursor[[id]] %||% 0L
    dir <- file.path(job_dir(id), "progress")
    records <- list()
    repeat {
        path <- file.path(dir, sprintf("%08d.json", last + 1L))
        record <- tryCatch(job_read_file(path), error = function(e) NULL)
        if (is.null(record)) {
            break
        }
        records[[length(records) + 1L]] <- record
        last <- last + 1L
    }
    if (!length(records)) {
        return(list())
    }
    cursor[[id]] <- last
    session$.job_progress_cursor <- cursor
    list(list(type = "progress", job = j, records = records))
}

bot_job_progress <- function(chat, cfg, session, job, records = list(),
                             final = FALSE) {
    destination <- tryCatch(bot_job_delivery_check(chat, job),
                            error = function(e) NULL)
    if (is.null(destination)) {
        return(invisible(FALSE))
    }
    acc <- session$.job_activity[[job$id]]
    if (is.null(acc)) {
        if (!length(records)) {
            return(invisible(NULL))
        }
        acc <- rooms_activity_new()
        acc$session <- session
        acc$detail <- isTRUE(cfg$activity_detail)
        acc$thread <- destination$thread
        session$.job_activity[[job$id]] <- acc
    }
    for (record in records) {
        rooms_activity_add(acc, record)
    }
    if (final || (rooms_activity_live(chat) &&
            rooms_activity_due(acc,
                               min_interval = rooms_activity_interval(cfg)))) {
        rooms_activity_flush(acc, chat, destination$room %||% session$room_id)
    }
    if (final) {
        session$.job_activity[[job$id]] <- NULL
    }
    invisible(NULL)
}
