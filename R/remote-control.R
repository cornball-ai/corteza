# Local mailboxes join a live REPL to the bot's existing Matrix poll loop.
# Only the bot holds a transport client; the CLI never runs a second /sync.
.remote_root <- function() {
    Sys.getenv("CORTEZA_REMOTE_DIR", file.path(tools::R_user_dir("corteza",
                "data"),
            "remote-control"))
}

.remote_read_file <- function(path) {
    tryCatch(job_read_file(path), error = function(e) NULL)
}

.remote_write <- function(path, value) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE,
               mode = "0700")
    job_write_file(path, value)
}

.remote_live <- function(record) {
    !is.null(record) && identical(record$host, Sys.info()[["nodename"]]) &&
    is.numeric(record$pid) && length(record$pid) == 1L &&
    is.finite(record$pid) && record$pid > 0 &&
    !job_lock_process_gone(record)
}

.remote_bot_file <- function(owner) {
    file.path(.remote_root(), "bots", paste0(digest::digest(owner), ".json"))
}

.remote_advertise <- function(cfg) {
    .remote_write(.remote_bot_file(bot_job_owner(cfg)), list(
            owner = bot_job_owner(cfg), operators = as.list(bot_operators(cfg)),
            pid = Sys.getpid(), host = Sys.info()[["nodename"]],
            seen = as.numeric(Sys.time())))
}

.remote_bots <- function() {
    files <- list.files(file.path(.remote_root(), "bots"),
                        pattern = "[.]json$", full.names = TRUE)
    Filter(function(x) .remote_live(x) && length(x$operators) > 0L &&
           isTRUE(as.numeric(Sys.time()) - x$seen < 120),
           lapply(files, .remote_read_file))
}

.remote_sessions <- function(owner = NULL, active = TRUE) {
    dirs <- list.dirs(file.path(.remote_root(), "sessions"),
                      recursive = FALSE, full.names = TRUE)
    Filter(function(x) !is.null(x) &&
           (is.null(owner) || identical(x$owner, owner)) &&
           (!active ||
            (.remote_live(x) && !file.exists(file.path(x$dir, "closed.json")))),
           lapply(dirs, function(dir) {
        x <- .remote_read_file(file.path(dir, "request.json"))
        if (!is.null(x)) x$dir <- dir
        x
    }))
}

.remote_request <- function(owner, room, controllers) {
    id <- job_new_id()
    dir <- file.path(.remote_root(), "sessions", id)
    .remote_write(file.path(dir, "request.json"), list(
            id = id, owner = owner, room = room, controllers = as.list(controllers),
            pid = Sys.getpid(), host = Sys.info()[["nodename"]],
            created = as.numeric(Sys.time())))
    dir
}

.remote_records <- function(dir, folder) {
    paths <- sort(list.files(file.path(dir, folder),
                             pattern = "^[0-9]+[.]json$", full.names = TRUE))
    lapply(paths, function(path) {
        x <- .remote_read_file(path)
        if (!is.null(x)) x$path <- path
        x
    })
}

.remote_append <- function(dir, folder, record) {
    records <- list.files(file.path(dir, folder), pattern = "^[0-9]+[.]json$")
    if (length(records)) {
        n <- max(as.numeric(sub("[.]json$", "", records))) + 1
    } else {
        n <- 1
    }
    .remote_write(file.path(dir, folder, sprintf("%012.0f.json", n)), record)
}

.remote_emit <- function(ctx, text, audit = NULL) {
    text <- .redact_text(text, ctx$session$secret_values)
    dir <- ctx$remote_dir
    if (!is.null(dir) && nzchar(paste(text, collapse = "\n"))) {
        if (is.function(ctx$remote_flush)) {
            ctx$remote_flush()
        }
        .remote_append(dir, "out",
                       list(type = "text", text = paste(text, collapse = "\n"), audit = audit))
    }
    invisible(NULL)
}

.remote_check_stop <- function(session) {
    dir <- session$remote_dir
    if (is.null(dir)) {
        return(invisible(NULL))
    }
    request <- .remote_read_file(file.path(dir, "stop.json"))
    if (!is.null(request) && !identical(request$id, session$remote_stop_id)) {
        session$remote_stop_id <- request$id
        stop(structure(list(message = "Stopped from Matrix", call = NULL),
                       class = c("interrupt", "condition")))
    }
    invisible(NULL)
}

.remote_connected <- function(ctx) {
    request <- .remote_read_file(file.path(ctx$remote_dir, "request.json"))
    bot <- .remote_read_file(.remote_bot_file(request$owner))
    if (!.remote_live(bot) ||
        !isTRUE(as.numeric(Sys.time()) - bot$seen < 120)) {
        stop("Matrix courier is unavailable. Returning to the local prompt.",
             call. = FALSE)
    }
    if (!length(intersect(unlist(request$controllers), unlist(bot$operators)))) {
        stop("Remote control permission was revoked. Returning to the local prompt.", call. = FALSE)
    }
    bot
}
