.remote_attach <- function(ctx, choice) {
    id <- ctx$disk_session$sessionId %||% ctx$session$sessionId
    dir <- .remote_request(choice$bot$owner, choice$room,
        unlist(choice$bot$operators), session_id = id, cwd = ctx$cwd)
    ctx$remote_dir <- dir
    ctx$session$remote_dir <- dir
    ctx$remote_consumed <- character()
    cat("Waiting for ", choice$bot$owner, " to attach ",
        choice$room %||% paste("session", id), "...\n", sep = "")
    accepted <- .remote_wait(ctx, function() .remote_read_file(file.path(dir, "accepted.json")))
    if (is.null(accepted)) stop("Bot did not acknowledge within 60 seconds.", call. = FALSE)
    if (!is.null(accepted$error)) stop(accepted$error, call. = FALSE)
    cat("Matrix room: ", accepted$room, "\n", sep = "")
    accepted
}

.remote_clear <- function(ctx) {
    old_dir <- ctx$remote_dir
    bot <- .remote_connected(ctx)
    .remote_emit(ctx, "Starting a new CLI conversation in a new room.")
    .remote_write(file.path(old_dir, "closed.json"), list(closed = TRUE))
    .repl_clear(ctx)
    accepted <- .remote_attach(ctx, list(bot = bot, room = NULL))
    .remote_append(old_dir, "out", list(type = "text", text = paste0(
        "New conversation: ", .remote_room_link(accepted$room, bot$owner))))
    .remote_emit(ctx, paste0("New CLI conversation: ", ctx$disk_session$sessionId,
        ". Send a task to continue. /clear starts another conversation; /rc off disconnects."))
    invisible(NULL)
}

.remote_room_link <- function(room, owner) {
    link <- paste0("https://matrix.to/#/", room)
    if (is.character(owner) && length(owner) == 1L &&
        grepl("^@[^:]+:.+$", owner)) {
        server <- sub("^@[^:]+:", "", owner)
        link <- paste0(link, "?via=", utils::URLencode(server, reserved = TRUE))
    }
    link
}
