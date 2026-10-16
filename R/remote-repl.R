.remote_choose <- function(ctx, text) {
    parts <- strsplit(trimws(text), "\\s+")[[1L]]
    owner <- ctx$config$remote_control$bot_id
    if (length(parts) >= 2L && identical(parts[[1L]], "--bot")) {
        owner <- parts[[2L]]
        parts <- parts[-c(1L, 2L)]
    }
    room <- trimws(paste(parts, collapse = " "))
    if (!nzchar(room)) room <- ctx$config$remote_control$room %||%
    paste0("~/", basename(ctx$cwd %||% getwd()))
    bots <- .remote_bots()
    if (!is.null(owner)) bots <- Filter(function(x) identical(x$owner, owner),
                                        bots)
    if (length(bots) != 1L) {
        stop("Choose one running bot with /remote-control --bot @bot:server <room>. ",
             "Available: ", paste(vapply(.remote_bots(), function(x) x$owner, ""), collapse = ", "),
             ". The bot must run this version and have configured operators.", call. = FALSE)
    }
    list(bot = bots[[1L]], room = room)
}

.remote_wait <- function(ctx, predicate, timeout = 60) {
    deadline <- Sys.time() + timeout
    repeat {
        .remote_connected(ctx)
        value <- predicate()
        if (!is.null(value)) {
            return(value)
        }
        .remote_check_stop(ctx$session)
        if (Sys.time() >= deadline) {
            return(NULL)
        }
        Sys.sleep(0.1)
    }
}

.remote_approve <- function(ctx, call, decision, timeout = 600) {
    id <- job_new_id()
    path <- file.path(ctx$remote_dir, "approvals", id)
    .remote_write(paste0(path, ".request"),
                  list(expires = as.numeric(Sys.time()) + timeout))
    on.exit(.remote_write(paste0(path, ".closed"), list(closed = TRUE)), add = TRUE)
    body <- jsonlite::toJSON(.redact_object(call$args %||% list()), auto_unbox = TRUE, pretty = TRUE)
    .remote_emit(ctx, paste0("Approval needed: ", .sanitize_inline(call$tool), "\n", body,
                             "\nReason: ", .sanitize_inline(decision$reason %||% "ask"),
                             "\n/approve ", id, " or /deny ", id, " (expires in ", timeout, "s)"))
    answer <- .remote_wait(ctx, function() .remote_read_file(paste0(path, ".answer")), timeout)
    isTRUE(answer$approved)
}

.remote_input <- function(ctx, prompt) {
    repeat {
        bot <- .remote_connected(ctx)
        tryCatch(.remote_check_stop(ctx$session), interrupt = function(e) {
            .remote_emit(ctx,
                         "Stopped. Send a new task or /remote-control off.")
        })
        records <- Filter(function(x) !is.null(x) &&
                          !x$id %in% ctx$remote_consumed, .remote_records(ctx$remote_dir, "in"))
        if (!length(records)) {
            .repl_pump_jobs(ctx)
            Sys.sleep(0.1)
            next
        }
        record <- records[[1L]]
        ctx$remote_consumed <- c(ctx$remote_consumed, record$id)
        if (!record$sender %in% unlist(bot$operators)) {
            next
        }
        text <- record$text
        if (text %in% c("/remote-control off", "/rc off")) {
            return(character())
        }
        if (isTRUE(record$media)) {
            .remote_emit(ctx, "Remote control currently accepts text. Describe the attachment in a message.")
            next
        }
        if (text %in% c("/remote-control status", "/rc status")) {
            .remote_emit(ctx, paste("Connected to the live CLI in", ctx$cwd))
            next
        }
        if (identical(prompt, "> ") && !startsWith(text, "/")) {
            return(text)
        }
        if (!nzchar(text)) {
            next
        }
        cmd <- strsplit(text, "\\s+")[[1L]][[1L]]
        rest <- trimws(sub("^[^ ]+ *", "", text))
        if (cmd == "/auto") {
            output <- capture.output(tryCatch(.auto_command(ctx, rest),
                    error = function(e) .remote_emit(ctx, conditionMessage(e))))
            # Auto's live progress uses .remote_emit(); command errors and
            # fresh-session help still need a response in the room.
            if (!any(grepl("\\[auto\\]", output))) {
                .remote_emit(ctx, output)
            }
        } else if (cmd == "/max-tokens") {
            .remote_emit(ctx, tryCatch(.output_budget_command(ctx$session, rest),
                                       error = function(e) conditionMessage(e)))
        } else {
            .remote_emit(ctx, paste("Send a task, /auto [--exec] [goal], /auto status,",
                                    "/max-tokens [N], /stop, or /remote-control off."))
        }
    }
}

.remote_run <- function(ctx, text) {
    if (!is.null(ctx$remote_dir)) {
        cat("Remote control is already active.\n")
        return(invisible(NULL))
    }
    if (trimws(text) %in% c("off", "status")) {
        cat("Remote control is off.\n")
        return(invisible(NULL))
    }
    choice <- .remote_choose(ctx, text)
    dir <- .remote_request(choice$bot$owner, choice$room,
                           unlist(choice$bot$operators))
    ctx$remote_dir <- dir
    ctx$remote_consumed <- character()
    session <- ctx$session
    saved <- list(read_input = ctx$read_input, render_reply = ctx$render_reply,
                  remote_flush = ctx$remote_flush,
                  eof_message = ctx$eof_message, approval_cb = session$approval_cb,
                  task_approval_cb = session$task_approval_cb, on_tool = session$on_tool)
    session$remote_dir <- dir
    on.exit({
        tryCatch({
            .remote_emit(ctx, "Remote control disconnected. The CLI session remains open locally.")
            .remote_write(file.path(dir, "closed.json"), list(closed = TRUE))
        }, error = function(e) message("Remote control cleanup: ", conditionMessage(e)))
        ctx$read_input <- saved$read_input
        ctx$render_reply <- saved$render_reply
        ctx$eof_message <- saved$eof_message
        session$approval_cb <- saved$approval_cb
        session$task_approval_cb <- saved$task_approval_cb
        session$on_tool <- saved$on_tool
        session$remote_dir <- NULL
        ctx$remote_dir <- NULL
        ctx$remote_flush <- saved$remote_flush
    }, add = TRUE)
    cat("Waiting for ", choice$bot$owner, " to attach ", choice$room, "...\n", sep = "")
    accepted <- .remote_wait(ctx, function() .remote_read_file(file.path(dir, "accepted.json")))
    if (is.null(accepted)) {
        stop("Bot did not acknowledge within 60 seconds.", call. = FALSE)
    }
    if (!is.null(accepted$error)) {
        stop(accepted$error, call. = FALSE)
    }
    .remote_emit(ctx, paste0("Remote control connected to the existing CLI session in ",
                             ctx$cwd, ". Send a task or /auto to continue. /stop stops at the next tool boundary; ",
                             "/remote-control off returns control to the terminal."))
    cat("Connected. Use the Matrix room; Ctrl+C here returns to the local prompt.\n")
    ctx$read_input <- function(prompt) .remote_input(ctx, prompt)
    ctx$render_reply <- function(reply) {
        .remote_emit(ctx, reply)
        saved$render_reply(reply)
    }
    ctx$eof_message <- "Remote control ended.\n"
    session$approval_cb <- function(call, decision) .remote_approve(ctx, call, decision)
    session$task_approval_cb <- function() .remote_approve(ctx,
        list(tool = "task_create", args = list(tasks = session$task_proposal)), list(reason = "Approve plan"))
    acc <- rooms_activity_new()
    last_flush <- as.numeric(Sys.time()) - 15
    ctx$remote_flush <- function() {
        frame <- rooms_activity_frame(acc)
        if (!nzchar(frame$text)) {
            return(invisible(NULL))
        }
        .remote_append(dir, "out", list(type = "text", text = frame$text))
        acc <<- rooms_activity_new()
        last_flush <<- as.numeric(Sys.time())
        invisible(NULL)
    }
    session$on_tool <- c(saved$on_tool, list(function(event) {
        rooms_activity_add(acc, rooms_activity_record(event))
        if (as.numeric(Sys.time()) - last_flush >= 15) ctx$remote_flush()
    }))
    tryCatch(run_repl_loop(ctx, keep_jobs = TRUE), interrupt = function(e) invisible(NULL))
    invisible(NULL)
}
