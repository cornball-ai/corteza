if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api unavailable")

local({
    root <- tempfile("remote-control-")
    dir.create(root)
    vars <- c("CORTEZA_REMOTE_DIR", "CORTEZA_STATE_DIR", "R_USER_DATA_DIR",
              "R_USER_CONFIG_DIR", "R_USER_CACHE_DIR")
    old <- Sys.getenv(vars, unset = NA)
    do.call(Sys.setenv, setNames(as.list(file.path(root, vars)), vars))
    on.exit({
        for (i in seq_along(vars)) if (is.na(old[[i]])) Sys.unsetenv(vars[[i]]) else
            do.call(Sys.setenv, setNames(list(old[[i]]), vars[[i]]))
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- list(user_id = "@courier:example", operators = "@owner:example",
                provider = "ollama", model = "fixture")
    cl <- chat.api::chat_loopback()
    sessions <- corteza:::bot_new_session_registry()
    route <- function(text, id, sender = "@owner:example", room = "room", thread = NULL,
                       ts = Sys.time()) {
        corteza:::.remote_bot_route(list(body = text, id = id, sender = sender,
            channel = room, thread = thread, ts = ts), cfg)
    }
    corteza:::.remote_advertise(cfg)
    expect_identical(length(corteza:::.remote_bots()), 1L)
    dir <- corteza:::.remote_request(cfg$user_id, "room", cfg$operators)
    corteza:::.remote_bot_pump(cfg, cl, sessions)
    expect_identical(corteza:::.remote_read_file(file.path(dir, "accepted.json"))$room, "room")
    expect_true(route("hello", "event1"))
    expect_true(route("hello", "event1"))
    expect_true(route("intruder", "event2", sender = "@stranger:example"))
    expect_false(route("other room", "event3", room = "elsewhere"))
    expect_false(route("thread", "event4", thread = "thread"))
    expect_true(route("old event", "event5", ts = Sys.time() - 60))
    inbox <- corteza:::.remote_records(dir, "in")
    expect_identical(length(inbox), 1L)
    expect_identical(inbox[[1L]]$text, "hello")
    # Another bot sharing the data directory never claims this request.
    other <- cfg
    other$user_id <- "@other:example"
    expect_false(corteza:::.remote_bot_route(list(channel = "room"), other))
    duplicate <- corteza:::.remote_request(cfg$user_id, "room", cfg$operators)
    corteza:::.remote_bot_pump(cfg, cl, sessions)
    expect_true(grepl("already controls", corteza:::.remote_read_file(
        file.path(duplicate, "accepted.json"))$error))
    # Replies survive a failed send and are not sent again on another pump.
    ctx <- new.env(parent = emptyenv())
    ctx$remote_dir <- dir
    ctx$session <- corteza::new_session("cli")
    ctx$session$remote_dir <- dir
    corteza:::.remote_emit(ctx, "first reply")
    ns <- asNamespace("corteza")
    original_send <- get("bot_reply_send", ns)
    on.exit(utils::assignInNamespace("bot_reply_send", original_send, "corteza"), add = TRUE)
    utils::assignInNamespace("bot_reply_send", function(...) NULL, "corteza")
    corteza:::.remote_bot_pump(cfg, cl, sessions)
    expect_identical(length(chat.api::chat_history(cl, "room")$messages), 0L)
    utils::assignInNamespace("bot_reply_send", original_send, "corteza")
    corteza:::.remote_bot_pump(cfg, cl, sessions)
    corteza:::.remote_bot_pump(cfg, cl, sessions)
    expect_identical(length(chat.api::chat_history(cl, "room")$messages), 1L)
    # Approval answers are bound to a pending id, never ordinary queued text.
    id <- corteza:::job_new_id()
    path <- file.path(dir, "approvals", id)
    corteza:::.remote_write(paste0(path, ".request"), list(expires = as.numeric(Sys.time()) + 60))
    expect_true(route(paste("/approve", id), "approval-intruder", sender = "@stranger:example"))
    expect_null(corteza:::.remote_read_file(paste0(path, ".answer")))
    route(paste("/approve", id), "approval-owner")
    expect_true(corteza:::.remote_read_file(paste0(path, ".answer"))$approved)
    route(paste("/deny", id), "late-denial")
    expect_true(corteza:::.remote_read_file(paste0(path, ".answer"))$approved)
    expired <- file.path(dir, "approvals", corteza:::job_new_id())
    corteza:::.remote_write(paste0(expired, ".request"), list(expires = 0))
    route(paste("/approve", basename(expired)), "expired")
    expect_null(corteza:::.remote_read_file(paste0(expired, ".answer")))
    route("/stop", "stop1")
    interrupted <- tryCatch({corteza:::.remote_check_stop(ctx$session); FALSE}, interrupt = function(e) TRUE)
    expect_true(interrupted)
    expect_null(corteza:::.remote_check_stop(ctx$session))
    # Revocation affects queued input as well as new messages and output.
    cfg$operators <- "@replacement:example"
    corteza:::.remote_advertise(cfg)
    route("revoked", "revoked")
    expect_identical(length(corteza:::.remote_records(dir, "in")), 1L)
    cfg$operators <- "@owner:example"
    corteza:::.remote_advertise(cfg)
    corteza:::.remote_write(file.path(dir, "closed.json"), list(closed = TRUE))
    expect_false(route("back to bot", "after-close"))

    # Drive the real bot_poll and REPL with a loopback transport and a fake
    # turn. No second sync consumer, model request, or live user state.
    originals <- mget(c("bot_load_config", "bot_chat_client", ".remote_wait"), ns)
    on.exit(for (name in names(originals)) utils::assignInNamespace(name, originals[[name]], "corteza"), add = TRUE)
    utils::assignInNamespace("bot_load_config", function() cfg, "corteza")
    utils::assignInNamespace("bot_chat_client", function(...) cl, "corteza")
    original_poll <- chat.api::chat_poll
    poll_cursor <- length(cl$env$log)
    on.exit(utils::assignInNamespace("chat_poll", original_poll, "chat.api"), add = TRUE)
    utils::assignInNamespace("chat_poll", function(client, ...) {
        result <- original_poll(client, since = poll_cursor, ...)
        poll_cursor <<- result$cursor
        result$messages <- lapply(result$messages, function(m) {
            m$self <- identical(m$sender, "loopback")
            m
        })
        result$first_run <- FALSE
        result
    }, "chat.api")
    # Feed transport messages as the human, not loopback's default self sender.
    send_human <- function(text) {
        id <- paste0("human-", length(cl$env$log) + 1L)
        cl$env$log[[length(cl$env$log) + 1L]] <- chat.api::chat_message(
            id, "next-room", "@owner:example", text, Sys.time(), self = FALSE)
    }
    utils::assignInNamespace(".remote_wait", function(ctx, predicate, timeout = 60) {
        corteza::bot_poll(sessions = sessions, system = "fixture")
        pending <- list.files(file.path(ctx$remote_dir, "approvals"), pattern = "[.]request$", full.names = TRUE)
        pending <- pending[!file.exists(sub("[.]request$", ".closed", pending))]
        if (length(pending)) {
            id <- sub("[.]request$", "", basename(tail(pending, 1L)))
            send_human(paste("/approve", id))
        } else send_human("Continue the fixture")
        corteza::bot_poll(sessions = sessions, system = "fixture")
        predicate()
    }, "corteza")
    ctx <- new.env(parent = emptyenv())
    ctx$cwd <- root
    ctx$ws_enabled <- FALSE
    ctx$config <- list(context_compact_pct = 100)
    ctx$provider <- "ollama"
    ctx$model <- "fixture"
    ctx$session <- corteza::new_session("cli", provider = "ollama")
    ctx$session$config <- ctx$config
    ctx$session$history <- list(list(role = "user", content = "Existing task"))
    same_session <- ctx$session
    ctx$palette <- setNames(as.list(rep("", 10)), c("dim", "reset", "cyan", "bold", "yellow", "green", "bright_magenta", "red", "magenta", "blue"))
    ctx$read_input <- function(...) character()
    saved_input <- ctx$read_input
    saved_approval <- ctx$session$approval_cb
    ctx$render_reply <- function(text) {
        send_human("/remote-control off")
        corteza::bot_poll(sessions = sessions, system = "fixture")
    }
    disk <- corteza:::session_new("ollama", "fixture", root)
    ctx$disk_session <- list(session = disk, sessionId = disk$sessionId)
    ctx$turn_fn <- function(prompt, session) {
        expect_identical(session, same_session)
        expect_identical(prompt, "Continue the fixture")
        expect_identical(session$history[[1L]]$content, "Existing task")
        expect_true(session$approval_cb(list(tool = "write_file", args = list(path = "fixture")), list(reason = "ask")))
        plan <- corteza:::task_tool_intercept(session, "task_create", list(tasks = "Verify the fixture"))
        expect_true(grepl("Plan approved", plan, fixed = TRUE))
        expect_null(session$task_proposal)
        corteza:::.fire_observers(session, list(outcome = "ran", success = TRUE,
            call = list(tool = "read_file", args = list(path = "fixture")), result = "found it"))
        list(reply = "Finished the fixture", usage = list())
    }
    out <- capture.output(corteza:::.remote_run(ctx, "--bot @courier:example next-room"))
    expect_identical(ctx$read_input, saved_input)
    expect_identical(ctx$session$approval_cb, saved_approval)
    expect_null(ctx$remote_dir)
    expect_identical(ctx$session, same_session)
    corteza::bot_poll(sessions = sessions, system = "fixture")
    messages <- chat.api::chat_history(cl, "next-room")$messages
    bodies <- vapply(messages, function(m) m$body, "")
    expect_true(any(grepl("Finished the fixture", bodies, fixed = TRUE)))
    expect_true(any(grepl("found it", bodies, fixed = TRUE)))
    expect_true(any(grepl("disconnected", bodies, fixed = TRUE)))
    expect_true(any(grepl("Verify the fixture", bodies, fixed = TRUE)))

    # The real stop check prevents tool dispatch, with the request already
    # mirrored in history available to the REPL's normal interruption repair.
    stop_dir <- corteza:::.remote_request(cfg$user_id, "stop-room", cfg$operators)
    session <- corteza::new_session("cli", provider = "ollama")
    session$remote_dir <- stop_dir
    corteza:::.remote_write(file.path(stop_dir, "stop.json"), list(id = "stop-before-tool"))
    executed <- FALSE
    handler <- corteza:::.make_tool_handler(session, tool_executor = function(...) {
        executed <<- TRUE
        "must not run"
    })
    stopped <- tryCatch({handler("read_file", list(path = "fixture")); FALSE}, interrupt = function(e) TRUE)
    expect_true(stopped)
    expect_false(executed)
    # Unsent replies are retained, including after the CLI disconnects.
    state <- new.env(parent = emptyenv())
    state$remote_dir <- stop_dir
    corteza:::.remote_bot_pump(cfg, cl, sessions)
    corteza:::.remote_emit(state, "final reply after disconnect")
    corteza:::.remote_write(file.path(stop_dir, "closed.json"), list(closed = TRUE))
    corteza:::.remote_bot_pump(cfg, cl, sessions)
    expect_true(any(vapply(chat.api::chat_history(cl, "stop-room")$messages,
        function(m) identical(m$body, "final reply after disconnect"), TRUE)))
    # A lost courier is an actionable error instead of an indefinite wait.
    bot <- corteza:::.remote_read_file(corteza:::.remote_bot_file(cfg$user_id))
    bot$seen <- 0
    corteza:::.remote_write(corteza:::.remote_bot_file(cfg$user_id), bot)
    expect_error(corteza:::.remote_connected(state), "courier is unavailable")
    cfg$operators <- character()
    corteza:::.remote_advertise(cfg)
    expect_identical(length(corteza:::.remote_bots()), 0L)
    expect_error(corteza:::.remote_connected(state), "permission was revoked")
})
