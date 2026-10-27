if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api unavailable")
local({
    ns <- asNamespace("corteza")
    root <- tempfile("resume-commands-")
    dir.create(root)
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = root)
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        utils::assignInNamespace(name, fn, package)
    }
    on.exit({
        for (x in rev(saved)) utils::assignInNamespace(x$name, x$fn, x$package)
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- list(user_id = "fixture-bot", operators = "operator", provider = "ollama")
    chat <- chat.api::chat_loopback()
    sessions <- ns$bot_new_session_registry()
    make <- function(room) {
        s <- corteza::new_session("matrix", provider = "ollama", system = "fixture")
        s$cwd <- root
        s$config <- list()
        s$job_key <- s$room_id <- room
        s$job_owner <- cfg$user_id
        s$rehydrate_checked <- TRUE
        s$history <- list(list(role = "user", content = "original task"),
            list(role = "assistant", content = "partial result"))
        assign(room, s, sessions)
        s
    }
    source <- make("source-room")
    target <- make("worker-room")
    id <- ns$job_create("fixture task", workspace = root, owner = cfg$user_id,
        requester = "requester", origin = list(session_key = target$job_key,
            room = target$room_id, from = list(session_key = source$job_key, room = source$room_id)))
    ready <- ns$.job_resume_save(id, ns$job_worker_identity(target), target, new.env(parent = emptyenv()))
    ns$job_settle(id, "incomplete", extra = list(resume = ready))
    # An independent writer holds the checkout, proving that resume queues
    # through the real lock rather than invoking a provider from the command.
    blocker <- ns$job_create("other writer", workspace = root, owner = cfg$user_id)
    ns$job_mark_dispatched(blocker)
    expect_true(ns$job_lock_acquire(ns$job_checkout(root), blocker, cfg$user_id)$ok)
    incoming <- chat.api::chat_message(id = "$resume", channel = source$room_id,
        sender = "requester", body = paste("/job resume", id), ts = Sys.time())
    patch("bot_load_config", function(...) cfg)
    patch("bot_chat_client", function(...) chat)
    patch("chat_whoami", function(...) chat.api::chat_identity(cfg$user_id), "chat.api")
    patch("chat_poll", function(...) list(first_run = FALSE, messages = list(incoming),
        reactions = list(), invites = list()), "chat.api")
    patch("bot_should_respond", function(...) TRUE)
    replies <- character()
    patch("bot_reply_send", function(chat, channel, text, ...) {
        replies <<- c(replies, text)
        paste0("$ack", length(replies))
    })
    turns <- 0L
    patch("bot_run_turn_in_cwd", function(prompt, session) {
        turns <<- turns + 1L
        out <- corteza::tool_job_resume(id, list(session = session))
        paste(vapply(out$content, function(b) b$text, ""), collapse = "\n")
    })
    expect_equal(corteza::bot_poll(sessions = sessions, timeout = 0), 1L)
    expect_identical(turns, 0L)
    next_id <- ns$job_read_file(file.path(ns$job_dir(id), "continuation", "request.json"))$id
    expect_identical(ns$job_read(next_id)$status, "queued")
    expect_true(any(grepl(next_id, replies, fixed = TRUE)))
    n <- length(ns$job_list())
    expect_equal(corteza::bot_poll(sessions = sessions, timeout = 0), 0L)
    expect_equal(length(ns$job_list()), n)
    incoming$id <- "$tool"
    incoming$body <- "continue that job"
    expect_equal(corteza::bot_poll(sessions = sessions, timeout = 0), 1L)
    expect_identical(turns, 1L)
    expect_equal(length(ns$job_list()), n)
    expect_null(source$job_resume)
    incoming$id <- "$unauthorized"
    incoming$sender <- "bystander"
    expect_equal(corteza::bot_poll(sessions = sessions, timeout = 0), 1L)
    expect_true(grepl("original requester", tail(replies, 1L), fixed = TRUE))
    expect_equal(length(ns$job_list()), n)
    source$cwd <- file.path(root, "different")
    expect_identical(ns$bot_job_resume(sessions, source, cfg, "operator", "$operator", id), next_id)
    target$cwd <- file.path(root, "different")
    expect_error(ns$bot_job_resume(sessions, source, cfg, "operator", "$moved", id), "workspace")
    target$cwd <- root
    expect_true(ns$job_cancel(target, next_id, by = "requester"))
    expect_identical(ns$job_read(next_id)$status, "cancelled")
    expect_identical(ns$bot_job_resume(sessions, source, cfg, "requester", "$again", id), next_id)
    # The shared REPL command returns the same attempt without creating more
    # work. It does not depend on the model remembering a command spelling.
    patch(".repl_jobs_setup", function(...) TRUE)
    source$job_requester <- "requester"
    source$job_resume <- function(id) ns$bot_job_resume(sessions, source, cfg, "requester", "$cli", id)
    output <- capture.output(ns$.repl_cmd_job_resume(list(session = source), c("/job", "resume", id)))
    expect_true(any(grepl(next_id, output, fixed = TRUE)))
    expect_equal(length(ns$job_list()), n)
    source$job_resume <- NULL
    ns$job_lock_release(ns$job_checkout(root), blocker)

    # Failure at publication leaves no readable snapshot. An existing one
    # cannot be rewritten, even by a later checkpoint of the same worker.
    bad <- ns$job_create("snapshot failure", workspace = root)
    patch(".job_resume_publish", function(...) FALSE)
    expect_error(ns$.job_resume_save(bad, ns$job_worker_identity(target), target,
        new.env(parent = emptyenv())), "publish")
    expect_false(dir.exists(ns$job_resume_path(bad)))
    expect_error(ns$job_resume_snapshot(bad, ns$job_worker_identity(target)), "missing")
    expect_error(ns$.job_resume_save(id, ns$job_worker_identity(target), target,
        new.env(parent = emptyenv())), "already exists")
})
