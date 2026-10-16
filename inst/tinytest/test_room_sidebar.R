if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("needs chat.api")
local({
    ns <- asNamespace("corteza")
    root <- tempfile("sidebar-")
    dir.create(root)
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = root)
    on.exit({
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- list(server = "https://example.invalid", user_id = "@bot:ex", device_id = "FIXTURE",
                token = "fixture-token", operators = "@operator:ex", room_id = "!private:ex")
    members <- c("@bot:ex", "@operator:ex")
    rule <- "invite"
    sent <- list()
    fail <- FALSE
    chat <- ns$bot_chat_client(cfg, save_cursor = FALSE,
        .members = function(...) members,
        .get_state = function(...) list(join_rule = rule),
        .send = function(client, text, room = NULL, ...) {
            if (fail) stop("fixture send failure")
            sent[[length(sent) + 1L]] <<- list(room = room, text = text)
            paste0("$sent", length(sent))
        })
    make <- function(room) {
        s <- corteza::new_session("matrix")
        s$job_owner <- cfg$user_id
        s$job_key <- s$room_id <- room
        s$config <- list()
        s
    }
    source <- make("!team:ex")
    target <- make(cfg$room_id)
    sessions <- new.env()
    assign(source$job_key, source, sessions)
    assign(target$job_key, target, sessions)
    for (i in 1:8) {
        ns$bot_transcript_add(source, paste0("$user", i), "user", paste("question", i))
        ns$bot_transcript_add(source, paste0("$bot", i), "assistant", paste("answer", i))
    }
    before <- source$transcript
    identity_before <- ns$job_worker_identity(source)
    job <- ns$job_create("fixture work", workspace = root, owner = cfg$user_id,
        origin = list(session_key = source$job_key, room = source$room_id))
    ns$job_mark_dispatched(job)
    go <- function(id = "$command", sender = "@operator:ex") {
        ns$bot_sidebar(chat, cfg, sessions, source, sender, id, 3L, function(room) target)
    }
    expect_identical(ns$bot_parse_sidebar("Let's take this offline")$turns, 5L)
    expect_identical(ns$bot_parse_sidebar("/sidebar 3")$turns, 3L)
    expect_true(ns$bot_should_respond(list(sender = "@operator:ex", body = "/sidebar"),
        "@bot:ex", c("@operator:ex", "@other:ex"), operators = "@operator:ex"))
    expect_false(ns$bot_should_respond(list(sender = "@other:ex", body = "/sidebar"),
        "@bot:ex", c("@operator:ex", "@other:ex"), operators = "@operator:ex"))
    expect_error(ns$bot_parse_sidebar("/sidebar 99"), "1 to 20")
    expect_error(go(sender = "@stranger:ex"), "operator")
    members <- c(members, "@outsider:ex")
    expect_error(go(), "just you")
    members <- c("@bot:ex", "@operator:ex")
    rule <- "public"
    expect_error(go(), "invite-only")
    rule <- "invite"
    fail <- TRUE
    expect_error(go(), "fixture send failure")
    expect_identical(ns$bot_job_delivery(ns$job_read(job))$room, source$room_id)
    fail <- FALSE
    ack <- go()
    expect_true(grepl("take this offline", ack, fixed = TRUE))
    expect_true(all(vapply(sent, function(m) identical(m$room, cfg$room_id), TRUE)))
    expect_true(grepl("question 6", sent[[1]]$text, fixed = TRUE))
    expect_false(grepl("question 5", sent[[1]]$text, fixed = TRUE))
    expect_true(grepl("Source: https://matrix.to/", sent[[1]]$text, fixed = TRUE))
    expect_identical(source$transcript, before)
    expect_true(length(target$history) > 0L)
    n <- length(sent)
    go()
    expect_identical(length(sent), n)
    expect_identical(ns$bot_job_delivery(ns$job_read(job))$room, cfg$room_id)
    expect_false(identical(ns$job_worker_identity(source), identity_before))
    target$job_requester <- "@operator:ex"
    expect_true(ns$job_visible_to(ns$job_read(job), target))
    target$job_requester <- "@outsider:ex"
    expect_false(ns$job_visible_to(ns$job_read(job), target))
    expect_true(grepl("private conversation", ns$bot_sidebar_job_status(ns$job_read(job), source)))
    review <- ns$job_create("review", workspace = root, owner = cfg$user_id,
        origin = list(session_key = source$job_key, room = source$room_id))
    review_job <- ns$job_read(review)
    review_job$review_of <- job
    expect_identical(ns$bot_job_delivery(review_job)$room, cfg$room_id)
    ns$job_settle(review, "cancelled")
    expect_identical(ns$bot_sidebar_job_session(sessions, target, list(job = job)), source)
    ns$bot_job_send(chat, source, ns$job_read(job), "Private progress")
    expect_identical(tail(sent, 1)[[1]]$room, cfg$room_id)
    expect_identical(source$transcript, before)
    members <- c(members, "@outsider:ex")
    expect_null(ns$bot_job_send(chat, source, ns$job_read(job), "Should stay private"))
    ns$job_settle(job, "done", result = "Private final result")
    ns$bot_job_settled(chat, source, ns$job_read(job), sessions)
    expect_false(any(vapply(source$history, function(x) grepl("Private final", x$content, fixed = TRUE), TRUE)))
    expect_true(ns$bot_notices_pending())
    members <- c("@bot:ex", "@operator:ex")
    ns$bot_notices_retry(chat, sessions, cfg$user_id, now = Sys.time() + 30)
    expect_identical(tail(sent, 1)[[1]]$room, cfg$room_id)
    expect_true(grepl("Private final result", tail(sent, 1)[[1]]$text, fixed = TRUE))
    source$transcript <- list()
    history_chat <- ns$bot_chat_client(cfg, save_cursor = FALSE, .history = function(...) list(
        chunk = list(list(event_id = "$thread", type = "m.room.message", sender = "@operator:ex",
            content = list(msgtype = "m.text", body = "Other thread",
                `m.relates_to` = list(rel_type = "m.thread", event_id = "$root"))),
            list(event_id = "$old", type = "m.room.message", sender = "@operator:ex",
                content = list(msgtype = "m.text", body = "Archived question"))), end = NULL))
    excerpt <- ns$bot_sidebar_excerpt(source, 3L, "$command", history_chat, cfg$user_id)
    expect_length(excerpt, 1L)
    expect_true(grepl("Archived question", excerpt[[1]]$content, fixed = TRUE))
})
