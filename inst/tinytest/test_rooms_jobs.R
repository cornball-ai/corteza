library(tinytest)

# Jobs in Matrix rooms: results posted back to their room, approval
# prompts raised without blocking, and reactions answering them in a
# later poll. Driven through chat.api's seams, so nothing reaches a
# homeserver, and against a private job ledger.

if (!requireNamespace("chat.api", quietly = TRUE) ||
    !requireNamespace("mx.client", quietly = TRUE)) {
    exit_file("chat.api and mx.client needed")
}

state <- tempfile("rooms-jobs-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

cfg <- list(server = "https://ex.invalid", user = "bot", token = "tok",
            user_id = "@bot:ex", device_id = "DEV", room_id = "!default:ex")

# A chat client whose sends and reactions are recorded, and whose room
# has the bot plus the given members.
make_chat <- function(rec, members = c("@bot:ex", "@troy:ex")) {
    rec$sent <- list()
    rec$reacted <- list()
    n <- 0L
    corteza:::bot_chat_client(cfg, save_cursor = FALSE,
        .sync = function(client, ...) {
            list(sync = list(rooms = list(join = list())), client = client,
                 first_run = FALSE)
        },
        .send = function(client, text, room = NULL, ...) {
            n <<- n + 1L
            rec$sent[[length(rec$sent) + 1L]] <- list(room = room,
                                                      text = text)
            sprintf("$ev%d", n)
        },
        .media = function(...) NULL,
        .members = function(sess, room) members,
        .react = function(session, room_id, event_id, key) {
            rec$reacted[[length(rec$reacted) + 1L]] <- list(
                room = room_id, event = event_id, key = key)
            "$seed"
        })
}

make_session <- function(key = "!room:ex") {
    s <- new.env()
    s$job_key <- key
    s$room_id <- key
    s$config <- list()
    s$seen_event_ids <- character()
    s
}
registry <- function(...) {
    r <- new.env()
    for (s in list(...)) assign(s$job_key, s, envir = r)
    r
}
rx <- function(key, target, sender = "@troy:ex", room = "!room:ex",
               self = FALSE) {
    chat.api::chat_reaction(id = "$r", channel = room, sender = sender,
                            target = target, key = key,
                            ts = as.POSIXct(NA), self = self)
}
# A dispatched job with one open approval request, as a worker would
# leave it.
job_waiting <- function(s) {
    id <- corteza:::job_create("rm stale files",
                               origin = list(session_key = s$job_key,
                                             room = s$room_id))
    corteza:::job_mark_dispatched(id)
    req <- corteza:::job_approval_request(
        id, list(tool = "bash", args = list(cmd = "rm x")),
        list(reason = "code/exec/matrix"))
    list(event = list(type = "approval", job = corteza:::job_read(id),
                      request = corteza:::job_approval_pending(id)[[1L]]),
         id = id, req = req)
}
answer_of <- function(id, req) {
    p <- corteza:::job_approval_path(id, req, "answer")
    if (file.exists(p)) jsonlite::fromJSON(p) else NULL
}

# --- A settled job is posted to its room and lands in history ---
local({
    rec <- new.env()
    chat <- make_chat(rec)
    s <- make_session()
    id <- corteza:::job_create("summarise", origin = list(
        session_key = s$job_key, room = "!room:ex", thread = "$root"))
    corteza:::job_settle(id, "done", result = "All good.")
    corteza:::bot_present_job_event(chat, cfg, s,
                                    list(type = "settled",
                                         job = corteza:::job_read(id)))
    expect_identical(length(rec$sent), 1L)
    expect_identical(rec$sent[[1L]]$room, "!room:ex")
    expect_true(grepl(id, rec$sent[[1L]]$text, fixed = TRUE))
    expect_true(grepl("All good.", rec$sent[[1L]]$text, fixed = TRUE))
    # The talker's history has it, and the echo will be skipped.
    last <- s$history[[length(s$history)]]
    expect_identical(last$role, "assistant")
    expect_true(grepl("All good.", last$content, fixed = TRUE))
    expect_true("$ev1" %in% s$seen_event_ids)
})

# Result text by status.
local({
    id <- corteza:::job_create("x")
    corteza:::job_settle(id, "indeterminate", reason = "worker died")
    txt <- corteza:::bot_job_result_text(corteza:::job_read(id))
    expect_true(grepl("indeterminate", txt))
    expect_true(grepl("worker died", txt))
    id2 <- corteza:::job_create("y")
    corteza:::job_settle(id2, "failed", error = "boom")
    expect_true(grepl("boom", corteza:::bot_job_result_text(
        corteza:::job_read(id2))))
})

# --- auto_approve_asks answers at once, with no prompt ---
local({
    rec <- new.env()
    chat <- make_chat(rec)
    s <- make_session()
    w <- job_waiting(s)
    corteza:::bot_present_job_event(chat, c(cfg, list(auto_approve_asks = TRUE)),
                                    s, w$event)
    a <- answer_of(w$id, w$req)
    expect_true(a$approved)
    expect_identical(a$by, "auto_approve_asks")
    expect_identical(length(rec$sent), 0L)
})

# --- No approver in the room: declined, with a notice ---
local({
    rec <- new.env()
    chat <- make_chat(rec, members = c("@bot:ex", "@a:ex", "@b:ex"))
    s <- make_session()
    w <- job_waiting(s)
    corteza:::bot_present_job_event(chat, cfg, s, w$event)
    expect_false(answer_of(w$id, w$req)$approved)
    expect_true(grepl("nobody here can approve", rec$sent[[1L]]$text))
    expect_null(s$.job_prompts)
})

# --- A prompt is posted, then a later poll's reaction answers it ---
local({
    rec <- new.env()
    chat <- make_chat(rec)
    s <- make_session()
    reg <- registry(s)
    w <- job_waiting(s)
    corteza:::bot_present_job_event(chat, cfg, s, w$event)
    expect_true(grepl("Approval needed: bash", rec$sent[[1L]]$text))
    expect_true(grepl(w$id, rec$sent[[1L]]$text, fixed = TRUE))
    # Both seeds, on the prompt.
    expect_identical(length(rec$reacted), 2L)
    expect_identical(rec$reacted[[1L]]$event, "$ev1")
    expect_identical(names(s$.job_prompts), "$ev1")
    # Nothing answered yet; the worker is still waiting.
    expect_null(answer_of(w$id, w$req))
    expect_true(corteza:::bot_jobs_active(reg))

    # The bot's own seeds and another bot's tap do not answer.
    corteza:::bot_handle_job_reactions(
        list(rx(intToUtf8(0x1F44D), "$ev1", sender = "@bot:ex", self = TRUE),
             rx(intToUtf8(0x1F44D), "$ev1", sender = "@codex:ex")),
        reg, chat, cfg)
    expect_null(answer_of(w$id, w$req))
    # A reaction on some other message does not either.
    corteza:::bot_handle_job_reactions(list(rx("yes", "$other")), reg, chat,
                                       cfg)
    expect_null(answer_of(w$id, w$req))

    # The room's human does.
    n <- corteza:::bot_handle_job_reactions(
        list(rx(intToUtf8(0x1F44D), "$ev1")), reg, chat, cfg)
    expect_identical(n, 1L)
    a <- answer_of(w$id, w$req)
    expect_true(a$approved)
    expect_identical(a$by, "@troy:ex")
    expect_identical(length(s$.job_prompts), 0L)
})

# --- An answer after the request closed is reported as too late ---
local({
    rec <- new.env()
    chat <- make_chat(rec)
    s <- make_session()
    reg <- registry(s)
    w <- job_waiting(s)
    corteza:::bot_present_job_event(chat, cfg, s, w$event)
    # The job is cancelled before anyone taps.
    corteza:::job_settle(w$id, "cancelled")
    corteza:::bot_handle_job_reactions(list(rx("no", "$ev1")), reg, chat, cfg)
    expect_null(answer_of(w$id, w$req))
    expect_true(grepl("too late", rec$sent[[length(rec$sent)]]$text))
})

# --- Which sessions get pumped ---
local({
    idle <- make_session("!idle:ex")
    busy <- make_session("!busy:ex")
    queued <- make_session("!queued:ex")
    corteza:::job_create("waiting", origin = list(session_key = "!queued:ex"))
    busy$.job_current <- "20260101T000000-00000000"
    got <- vapply(corteza:::bot_job_sessions(registry(idle, busy, queued)),
                  function(s) s$job_key, "")
    expect_identical(sort(unname(got)), c("!busy:ex", "!queued:ex"))
    expect_false(corteza:::bot_jobs_active(registry(idle)))
    expect_identical(corteza:::bot_job_sessions(NULL), list())
    # A job queued by another bot in the same room does not make this
    # bot's session pumpable: two bots share a room's key.
    shared <- make_session("!shared:ex")
    shared$job_owner <- "@claude:ex"
    corteza:::job_create("codex's job", owner = "@codex:ex",
                         origin = list(session_key = "!shared:ex"))
    expect_identical(length(corteza:::bot_job_sessions(registry(shared))), 0L)
    expect_identical(corteza:::bot_job_owner(list(user_id = "@claude:ex")),
                     "@claude:ex")
})

# --- Startup recovery tells each room what it found ---
local({
    # A ledger of its own, so jobs left open by the tests above are not
    # swept into this one.
    dir <- tempfile("recover-state")
    dir.create(dir)
    Sys.setenv(CORTEZA_STATE_DIR = dir)
    on.exit(Sys.setenv(CORTEZA_STATE_DIR = state))
    rec <- new.env()
    chat <- make_chat(rec)
    lost <- corteza:::job_create("half done", origin = list(
        session_key = "!a:ex", room = "!a:ex"))
    corteza:::job_mark_dispatched(lost)
    waiting <- corteza:::job_create("not started", origin = list(
        session_key = "!b:ex", room = "!b:ex"))
    # Another bot's running job, sharing the ledger. Not ours to settle.
    theirs <- corteza:::job_create("other bot's job", owner = "@codex:ex",
                                   origin = list(session_key = "!a:ex",
                                                 room = "!a:ex"))
    corteza:::job_mark_dispatched(theirs)
    v <- suppressMessages(corteza:::bot_recover_jobs(chat, owner = "local"))
    expect_identical(nrow(v), 2L)
    expect_identical(corteza:::job_read(theirs)$status, "running")
    rooms <- vapply(rec$sent, function(x) x$room, "")
    texts <- vapply(rec$sent, function(x) x$text, "")
    expect_true(grepl("will not be re-run", texts[rooms == "!a:ex"]))
    expect_true(grepl("still queued", texts[rooms == "!b:ex"]))
    expect_identical(corteza:::job_read(lost)$status, "indeterminate")
    expect_identical(corteza:::job_read(waiting)$status, "queued")
})

if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(state, recursive = TRUE)
