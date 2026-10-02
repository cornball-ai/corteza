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

# --- Handing a job to another room ---
# One bot, two rooms. The talker in room A hands a task to room B's
# doer: the job is B's to run, B is told, and the result goes to both.
# The worker is real (a callr child) with its model call replaced.

rooms_dir <- list(
    list(id = "!a:ex", name = "llm.api", cwd = tempdir()),
    list(id = "!b:ex", name = "Corteza", cwd = file.path(tempdir(), "corteza")),
    list(id = "!c:ex", name = NULL, cwd = tempdir()))
dir.create(rooms_dir[[2L]]$cwd, showWarnings = FALSE)

worker_session <- function(key, cwd = tempdir()) {
    s <- corteza::new_session("matrix")
    s$cwd <- cwd
    s$config <- list()
    s$job_key <- key
    s$room_id <- key
    s$job_owner <- "@bot:ex"
    s$seen_event_ids <- character()
    s$job_worker_spec <- list(
        init_fn = function(spec) invisible(TRUE),
        run_fn = function(task) list(reply = paste("did:", task)))
    s
}
ops_cfg <- c(cfg, list(operators = "@troy:ex"))
text_of <- function(res) res$content[[1L]]$text

# Room matching: id, then name, then directory, then its last component.
rr <- function(want) {
    vapply(corteza:::bot_resolve_room(rooms_dir, want), function(r) r$id, "")
}
expect_identical(rr("!b:ex"), "!b:ex")
expect_identical(rr("corteza"), "!b:ex")
expect_identical(rr(" LLM.API "), "!a:ex")
expect_identical(rr(rooms_dir[[2L]]$cwd), "!b:ex")
# Two rooms share a directory: naming it is ambiguous, and the caller
# is told rather than given the first.
expect_identical(sort(rr(tempdir())), c("!a:ex", "!c:ex"))
expect_identical(sort(rr(basename(tempdir()))), c("!a:ex", "!c:ex"))
expect_identical(rr("nowhere"), character())
# A room with no name is known by its id.
expect_identical(corteza:::bot_room_label(rooms_dir[[3L]]), "!c:ex")

# The directory comes from the transport: joined rooms, names, topics.
local({
    proj <- tempfile("handoff-proj")
    dir.create(proj)
    on.exit(unlink(proj, recursive = TRUE), add = TRUE)
    chat <- corteza:::bot_chat_client(cfg, save_cursor = FALSE,
        .sync = function(client, ...) NULL,
        .send = function(...) "$x", .media = function(...) NULL,
        .channels = function(sess) c("!a:ex", "!b:ex"),
        .info = list(
            name = function(sess, room) if (room == "!a:ex") "llm.api",
            topic = function(sess, room) {
                if (room == "!b:ex") paste0(proj, " | the project")
            }))
    d <- corteza:::bot_room_directory(chat, cfg)
    expect_identical(vapply(d, function(r) r$id, ""), c("!a:ex", "!b:ex"))
    expect_identical(d[[1L]]$name, "llm.api")
    expect_identical(d[[2L]]$cwd, proj)
    expect_identical(d[[1L]]$cwd, corteza:::bot_default_cwd(cfg))
})

local({
    rec <- new.env()
    chat <- make_chat(rec)
    a <- worker_session("!a:ex")
    b <- worker_session("!b:ex", rooms_dir[[2L]]$cwd)
    reg <- registry(a, b)
    on.exit({
        corteza:::job_worker_close_all(a)
        corteza:::job_worker_close_all(b)
    }, add = TRUE)
    origin <- list(room = "!a:ex", thread = "$t1")
    opened <- character()
    hand <- function(room, sender = "@troy:ex", cfg_now = ops_cfg) {
        corteza:::bot_job_handoff(reg, a, cfg_now, chat, sender, origin,
            room = room, task = "raise the llm.api floor", rooms = rooms_dir,
            new_session = function(id) {
                opened <<- c(opened, id)
                if (exists(id, envir = reg, inherits = FALSE)) {
                    get(id, envir = reg)
                }
            })
    }
    before <- length(corteza:::job_list())

    # Only an operator's request is handed over, and a refusal neither
    # lists the rooms nor opens a session.
    no <- hand("corteza", sender = "@guest:ex")
    expect_true(isTRUE(no$isError))
    expect_true(grepl("operator", text_of(no)))
    expect_false(grepl("!b:ex", text_of(no), fixed = TRUE))
    none <- hand("corteza", cfg_now = cfg)
    expect_true(isTRUE(none$isError))
    # A miss lists the rooms; an ambiguous name asks for an id.
    miss <- hand("nowhere")
    expect_true(isTRUE(miss$isError))
    expect_true(grepl("!b:ex", text_of(miss), fixed = TRUE))
    amb <- hand(tempdir())
    expect_true(grepl("more than one room", text_of(amb)))
    # This room is not another room.
    same <- hand("llm.api")
    expect_true(grepl("this room", text_of(same)))
    expect_identical(length(corteza:::job_list()), before)
    expect_identical(length(rec$sent), 0L)
    expect_identical(opened, character())

    # The hand-off itself.
    res <- hand("corteza")
    expect_null(res$isError)
    expect_identical(opened, "!b:ex")
    jobs <- corteza:::job_list(origin_key = "!b:ex", owner = "@bot:ex")
    expect_identical(length(jobs), 1L)
    j <- jobs[[1L]]
    expect_true(grepl(j$id, text_of(res), fixed = TRUE))
    expect_true(grepl("Corteza", text_of(res), fixed = TRUE))
    # The job is room B's: its key, its directory, its room.
    expect_identical(j$origin$session_key, "!b:ex")
    expect_identical(j$origin$room, "!b:ex")
    expect_identical(normalizePath(j$workspace),
                     normalizePath(rooms_dir[[2L]]$cwd))
    expect_identical(j$requester, "@troy:ex")
    # And it records who asked, thread included.
    expect_identical(j$origin$from$session_key, "!a:ex")
    expect_identical(j$origin$from$room, "!a:ex")
    expect_identical(j$origin$from$thread, "$t1")
    expect_identical(j$origin$from$label, "llm.api")
    # Room B is told at once: a post there, and in its talker's history.
    expect_identical(length(rec$sent), 1L)
    expect_identical(rec$sent[[1L]]$room, "!b:ex")
    expect_true(grepl("accepted from llm.api", rec$sent[[1L]]$text,
                      fixed = TRUE))
    expect_true(grepl("@troy:ex", rec$sent[[1L]]$text, fixed = TRUE))
    expect_identical(length(b$history), 1L)
    expect_true(grepl(j$id, b$history[[1L]]$content, fixed = TRUE))
    expect_identical(length(a$history), 0L)

    # Either room may ask about it; an unrelated room may not.
    expect_true(corteza:::job_visible_to(j, a))
    expect_true(corteza:::job_visible_to(j, b))
    expect_false(corteza:::job_in_session(j, a))
    other <- worker_session("!c:ex")
    expect_false(corteza:::job_visible_to(j, other))
    st <- corteza:::tool_job_status(ctx = list(session = a))
    expect_true(grepl(j$id, text_of(st), fixed = TRUE))
    expect_true(grepl(j$id, text_of(corteza:::tool_job_status(
        id = j$id, ctx = list(session = a))), fixed = TRUE))
    expect_true(isTRUE(corteza:::tool_job_status(
        id = j$id, ctx = list(session = other))$isError))
    expect_false(grepl(j$id, text_of(corteza:::tool_job_status(
        ctx = list(session = other))), fixed = TRUE))
    # Another bot in room A, sharing the key, did not ask for it.
    twin <- worker_session("!a:ex")
    twin$job_owner <- "@codex:ex"
    expect_false(corteza:::job_visible_to(j, twin))

    # The result goes to both rooms and both talkers.
    deadline <- Sys.time() + 30
    repeat {
        corteza:::bot_pump_jobs(reg, chat, ops_cfg)
        if (corteza:::job_read(j$id)$status %in%
            corteza:::JOB_STATUSES_FINAL || Sys.time() > deadline) {
            break
        }
        Sys.sleep(0.1)
    }
    expect_identical(corteza:::job_read(j$id)$status, "done")
    posts <- rec$sent[-1L]
    where <- vapply(posts, function(x) x$room, "")
    expect_identical(sort(where), c("!a:ex", "!b:ex"))
    in_b <- posts[[which(where == "!b:ex")]]$text
    in_a <- posts[[which(where == "!a:ex")]]$text
    expect_true(grepl("did: raise the llm.api floor", in_b, fixed = TRUE))
    expect_true(grepl("Requested from llm.api", in_b, fixed = TRUE))
    expect_true(grepl("did: raise the llm.api floor", in_a, fixed = TRUE))
    expect_true(grepl("Run by the doer in Corteza", in_a, fixed = TRUE))
    expect_identical(length(a$history), 1L)
    expect_identical(a$history[[1L]]$content, in_a)
    expect_identical(b$history[[2L]]$content, in_b)
    # The echo of the post in room A will be skipped, not re-ingested.
    expect_identical(length(a$seen_event_ids), 1L)

    # The room that asked can cancel what it handed over.
    res2 <- hand("!b:ex")
    j2 <- corteza:::job_list(status = corteza:::JOB_STATUSES_OPEN,
                             origin_key = "!b:ex", owner = "@bot:ex")[[1L]]
    expect_error(corteza:::job_cancel(other, j2$id), "no job")
    expect_true(corteza:::job_cancel(a, j2$id, by = "@troy:ex"))
})

# With the asking session gone, the result is still posted where it was
# asked for.
local({
    rec <- new.env()
    chat <- make_chat(rec)
    b <- make_session("!b:ex")
    id <- corteza:::job_create("handed over", origin = list(
        session_key = "!b:ex", room = "!b:ex", label = "Corteza",
        from = list(session_key = "!gone:ex", room = "!gone:ex",
                    label = "Gone")))
    corteza:::job_settle(id, "done", result = "Finished.")
    corteza:::bot_present_job_event(chat, cfg, b,
        list(type = "settled", job = corteza:::job_read(id)),
        sessions = registry(b))
    expect_identical(vapply(rec$sent, function(x) x$room, ""),
                     c("!b:ex", "!gone:ex"))
    # A job with no asker is posted once, as before.
    rec2 <- new.env()
    chat2 <- make_chat(rec2)
    own <- corteza:::job_create("own work", origin = list(
        session_key = "!b:ex", room = "!b:ex"))
    corteza:::job_settle(own, "done", result = "Finished.")
    corteza:::bot_present_job_event(chat2, cfg, b,
        list(type = "settled", job = corteza:::job_read(own)),
        sessions = registry(b))
    expect_identical(length(rec2$sent), 1L)
    expect_false(grepl("Requested from", rec2$sent[[1L]]$text))
})

# `delegate` with a room goes through the surface's hand-off, and a
# session with none says so instead of running the task here.
local({
    s <- corteza::new_session("cli")
    s$job_key <- "repl"
    refused <- corteza:::tool_delegate("do x", room = "corteza",
                                       ctx = list(session = s))
    expect_true(isTRUE(refused$isError))
    expect_true(grepl("no other rooms", text_of(refused)))
    expect_identical(length(corteza:::job_list(origin_key = "repl")), 0L)
    got <- NULL
    s$job_handoff <- function(room, task, review = NULL) {
        got <<- list(room = room, task = task, review = review)
        corteza:::ok("handed")
    }
    res <- corteza:::tool_delegate("do x", review = TRUE, room = "corteza",
                                   ctx = list(session = s))
    expect_identical(text_of(res), "handed")
    expect_identical(got, list(room = "corteza", task = "do x",
                               review = TRUE))
    # An empty room is this room.
    s$job_handoff <- function(...) stop("should not be called")
    s$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE),
                              run_fn = function(task) list(reply = "ok"))
    s$cwd <- tempdir()
    s$config <- list()
    here <- corteza:::tool_delegate("do y", room = "",
                                    ctx = list(session = s))
    expect_null(here$isError)
    corteza:::job_worker_close_all(s)
})

if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(state, recursive = TRUE)
