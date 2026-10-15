# One bot, two independent conversations. Fake model calls and transport;
# real durable jobs, child processes, tool policy, snapshots, and polling.
local({
    if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api needed")
    root <- tempfile("room-consult-")
    dir.create(root)
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = root)
    f <- function(name) get(name, asNamespace("corteza"))
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        utils::assignInNamespace(name, fn, package)
    }
    on.exit({
        for (id in ls(f(".room_consult_workers"))) f(".room_consult_close")(id)
        for (x in rev(saved)) utils::assignInNamespace(x$name, x$fn, x$package)
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- list(user_id = "bot", operators = "human", room_consultation = TRUE)
    chat <- chat.api::chat_loopback()
    registry <- new.env(parent = emptyenv())
    make <- function(room, thread = NULL) {
        s <- corteza::new_session(channel = "matrix", provider = "anthropic",
            model_map = list(cloud = "conversation-model"), system = "Destination room instructions",
            approval_cb = function(...) TRUE)
        s$config <- list()
        s$cwd <- root
        s$room_id <- room
        s$job_key <- f("bot_session_key")(room, thread)
        s$job_owner <- "bot"
        s$job_requester <- "human"
        s$seen_event_ids <- character()
        f("journal_install")(s, dir = file.path(root, paste0("journal-", digest::digest(s$job_key))))
        assign(s$job_key, s, registry)
        s
    }
    a <- make("alpha", "origin-thread")
    b <- make("beta")
    b$doer_model <- "doer-model-must-not-run"
    b$history <- list(list(role = "user", content = "We chose the green fixture."),
        list(role = "assistant", content = list(list(type = "thinking", thinking = "secret-thought", signature = "signed"),
            list(type = "text", text = "I recommended the green fixture."),
            list(type = "tool_use", id = "call1", name = "read_file", input = list(path = "fixture")))),
        list(role = "user", content = list(list(type = "tool_result", tool_use_id = "call1", content = "Fixture deadline: Friday"))),
        list(role = "assistant", content = "The deadline is Friday."))
    original <- b$history
    directory <- list(list(id = "alpha", name = "Alpha", cwd = root),
                      list(id = "beta", name = "Beta", cwd = root))
    opened <- 0L
    factory <- function(room, thread) {
        opened <<- opened + 1L
        key <- f("bot_session_key")(room, thread)
        if (exists(key, registry, inherits = FALSE)) get(key, registry) else make(room, thread)
    }
    text <- function(x) x$content[[1L]]$text
    submitted_id <- NULL
    submit <- function(question = "What did we decide?", room = "Beta", thread = NULL,
                       config = cfg, sender = "human", rooms = directory) {
        result <- f(".room_consult_submit")(a, config, chat, sender,
            list(room = "alpha", thread = "origin-thread"), room, question,
            thread, factory, rooms = rooms)
        if (!isTRUE(result$isError)) submitted_id <<- regmatches(text(result),
            regexpr("[0-9]{8}T[0-9]{6}-[0-9a-f]{8}", text(result)))
        result
    }
    jobs <- function() f("job_list")(owner = "bot")
    last_id <- function() submitted_id
    disabled <- cfg
    disabled$room_consultation <- FALSE
    expect_true(submit(config = disabled, rooms = stop("must not list rooms"))$isError)
    expect_true(submit(sender = "stranger", rooms = stop("must not list rooms"))$isError)
    expect_equal(opened, 0L)
    expect_true(submit(question = paste(rep("x", 7001), collapse = ""))$isError)
    expect_true(submit(room = "unknown")$isError)
    expect_true(submit(room = root)$isError)
    expect_true(submit(room = "Alpha", thread = "origin-thread")$isError)
    a$plan_mode <- TRUE
    expect_true(submit()$isError)
    a$plan_mode <- FALSE
    expect_length(jobs(), 0L)

    expect_null(submit()$isError)
    id <- last_id()
    j <- f("job_read")(id)
    expect_identical(j$backend, "consultation")
    expect_identical(j$role, "consultation")
    expect_identical(j$origin$session_key, "beta")
    expect_identical(j$origin$from$session_key, a$job_key)
    expect_identical(j$origin$from$thread, "origin-thread")
    expect_true(f("job_visible_to")(j, a))
    expect_true(f("job_visible_to")(j, b))
    expect_false(f("job_role_locks")(j$role))
    expect_length(f("job_dispatch_next")(b), 0L)
    expect_null(b$.job_worker)
    expect_identical(b$history, original)
    expect_true(grepl(id, text(submit()), fixed = TRUE))
    expect_length(jobs(), 1L)
    snap <- readRDS(file.path(f("job_dir")(id), "consultation.rds"))
    expect_identical(snap$args$model, "conversation-model")
    expect_identical(snap$history, original)
    rendered <- paste(vapply(snap$args$history, function(x) x$content, ""), collapse = "\n")
    expect_true(grepl("green fixture", rendered))
    expect_true(grepl("Fixture deadline: Friday", rendered))
    expect_false(grepl("secret-thought|signed", rendered))
    expect_false(snap$args$web_search)
    expect_null(snap$args$tools)
    expect_null(snap$args$tool_handler)

    # Reasoning settings use the same provider-specific wire mapping as turn.
    b$reasoning_effort <- "high"
    expect_identical(f(".room_consult_snapshot")(b, "q", "Alpha")$args$output_config$effort, "high")
    b$provider <- "openai"
    expect_identical(f(".room_consult_snapshot")(b, "q", "Alpha")$args$reasoning_effort, "high")
    b$provider <- "anthropic"
    b$reasoning_effort <- NULL
    # An OpenAI-native call/result pair also survives the text projection.
    b$history <- list(list(role = "user", content = "Q"),
        list(role = "assistant", content = NULL, tool_calls = list(list(id = "c", type = "function",
            `function` = list(name = "read_file", arguments = "{}")))),
        list(role = "tool", tool_call_id = "c", content = "OpenAI fixture result"),
        list(role = "assistant", content = "OpenAI reply"))
    openai <- f(".room_consult_snapshot")(b, "q", "Alpha")
    expect_true(grepl("OpenAI fixture result", paste(vapply(openai$args$history, function(x) x$content, ""), collapse = "\n")))
    b$history <- original
    nchat <- 0L
    reply <- f(".room_consult_answer")(snap, .chat = function(...) {
        nchat <<- nchat + 1L
        list(content = "Green, due Friday", finish_reason = "stop", usage = list(input = 42))
    })
    expect_equal(nchat, 1L)
    expect_identical(reply$reply, "Green, due Friday")
    expect_equal(reply$usage$input, 42)
    cut <- f(".room_consult_answer")(snap, .chat = function(...) list(content = "Green", finish_reason = "length"))
    expect_true(grepl("incomplete", cut$error))
    expect_null(cut$reply)
    expect_true(grepl("no answer", f(".room_consult_answer")(snap, .chat = function(...) list(content = ""))$error))

    # A real child answers from the durable snapshot. No bot credentials or
    # tool callbacks are serialized. Mutating the live room after submission
    # must not change this query's input or overwrite later conversation.
    start <- function(id) callr::r_bg(function(id) {
        snapshot <- readRDS(file.path(corteza:::job_dir(id), "consultation.rds"))
        corteza:::.room_consult_answer(snapshot, .chat = function(history, model, web_search, ...) {
            stopifnot(model == "conversation-model", !web_search)
            body <- paste(vapply(history, function(x) x$content, ""), collapse = "\n")
            stopifnot(grepl("green fixture", body), !grepl("newer message", body))
            list(content = "Green, due Friday", finish_reason = "stop", usage = list(input = 42))
        })
    }, list(id = id), libpath = .libPaths(), supervise = TRUE)
    b$history <- c(b$history, list(list(role = "user", content = "newer message")))
    b$.job_current <- "unrelated-doer"
    f(".room_consult_pump")(chat, cfg, registry, .start = start)
    expect_identical(f("job_read")(id)$status, "running")
    expect_identical(b$.job_current, "unrelated-doer")
    expect_true(f("bot_jobs_active")(registry))
    expect_length(chat$env$log, 2L)
    deadline <- Sys.time() + 15
    while (f("job_read")(id)$status == "running" && Sys.time() < deadline) {
        Sys.sleep(0.05)
        f(".room_consult_pump")(chat, cfg, registry, .start = start)
    }
    expect_identical(f("job_read")(id)$status, "done")
    expect_equal(f("job_read")(id)$outcome$usage$input, 42)
    expect_length(chat$env$log, 4L)
    expect_identical(b$history[seq_along(original)], original)
    expect_identical(b$history[[length(original) + 1L]]$content, "newer message")
    expect_true(grepl("Green, due Friday", utils::tail(a$history, 1)[[1]]$content))
    expect_true(grepl("Green, due Friday", utils::tail(b$history, 1)[[1]]$content))
    expect_identical(f("journal_load")(b$journal_dir)$live$history, b$history)
    expect_identical(utils::tail(chat$env$log, 1)[[1]]$thread, "origin-thread")
    f(".room_consult_pump")(chat, cfg, registry, .start = stop("must not restart"))
    expect_length(chat$env$log, 4L)
    b$.job_current <- NULL

    # Thread destinations are distinct conversations; no main-room fallback.
    expect_true(submit(question = "thread question", thread = "target-thread")$isError)
    thread <- get(f("bot_session_key")("beta", "target-thread"), registry)
    thread$history <- list(list(role = "user", content = "Thread-only fact"))
    expect_null(submit(question = "thread question", thread = "target-thread")$isError)
    threaded <- last_id()
    expect_identical(f("job_read")(threaded)$origin$thread, "target-thread")
    expect_true(f("job_cancel")(a, threaded, "human"))
    f(".room_consult_pump")(chat, cfg, registry, .start = stop("cancelled job must not run"))
    expect_identical(f("job_read")(threaded)$status, "cancelled")

    # Timeout, cancellation during a query, revocation and restart each end
    # the request visibly. They never fall back to running a doer.
    held <- function(id) list(is_alive = function() TRUE, kill = function() TRUE)
    submit(question = "cancel running")
    cancelled <- last_id()
    f(".room_consult_pump")(chat, cfg, registry, now = 100, .start = held)
    expect_true(f("job_cancel")(b, cancelled, "human"))
    f(".room_consult_pump")(chat, cfg, registry, now = 101, .start = held)
    expect_identical(f("job_read")(cancelled)$status, "cancelled")
    submit(question = "timeout")
    timed <- last_id()
    f(".room_consult_pump")(chat, cfg, registry, now = 100, .start = held)
    f(".room_consult_pump")(chat, cfg, registry, now = 221, .start = held)
    expect_identical(f("job_read")(timed)$status, "failed")
    submit(question = "revoked")
    revoked <- last_id()
    f(".room_consult_pump")(chat, disabled, registry, .start = stop("disabled must not run"))
    expect_identical(f("job_read")(revoked)$status, "cancelled")
    submit(question = "lost child")
    lost <- last_id()
    f("job_mark_dispatched")(lost)
    f(".room_consult_pump")(chat, cfg, registry, .start = stop("lost query must not restart"))
    expect_identical(f("job_read")(lost)$status, "indeterminate")
    submit(question = "recovery")
    recovered <- last_id()
    f("job_mark_dispatched")(recovered)
    verdict <- callr::r(function() corteza:::job_recover(owner = "bot"))
    expect_identical(verdict$verdict, "indeterminate")
    expect_identical(f("job_read")(recovered)$status, "indeterminate")
    f(".room_consult_pump")(chat, cfg, registry, .start = stop("recovered query must not restart"))

    # Tool schemas and the actual turn handler preserve policy and dry run.
    patch("bot_room_directory", function(...) directory)
    f(".room_consult_bind")(a, cfg, chat, "human", list(room = "alpha", thread = "origin-thread"), factory)
    schema <- utils::tail(f(".room_consult_tools")(list(), a), 1)[[1]]
    expect_identical(schema$name, "ask_room")
    expect_true(grepl('"required":\\["room","question"\\]', jsonlite::toJSON(schema, auto_unbox = TRUE)))
    count <- length(jobs())
    gates <- 0L
    a$auto_gate <- function(...) { gates <<- gates + 1L; list(action = "proceed") }
    h <- f(".make_tool_handler")(a, function(...) stop("ordinary executor reached"))
    expect_true(grepl("Queued question", h("ask_room", list(room = "Beta", question = "handler query"))))
    expect_equal(gates, 1L)
    expect_equal(length(jobs()), count + 1L)
    a$dry_run <- TRUE
    expect_true(grepl("Dry run", h("ask_room", list(room = "Beta", question = "dry"))))
    expect_equal(length(jobs()), count + 1L)
    a$dry_run <- FALSE
    a$plan_mode <- TRUE
    expect_length(f(".room_consult_tools")(list(), a), 0L)
    a$plan_mode <- FALSE
    f(".room_consult_bind")(a, cfg, chat, "stranger", list(room = "alpha"), factory)
    expect_null(a$ask_room)

    # Failed sends use the existing durable notice queue. Re-pumping does
    # not call the model again or queue another copy of each notice.
    pending <- Filter(function(j) identical(j$task, "handler query"), jobs())[[1L]]$id
    f("job_settle")(pending, "done", result = "offline result")
    f(".room_consult_pump")(NULL, cfg, registry, .start = stop("settled query must not run"))
    paths <- list.files(f("bot_notice_dir")(), full.names = TRUE)
    expect_length(paths, 2L)
    f(".room_consult_pump")(NULL, cfg, registry, .start = stop("settled query must not run"))
    expect_identical(list.files(f("bot_notice_dir")(), full.names = TRUE), paths)
    f("bot_notices_retry")(chat, registry, "bot", now = as.numeric(Sys.time()) + 10)
    expect_length(list.files(f("bot_notice_dir")()), 0L)

    # A queued query stays queued behind another consultation of that same
    # conversation, while unrelated ordinary work uses no consultation worker.
    submit(question = "first queued")
    first <- last_id()
    f(".room_consult_pump")(chat, cfg, registry, now = 100, .start = held)
    submit(question = "second queued")
    second <- last_id()
    f(".room_consult_pump")(chat, cfg, registry, now = 101, .start = held)
    expect_identical(f("job_read")(first)$status, "running")
    expect_identical(f("job_read")(second)$status, "queued")
    expect_length(f("job_dispatch_next")(b), 0L)
    f("job_cancel")(a, first, "human")
    f("job_cancel")(a, second, "human")
    f(".room_consult_pump")(chat, cfg, registry, .start = stop("cancelled queue must not run"))

    # Real bot_poll binds ask_room only for the human's turn, queues the
    # consultation through the real handler, and removes that callback after.
    incoming <- chat.api::chat_message(id = "consult-request", channel = "alpha", sender = "human",
        body = "Ask Beta what we decided", thread = "origin-thread", ts = Sys.time())
    patch("bot_load_config", function(...) cfg)
    patch("bot_chat_client", function(...) chat)
    patch("chat_whoami.chat_loopback", function(...) chat.api::chat_identity("bot"), "chat.api")
    patch("chat_poll", function(...) list(first_run = FALSE, messages = list(incoming),
        reactions = list(), invites = list()), "chat.api")
    patch("bot_should_respond", function(...) TRUE)
    patch(".room_consult_start", held)
    calls <- 0L
    patch("bot_run_turn_in_cwd", function(prompt, session) {
        calls <<- calls + 1L
        stopifnot(identical(session, a), is.function(session$ask_room))
        result <- f(".make_tool_handler")(session)("ask_room", list(room = "Beta", question = "actual poll question"))
        stopifnot(grepl("Queued question", result))
        "I will ask Beta."
    })
    count <- length(jobs())
    expect_equal(corteza::bot_poll(sessions = registry), 1L)
    expect_equal(calls, 1L)
    expect_null(a$ask_room)
    expect_equal(length(jobs()), count + 1L)
    polled <- Filter(function(j) identical(j$task, "actual poll question"), jobs())[[1L]]
    expect_identical(polled$origin$from$thread, "origin-thread")
    expect_equal(corteza::bot_poll(sessions = registry), 0L)
    expect_equal(calls, 1L)
    expect_identical(f("job_read")(polled$id)$status, "running")
    f("job_settle")(polled$id, "done", result = "poll answer")
    f(".room_consult_pump")(chat, cfg, registry)
    echo <- utils::tail(chat$env$log, 1)[[1L]]
    echo$self <- TRUE
    before <- a$history
    incoming <- echo
    expect_equal(corteza::bot_poll(sessions = registry), 0L)
    expect_equal(calls, 1L)
    expect_identical(a$history, before)
})
