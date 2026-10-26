# Two identities on an offline chat.api loopback bus. All ledgers, workers,
# approval files and configuration live in a fresh temporary state directory.
local({
    root <- tempfile("delegation-")
    dir.create(root)
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = root)
    on.exit({
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old_state)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    f <- function(name) get(name, asNamespace("corteza"))
    patch <- function(name, fn, package = "corteza") {
        original <- get(name, asNamespace(package))
        utils::assignInNamespace(name, fn, package)
        original
    }
    original_send <- get("chat_send.chat_loopback", asNamespace("chat.api"))
    original_caps <- get("chat_capabilities.chat_loopback", asNamespace("chat.api"))
    original_factory <- get("bot_get_or_create_session", asNamespace("corteza"))
    original_specs <- get("job_worker_spec", asNamespace("corteza"))
    on.exit({
        utils::assignInNamespace("chat_send.chat_loopback", original_send, "chat.api")
        utils::assignInNamespace("chat_capabilities.chat_loopback", original_caps, "chat.api")
        utils::assignInNamespace("bot_get_or_create_session", original_factory, "corteza")
        utils::assignInNamespace("job_worker_spec", original_specs, "corteza")
    }, add = TRUE)
    bus <- chat.api::chat_loopback()
    a <- b <- bus
    a$test_id <- "requester"
    b$test_id <- "worker"
    fail_send <- FALSE
    patch("chat_send.chat_loopback", function(client, channel, text, ..., mentions = NULL) {
        if (fail_send) stop("offline delivery failure")
        id <- original_send(client, channel, text, ...,
            identity = list(name = client$test_id))
        i <- length(client$env$log)
        client$env$log[[i]]$mentions <- mentions
        id
    }, "chat.api")
    patch("chat_capabilities.chat_loopback", function(client, ...) {
        caps <- original_caps(client)
        caps$threads <- caps$thread_replies <- caps$reaction_events <- FALSE
        caps$max_message_bytes <- 64000L
        caps
    }, "chat.api")
    cfg_a <- list(user_id = "requester", operators = "human",
        delegates = list(worker = list(bot = "worker", channel = "jobs", transport = "loopback")))
    cfg_b <- list(user_id = "worker", operators = "human", delegators = list("requester"))
    registry_a <- new.env(parent = emptyenv())
    registry_b <- new.env(parent = emptyenv())
    make <- function(owner, key) {
        s <- corteza::new_session(provider = "anthropic", system = "fixture")
        s$cwd <- root
        s$room_id <- key
        s$job_key <- key
        s$job_owner <- owner
        s$job_requester <- "human"
        s$job_origin <- list(room = key)
        s$config <- list()
        s
    }
    source <- make("requester", "human-room")
    assign("human-room", source, registry_a)
    patch("bot_get_or_create_session", function(registry, key, cfg, ..., room_id = key) {
        if (!exists(key, registry, inherits = FALSE)) assign(key, make(cfg$user_id, key), registry)
        get(key, registry)
    })
    patch("job_worker_spec", function(...) list(tools = c("read_file", "write_file")))
    last <- function() tail(bus$env$log, 1)[[1]]
    route <- function(message, cfg, chat, registry) {
        m <- f("bot_msg_record")(message, addressed = cfg$user_id %in% message$mentions)
        f(".delegation_route")(m, cfg, chat, registry)
    }
    pump_a <- function(...) f(".delegation_pump")(a, cfg_a, registry_a, ...)
    pump_b <- function(...) f(".delegation_pump")(b, cfg_b, registry_b, ...)
    decode <- function(message = last()) f(".delegation_parse")(message$body)
    request <- function(id = f("job_new_id")(), ...) {
        fields <- utils::modifyList(list(id = id, hop = "1", hops = "3", review = "false",
            expires = as.character(as.numeric(Sys.time()) + 120)), list(...))
        chat.api::chat_message(id = paste0("event-", id), channel = "jobs", sender = "requester",
            body = f(".delegation_body")("request", fields, "Read a synthetic fixture"),
            mentions = "worker", kind = "notice", ts = Sys.time())
    }

    expect_null(f(".delegation_parse")("ordinary conversation"))
    expect_identical(f(".delegation_parse")("[corteza-job malformed]")$kind, "invalid")
    expect_identical(f(".delegation_parse")(f(".delegation_body")("status", list(id = "a b/%")))$fields$id, "a b/%")
    expect_identical(f(".delegation_parse")("[corteza-job v1 request id=a id=b]\nx")$kind, "invalid")
    f(".delegation_bind")(source, cfg_a)
    expect_true(is.function(source$delegate_to))
    schema <- tail(f(".delegation_tools")(list(), source), 1)[[1]]
    expect_identical(schema$name, "delegate_to")
    expect_true(grepl('"required":\\["bot","task"\\]', jsonlite::toJSON(schema, auto_unbox = TRUE)))
    source$plan_mode <- TRUE
    expect_length(f(".delegation_tools")(list(), source), 0L)
    source$plan_mode <- FALSE
    source$job_requester <- "stranger"
    expect_error(f(".delegation_submit")(source, cfg_a, "worker", "task"), "operator")
    source$job_requester <- "human"
    expect_error(f(".delegation_submit")(source, cfg_a, "stranger", "task"), "configured")
    expect_error(f(".delegation_submit")(source, cfg_a, "worker", paste(rep("x", 7001), collapse = "")), "7000")

    # Durable intent, no requester-side worker or checkout lock; worker accepts
    # once even if transport redelivery gives the request another event ID.
    id <- f(".delegation_submit")(source, cfg_a, "worker", "Read the fixture", TRUE)
    expect_identical(f("job_read")(id)$backend, "remote")
    expect_length(f("job_dispatch_next")(source), 0L)
    pump_a()
    req <- last()
    expect_identical(decode(req)$kind, "request")
    expect_identical(req$kind, "notice")
    expect_identical(req$mentions, "worker")
    expect_null(req$thread)
    expect_true(route(req, cfg_b, b, registry_b))
    ack <- last()
    worker <- decode(ack)$fields$job
    expect_identical(f("job_read")(worker)$requester, "requester")
    expect_equal(f("job_read")(worker)$hop, 1)
    expect_true(f("job_read")(worker)$review)
    duplicate <- req
    duplicate$id <- "redelivery"
    route(duplicate, cfg_b, b, registry_b)
    expect_identical(decode()$fields$job, worker)
    expect_length(f("job_list")(owner = "worker"), 1L)
    route(ack, cfg_a, a, registry_a)
    expect_identical(f(".delegation_state")(id)$job, worker)
    # Forged/wrong-room result cannot settle the requester's job.
    fake <- ack
    fake$body <- f(".delegation_body")("result", list(id = id, job = worker, status = "done"), "forged")
    fake$sender <- "stranger"
    route(fake, cfg_a, a, registry_a)
    expect_identical(f("job_read")(id)$status, "running")
    fake$sender <- "worker"
    fake$channel <- "elsewhere"
    route(fake, cfg_a, a, registry_a)
    expect_identical(f("job_read")(id)$status, "running")
    f("job_settle")(worker, "done", result = "Fixture verified")
    pump_b()
    result <- last()
    expect_identical(decode(result)$kind, "result")
    route(result, cfg_a, a, registry_a)
    route(result, cfg_a, a, registry_a)
    expect_identical(f("job_read")(id)$status, "done")
    pump_a()
    posted <- length(bus$env$log)
    pump_a()
    expect_equal(length(bus$env$log), posted)
    expect_true(any(grepl("Fixture verified", vapply(source$history, f(".compact_entry_body"), ""))))

    # Reject unauthorized, unverified, future-version, expired and excessive-
    # hop requests before creating a job. Configured identities count as bots.
    expect_true("requester" %in% f("bot_known_bots")(cfg_b, "worker"))
    for (reason in c("not-allowed", "unverified", "version", "expired", "hop-limit")) {
        msg <- request()
        if (reason == "not-allowed") msg$sender <- "outsider"
        if (reason == "unverified") { msg$encrypted <- TRUE; msg$sender_verified <- FALSE }
        if (reason == "version") msg$body <- sub("v1", "v2", msg$body, fixed = TRUE)
        if (reason == "expired") msg <- request(expires = "1")
        if (reason == "hop-limit") msg <- request(hop = "4")
        route(msg, cfg_b, b, registry_b)
        expect_identical(decode()$fields$reason, reason)
    }
    expect_length(f("job_list")(owner = "worker"), 1L)
    # A request without an addressing mention is consumed but not executed.
    msg <- request()
    msg$mentions <- NULL
    expect_true(route(msg, cfg_b, b, registry_b))
    expect_length(f("job_list")(owner = "worker"), 1L)

    # Runtime cycle bound, narrowed tools, and compulsory review.
    cfg_b$delegators <- list(list(bot = "requester", tools = list("read_file", "sudo"), review = TRUE))
    msg <- request(root = "chain-root")
    route(msg, cfg_b, b, registry_b)
    narrow_id <- decode()$fields$job
    narrow <- f("job_read")(narrow_id)
    expect_identical(narrow$origin$delegation$tools, "read_file")
    expect_true(narrow$review)
    route(request(root = "chain-root"), cfg_b, b, registry_b)
    expect_identical(decode()$fields$reason, "busy")
    # A hallucinated tool is blocked in the actual turn handler, not only
    # omitted from the model's schema. Local calls remain supervised normally.
    s <- make("worker", "jobs")
    s$job_allowed_tools <- "read_file"
    executed <- 0L
    handler <- f(".make_tool_handler")(s, function(...) { executed <<- executed + 1L; "read" })
    expect_true(grepl("job policy denied", handler("write_file", list(path = "fixture"))))
    expect_equal(executed, 0L)

    # Cancel-before-request records a tombstone; a delayed request cannot run.
    delayed <- request()
    cancel <- delayed
    cancel$id <- "cancel-first"
    cancel$body <- f(".delegation_body")("cancel", list(id = decode(delayed)$fields$id))
    route(cancel, cfg_b, b, registry_b)
    route(delayed, cfg_b, b, registry_b)
    expect_identical(decode()$fields$reason, "cancelled")
    cancel$body <- f(".delegation_body")("cancel", list(id = decode(msg)$fields$id))
    cancel$id <- "cancel-existing"
    route(cancel, cfg_b, b, registry_b)
    route(cancel, cfg_b, b, registry_b)
    expect_identical(f("job_read")(narrow_id)$status, "cancelled")

    # The requesting bot cannot approve, even if accidentally listed among
    # operators. Human message approvals work with reactions/threads disabled.
    cfg_b$operators <- c("human", "requester")
    # Use a live queued job for the real approval lifecycle.
    route(request(), cfg_b, b, registry_b)
    approval_job <- f("job_read")(decode()$fields$job)
    req_id <- f("job_approval_request")(approval_job$id,
        list(tool = "write_file", args = list(path = "fixture")), list(reason = "test"))
    req_obj <- f("job_approval_pending")(approval_job$id)[[1]]
    s <- get("jobs", registry_b)
    f("bot_job_ask_person")(b, cfg_b, s, approval_job, req_obj)
    p <- tail(s$.job_prompts, 1)[[1]]
    expect_identical(p$approvers, "human")
    answer <- chat.api::chat_message(id = "answer", channel = "jobs", sender = "requester",
        body = paste("approve", req_id), ts = Sys.time())
    route(answer, cfg_b, b, registry_b)
    expect_length(f("job_approval_pending")(approval_job$id), 1L)
    answer$sender <- "human"
    expect_true(route(answer, cfg_b, b, registry_b))
    expect_length(f("job_approval_pending")(approval_job$id), 0L)

    # Actual child initializer and reused worker loop: apply the delegator's
    # tool intersection for the job, then restore the worker's ordinary tools.
    # The child gets no Matrix credentials and makes no model requests.
    child <- callr::r(function(root, job_id) {
        library(corteza)
        parent <- new_session(provider = "anthropic", system = "fixture")
        parent$cwd <- root
        parent$config <- list()
        parent$model_map$cloud <- "fixture-model"
        spec <- corteza:::job_worker_spec(parent)
        spec$tools <- c("read_file", "write_file")
        state_dir <- file.path(root, "worker-state")
        corteza:::.job_worker_child_init(root, spec, state_dir)
        s <- corteza:::.subagent_state$session
        before <- s$tools_filter
        observed <- NULL
        spec$run_fn <- function(task) {
            handler <- corteza:::.make_tool_handler(s, function(...) stop("disallowed call executed"))
            observed <<- list(tools = s$tools_filter, allowed = s$job_allowed_tools,
                search = corteza:::.session_web_search(s),
                denial = handler("write_file", list(path = "fixture")))
            list(reply = "checked")
        }
        result <- corteza:::.job_worker_child_run(job_id, "fixture", spec, state_dir)
        list(observed = observed, restored = identical(s$tools_filter, before),
             allowed_after = s$job_allowed_tools, reply = result$reply)
    }, list(root = root, job_id = approval_job$id))
    expect_identical(child$observed$tools, "read_file")
    expect_identical(child$observed$allowed, "read_file")
    expect_false(child$observed$search)
    expect_true(grepl("job policy denied", child$observed$denial))
    expect_true(child$restored)
    expect_null(child$allowed_after)
    expect_identical(child$reply, "checked")

    # A review can arrive before its doer's result when one send is delayed.
    # It is retained until the matching doer finishes, then delivered once.
    reviewed <- f(".delegation_submit")(source, cfg_a, "worker", "Review fixture", TRUE)
    pump_a()
    review_request <- last()
    route(review_request, cfg_b, b, registry_b)
    review_doer <- decode()$fields$job
    route(last(), cfg_a, a, registry_a)
    addendum <- chat.api::chat_message(id = "review-first", channel = "jobs", sender = "worker",
        body = f(".delegation_body")("result", list(id = reviewed, job = f("job_new_id")(),
            of = review_doer, status = "done"), "Independent review passed"),
        mentions = "requester", kind = "notice", ts = Sys.time())
    route(addendum, cfg_a, a, registry_a)
    expect_identical(f("job_read")(reviewed)$status, "running")
    expect_identical(f(".delegation_state")(reviewed)$review$text, "Independent review passed")
    completion <- addendum
    completion$id <- "doer-second"
    completion$body <- f(".delegation_body")("result", list(id = reviewed,
        job = review_doer, status = "done"), "Doer finished")
    route(completion, cfg_a, a, registry_a)
    pump_a()
    expect_true(f(".delegation_state")(reviewed)$review_reported)
    route(addendum, cfg_a, a, registry_a)
    n <- length(bus$env$log)
    pump_a()
    expect_equal(length(bus$env$log), n)

    queued_cancel <- f(".delegation_submit")(source, cfg_a, "worker", "Never dispatch")
    expect_true(f("job_cancel")(source, queued_cancel, "human"))
    pump_a()
    expect_identical(f("job_read")(queued_cancel)$status, "cancelled")
    expect_null(f("job_read")(queued_cancel)$dispatch)

    # Crash after dispatch, before delivery: ordinary worker recovery leaves
    # a remote job alone. A fresh pump probes; it never issues another request.
    crash <- f(".delegation_submit")(source, cfg_a, "worker", "Crash fixture")
    f("job_mark_dispatched")(crash, list(bot = "worker"))
    f("job_recover")(owner = "requester")
    expect_identical(f("job_read")(crash)$status, "running")
    before_probe <- length(bus$env$log)
    pump_a(now = as.numeric(Sys.time()))
    protocol <- lapply(bus$env$log[-seq_len(before_probe)], decode)
    for_crash <- Filter(function(p) identical(p$fields$id, crash), protocol)
    expect_identical(vapply(for_crash, function(p) p$kind, ""), "status")
    state <- f(".delegation_state")(crash)
    pump_a(now = state$probe + 121)
    expect_identical(f("job_read")(crash)$status, "indeterminate")

    # Failed delivery remains durable and owner-scoped, including bounded
    # UTF-8 result notices. Repeated flushes do not resend a delivered notice.
    fail_send <- TRUE
    f(".delegation_post")(b, "worker", "requester", "jobs", "result",
        list(id = id, job = worker, status = "done"), paste(rep("\u03b1", 20000), collapse = ""), key = "large-result")
    fail_send <- FALSE
    count <- length(bus$env$log)
    f(".delegation_flush")(a, "requester")
    expect_equal(length(bus$env$log), count)
    f(".delegation_flush")(b, "worker")
    expect_equal(length(bus$env$log), count + 1L)
    expect_true(nchar(last()$body, type = "bytes") < 8000L)
    expect_identical(decode()$fields$truncated, "true")
    f(".delegation_flush")(b, "worker")
    expect_equal(length(bus$env$log), count + 1L)

    # Real tool dispatch still consults policy and the unattended gate before
    # queuing remote work. Dry-run must not create a durable intent.
    gates <- 0L
    source$auto_gate <- function(call, decision) {
        gates <<- gates + 1L
        list(action = "proceed")
    }
    handler <- f(".make_tool_handler")(source, function(...) stop("wrong executor"))
    before <- length(f("job_list")(owner = "requester"))
    expect_true(grepl("remote job", handler("delegate_to", list(bot = "worker", task = "Dispatched through policy"))))
    expect_equal(gates, 1L)
    expect_equal(length(f("job_list")(owner = "requester")), before + 1L)
    source$dry_run <- TRUE
    expect_true(grepl("Dry run", handler("delegate_to", list(bot = "worker", task = "Preview only"))))
    expect_equal(length(f("job_list")(owner = "requester")), before + 1L)
    source$dry_run <- FALSE

    # Fresh processes on both sides recover solely from the durable ledger.
    # Repeat the worker delivery in another process to exercise restart dedup.
    restart_id <- f(".delegation_submit")(source, cfg_a, "worker", "Fresh-process fixture")
    pump_a()
    restart_request <- Filter(function(m) identical(decode(m)$fields$id, restart_id) &&
        identical(decode(m)$kind, "request"), bus$env$log)[[1]]
    worker_process <- function(root, cfg, msg) {
        library(corteza)
        chat <- chat.api::chat_loopback()
        send <- get("chat_send.chat_loopback", asNamespace("chat.api"))
        utils::assignInNamespace("chat_send.chat_loopback", function(client, channel, text, ..., mentions = NULL) {
            id <- send(client, channel, text, ..., identity = list(name = "worker"))
            i <- length(client$env$log)
            client$env$log[[i]]$mentions <- mentions
            id
        }, "chat.api")
        utils::assignInNamespace("bot_get_or_create_session", function(registry, key, cfg, ..., room_id = key) {
            s <- new_session(provider = "anthropic", system = "fixture")
            s$cwd <- root
            s$job_key <- key
            s$job_owner <- cfg$user_id
            s$room_id <- room_id
            s$config <- list()
            assign(key, s, registry)
            s
        }, "corteza")
        registry <- new.env(parent = emptyenv())
        corteza:::.delegation_route(corteza:::bot_msg_record(msg, addressed = TRUE), cfg, chat, registry)
        reply <- utils::tail(chat$env$log, 1)[[1]]
        id <- corteza:::.delegation_parse(reply$body)$fields$job
        corteza:::job_settle(id, "done", result = "Fresh worker result")
        corteza:::.delegation_worker_reply(chat, corteza:::job_read(id))
        list(id = id, result = utils::tail(chat$env$log, 1)[[1]])
    }
    fresh <- callr::r(worker_process, list(root = root, cfg = cfg_b, msg = restart_request))
    again <- callr::r(worker_process, list(root = root, cfg = cfg_b, msg = restart_request))
    expect_identical(again$id, fresh$id)
    settled <- callr::r(function(cfg, msg, id) {
        library(corteza)
        corteza:::.delegation_route(corteza:::bot_msg_record(msg, addressed = TRUE),
            cfg, chat.api::chat_loopback(), new.env(parent = emptyenv()))
        corteza:::job_read(id)
    }, list(cfg = cfg_a, msg = fresh$result, id = restart_id))
    expect_identical(settled$status, "done")
    expect_true(grepl("Fresh worker result", settled$outcome$result))
    expect_identical(f(".delegation_state")(restart_id)$job, fresh$id)

    # bot_poll's actual message loop intercepts protocol notices before the
    # conversation gate and never starts a model turn for them. Re-delivery
    # through a second poll still creates exactly one worker job.
    local({
        incoming <- request()
        saved <- list()
        replace <- function(name, fn, package = "corteza") {
            saved[[length(saved) + 1L]] <<- list(name = name, package = package,
                fn = patch(name, fn, package))
        }
        on.exit(for (entry in rev(saved)) utils::assignInNamespace(entry$name, entry$fn, entry$package), add = TRUE)
        replace("bot_load_config", function(...) cfg_b)
        replace("bot_chat_client", function(...) b)
        replace("chat_whoami.chat_loopback", function(client, ...) chat.api::chat_identity(client$test_id), "chat.api")
        replace("chat_poll", function(...) list(first_run = FALSE, messages = list(incoming),
            reactions = list(), invites = list()), "chat.api")
        replace("turn", function(...) stop("protocol notice reached the model"))
        before <- length(f("job_list")(owner = "worker"))
        expect_equal(corteza::bot_poll(sessions = registry_b), 0L)
        expect_equal(length(f("job_list")(owner = "worker")), before + 1L)
        expect_equal(corteza::bot_poll(sessions = registry_b), 0L)
        expect_equal(length(f("job_list")(owner = "worker")), before + 1L)
    })
})
