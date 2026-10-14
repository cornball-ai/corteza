local({
    event <- function(n, outcome = "ran", text = "", index = 1L,
                       command = "printf hello", result = "hello\nworld") {
        list(outcome = outcome, success = TRUE, turn_number = n,
             call = list(tool = "bash", args = list(command = command),
                         model_context = list(agent_turn = n, call_index = index,
                                              assistant_text = text)), result = result)
    }
    acc <- corteza:::rooms_activity_new()
    obs <- corteza:::rooms_activity_observer(acc)
    obs(event(1L, "start", "Checking the build."))
    before <- corteza:::rooms_activity_frame(acc)
    expect_identical(before$text, "Checking the build.")
    expect_identical(length(acc$events), 0L)
    acc$last_text <- before$text
    acc$last_at <- Sys.time() - 20
    obs(event(1L, text = "Checking the build."))
    expect_true(corteza:::rooms_activity_due(acc))
    obs(event(2L, index = 2L, text = "Checking the build."))
    obs(event(3L, "start", "The build passed. Checking labels."))
    obs(event(3L, text = "The build passed. Checking labels.", result = "labels stable"))
    frame <- corteza:::rooms_activity_frame(acc)
    expect_identical(length(acc$events), 3L)
    expect_identical(sum(vapply(acc$items, function(x) identical(x$type, "commentary"), TRUE)), 2L)
    expect_true(grepl("Bash(printf hello)", frame$text, fixed = TRUE))
    expect_true(grepl("hello\n  world", frame$text, fixed = TRUE))
    expect_true(grepl("</details><p>The build passed", frame$html, fixed = TRUE))
    expect_true(grepl("<pre><code>labels stable</code></pre>", frame$html, fixed = TRUE))
    expect_identical(length(gregexpr("<details>", frame$html, fixed = TRUE)[[1L]]), 2L)

    # Format escaping, control characters, preview length, and Matrix byte cap.
    obs(event(4L, text = "<img src=x>", command = "echo <script>",
              result = paste(rep("\033[31m<secret-looking markup>&", 100L), collapse = "\n")))
    frame <- corteza:::rooms_activity_frame(acc)
    expect_false(grepl("<img", frame$html, fixed = TRUE))
    expect_false(grepl("<script>", frame$html, fixed = TRUE))
    expect_false(grepl("\033", frame$text, fixed = TRUE))
    expect_true(grepl("&lt;script&gt;", frame$html, fixed = TRUE))
    expect_true(grepl("...", tail(acc$events, 1L)[[1L]]$output, fixed = TRUE))
    for (i in 5:100) obs(event(i, text = paste(rep("<&", 300L), collapse = "")))
    frame <- corteza:::rooms_activity_frame(acc)
    expect_true(nchar(frame$text, type = "bytes") + nchar(frame$html, type = "bytes") < 40000L)
    content <- list(body = frame$text, formatted_body = frame$html)
    replacement <- c(content, list(m.new_content = content))
    expect_true(nchar(jsonlite::toJSON(replacement, auto_unbox = TRUE), type = "bytes") < 60000L)
    expect_true(grepl("Earlier activity omitted", frame$text, fixed = TRUE))

    # Same batch, new result: the detail changed even when the count summary did not.
    expect_false(corteza:::rooms_activity_flush(acc, NULL, "room", send = function(...) NULL))
    expect_null(acc$message_id)
})

# Direct turns use the real observer chain and actual transport renderer.
# Intermediate narration is visible before a tool completes; thread routing and
# observer cleanup survive both normal completion and a provider failure.
if (requireNamespace("chat.api", quietly = TRUE)) local({
    cl <- chat.api::chat_loopback()
    s <- corteza::new_session("matrix")
    s$on_tool <- list()
    got <- corteza:::rooms_with_activity(s, cl, "room", function() {
        call <- list(tool = "bash", args = list(command = "printf hello"),
                     model_context = list(assistant_text = "Checking it now.", call_index = 1L))
        corteza:::.fire_observers(s, list(call = call, outcome = "start", turn_number = 1L))
        msgs <- chat.api::chat_history(cl, "room")$messages
        expect_identical(length(msgs), 1L)
        expect_identical(msgs[[1L]]$body, "Checking it now.")
        expect_identical(msgs[[1L]]$thread, "root")
        corteza:::.fire_observers(s, list(call = call, outcome = "ran", turn_number = 1L,
                                         success = TRUE, result = "hello"))
        "done"
    }, cfg = list(activity_interval = 0), thread = "root")
    expect_identical(got, "done")
    expect_identical(length(s$on_tool), 0L)
    msgs <- chat.api::chat_history(cl, "room")$messages
    expect_true(any(vapply(msgs, function(m) grepl("Bash(printf hello)", m$body, fixed = TRUE), TRUE)))
    expect_true(any(vapply(msgs, function(m) grepl("hello", m$body, fixed = TRUE), TRUE)))
})

# Exercise the actual callr child init/run, atomic progress files, parent pump,
# and room renderer. No model/network, home files, or shared job state.
if (requireNamespace("chat.api", quietly = TRUE)) local({
    root <- tempfile("room-progress-")
    dir.create(root)
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = root)
    s <- corteza::new_session("matrix")
    on.exit({
        corteza:::job_worker_close(s)
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    s$cwd <- root
    s$config <- list()
    s$job_owner <- "fixture-bot"
    s$job_key <- s$room_id <- "room"
    s$job_worker_spec <- list(
        init_fn = function(spec) {
            assign("session", corteza::new_session("matrix"), corteza:::.subagent_state)
        },
        run_fn = function(task) {
            session <- corteza:::.subagent_state$session
            call <- list(tool = "bash", args = list(command = "printf worker"),
                         model_context = list(assistant_text = "Worker checking labels.", call_index = 1L))
            corteza:::.fire_observers(session, list(call = call, outcome = "start", turn_number = 1L))
            Sys.sleep(0.6)
            corteza:::.fire_observers(session, list(call = call, outcome = "ran", turn_number = 1L,
                                                   success = TRUE, result = "stable labels"))
            list(reply = "Finished.")
        })
    id <- corteza:::job_submit(s, "check labels", origin = list(room = "room", thread = "root"))
    cl <- chat.api::chat_loopback()
    seen <- list()
    deadline <- Sys.time() + 30
    repeat {
        evs <- corteza:::job_pump(s)
        seen <- c(seen, evs)
        for (ev in evs) corteza:::bot_present_job_event(cl, list(activity_interval = 0), s, ev)
        if (any(vapply(seen, function(e) identical(e$type, "settled"), TRUE))) break
        if (Sys.time() > deadline) stop("worker did not finish")
        Sys.sleep(0.05)
    }
    types <- vapply(seen, function(e) e$type, "")
    expect_true("progress" %in% types)
    expect_true(max(which(types == "progress")) < which(types == "settled"))
    records <- unlist(lapply(seen[types == "progress"], function(e) e$records), recursive = FALSE)
    expect_identical(length(records), 2L)
    expect_identical(records[[2L]]$tool$output, "stable labels")
    expect_identical(corteza:::job_read(id)$status, "done")
    expect_identical(length(s$.job_activity), 0L)
    msgs <- chat.api::chat_history(cl, "room")$messages
    notices <- Filter(function(m) identical(m$kind, "notice"), msgs)
    expect_true(length(notices) > 0L)
    expect_true(all(vapply(notices, function(m) identical(m$thread, "root"), TRUE)))
    expect_true(any(vapply(notices, function(m) grepl("stable labels", m$body, fixed = TRUE), TRUE)))
    expect_true(any(vapply(notices, function(m) grepl("Worker checking labels.", m$body, fixed = TRUE), TRUE)))
    # Another owner sharing the room cannot consume this job's records.
    other <- corteza::new_session("matrix")
    other$job_key <- "room"
    other$job_owner <- "different-bot"
    expect_identical(corteza:::job_progress_read(other, id), list())

    # A persistent worker's second job gets fresh sequence numbers and one
    # observer. Nothing from the first job is replayed into its activity notice.
    id2 <- corteza:::job_submit(s, "check again", origin = list(room = "room", thread = "root"))
    next_events <- list()
    deadline <- Sys.time() + 30
    repeat {
        next_events <- c(next_events, corteza:::job_pump(s))
        if (corteza:::job_read(id2)$status %in% corteza:::JOB_STATUSES_FINAL) break
        if (Sys.time() > deadline) stop("second job did not finish")
        Sys.sleep(0.05)
    }
    progress <- Filter(function(e) identical(e$type, "progress"), next_events)
    expect_identical(sum(vapply(progress, function(e) length(e$records), 1L)), 2L)
    expect_true(all(vapply(progress, function(e) identical(e$job$id, id2), TRUE)))
    expect_identical(corteza:::job_read(id2)$status, "done")
})

# A cosmetic send/edit failure neither discards the current preview nor stops
# the turn. Transports without edits get one complete message after an error.
if (requireNamespace("chat.api", quietly = TRUE)) local({
    s <- corteza::new_session("matrix")
    cl <- chat.api::chat_loopback()
    original <- corteza:::rooms_activity_live
    utils::assignInNamespace("rooms_activity_live", function(chat) FALSE, "corteza")
    on.exit(utils::assignInNamespace("rooms_activity_live", original, "corteza"), add = TRUE)
    expect_error(corteza:::rooms_with_activity(s, cl, "room", function() {
        corteza:::.fire_observers(s, list(outcome = "deny", success = FALSE,
            call = list(tool = "bash", args = list(command = "restricted command"),
                model_context = list(assistant_text = "Checking permission.", call_index = 1L)),
            result = "Permission required"))
        expect_identical(length(chat.api::chat_history(cl, "room")$messages), 0L)
        stop("provider failed")
    }), "provider failed")
    messages <- chat.api::chat_history(cl, "room")$messages
    expect_identical(length(messages), 1L)
    expect_true(grepl("Checking permission.", messages[[1L]]$body, fixed = TRUE))
    expect_true(grepl("refused", messages[[1L]]$body, fixed = TRUE))
    expect_true(grepl("Permission required", messages[[1L]]$body, fixed = TRUE))
    expect_identical(length(s$on_tool), 0L)
})
