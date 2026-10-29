local({
    ns <- asNamespace("corteza")
    root <- tempfile("resume-jobs-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = file.path(root, "state"))
    s <- corteza::new_session("matrix", provider = "anthropic", system = "fixture")
    s$cwd <- root
    s$config <- list(context_edit = list(enabled = FALSE))
    s$job_key <- s$room_id <- "fixture-room"
    s$job_owner <- "fixture-bot"
    s$job_worker_spec <- list(init_fn = function(spec) {
        Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
        corteza::subagent_turn_init(provider = "anthropic", model = "fixture",
            tools_filter = "run_r", system = "fixture", max_turns = 1L, web_search = FALSE)
        session <- corteza:::.subagent_state$session
        session$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = 100,
            permissions = list(run_r = "allow"))
        utils::assignInNamespace(".agent_anthropic", function(messages, ...) {
            cat("request\n", file = "provider-count.txt", append = TRUE)
            last <- corteza:::message_text(tail(messages, 1L)[[1L]])
            continuing <- startsWith(last, "Continue the incomplete job")
            other <- identical(last, "other task")
            if (continuing) {
                text <- paste(vapply(messages, corteza:::message_text, ""), collapse = "\n")
                stopifnot(!grepl("other task", text, fixed = TRUE),
                    identical(get("counter", globalenv()), 1L),
                    !exists("unrelated", globalenv(), inherits = FALSE),
                    !exists("live_connection", globalenv(), inherits = FALSE),
                    identical(readLines("changed.txt"), "newer checkout"),
                    grepl("live_connection", text, fixed = TRUE))
            }
            if (other) {
                assign("counter", 99L, globalenv())
                assign("unrelated", TRUE, globalenv())
            }
            call <- list(id = "completed-call", name = "run_r", arguments = list(code = paste(
                'counter <- 1L; live_connection <- file("connection.txt", "w");',
                'cat("executed once\\n", file = "tool-count.txt", append = TRUE); cat("completed tool")')))
            more <- continuing && file.exists("another-cutoff")
            if (more) call <- list(id = "continuation-step", name = "run_r",
                arguments = list(code = 'cat("continued work")'))
            tools <- if ((continuing && !more) || other) list() else list(call)
            list(text = "fixture result", tool_calls = tools,
                assistant_message = list(role = "assistant", content = if (length(tools)) {
                    list(list(type = "tool_use", id = call$id, name = call$name, input = call$arguments))
                } else list(list(type = "text", text = "fixture result"))),
                usage = list(input_tokens = 2L, output_tokens = 3L))
        }, "llm.api")
    }, tools = "run_r", max_turns = 1L)
    on.exit({
        ns$job_worker_close_all(s)
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    pump <- function(id) {
        deadline <- Sys.time() + 40
        repeat {
            ns$job_pump(s)
            j <- ns$job_read(id)
            if (j$status %in% ns$JOB_STATUSES_FINAL) return(j)
            if (Sys.time() > deadline) stop("resume fixture timed out")
            Sys.sleep(0.05)
        }
    }
    id <- ns$job_submit(s, "first task", requester = "requester",
        origin = list(room = s$room_id))
    first <- pump(id)
    expect_identical(first$status, "incomplete")
    expect_true(first$outcome$resume$ready)
    history <- readRDS(file.path(ns$job_resume_path(id), "history.rds"))
    expect_true(grepl("completed tool", history[[3L]]$content[[1L]]$content, fixed = TRUE))
    expect_true("live_connection" %in% unlist(first$outcome$resume$omitted))
    snapshot <- ns$job_resume_snapshot(id, ns$job_worker_identity(s))
    hashes <- snapshot$hashes
    if (.Platform$OS.type != "windows") {
        expect_equal(file.mode(ns$job_resume_path(id)), as.octmode("0700"))
        expect_equal(file.mode(file.path(ns$job_resume_path(id), "history.rds")), as.octmode("0600"))
    }
    completed <- pump(ns$job_submit(s, "other task", requester = "requester"))
    expect_identical(completed$status, "done")
    expect_identical(ns$job_resume_snapshot(id, ns$job_worker_identity(s))$hashes, hashes)
    writeLines("newer checkout", file.path(root, "changed.txt"))
    ns$job_worker_close_all(s)
    expect_error(ns$job_resume(s, id, "bystander"), "original requester")
    foreign <- new.env(parent = emptyenv())
    foreign$cwd <- root
    foreign$job_key <- s$job_key
    foreign$job_owner <- "other-bot"
    expect_error(ns$job_resume(foreign, id, "requester"), "conversation")
    foreign$job_owner <- s$job_owner
    foreign$job_key <- paste0(s$job_key, "::thread")
    expect_error(ns$job_resume(foreign, id, "requester"), "conversation")
    s$plan_mode <- TRUE
    expect_error(ns$job_resume(s, id, "requester"), "plan mode")
    s$plan_mode <- FALSE
    second_id <- ns$job_resume(s, id, "operator", operators = "operator", event = "$resume")
    expect_identical(ns$job_resume(s, id, "requester", event = "$duplicate"), second_id)
    second <- pump(second_id)
    expect_identical(second$status, "done")
    expect_identical(second$resume_of, id)
    expect_equal(second$outcome$usage$total_tokens, 5)
    expect_equal(second$outcome$cumulative_usage$total_tokens, 10)
    expect_identical(second$requester, "requester")
    expect_identical(ns$job_read(id)$outcome, first$outcome)
    expect_identical(ns$job_resume(s, id, "requester", event = "$resume"), second_id)
    expect_false(file.exists(file.path(ns$job_dir(second_id), "approval-mode.json")))
    expect_identical(readLines(file.path(root, "changed.txt")), "newer checkout")
    expect_identical(readLines(file.path(root, "tool-count.txt")), "executed once")
    expect_error(ns$job_resume(s, completed$id, "requester"), "incomplete")

    # An explicit continuation can stop again. It gets its own snapshot and
    # only another explicit request creates the third attempt in the chain.
    next_first <- pump(ns$job_submit(s, "first task", requester = "requester"))
    writeLines("stop again", file.path(root, "another-cutoff"))
    next_second <- pump(ns$job_resume(s, next_first$id, "requester"))
    expect_identical(next_second$status, "incomplete")
    expect_true(next_second$outcome$resume$ready)
    expect_null(s$.job_current)
    expect_equal(next_second$outcome$cumulative_usage$total_tokens, 10)
    unlink(file.path(root, "another-cutoff"))
    next_third <- pump(ns$job_resume(s, next_second$id, "requester"))
    expect_identical(next_third$status, "done")
    expect_equal(next_third$outcome$cumulative_usage$total_tokens, 15)
    expect_length(readLines(file.path(root, "tool-count.txt")), 2L)

    # A later stopped attempt remains independent; corrupted bytes, missing
    # files, and an uncommitted staging directory cannot become resume points.
    third <- pump(ns$job_submit(s, "first task", requester = "requester"))
    path <- ns$job_resume_path(third$id)
    info <- ns$job_resume_snapshot(third$id, ns$job_worker_identity(s))
    expect_error(ns$job_resume_snapshot(third$id, list(owner = "wrong")), "different worker")
    writeLines("corrupt", file.path(path, "history.rds"))
    expect_error(ns$job_resume(s, third$id, "requester"), "integrity")
    unlink(file.path(path, "workspace.RData"))
    expect_error(ns$job_resume_snapshot(third$id, ns$job_worker_identity(s)), "integrity")
    fourth_id <- ns$job_create("old incomplete record", owner = s$job_owner,
        workspace = root, requester = "requester", origin = list(session_key = s$job_key))
    ns$job_settle(fourth_id, "incomplete")
    dir.create(file.path(ns$job_dir(fourth_id), ".resume-stray"))
    expect_error(ns$job_resume(s, fourth_id, "requester"), "no usable")

    # A journal refusal must happen before the resumed provider request and
    # must not replace the shared checkpoint with the empty fresh worker.
    fifth <- pump(ns$job_submit(s, "first task", requester = "requester"))
    ns$job_worker_close_all(s)
    shared <- ns$job_worker_state_dir(ns$job_worker_identity(s))
    shared_files <- file.path(shared, c("workspace.RData", "history.rds", "checkpoint.json"))
    before <- unname(tools::md5sum(shared_files))
    requests_before <- length(readLines(file.path(root, "provider-count.txt")))
    s$job_worker_spec$init_fn <- local({
        initialize <- s$job_worker_spec$init_fn
        function(spec) {
            initialize(spec)
            utils::assignInNamespace("journal_rewrite", function(...) stop("fixture journal refusal"), "corteza")
        }
    })
    refused <- pump(ns$job_resume(s, fifth$id, "requester"))
    expect_identical(refused$status, "failed")
    expect_true(grepl("fixture journal refusal", refused$outcome$error, fixed = TRUE))
    expect_identical(length(readLines(file.path(root, "provider-count.txt"))), requests_before)
    expect_identical(unname(tools::md5sum(shared_files)), before)
    expect_null(ns$job_lock_holder(ns$job_checkout(root)))
    expect_null(ns$bot_parse_job_resume("please resume"))
    expect_identical(ns$bot_parse_job_resume(paste("/job resume", id))$id, id)
    expect_true(!is.null(ns$bot_parse_job_resume("/job")$error))
    corteza::ensure_skills()
    tools <- ns$skills_as_api_tools("job_resume")
    expect_length(tools, 1L)
    expect_true("id" %in% unlist(tools[[1L]]$input_schema$required))
})
