# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# A conversation's lifecycle as the job system sees it
# (R/job-conversation.R; WP20a, rules 2 to 5). A /clear moves the doer's
# checkpoint to the new conversation, which starts with the R objects and
# no history; a warm worker builds its session again for a job from
# another conversation; a job in flight finishes where it started.

ns <- asNamespace("corteza")

identity_for <- function(dir, key) {
    list(owner = "local:fixture", key = key, workspace = dir, role = "doer")
}

# A checkpoint as a worker leaves one: x = 42, and a history of one
# message unless `history` is FALSE.
put_checkpoint <- function(dir, identity, conversation, history = TRUE) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    env <- new.env()
    env$x <- 42
    objects <- ns$.workspace_checkpoint_write(env,
                                              file.path(dir, "workspace.RData"))
    if (history) {
        saveRDS(list(list(role = "user", content = "earlier")),
                file.path(dir, "history.rds"))
    }
    ns$job_write_file(file.path(dir, "checkpoint.json"),
                      list(identity = identity, job = "j1", at = "then",
                           objects = objects, packages = character(),
                           conversation = conversation))
}

# ---- A checkpoint's conversation decides its history ----------------------
local({
    root <- tempfile("conversation-restore-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- corteza::new_session("cli", provider = "anthropic")
    state <- ns$.subagent_state
    had <- exists("session", envir = state, inherits = FALSE)
    old <- if (had) get("session", envir = state)
    assign("session", s, envir = state)
    on.exit(if (had) assign("session", old, envir = state) else
            rm(list = "session", envir = state), add = TRUE)
    id <- identity_for(root, "repl:a")
    restore <- function(dir, conversation) {
        s$history <- list()
        env <- new.env()
        out <- ns$.job_worker_child_restore(dir, id, workspace = env,
                                            conversation = conversation)
        list(out = out, x = env$x, history = length(s$history))
    }
    put_checkpoint(file.path(root, "a"), id, "conv-a")
    # Its own conversation: the objects and the history.
    got <- restore(file.path(root, "a"), "conv-a")
    expect_identical(got$x, 42)
    expect_identical(got$history, 1L)
    expect_true(got$out$history)
    # Another conversation: the objects, and no history.
    got <- restore(file.path(root, "a"), "conv-b")
    expect_identical(got$x, 42)
    expect_identical(got$history, 0L)
    expect_false(got$out$history)
    # A checkpoint from before checkpoints named their conversation: a
    # room's could hold a history a /clear ended, so a conversation takes
    # its objects only. A session with no conversation of its own takes
    # its history, as before.
    put_checkpoint(file.path(root, "old"), id, NULL)
    got <- restore(file.path(root, "old"), "conv-b")
    expect_identical(got$x, 42)
    expect_identical(got$history, 0L)
    got <- restore(file.path(root, "old"), NULL)
    expect_identical(got$history, 1L)
})

# ---- A starting checkpoint copied from the last conversation's ------------
local({
    root <- tempfile("conversation-copy-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    from <- file.path(root, "a")
    to <- file.path(root, "b")
    put_checkpoint(from, identity_for(root, "repl:a"), "conv-a")
    new_id <- identity_for(root, "repl:b")
    expect_true(ns$job_checkpoint_copy(from, to, new_id, "conv-b"))
    info <- ns$job_read_file(file.path(to, "checkpoint.json"))
    expect_identical(info$conversation, "conv-b")
    expect_identical(info$identity$key, "repl:b")
    expect_null(info$job)
    expect_false(file.exists(file.path(to, "history.rds")))
    expect_identical(unname(tools::md5sum(file.path(to, "workspace.RData"))),
                     unname(tools::md5sum(file.path(from, "workspace.RData"))))
    expect_identical(sort(list.files(to)), c("checkpoint.json",
                                             "workspace.RData"))
    # A copy, never a reference: the old one changing leaves it alone.
    put_checkpoint(from, identity_for(root, "repl:a"), "conv-a")
    writeLines("changed", file.path(from, "workspace.RData"))
    expect_false(identical(unname(tools::md5sum(file.path(to, "workspace.RData"))),
                           unname(tools::md5sum(file.path(from, "workspace.RData")))))
    # Nothing to copy from: nothing written.
    expect_false(ns$job_checkpoint_copy(file.path(root, "none"),
                                        file.path(root, "c"), new_id, "conv-c"))
    expect_false(dir.exists(file.path(root, "c")))
    # A worker's starting checkpoint names its conversation and leaves no
    # history file, even where one was. (This process's own global
    # environment is what it saves.)
    put_checkpoint(file.path(root, "d"), identity_for(root, "repl:d"), "conv-d")
    ns$.job_worker_child_start_checkpoint(file.path(root, "d"),
                                          identity_for(root, "repl:d"), "conv-e")
    info <- ns$job_read_file(file.path(root, "d", "checkpoint.json"))
    expect_identical(info$conversation, "conv-e")
    expect_null(info$job)
    expect_false(file.exists(file.path(root, "d", "history.rds")))
})

# ---- /clear with no worker alive -------------------------------------------
local({
    root <- normalizePath(tempfile("conversation-clear-"), mustWork = FALSE)
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- corteza::new_session("cli", provider = "anthropic")
    s$cwd <- root
    s$job_checkpoint_owner <- "local:fixture"
    s$job_checkpoint_key <- "repl:conv-a"
    s$sessionId <- "conv-a"
    dir_of <- function(key) {
        ns$job_worker_state_dir(identity_for(ns$job_worker_dir(s), key))
    }
    put_checkpoint(dir_of("repl:conv-a"),
                   identity_for(ns$job_worker_dir(s), "repl:conv-a"), "conv-a")
    # The last checkpoint is copied, under the new conversation.
    s$sessionId <- "conv-b"
    ns$job_conversation_clear(s, "conv-b", checkpoint_key = "repl:conv-b")
    expect_identical(s$job_checkpoint_key, "repl:conv-b")
    info <- ns$job_read_file(file.path(dir_of("repl:conv-b"), "checkpoint.json"))
    expect_identical(info$conversation, "conv-b")
    expect_false(file.exists(file.path(dir_of("repl:conv-b"), "history.rds")))
    expect_identical(length(s$.job_starts), 0L)
    # A job in flight: written when it settles, each in order, so a
    # second clear takes the first one's.
    s$.job_current <- "in-flight"
    s$sessionId <- "conv-c"
    ns$job_conversation_clear(s, "conv-c", checkpoint_key = "repl:conv-c")
    s$sessionId <- "conv-d"
    ns$job_conversation_clear(s, "conv-d", checkpoint_key = "repl:conv-d")
    expect_identical(length(s$.job_starts), 2L)
    expect_false(dir.exists(dir_of("repl:conv-c")))
    s$.job_current <- NULL
    ns$job_start_checkpoints(s)
    expect_identical(length(s$.job_starts), 0L)
    for (key in c("conv-c", "conv-d")) {
        info <- ns$job_read_file(file.path(dir_of(paste0("repl:", key)),
                                           "checkpoint.json"))
        expect_identical(info$conversation, key)
    }
    # A surface that keeps one checkpoint for all its conversations (a
    # room) passes no key: nothing is copied.
    before <- list.files(dirname(dir_of("repl:conv-a")))
    stamp <- tools::md5sum(file.path(dir_of("repl:conv-d"), "checkpoint.json"))
    s$sessionId <- "conv-e"
    ns$job_conversation_clear(s, "conv-e")
    expect_identical(list.files(dirname(dir_of("repl:conv-a"))), before)
    expect_identical(length(s$.job_starts), 0L)
    expect_identical(tools::md5sum(file.path(dir_of("repl:conv-d"),
                                             "checkpoint.json")), stamp)
})

# ---- A queued job keeps what its conversation allowed -----------------------
# Codex's finding on #281: a job queued in one conversation started after
# /clear under the next one's settings. job_conversation_hold() keeps,
# for each job the ending conversation queued, the spec and authority it
# gives now; job_dispatch() starts the job under them, and refuses one
# from an ended conversation that nothing was kept for.
local({
    root <- normalizePath(tempfile("conversation-hold-"), mustWork = FALSE)
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- corteza::new_session("cli", provider = "anthropic")
    s$cwd <- root
    s$job_key <- "hold-fixture"
    s$job_checkpoint_owner <- "local:fixture"
    s$job_checkpoint_key <- "repl:conv-a"
    s$sessionId <- "conv-a"
    s$config <- list(approval_mode = "ask")
    queue <- function(conversation) {
        ns$job_create("fixture task", workspace = root,
                      origin = list(session_key = ns$job_worker_key(s),
                                    conversation = conversation),
                      owner = ns$job_worker_owner(s))
    }
    mine <- queue("conv-a")
    other <- queue("conv-x")
    ended <- queue("conv-a")
    ns$job_settle(ended, "done", result = "r")
    # Plan mode turned on after the job was queued: the hold takes it.
    s$plan_mode <- TRUE
    expect_identical(ns$job_conversation_hold(s), 1L)
    held <- s$.job_held[[mine]]
    expect_identical(held$conversation, "conv-a")
    expect_true(isTRUE(held$spec$plan_mode))
    expect_identical(held$spec$conversation, "conv-a")
    expect_identical(held$spec$identity$key, "repl:conv-a")
    expect_true(isTRUE(held$authority$plan_mode))
    expect_identical(held$authority$approval_mode, "ask")
    expect_null(s$.job_held[[other]])
    expect_null(s$.job_held[[ended]])
    # The next conversation holds nothing of the last one's again.
    s$sessionId <- "conv-b"
    s$job_checkpoint_key <- "repl:conv-b"
    s$plan_mode <- FALSE
    expect_identical(ns$job_conversation_hold(s), 0L)
    expect_true(isTRUE(s$.job_held[[mine]]$spec$plan_mode))
    # A held job's record is written from what was held, and the next
    # conversation's pump does not write it again.
    ns$job_authority_write(s, mine, values = held$authority,
                           conversation = "conv-a")
    expect_true(isTRUE(ns$job_read_file(ns$job_authority_path(mine))$plan_mode))
    s$.job_current <- mine
    expect_false(ns$job_authority_sync(s))
    expect_true(isTRUE(ns$job_read_file(ns$job_authority_path(mine))$plan_mode))
    s$.job_current <- NULL
    # A job from an ended conversation that nothing was held for does
    # not run, and says why.
    invisible(ns$job_dispatch(s, ns$job_read(other), NULL))
    out <- ns$job_read(other)
    expect_identical(out$status, "failed")
    expect_true(grepl("ended before it started", out$outcome$error,
                      fixed = TRUE))
    expect_false(file.exists(ns$job_authority_path(other)))
    # What is held moves with the jobs to a session that replaces this
    # one (a room's reset).
    moved <- new.env(parent = emptyenv())
    ns$job_state_move(s, moved)
    expect_true(isTRUE(moved$.job_held[[mine]]$spec$plan_mode))
    expect_null(s$.job_held)
    ns$job_state_move(moved, s)
    # A held job that is no longer queued is let go at the next hold.
    ns$job_settle(mine, "cancelled", reason = "fixture")
    ns$job_conversation_hold(s)
    expect_null(s$.job_held[[mine]])
})

# The terminal's /clear and a room's reach job_conversation_clear(), each
# once: test_room_conversation.R.

# ---- A result that settles after /clear ------------------------------------
# It goes to the conversation that asked for it, and the new one's talker
# does not take it.
local({
    root <- tempfile("conversation-result-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    id <- ns$job_create("fixture task", workspace = root,
                        origin = list(session_key = "repl",
                                      conversation = "conv-a"))
    ns$job_settle(id, "done", result = "RESULT-TEXT")
    ctx <- new.env(parent = emptyenv())
    ctx$palette <- list(dim = "", reset = "", cyan = "")
    ctx$session <- new.env(parent = emptyenv())
    ctx$session$history <- list()
    ctx$session$cwd <- root
    ctx$session$sessionId <- "conv-b"
    out <- capture.output(ns$.repl_present_job_event(ctx,
        list(type = "settled", job = ns$job_read(id))))
    expect_true(any(grepl("RESULT-TEXT", out, fixed = TRUE)))
    expect_true(any(grepl("asked in session conv-a, before /clear", out,
                          fixed = TRUE)))
    expect_identical(length(ctx$session$history), 0L)
    asked <- ns$transcript_load("conv-a", from_compaction = FALSE)
    last <- ""
    if (length(asked)) {
        last <- asked[[length(asked)]]$content[[1L]]$text
    }
    expect_true(grepl("RESULT-TEXT", last, fixed = TRUE))
    # The conversation's own job, and one from before jobs named their
    # conversation: into the talker's history, as before.
    ctx$session$sessionId <- "conv-a"
    invisible(capture.output(ns$.repl_present_job_event(ctx,
        list(type = "settled", job = ns$job_read(id)))))
    expect_identical(length(ctx$session$history), 1L)
    old <- ns$job_create("older task", workspace = root,
                         origin = list(session_key = "repl"))
    ns$job_settle(old, "done", result = "OLDER")
    ctx$session$sessionId <- "conv-b"
    invisible(capture.output(ns$.repl_present_job_event(ctx,
        list(type = "settled", job = ns$job_read(old)))))
    expect_identical(length(ctx$session$history), 2L)
})

# ---- What a job spent, when it settles after /clear -----------------------
# Codex's case on #296: conv-a spent 100 tokens, a /clear, conv-b 300, and
# conv-a's job settles with 700. /spent says conv-a 800 and conv-b 300.
local({
    root <- tempfile("conversation-spend-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    settled <- function(conversation, tokens) {
        id <- ns$job_create("fixture task", workspace = root,
                            origin = list(session_key = "repl",
                                          conversation = conversation))
        ns$job_settle(id, "done", result = "Done",
                      usage = list(input_tokens = tokens))
        list(type = "settled", job = ns$job_read(id))
    }
    s <- new.env(parent = emptyenv())
    s$cwd <- root
    ctx <- new.env(parent = emptyenv())
    ctx$palette <- list(dim = "", reset = "", cyan = "")
    ctx$session <- s
    tokens <- function() {
        vapply(s$spend$segments, function(g) g$input_tokens, numeric(1))
    }
    ids <- function() vapply(s$spend$segments, function(g) g$id, "")
    s$sessionId <- "conv-a"
    ns$session_accumulate_spend(s, list(input_tokens = 100L))
    ns$spend_open_segment(s)
    s$sessionId <- "conv-b"
    # Before conv-b spends: conv-a's segment takes it, and conv-b's opens
    # with its own first turn.
    invisible(capture.output(ns$.repl_present_job_event(ctx,
        settled("conv-a", 50L))))
    expect_identical(ids(), "conv-a")
    expect_equal(tokens(), 150)
    ns$session_accumulate_spend(s, list(input_tokens = 300L))
    expect_identical(ids(), c("conv-a", "conv-b"))
    invisible(capture.output(ns$.repl_present_job_event(ctx,
        settled("conv-a", 650L))))
    expect_equal(tokens(), c(800, 300))
    expect_identical(s$spend$segments[[1L]]$turns, 3L)
    expect_identical(s$spend$segments[[2L]]$turns, 1L)
    # The open conversation's own job is its own.
    invisible(capture.output(ns$.repl_present_job_event(ctx,
        settled("conv-b", 20L))))
    expect_equal(tokens(), c(800, 320))
    # A conversation that never spent gets a segment before the open one,
    # which stays last, and /spent still marks it current.
    invisible(capture.output(ns$.repl_present_job_event(ctx,
        settled("conv-z", 5L))))
    expect_identical(ids(), c("conv-a", "conv-z", "conv-b"))
    expect_equal(tokens(), c(800, 5, 320))
    report <- strsplit(ns$format_spend(s), "\n", fixed = TRUE)[[1L]]
    expect_true(grepl("conv-b", report[grepl("current", report)]))
    ns$session_accumulate_spend(s, list(input_tokens = 1L))
    expect_equal(tokens(), c(800, 5, 321))
})

# ---- A warm worker across /clear --------------------------------------------
# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}
# The worker's model is a script: the task names the calls to make (`r
# <code>` runs R, `write <file> <gate>` waits for the file <gate>, then
# writes). Its last reply says how many tasks its history held, whether
# its prompt carried the project's instructions and its skill, and the
# tool results it saw. The worker's prompt is built as a real one is
# (job_worker_context()), so trust decides what it holds.
local({
    root <- tempfile("conversation-jobs-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    vars <- c("CORTEZA_STATE_DIR", "R_USER_CONFIG_DIR")
    old <- Sys.getenv(vars, unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = file.path(root, "state"),
               R_USER_CONFIG_DIR = file.path(root, "config"))
    on.exit({
        for (v in vars) {
            if (is.na(old[[v]])) {
                Sys.unsetenv(v)
            } else {
                do.call(Sys.setenv, stats::setNames(list(old[[v]]), v))
            }
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    proj <- file.path(root, "proj")
    dir.create(proj)
    writeLines("PROJECT-RULES-MARKER work this way", file.path(proj, "AGENTS.md"))
    skill <- file.path(ns$project_skills_dir(proj), "fixture", "SKILL.md")
    dir.create(dirname(skill), recursive = TRUE)
    writeLines(c("---", "name: fixture", "description: FIXTURE-SKILL does a thing",
                 "---", "Body."), skill)
    s <- corteza::new_session("cli", provider = "anthropic", web_search = FALSE,
                              model_map = list(cloud = "model-a", local = NULL))
    s$cwd <- proj
    s$config <- list(approval_mode = "allow", jobs = list(max_turns = 8L))
    s$job_key <- "conversation-fixture"
    s$job_checkpoint_owner <- "local:fixture"
    s$job_worker_spec <- list(
        tools = c("read_file", "write_file", "run_r", "skill_instructions"),
        init_fn = function(spec) {
            Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
            ns <- asNamespace("corteza")
            system <- ns$job_worker_context(spec$system, scope = "full")
            corteza::subagent_turn_init(provider = "anthropic", model = "fixture",
                tools_filter = spec$tools, system = system, max_turns = 8L,
                web_search = FALSE)
            session <- ns$.subagent_state$session
            session$config <- list(context_edit = list(enabled = FALSE),
                                   context_compact_pct = 100)
            utils::assignInNamespace(".agent_anthropic", function(messages, tools,
                                                                  system, model, ...) {
                text_of <- function(m) {
                    b <- m$content
                    if (is.character(b)) return(paste(b, collapse = " "))
                    paste(unlist(lapply(b, function(x) {
                        inner <- x$content
                        if (is.list(inner)) {
                            inner <- vapply(inner, function(y) {
                                if (is.null(y$text)) "" else y$text
                            }, "")
                        }
                        c(x$text, inner)
                    })), collapse = " ")
                }
                is_task <- vapply(messages, function(m) {
                    identical(m$role, "user") &&
                        grepl("script:", text_of(m), fixed = TRUE)
                }, logical(1))
                at <- max(which(is_task))
                task <- text_of(messages[[at]])
                script <- sub("^script: *", "",
                              regmatches(task, regexpr("script:[^\n]*", task)))
                steps <- strsplit(script, " ; ", fixed = TRUE)[[1L]]
                steps <- steps[nzchar(steps)]
                after <- messages[-seq_len(at)]
                step <- length(after) %/% 2L + 1L
                usage <- list(input_tokens = 1L, output_tokens = 1L)
                if (step > length(steps)) {
                    users <- Filter(function(m) identical(m$role, "user"), after)
                    seen <- paste(vapply(users, text_of, ""), collapse = " | ")
                    prompt <- paste(unlist(system), collapse = " ")
                    text <- sprintf("tasks=%d rules=%s skill=%s results=%s",
                                    sum(is_task),
                                    grepl("PROJECT-RULES-MARKER", prompt, fixed = TRUE),
                                    grepl("FIXTURE-SKILL", prompt, fixed = TRUE),
                                    seen)
                    return(list(text = text, tool_calls = list(), usage = usage,
                                assistant_message = list(role = "assistant",
                                    content = list(list(type = "text", text = text)))))
                }
                this <- steps[[step]]
                kind <- sub(" .*$", "", this)
                rest <- sub("^[^ ]+ ?", "", this)
                if (identical(kind, "write")) {
                    words <- strsplit(rest, " ", fixed = TRUE)[[1L]]
                    if (length(words) >= 2L) {
                        writeLines("waiting", paste0(words[[2L]], ".waiting"))
                        deadline <- Sys.time() + 30
                        while (!file.exists(words[[2L]]) && Sys.time() < deadline) {
                            Sys.sleep(0.05)
                        }
                    }
                    name <- "write_file"
                    args <- list(path = file.path(getwd(), words[[1L]]),
                                 content = "x")
                } else {
                    name <- "run_r"
                    args <- list(code = rest)
                }
                id <- paste0("call-", length(messages))
                list(text = "", usage = usage,
                     tool_calls = list(list(id = id, name = name, arguments = args)),
                     assistant_message = list(role = "assistant", content = list(
                         list(type = "tool_use", id = id, name = name, input = args))))
            }, "llm.api")
        })
    on.exit(ns$job_worker_close_all(s), add = TRUE, after = FALSE)
    at <- function(x) file.path(proj, x)
    unasked <- 0L
    pump_until <- function(done) {
        deadline <- Sys.time() + 90
        repeat {
            for (ev in ns$job_pump(s)) {
                if (identical(ev$type, "approval")) {
                    unasked <<- unasked + 1L
                    ns$job_answer(s, ev$job$id, ev$request$id, FALSE)
                }
            }
            if (isTRUE(done())) return(invisible(TRUE))
            if (Sys.time() > deadline) stop("fixture job timed out")
            Sys.sleep(0.05)
        }
    }
    run <- function(task, gate = NULL, while_waiting = NULL) {
        id <- ns$job_submit(s, task)
        if (!is.null(gate)) {
            pump_until(function() file.exists(at(paste0(gate, ".waiting"))))
            while_waiting(id)
            writeLines("go", at(gate))
        }
        pump_until(function() ns$job_read(id)$status %in% ns$JOB_STATUSES_FINAL)
        ns$job_read(id)
    }
    # The terminal's /clear, as far as jobs see it (R/repl-clear.R).
    clear_to <- function(id) {
        ns$job_conversation_hold(s)
        s$sessionId <- id
        ns$job_conversation_clear(s, id, checkpoint_key = paste0("repl:", id))
    }
    # As a terminal resumed in `id` starts: its key and id, no worker.
    resume <- function(id) {
        ns$job_worker_close_all(s)
        s$sessionId <- id
        s$job_checkpoint_key <- paste0("repl:", id)
    }
    checkpoint <- function(id) {
        key <- s$job_checkpoint_key
        on.exit(s$job_checkpoint_key <- key)
        s$job_checkpoint_key <- paste0("repl:", id)
        dir <- ns$job_worker_state_dir(ns$job_worker_identity(s, "doer"))
        info <- ns$job_read_file(file.path(dir, "checkpoint.json"))
        info$history <- file.exists(file.path(dir, "history.rds"))
        info
    }
    has <- function(j, text) grepl(text, j$outcome$result, fixed = TRUE)
    resume("conv-a")

    # Two jobs in one conversation: the worker keeps its history.
    j <- run("script: r x <- 42 ; r cat('x=', x)")
    expect_identical(j$status, "done")
    expect_true(has(j, "x= 42"))
    expect_true(has(j, "tasks=1 rules=FALSE skill=FALSE"))
    pid <- s$.job_worker$get_pid()
    j <- run("script: r cat('again')")
    expect_true(has(j, "tasks=2"))
    expect_identical(j$origin$conversation, "conv-a")

    # Codex's case: an untrusted project, /trust, /clear, a job in the
    # same worker. The idle doer wrote the new conversation's start; the
    # job sees the object, no history, and the prompt and tools of a
    # session built now.
    ns$project_trust_add(proj)
    clear_to("conv-b")
    start <- checkpoint("conv-b")
    expect_identical(start$conversation, "conv-b")
    expect_false(start$history)
    expect_true("x" %in% unlist(start$objects))
    j <- run("script: r cat('x=', x)")
    expect_identical(s$.job_worker$get_pid(), pid)
    expect_true(has(j, "x= 42"))
    expect_true(has(j, "tasks=1 rules=TRUE skill=TRUE"))
    expect_identical(checkpoint("conv-b")$conversation, "conv-b")
    expect_true(checkpoint("conv-b")$history)
    expect_identical(checkpoint("conv-a")$conversation, "conv-a")

    # 2 clears with no job between, the worker retired, then a job: a
    # new process, the objects, no history. The first start is written
    # by the idle doer from what it holds, not copied: the last
    # checkpoint is gone.
    gone <- ns$job_worker_state_dir(ns$job_worker_identity(s, "doer"))
    unlink(gone, recursive = TRUE)
    clear_to("conv-c")
    expect_true("x" %in% unlist(checkpoint("conv-c")$objects))
    clear_to("conv-d")
    expect_identical(checkpoint("conv-d")$conversation, "conv-d")
    ns$job_workers_retire(list(s), idle_minutes = 0, max_workers = 0L)
    expect_false(ns$job_worker_alive(s))
    j <- run("script: r cat('x=', x)")
    expect_false(identical(s$.job_worker$get_pid(), pid))
    expect_true(has(j, "x= 42"))
    expect_true(has(j, "tasks=1 rules=TRUE"))

    # /clear with no worker alive: the last checkpoint's objects are
    # copied, and its history is not.
    ns$job_workers_retire(list(s), idle_minutes = 0, max_workers = 0L)
    clear_to("conv-e")
    expect_identical(checkpoint("conv-e")$conversation, "conv-e")
    expect_false(checkpoint("conv-e")$history)
    j <- run("script: r cat('x=', x)")
    expect_true(has(j, "x= 42"))
    expect_true(has(j, "tasks=1"))

    # /clear with a job in flight: it settles and checkpoints under its
    # own conversation, and the new one's start is written then, with
    # what the job made.
    j <- run("script: r y <- 7 ; write f1 go-f1", gate = "go-f1",
             while_waiting = function(id) clear_to("conv-f"))
    expect_identical(j$status, "done")
    expect_identical(j$origin$conversation, "conv-e")
    expect_identical(checkpoint("conv-e")$conversation, "conv-e")
    expect_true(checkpoint("conv-e")$history)
    expect_identical(checkpoint("conv-f")$conversation, "conv-f")
    expect_false(checkpoint("conv-f")$history)
    expect_true("y" %in% unlist(checkpoint("conv-f")$objects))

    # Codex's case: 3 clears while a job from the first conversation
    # runs, and plan mode in the newest. The job keeps its own
    # conversation's settings and record, settles under its own id, and
    # no later conversation takes its history.
    record <- NULL
    j <- run("script: r z <- 9 ; write g1 go-g1", gate = "go-g1",
             while_waiting = function(id) {
        clear_to("conv-g")
        clear_to("conv-h")
        clear_to("conv-i")
        s$plan_mode <- TRUE
        ns$job_pump(s)
        record <<- ns$job_read_file(ns$job_authority_path(id))
    })
    s$plan_mode <- FALSE
    expect_identical(j$status, "done")
    expect_true(file.exists(at("g1")))
    expect_identical(record$seq, 1L)
    expect_false(record$plan_mode)
    expect_identical(checkpoint("conv-f")$conversation, "conv-f")
    for (id in c("conv-g", "conv-h", "conv-i")) {
        expect_identical(checkpoint(id)$conversation, id, info = id)
        expect_false(checkpoint(id)$history, info = id)
        expect_true("z" %in% unlist(checkpoint(id)$objects), info = id)
    }
    j <- run("script: r cat('z=', z)")
    expect_true(has(j, "z= 9"))
    expect_true(has(j, "tasks=1"))

    # Codex's case on #281: a job queued behind one in flight, its
    # conversation turns plan mode on, /clear, and the new conversation
    # turns it off. The queued job runs under what its own conversation
    # allowed as it ended: plan mode, so its write is refused.
    s$plan_mode <- FALSE
    asked <- s$sessionId
    first <- ns$job_submit(s, "script: write h1 go-h1")
    pump_until(function() file.exists(at("go-h1.waiting")))
    second <- ns$job_submit(s, "script: write h2")
    expect_identical(ns$job_read(second)$status, "queued")
    s$plan_mode <- TRUE
    ns$job_pump(s)
    clear_to("conv-j")
    s$plan_mode <- FALSE
    writeLines("go", at("go-h1"))
    pump_until(function() {
        ns$job_read(second)$status %in% ns$JOB_STATUSES_FINAL
    })
    expect_identical(ns$job_read(first)$status, "done")
    expect_identical(ns$job_read(second)$status, "done")
    expect_identical(ns$job_read(second)$origin$conversation, asked)
    record <- ns$job_read_file(ns$job_authority_path(second))
    expect_true(isTRUE(record$plan_mode))
    expect_false(file.exists(at("h2")))
    expect_identical(length(s$.job_held), 0L)
    # It ran in its own conversation's history: the job before the
    # scenario, the one it waited behind, and itself.
    expect_true(has(ns$job_read(second), "tasks=3"),
                info = ns$job_read(second)$outcome$result)
    expect_identical(checkpoint(asked)$conversation, asked)
    expect_true(checkpoint(asked)$history)
    # The next job is the new conversation's, under its settings.
    j <- run("script: write h3")
    expect_identical(j$origin$conversation, "conv-j")
    expect_true(file.exists(at("h3")))

    # --resume: a conversation's doer comes back as its last job left it.
    # Codex's case: clear P into Q, quit, resume P and change an object,
    # quit, resume Q. Q has the object as it was at the clear.
    resume("conv-p")
    j <- run("script: r p <- 1")
    clear_to("conv-q")
    resume("conv-p")
    j <- run("script: r p <- 2 ; r cat('p=', p)")
    expect_true(has(j, "p= 2"))
    expect_true(has(j, "tasks=2"))
    resume("conv-q")
    j <- run("script: r cat('p=', p)")
    expect_true(has(j, "p= 1"))
    expect_true(has(j, "tasks=1"))

    expect_identical(unasked, 0L)
})

# ---- Retention leaves the folders a live worker uses ----------------------
# Codex's case on #295: the worker names its process in the folder it
# starts in, and a /clear has it write the new conversation's start in
# another. Retention run by another session, its clock past the limit,
# took that folder while the worker lived. Each checkpoint names the
# process now.
local({
    root <- tempfile("conversation-retention-")
    dir.create(root)
    vars <- c("R_USER_DATA_DIR", "R_USER_CONFIG_DIR", "CORTEZA_STATE_DIR",
              "CORTEZA_REMOTE_DIR")
    old <- Sys.getenv(vars, unset = NA)
    Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
               R_USER_CONFIG_DIR = file.path(root, "config"))
    Sys.setenv(CORTEZA_STATE_DIR = file.path(ns$corteza_data_dir(), "state"),
               CORTEZA_REMOTE_DIR = file.path(ns$corteza_data_dir(), "remote"))
    on.exit({
        for (v in vars) {
            if (is.na(old[[v]])) {
                Sys.unsetenv(v)
            } else {
                do.call(Sys.setenv, stats::setNames(list(old[[v]]), v))
            }
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    proj <- file.path(root, "proj")
    dir.create(proj)
    s <- corteza::new_session("cli", provider = "anthropic", web_search = FALSE)
    s$cwd <- proj
    s$sessionId <- "conv-a"
    s$job_checkpoint_key <- "repl:conv-a"
    s$job_checkpoint_owner <- "local:fixture"
    s$config <- list(jobs = list(idle_minutes = 0))
    s$job_worker_spec <- list(init_fn = function(spec) {
        assign("session", corteza::new_session("cli", provider = "anthropic",
                                               web_search = FALSE),
               envir = asNamespace("corteza")$.subagent_state)
    })
    folder <- function() {
        ns$job_worker_state_dir(ns$job_worker_identity(s, "doer"))
    }
    first <- folder()
    ns$job_worker_start(s)
    on.exit(ns$job_worker_close(s), add = TRUE, after = FALSE)
    s$.job_worker$run(function() assign("kept", 42, globalenv()))
    s$sessionId <- "conv-b"
    ns$job_conversation_clear(s, "conv-b", checkpoint_key = "repl:conv-b")
    second <- folder()
    expect_false(identical(first, second))
    expect_true(file.exists(file.path(second, "checkpoint.json")))
    marked <- ns$job_read_file(file.path(second, "worker.json"))
    expect_identical(as.integer(marked$pid), s$.job_worker$get_pid())
    expect_identical(marked$host, Sys.info()[["nodename"]])
    # Another session's run: its own conversation is all it keeps.
    later <- Sys.time() + 100 * 24 * 60 * 60
    found <- ns$retention_prune(90, keep = "elsewhere", now = later)
    expect_false(any(found$removed))
    expect_true(file.exists(file.path(second, "checkpoint.json")))
    expect_true(file.exists(file.path(first, "worker.json")))
    expect_true(s$.job_worker$is_alive())
    # With the worker gone, the same run takes both folders: nothing but
    # the worker held them.
    ns$job_worker_close(s)
    if (.Platform$OS.type != "windows") {
        found <- ns$retention_prune(90, keep = "elsewhere", now = later)
        expect_true(second %in% found$path[found$removed])
        expect_true(first %in% found$path[found$removed])
        expect_false(file.exists(file.path(second, "checkpoint.json")))
    }
})
