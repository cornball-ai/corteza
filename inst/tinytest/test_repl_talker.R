# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# The terminal's commands in a talker session (WP20c). The command table
# says what each acts on there: the talker's conversation, the doer and
# its jobs, or both. This file types each one into run_repl_loop() in a
# talker session and checks that it does so, then holds the table to
# what it found. No worker starts and no provider is called: what would
# reach a doer is read off its job's spec (job_worker_spec()) and the
# settings its authority record carries (job_authority_now()), and a few
# helpers are stood in for.

ns <- asNamespace("corteza")

state <- tempfile("repl-talker-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

palette <- list(dim = "", reset = "", cyan = "", bold = "", yellow = "",
                green = "", bright_magenta = "", red = "", magenta = "")
scripted <- function(lines) {
    i <- 0L
    function(p) {
        i <<- i + 1L
        if (i <= length(lines)) lines[[i]] else character(0)
    }
}

# A terminal whose session is a talker, its doer on "doer-model". The
# talker follows the doer's model unless `talker` names its own.
talker_ctx <- function(talker = list(enabled = TRUE)) {
    cwd <- tempfile("repl-talker-cwd")
    dir.create(cwd)
    cwd <- normalizePath(cwd)
    s <- corteza::new_session("cli", provider = "anthropic",
                              model_map = list(cloud = "doer-model"))
    s$cwd <- cwd
    s$config <- list(talker = talker)
    disk <- ns$session_new("anthropic", "doer-model", cwd, surface = "cli")
    s$sessionId <- disk$sessionId
    ctx <- new.env(parent = emptyenv())
    ctx$session <- s
    ctx$config <- s$config
    ctx$cwd <- cwd
    ctx$model <- "doer-model"
    ctx$provider <- "anthropic"
    ctx$disk_session <- list(session = disk, sessionId = disk$sessionId,
                             resumed = FALSE)
    ctx$ws_enabled <- FALSE
    ctx$palette <- palette
    ctx$help_text <- function() "HELP TEXT"
    ctx$handle_copy <- function(x) ctx$copied <- x
    ctx$format_tools <- ns$chat_format_tools_list
    ctx$pending_r_context <- character(0)
    ctx$last_assistant_response <- ""
    ctx$render_reply <- function(txt) cat(txt, "\n")
    ctx$prompts <- character()
    ctx$turn_fn <- function(prompt, session) {
        ctx$prompts <- c(ctx$prompts, prompt)
        list(reply = "talker reply", usage = NULL)
    }
    ctx
}
# Type `...` at the prompt, then end the input.
type <- function(ctx, ...) {
    ctx$read_input <- scripted(c(...))
    capture.output(ns$run_repl_loop(ctx))
}
# Evaluate `code` with corteza's `name` replaced by `fn`.
with_stub <- function(name, fn, code) {
    old <- get(name, envir = ns)
    assignInNamespace(name, fn, "corteza")
    on.exit(assignInNamespace(name, old, "corteza"), add = TRUE)
    force(code)
}

# ---- The default ---------------------------------------------------------------
# The CLI is a talker unless its config says otherwise, and says so once
# it starts; chat() is not. A session whose tools are restricted is no
# talker by default, as a room's is not: a doer gets the work tools
# whatever the list says.
local({
    start <- function(config = list(), default = TRUE, tools = NULL) {
        ctx <- talker_ctx()
        ctx$session$config <- config
        ctx$config <- config
        ctx$session$tools_filter <- tools
        ctx$talker_default <- default
        out <- type(ctx)
        list(talker = isTRUE(ctx$session$talker),
             said = any(grepl("Talker mode: this session answers", out,
                              fixed = TRUE)))
    }
    expect_identical(start(), list(talker = TRUE, said = TRUE))
    expect_identical(start(default = FALSE), list(talker = FALSE, said = FALSE))
    expect_identical(start(list(talker = list(enabled = FALSE))),
                     list(talker = FALSE, said = FALSE))
    expect_identical(start(tools = c("read_file", "bash")),
                     list(talker = FALSE, said = FALSE))
    # A talker block that says nothing of `enabled` leaves the default.
    expect_identical(start(list(talker = list(model = "talker-model"))),
                     list(talker = TRUE, said = TRUE))
    # Where the config turns it on, it is on everywhere, and nothing is
    # said.
    on <- list(talker = list(enabled = TRUE))
    expect_identical(start(on, default = FALSE), list(talker = TRUE, said = FALSE))
    expect_identical(start(on, tools = "read_file"),
                     list(talker = TRUE, said = FALSE))
    # The installed command takes the default, and chat() does not.
    bin <- system.file("bin", "corteza", package = "corteza")
    if (nzchar(bin)) {
        expect_true(any(grepl("ctx$talker_default <- TRUE", readLines(bin),
                              fixed = TRUE)))
    }
    expect_false("talker_default" %in% all.names(body(corteza::chat)))
})

# /clear starts a new conversation, and the session stays a talker with
# the doer it had: its model, and a checkpoint of the new conversation's.
local({
    ctx <- talker_ctx()
    ctx$new_session_fn <- function() {
        fresh <- ns$session_new(ctx$provider, ctx$model, ctx$cwd, surface = "cli")
        list(session = fresh, sessionId = fresh$sessionId, resumed = FALSE)
    }
    type(ctx)
    s <- ctx$session
    old <- ns$session_conversation(s)
    type(ctx, "/model other-model", "/clear")
    expect_true(isTRUE(s$talker))
    expect_identical(ns$job_worker_spec(s)$model, "other-model")
    expect_false(identical(ns$session_conversation(s), old))
    expect_identical(s$job_checkpoint_key,
                     paste0("repl:", ctx$disk_session$sessionId))
})

# ---- What acts on the talker ------------------------------------------------
# Each of these reads or changes the terminal's own session, and the
# doer's next job is as it was.
local({
    ctx <- talker_ctx()
    type(ctx)
    s <- ctx$session
    expect_true(isTRUE(s$talker))
    before <- ns$job_worker_spec(s)

    expect_true(any(grepl("HELP TEXT", type(ctx, "/help"), fixed = TRUE)))
    # The talker's tools: it reads and hands work on.
    tools <- paste(type(ctx, "/tools"), collapse = "\n")
    expect_true(grepl("delegate", tools, fixed = TRUE))
    expect_false(grepl("write_file", tools, fixed = TRUE))
    # Its last reply, its task list, its own tool calls and outputs.
    ctx$last_assistant_response <- "the talker's words"
    type(ctx, "/copy")
    expect_identical(ctx$copied, "the talker's words")
    expect_true(any(grepl("No tasks", type(ctx, "/tasks"), fixed = TRUE)))
    expect_true(any(grepl("No tool calls recorded", type(ctx, "/trace"),
                          fixed = TRUE)))
    expect_true(any(grepl("No tool outputs yet", type(ctx, "/last"),
                          fixed = TRUE)))
    expect_true(any(grepl("No tool outputs yet", type(ctx, "/outputs"),
                          fixed = TRUE)))
    # Its conversation, empty so far, and the conversations of this
    # directory.
    expect_true(any(grepl("Nothing to compact", type(ctx, "/compact"),
                          fixed = TRUE)))
    expect_true(any(grepl("Nothing to flush", type(ctx, "/flush"),
                          fixed = TRUE)))
    expect_true(any(grepl("No lessons recorded", type(ctx, "/refine list"),
                          fixed = TRUE)))
    expect_true(any(grepl("anthropic/doer-model", type(ctx, "/sessions"),
                          fixed = TRUE)))

    # Subagents are children of the terminal's process.
    parent <- NULL
    with_stub("subagent_spawn", function(task, model = NULL, tools = NULL,
                                         preset = NULL, parent_session = NULL,
                                         ...) {
        parent <<- parent_session
        "00000000-0000-0000-0000-000000000000"
    }, type(ctx, "/spawn look around"))
    expect_identical(parent, s)
    expect_true(any(grepl("No active subagents", type(ctx, "/agents"),
                          fixed = TRUE)))
    for (cmd in c("/ask 99 hi", "/queue 99 hi", "/collect 99", "/kill 99")) {
        expect_true(any(grepl("Subagent not found: 99", type(ctx, cmd),
                              fixed = TRUE)), info = cmd)
    }

    # /r runs here, and its output goes with the talker's next message.
    on.exit(suppressWarnings(rm(".repl_talker_probe", envir = globalenv())),
            add = TRUE)
    type(ctx, "/r .repl_talker_probe <- 6 * 7", "what is it")
    expect_identical(get(".repl_talker_probe", envir = globalenv()), 42)
    expect_true(grepl("[/r] .repl_talker_probe", ctx$prompts[[1L]],
                      fixed = TRUE))
    # /paste sends its lines to the talker as one message.
    with_stub("read_paste_block", function(seed = NULL, ...) "line one\nline two",
              type(ctx, "/paste"))
    expect_identical(ctx$prompts[[2L]], "line one\nline two")

    # /auto starts no run in a talker session, and says what would.
    out <- type(ctx, "/auto tidy the package")
    expect_true(any(grepl("turn talker mode off", out, fixed = TRUE)))
    expect_null(ctx$last_auto_request)
    expect_true(any(grepl("Auto is off", type(ctx, "/auto status"),
                          fixed = TRUE)))

    expect_identical(ns$job_worker_spec(s), before)
})

# ---- What acts on the doer ------------------------------------------------------
# /provider and /max-tokens set the doer's, as /model and /effort do
# (test_talker_commands.R). The talker follows /provider where its
# config names no model or provider of its own.
local({
    ctx <- talker_ctx()
    type(ctx)
    s <- ctx$session
    out <- type(ctx, "/provider openai")
    expect_identical(ns$job_worker_spec(s)$provider, "openai")
    expect_identical(s$provider, "openai")
    expect_identical(ctx$disk_session$session$provider, "openai")
    expect_true(any(grepl("for the next reply and the next job", out,
                          fixed = TRUE)))

    out <- type(ctx, "/max-tokens 4000")
    expect_equal(ns$job_worker_spec(s)$max_tokens, 4000)
    expect_null(s$max_tokens)
    expect_true(any(grepl("Output budget of the doer's jobs: 4000", out,
                          fixed = TRUE)))
    type(ctx, "/max-tokens default")
    expect_null(ns$job_worker_spec(s)$max_tokens)

    # The doer's jobs.
    expect_true(any(grepl("No jobs in this session", type(ctx, "/jobs"),
                          fixed = TRUE)))
    expect_true(any(grepl("Usage", type(ctx, "/cancel"), fixed = TRUE)))
})
# A Ctrl+C at a job's question is a no, as EOF is: each request is asked
# once, and one left open would hold the worker until its wait ran out.
local({
    ctx <- talker_ctx()
    type(ctx)
    s <- ctx$session
    job <- ns$job_create("Write the notes.", workspace = ctx$cwd,
                         owner = s$job_owner,
                         origin = list(session_key = s$job_key))
    ns$job_approval_request(job,
        list(tool = "write_file", class = "write",
             args = list(path = file.path(ctx$cwd, "notes.md"), content = "x")),
        list(approval = "ask", reason = "a write"))
    req <- ns$job_approval_pending(job)[[1L]]
    ctx$read_input <- function(p) {
        stop(structure(class = c("interrupt", "condition"),
                       list(message = "", call = NULL)))
    }
    invisible(capture.output(ns$.repl_job_approval(ctx,
        list(job = ns$job_read(job), request = req))))
    answer <- ns$job_read_file(ns$job_approval_path(job, req$id, "answer"))
    expect_identical(answer$approved, FALSE)
    ns$job_settle(job, "cancelled")
})
local({
    ctx <- talker_ctx(list(enabled = TRUE, model = "talker-model"))
    type(ctx)
    s <- ctx$session
    out <- type(ctx, "/provider openai")
    expect_identical(ns$job_worker_spec(s)$provider, "openai")
    expect_identical(s$provider, "anthropic")
    expect_true(any(grepl("replies stay on anthropic", out, fixed = TRUE)))
})
# /review reviews with the work's model, and /doctor and /config speak
# of it, where the talker runs on another provider.
local({
    ctx <- talker_ctx(list(enabled = TRUE, provider = "openai",
                           model = "talker-model"))
    type(ctx)
    expect_identical(ctx$session$provider, "openai")
    seen <- list()
    diff <- function(ref = NULL) {
        list(ok = TRUE, target = "HEAD", status = "", diff = "+the doer's line")
    }
    with_stub("collect_git_diff", diff,
              with_stub("provider_status", function(provider, model) list(ok = TRUE),
                        with_stub("run_review", function(provider, model, ...) {
        seen$review <<- c(provider, model)
        list(content = "No findings.")
    }, type(ctx, "/review"))))
    expect_identical(seen$review, c("anthropic", "doer-model"))
    with_stub("format_doctor_report", function(...) {
        a <- list(...)
        seen$doctor <<- c(a$provider, a$display_model)
        "DOCTOR"
    }, type(ctx, "/doctor"))
    expect_identical(seen$doctor, c("anthropic", "doer-model"))
    with_stub("format_config_summary", function(...) {
        a <- list(...)
        seen$config <<- c(a$provider, a$display_model)
        "CONFIG"
    }, type(ctx, "/config"))
    expect_identical(seen$config, c("anthropic", "doer-model"))
    # /diff shows the checkout both work in.
    out <- with_stub("collect_git_diff", diff, type(ctx, "/diff"))
    expect_true(any(grepl("+the doer's line", out, fixed = TRUE)))
})

# ---- What acts on both ----------------------------------------------------------
local({
    ctx <- talker_ctx()
    type(ctx)
    s <- ctx$session
    # /plan, /permissions and /dryrun change the talker's session, and a
    # job reads them from its authority record before each call
    # (R/job-authority.R).
    type(ctx, "/plan")
    expect_true(isTRUE(s$plan_mode))
    expect_true(ns$job_authority_now(s)$plan_mode)
    type(ctx, "/plan")
    type(ctx, "/permissions review")
    expect_identical(ns$job_authority_now(s)$approval_mode, "review")
    expect_true(is.function(s$auto_gate))
    type(ctx, "/permissions ask")
    expect_null(s$auto_gate)
    type(ctx, "/dryrun")
    expect_true(ns$job_authority_now(s)$dry_run)
    type(ctx, "/dryrun")
    expect_false(ns$job_authority_now(s)$dry_run)

    # /spent counts the doer's jobs with the talker's replies. A settled
    # job's result goes into the talker's history and the transcript,
    # which --resume and the reviewer read.
    job <- ns$job_create("Count the files.", workspace = ctx$cwd,
                         owner = s$job_owner,
                         origin = list(session_key = s$job_key,
                                       conversation = ns$session_conversation(s)))
    ns$job_settle(job, "done", result = "There are 3 files.",
                  usage = list(input_tokens = 1200L, output_tokens = 300L))
    invisible(capture.output(ns$.repl_present_job_event(ctx,
        list(type = "settled", job = ns$job_read(job)))))
    expect_equal(s$spend$segments[[1L]]$input_tokens, 1200)
    expect_true(any(grepl("(1.2K in / 300 out)", type(ctx, "/spent"),
                          fixed = TRUE)))
    last <- s$history[[length(s$history)]]
    expect_true(grepl("There are 3 files.", last$content, fixed = TRUE))
    transcript <- readLines(ns$session_transcript_path(ctx$disk_session$sessionId))
    expect_true(any(grepl("There are 3 files.", transcript, fixed = TRUE)))

    # The room's view of the session, and the skills both use: the
    # talker reads their instructions and the doer runs them.
    expect_true(any(grepl("Remote control is off",
                          type(ctx, "/remote-control status"), fixed = TRUE)))
    expect_true(any(grepl("No skills installed", type(ctx, "/skill list"),
                          fixed = TRUE)))

    # /quit ends the conversation and stops its jobs.
    queued <- ns$job_create("Never started.", workspace = ctx$cwd,
                            owner = s$job_owner,
                            origin = list(session_key = s$job_key))
    out <- type(ctx, "/quit")
    expect_true(any(grepl("Bye.", out, fixed = TRUE)))
    expect_identical(ns$job_read(queued)$status, "cancelled")
})

# ---- The table says what this file shows ------------------------------------------
local({
    tbl <- ns$corteza_commands()
    main <- tbl[tbl$terminal & !nzchar(tbl$same_as), ]
    shown <- c(quit = "both", clear = "both", help = "talker", jobs = "doer",
               job = "doer", cancel = "doer", copy = "talker", tasks = "talker",
               tools = "talker", model = "doer", effort = "doer",
               provider = "doer", spawn = "talker", agents = "talker",
               ask = "talker", queue = "talker", collect = "talker",
               kill = "talker", sessions = "talker", trace = "talker",
               permissions = "both", trust = "both", auto = "talker",
               `max-tokens` = "doer", `remote-control` = "both",
               dryrun = "both", paste = "talker", plan = "both",
               spent = "both", `context-edit` = "both", status = "both",
               compact = "talker", refine = "talker", flush = "talker",
               doctor = "both", config = "both", last = "talker",
               outputs = "talker", diff = "both", review = "doer",
               skill = "both", r = "talker")
    expect_true(setequal(names(shown), main$name),
                info = paste(setdiff(union(names(shown), main$name),
                                     intersect(names(shown), main$name)),
                             collapse = " "))
    expect_identical(stats::setNames(main$talker, main$name)[names(shown)],
                     shown)
})

if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(state, recursive = TRUE)
