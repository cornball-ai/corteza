# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

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
# The CLI and chat() run tools directly unless the config enables a
# talker. A surface can still request a talker default, except where
# tools are restricted: a doer gets the work tools whatever the list says.
local({
    start <- function(config = list(), default = FALSE, tools = NULL) {
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
    expect_identical(start(), list(talker = FALSE, said = FALSE))
    expect_identical(start(default = TRUE), list(talker = TRUE, said = TRUE))
    expect_identical(start(list(talker = list(enabled = FALSE))),
                     list(talker = FALSE, said = FALSE))
    expect_identical(start(tools = c("read_file", "bash")),
                     list(talker = FALSE, said = FALSE))
    # A talker block that says nothing of `enabled` leaves the default.
    expect_identical(start(list(talker = list(model = "talker-model"))),
                     list(talker = FALSE, said = FALSE))
    # Where the config turns it on, it is on everywhere, and nothing is
    # said.
    on <- list(talker = list(enabled = TRUE))
    expect_identical(start(on, default = FALSE), list(talker = TRUE, said = FALSE))
    expect_identical(start(on, tools = "read_file"),
                     list(talker = TRUE, said = FALSE))
    # The installed command and chat() both default to direct execution.
    bin <- system.file("bin", "corteza", package = "corteza")
    if (nzchar(bin)) {
        expect_true(any(grepl("ctx$talker_default <- FALSE", readLines(bin),
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
    expect_true(any(grepl("Getting to know corteza", type(ctx, "/guide"), fixed = TRUE)))
    expect_true(any(grepl("workspace_snapshot_days", type(ctx, "/guide storage"), fixed = TRUE)))
    expect_true(any(grepl('"cadence": "always"', type(ctx, "/guide context"), fixed = TRUE)))
    # The talker's tools: it reads and hands work on.
    tools <- paste(type(ctx, "/tools"), collapse = "\n")
    expect_true(grepl("delegate", tools, fixed = TRUE))
    expect_false(grepl("write_file", tools, fixed = TRUE))
    # Its last reply, its task list, its own tool calls and outputs.
    ctx$last_assistant_response <- "the talker's words"
    type(ctx, "/copy")
    expect_identical(ctx$copied, "the talker's words")
    # Its task list is off unless its config turns it on, and /tasks says
    # so (task_list_enabled).
    expect_true(any(grepl("The task list is off", type(ctx, "/tasks"),
                          fixed = TRUE)))
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
    # The process's registry, which other files in it may have used.
    expect_true(grepl(ns$format_subagent_list(ns$subagent_list()),
                      paste(type(ctx, "/agents"), collapse = "\n"),
                      fixed = TRUE))
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
# (test_talker_commands.R). A provider switch moves both roles to that
# provider's pair, including a talker with a configured model.
local({
    ctx <- talker_ctx()
    type(ctx)
    s <- ctx$session
    out <- type(ctx, "/provider openai")
    expect_identical(ns$job_worker_spec(s)$provider, "openai")
    expect_identical(s$provider, "openai")
    expect_identical(ctx$disk_session$session$provider, "openai")
    expect_true(any(grepl("on the next reply and the next job", out,
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

# ---- "Allow always" and the doer's calls -----------------------------------
# A tool the person allowed always in this directory is allowed for a
# job's call as for the session's own: without a prompt, and before the
# reviewer. Never for the credential rule's ask, nor for a request that
# names no rule, as none did before 0.8.1. The job's prompt offers
# "always" and keeps it as the session's own prompt does: the installed
# command's callback saves it for the directory, chat()'s holds it for
# the session.
local({
    config <- tempfile("repl-talker-config")
    old <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
    Sys.setenv(R_USER_CONFIG_DIR = config)
    on.exit({
        if (is.na(old)) {
            Sys.unsetenv("R_USER_CONFIG_DIR")
        } else {
            Sys.setenv(R_USER_CONFIG_DIR = old)
        }
        unlink(config, recursive = TRUE)
    }, add = TRUE)
    ctx <- talker_ctx()
    type(ctx)
    s <- ctx$session
    prompts <- character()
    replies <- character()
    recorder <- function(p) {
        prompts <<- c(prompts, p)
        if (!length(replies)) {
            return(character())
        }
        reply <- replies[[1L]]
        replies <<- replies[-1L]
        reply
    }
    # One request from a job of this session, answered. `rule` NA is a
    # request from before 0.8.1, which names none.
    ask <- function(tool, rule = "", reply = character(), route = NULL) {
        # type() leaves its own script in place.
        ctx$read_input <- recorder
        replies <<- reply
        n <- length(prompts)
        job <- ns$job_create("Run the checks.", workspace = ctx$cwd,
                             owner = s$job_owner,
                             origin = list(session_key = s$job_key))
        decision <- list(approval = "ask", reason = "config: needs approval")
        if (!is.na(rule) && nzchar(rule)) {
            decision$rule <- rule
        }
        extra <- if (is.null(route)) list() else list(route = route)
        ns$job_approval_request(job,
            list(tool = tool, class = "exec", args = list(command = "make check")),
            decision, extra = extra)
        req <- ns$job_approval_pending(job)[[1L]]
        if (is.na(rule)) {
            req$rule <- NULL
        }
        said <- capture.output(ns$.repl_job_approval(ctx,
            list(job = ns$job_read(job), request = req)))
        answer <- ns$job_read_file(ns$job_approval_path(job, req$id, "answer"))
        ns$job_settle(job, "done", result = "ran")
        list(approved = isTRUE(answer$approved), by = answer$by, said = said,
             asked = prompts[seq_along(prompts) > n])
    }
    offered <- "Approve? [y/a(lways)/N] "
    plain <- "Approve? [y/N] "

    bin <- system.file("bin", "corteza", package = "corteza")
    if (nzchar(bin)) {
        defs <- Filter(function(e) {
            is.call(e) && identical(e[[1L]], as.name("<-")) &&
                is.name(e[[2L]]) && identical(as.character(e[[2L]]), "cli_approval_cb")
        }, as.list(parse(bin, keep.source = FALSE)))
        expect_equal(length(defs), 1L)
        cli <- new.env(parent = globalenv())
        eval(defs[[1L]], envir = cli)
        cli$color <- list(yellow = "", reset = "", cyan = "", dim = "")
        cli$cli_read_line <- function(prompt_str = "> ") {
            stop("the session's own prompt is not asked here")
        }
        s$approval_cb <- cli$cli_approval_cb(ctx$cwd)
        # Nothing saved: asked, with "always" offered. Once is once.
        got <- ask("bash", reply = "y")
        expect_identical(got$asked, offered)
        expect_true(got$approved)
        expect_identical(got$by, "local")
        expect_identical(ns$approvals_saved(ctx$cwd), character())
        # "a" approves and saves it for the directory.
        got <- ask("bash", reply = "a")
        expect_true(got$approved)
        expect_identical(ns$approvals_saved(ctx$cwd), "bash")
        expect_true(any(grepl("bash is allowed always in this project",
                              got$said, fixed = TRUE)))
        # From then on, without a prompt.
        got <- ask("bash")
        expect_identical(got$asked, character())
        expect_true(got$approved)
        expect_identical(got$by, "always")
        expect_true(any(grepl("bash allowed, as you allowed it always here",
                              got$said, fixed = TRUE)))
        # The session's own calls take it as well, in a new session too.
        expect_true(isTRUE(attr(cli$cli_approval_cb(ctx$cwd), "answered")(
            list(tool = "bash"), list(approval = "ask"))))
        # Not the credential rule's ask, which offers no "always": an "a"
        # there is no yes.
        got <- ask("bash", rule = "credential", reply = "a")
        expect_identical(got$asked, plain)
        expect_false(got$approved)
        got <- ask("bash", rule = "credential", reply = "y")
        expect_true(got$approved)
        expect_identical(got$by, "local")
        # Nor a request that names no rule.
        got <- ask("bash", rule = NA, reply = "y")
        expect_identical(got$asked, plain)
        expect_identical(got$by, "local")
        # Codex's case on #297: under review, the doer's gate files the
        # request. The credential rule goes with it, so the saved "always"
        # for the tool does not answer it there either. A write in the
        # project, through the same gate, it does answer.
        ns$approval_save(ctx$cwd, "write_file")
        worker <- ns$.job_worker_state
        gate <- ns$.job_worker_child_review_gate(ctx$cwd)
        filed <- function(path) {
            call <- list(tool = "write_file", class = "write",
                         args = list(path = path, content = "x"))
            job <- ns$job_create("Write it.", workspace = ctx$cwd,
                                 owner = s$job_owner,
                                 origin = list(session_key = s$job_key))
            worker$job_id <- job
            worker$approval_timeout <- 0.2
            on.exit({
                worker$job_id <- NULL
                worker$approval_timeout <- NULL
                ns$job_settle(job, "done", result = "ran")
            })
            gate(call, corteza::policy(call))
            files <- list.files(file.path(ns$job_dir(job), "approvals"),
                                "[.]request[.]json$", full.names = TRUE)
            ns$job_read_file(files[[1L]])
        }
        secret <- filed("~/.ssh/config")
        expect_identical(secret$rule, "credential")
        expect_false(ns$.repl_job_answered(s$approval_cb, secret,
                                           ns$job_request_call(secret)))
        expect_null(ns$.repl_job_always(s$approval_cb, secret))
        notes <- filed(file.path(ctx$cwd, "notes.md"))
        expect_identical(notes$rule, "")
        expect_true(ns$.repl_job_answered(s$approval_cb, notes,
                                          ns$job_request_call(notes)))
        # Another tool is asked.
        expect_identical(ask("run_r", reply = "n")$asked, offered)
        # Under review, a call the person allowed always goes by the
        # reviewer; one they did not goes to it.
        reviewed <- 0L
        type(ctx, "/permissions review")
        with_stub(".repl_job_review", function(ctx, job, req) {
            reviewed <<- reviewed + 1L
            list(reason = "The reviewer passed this to you: unsure")
        }, {
            got <- ask("bash", route = "monitor")
            expect_identical(got$by, "always")
            expect_identical(reviewed, 0L)
            got <- ask("run_r", route = "monitor", reply = "n")
            expect_identical(reviewed, 1L)
            expect_false(got$approved)
        })
        type(ctx, "/permissions ask")
    }

    # chat()'s callback holds "always" for the session.
    s$approval_cb <- ns$chat_approval_cb(ctx$cwd)
    got <- ask("write_file", reply = "a")
    expect_true(got$approved)
    expect_true(any(grepl("write_file is allowed always in this session",
                          got$said, fixed = TRUE)))
    expect_identical(ask("write_file")$by, "always")
    s$approval_cb <- ns$chat_approval_cb(ctx$cwd)
    expect_identical(ask("write_file", reply = "y")$asked, offered)
    # A callback with neither offers no "always", and answers nothing.
    s$approval_cb <- function(call, decision) TRUE
    got <- ask("write_file", reply = "a")
    expect_identical(got$asked, plain)
    expect_false(got$approved)
})
local({
    ctx <- talker_ctx(list(enabled = TRUE, model = "talker-model"))
    type(ctx)
    s <- ctx$session
    out <- type(ctx, "/provider openai")
    expect_identical(ns$job_worker_spec(s)$provider, "openai")
    expect_identical(s$provider, "openai")
    expect_identical(s$model_map$cloud, ns$job_worker_spec(s)$model)
    expect_true(any(grepl("on the next reply and the next job", out, fixed = TRUE)))
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
    # 1,500 tokens in all. Whether /spent prints its short form depends
    # on the process's subagent spend, which an earlier file in the same
    # process (R CMD check runs them in one) may have left.
    expect_true(any(grepl("1.5K tok", type(ctx, "/spent"), fixed = TRUE)))
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
    out <- with_stub("job_dispatch_next", function(session) list(), type(ctx, "/quit"))
    expect_true(any(grepl("Bye.", out, fixed = TRUE)))
    expect_identical(ns$job_read(queued)$status, "cancelled")
    expect_null(ns$job_read(queued)$dispatch)
})

# ---- The table says what this file shows ------------------------------------------
local({
    tbl <- ns$corteza_commands()
    main <- tbl[tbl$terminal & !nzchar(tbl$same_as), ]
    shown <- c(quit = "both", clear = "both", help = "talker", jobs = "doer",
               job = "doer", cancel = "doer", copy = "talker", tasks = "talker",
               tools = "talker", model = "both", effort = "doer",
               provider = "both", spawn = "talker", agents = "talker",
               ask = "talker", queue = "talker", collect = "talker",
               kill = "talker", sessions = "talker", trace = "talker",
               permissions = "both", trust = "both", auto = "talker",
               `max-tokens` = "doer", `remote-control` = "both",
               dryrun = "both", paste = "talker", plan = "both",
               spent = "both", `context-edit` = "both", status = "both",
               compact = "talker", refine = "talker", flush = "talker",
               doctor = "both", config = "both", guide = "both", last = "talker",
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
