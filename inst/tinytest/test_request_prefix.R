# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# WP14 (decision S1): a session's system text and tool list are fixed,
# so 2 consecutive requests differ only by the messages appended to the
# second. A change of mode or of the task list reaches the model as a
# note at the head of the next prompt; permission is checked at the
# call. These tests read the request body llm.api serializes, through
# curl, faked here: nothing reaches a provider.

if (!"request_callback" %in% names(formals(llm.api::agent))) {
    exit_file("older llm.api")
}
ns <- asNamespace("corteza")

# ---- The fake provider -----------------------------------------------------
# Every request body is kept; every reply is the text "done".
bodies <- list()
saved <- list()
replace <- function(package, name, fn) {
    saved[[paste(package, name)]] <<- list(package = package, name = name,
                                            fn = get(name, asNamespace(package)))
    utils::assignInNamespace(name, fn, package)
}
restore <- function() {
    for (item in rev(saved)) {
        utils::assignInNamespace(item$name, item$fn, item$package)
    }
}
replace("llm.api", ".get_provider_config", function(provider) {
    list(provider = provider, api_key = "fake-auth",
         base_url = "https://example.invalid", chat_path = "/messages",
         default_model = "fixture")
})
setopt <- curl::handle_setopt
replace("curl", "handle_setopt", function(handle, ..., .list = list()) {
    args <- c(list(...), .list)
    if (!is.null(args$postfields)) {
        bodies[[length(bodies) + 1L]] <<- jsonlite::fromJSON(
            as.character(args$postfields), simplifyVector = FALSE)
    }
    setopt(handle, ..., .list = .list)
})
emit <- function(fun, object) {
    fun(charToRaw(paste0("data: ", jsonlite::toJSON(object, auto_unbox = TRUE,
                                                    null = "null"), "\n\n")))
}
replace("curl", "curl_fetch_stream", function(url, fun, handle) {
    emit(fun, list(type = "message_start", message = list(id = "msg",
                   model = "fixture", usage = list(input_tokens = 1L, output_tokens = 0L))))
    emit(fun, list(type = "content_block_start", index = 0L,
                   content_block = list(type = "text", text = "done")))
    emit(fun, list(type = "content_block_stop", index = 0L))
    emit(fun, list(type = "message_delta", delta = list(stop_reason = "end_turn"),
                   usage = list(output_tokens = 1L)))
    list(status_code = 200L, headers = charToRaw("x-request-id: fixture\r\n"))
})
logged <- list()
log_original <- get("log_event", ns)
replace("corteza", "log_event", function(event, ...) {
    logged[[length(logged) + 1L]] <<- c(list(event = event), list(...))
    invisible(NULL)
})

# What a request carries ahead of its messages, with cache markers taken
# out: a marker moves to the newest message by design. To carry it,
# llm.api gives that message's text the form of a single text block,
# which the next request sends as a string again; the API reads the 2
# as the same content, as it ignores the marker.
unmark <- function(x) {
    if (!is.list(x)) {
        return(x)
    }
    x$cache_control <- NULL
    x <- lapply(x, unmark)
    content <- x$content
    if (is.list(content) && length(content) == 1L && is.list(content[[1L]]) &&
        identical(content[[1L]]$type, "text") &&
        identical(sort(names(content[[1L]])), c("text", "type"))) {
        x$content <- content[[1L]]$text
    }
    x
}
prefix_of <- function(body) unmark(list(system = body$system, tools = body$tools))
text_of <- function(message) {
    content <- message$content
    if (is.character(content)) {
        return(paste(content, collapse = "\n"))
    }
    paste(vapply(content, function(b) if (is.null(b$text)) "" else b$text, ""),
          collapse = "\n")
}
last_prompt <- function(body) text_of(body$messages[[length(body$messages)]])
count_notes <- function(text) {
    parts <- strsplit(text, "\n\n", fixed = TRUE)[[1L]]
    sum(grepl("^\\[(Plan mode|An unattended|The /auto run|Task list|The task list)", parts))
}
# The earlier request's messages, the reply to it, then the new prompt:
# nothing before the new prompt changed.
appended_only <- function(earlier, later) {
    n <- length(earlier$messages)
    length(later$messages) == n + 2L &&
    identical(unmark(later$messages[seq_len(n)]), unmark(earlier$messages))
}

local({
    on.exit(restore(), add = TRUE)
    opt <- options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "fixture")
    })
    on.exit(options(opt), add = TRUE)
    dir <- tempfile("prefix-")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    # The git tools' availability reads the working directory: the
    # session starts outside any repository.
    old_wd <- setwd(dir)
    on.exit(setwd(old_wd), add = TRUE)
    s <- corteza::new_session(channel = "console", provider = "anthropic",
                              model_map = list(cloud = "fixture"),
                              web_search = FALSE, system = "Fixture system text.")
    s$cwd <- dir
    # The task list is off by default; its notes are part of what this
    # session is tested for.
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = Inf,
                     task_list_enabled = TRUE)
    ask <- function(prompt) {
        before <- length(bodies)
        corteza::turn(prompt, s)
        expect_equal(length(bodies), before + 1L, info = prompt)
        bodies[[length(bodies)]]
    }

    first <- ask("hello")
    names_sent <- vapply(first$tools, function(t) t$name, "")
    # A console session can be in plan mode, so exit_plan_mode is in its
    # list from the start; the task tools are there too.
    expect_true("exit_plan_mode" %in% names_sent)
    expect_true(all(c("task_create", "task_update") %in% names_sent))
    expect_equal(count_notes(last_prompt(first)), 0L)
    # The system text carries the task guidance, never a plan or a list.
    expect_true(grepl("Multi-step requests", paste(unlist(first$system), collapse = " ")))

    # Plan mode on, then off: one note each, and the same prefix.
    s$plan_mode <- TRUE
    planned <- ask("plan this")
    expect_identical(prefix_of(planned), prefix_of(first))
    expect_true(appended_only(first, planned))
    expect_true(grepl("^\\[Plan mode is on\\.", last_prompt(planned)))
    expect_equal(count_notes(last_prompt(planned)), 1L)
    expect_true(grepl("plan this$", last_prompt(planned)))
    again <- ask("still planning")
    expect_equal(count_notes(last_prompt(again)), 0L)
    expect_true(appended_only(planned, again))
    # Plan mode still denies a write, and a tool nobody classified.
    handler <- ns$.make_tool_handler(s, function(...) stop("ran in plan mode"))
    expect_true(grepl("denied", handler("write_file", list(path = file.path(dir, "x"),
                                                           content = "x"))))
    expect_identical(corteza::policy(list(tool = "no_class_fixture", args = list(),
                                          context = list(plan_mode = TRUE)),
                                     list())$approval, "deny")
    s$plan_mode <- FALSE
    unplanned <- ask("go")
    expect_identical(prefix_of(unplanned), prefix_of(first))
    expect_true(appended_only(again, unplanned))
    expect_true(grepl("^\\[Plan mode is off\\.", last_prompt(unplanned)))
    expect_equal(count_notes(last_prompt(unplanned)), 1L)
    # Outside plan mode exit_plan_mode is offered and refused at the call.
    handler <- ns$.make_tool_handler(s, function(...) stop("exit ran"))
    expect_true(grepl("exit_plan_mode refused", handler("exit_plan_mode",
                                                        list(plan = "p"))))

    # A task update: one note with the whole list, the same prefix.
    s$tasks <- list(list(text = "first step", status = "in_progress"),
                    list(text = "second step", status = "pending"))
    tasked <- ask("next")
    expect_identical(prefix_of(tasked), prefix_of(first))
    expect_true(appended_only(unplanned, tasked))
    expect_true(grepl("^\\[Task list:\n1\\. \\[>\\] first step", last_prompt(tasked)))
    expect_equal(count_notes(last_prompt(tasked)), 1L)
    s$tasks <- list()
    emptied <- ask("done with those")
    expect_true(grepl("^\\[The task list is empty\\.\\]", last_prompt(emptied)))

    # An /auto run: a note when it starts and when it ends, the same
    # prefix, and what it may not call is refused at the call.
    s$auto_config <- list()
    s$auto_run_id <- "fixture-run"
    auto <- ask("work unattended")
    expect_identical(prefix_of(auto), prefix_of(first))
    expect_true(appended_only(emptied, auto))
    expect_true(grepl("^\\[An unattended /auto run has started", last_prompt(auto)))
    expect_equal(count_notes(last_prompt(auto)), 1L)
    handler <- ns$.make_tool_handler(s, function(...) stop("ran in an auto run"))
    expect_true(grepl("refused during this /auto run: bash",
                      handler("bash", list(command = "true"))))
    s$auto_config <- NULL
    s$auto_run_id <- NULL
    ended <- ask("back")
    expect_identical(prefix_of(ended), prefix_of(first))
    expect_true(appended_only(auto, ended))
    expect_true(grepl("^\\[The /auto run has ended\\.", last_prompt(ended)))

    # A git init during the session adds no tools: availability was
    # tested when the list was composed.
    if (nzchar(Sys.which("git"))) {
        system2("git", c("init", "-q", shQuote(dir)), stdout = FALSE, stderr = FALSE)
        inited <- ask("after git init")
        expect_identical(prefix_of(inited), prefix_of(first))
        expect_false("git_status" %in% vapply(inited$tools, function(t) t$name, ""))
    }

    # Several changes at once: one note each, in order.
    s$plan_mode <- TRUE
    s$tasks <- list(list(text = "only step", status = "pending"))
    both <- ask("both")
    expect_equal(count_notes(last_prompt(both)), 2L)
    expect_identical(prefix_of(both), prefix_of(first))

    # A rewrite that may drop notes (a compaction) makes the next prompt
    # state the session again; a thinking strip does not.
    ns$history_replace(s, s$history, "thinking_refused")
    kept <- ask("after a strip")
    expect_equal(count_notes(last_prompt(kept)), 0L)
    ns$history_replace(s, s$history[seq_len(2L)], "threshold")
    restated <- ask("after a compaction")
    expect_equal(count_notes(last_prompt(restated)), 2L)
    expect_identical(prefix_of(restated), prefix_of(first))

    # exit_plan_mode's result tells the model, so no note follows it. A
    # person approves the plan (check_plan_exit()).
    asked <- NULL
    s$approval_cb <- function(call, decision) {
        asked <<- decision$rule
        TRUE
    }
    handler <- ns$.make_tool_handler(s, function(name, args) ns$ok("Plan approved."))
    handler("exit_plan_mode", list(plan = "the plan"))
    expect_identical(asked, "plan_exit")
    expect_false(isTRUE(s$plan_mode))
    s$approval_cb <- NULL
    s$tasks <- list(list(text = "only step", status = "pending"))
    quiet <- ask("proceed")
    expect_equal(count_notes(last_prompt(quiet)), 0L)

    # A prompt with an image keeps its parts: the note goes ahead as a
    # text part of its own, and the image goes out as an image.
    png <- file.path(dir, "fixture.png")
    writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), png)
    s$plan_mode <- TRUE
    before <- length(bodies)
    corteza::turn(llm.api::llm_content("look at this", llm.api::llm_image(png)), s)
    expect_equal(length(bodies), before + 1L)
    pictured <- bodies[[length(bodies)]]
    blocks <- pictured$messages[[length(pictured$messages)]]$content
    expect_identical(vapply(blocks, function(b) b$type, ""),
                     c("text", "text", "image"))
    expect_true(grepl("^\\[Plan mode is on\\.", blocks[[1L]]$text))
    expect_identical(blocks[[2L]]$text, "look at this")
    expect_identical(prefix_of(pictured), prefix_of(first))
    expect_true(appended_only(quiet, pictured))
    s$plan_mode <- FALSE
    # Content given as plain blocks keeps them the same way.
    blocks <- ns$.prefix_prompt(list(list(type = "text", text = "p")), "[note]")
    expect_identical(blocks, list(list(type = "text", text = "[note]"),
                                  list(type = "text", text = "p")))

    # What the prefix is made from changes: composed again, and logged.
    # The tools stay: availability was tested for the table, and the
    # repository made above adds no git tool.
    logged <<- list()
    s$system <- "Another system text."
    changed <- ask("new text")
    expect_false(identical(prefix_of(changed)$system, prefix_of(first)$system))
    expect_identical(prefix_of(changed)$tools, prefix_of(first)$tools)
    resets <- Filter(function(x) identical(x$event, "request_prefix_reset"), logged)
    expect_equal(length(resets), 1L)
    expect_true("system" %in% unlist(resets[[1L]]$changed))
})

# ---- 2 jobs in one worker, and a job that may not search -------------------
# The worker's own initializer and job loop, in a child process with the
# same fake provider. The jobs either side of one that may not search
# send the same prefix; that one sends the provider's tools without
# search, and the 2 resets are logged. Slow: skipped on CRAN.
if (tinytest::at_home() || identical(Sys.getenv("NOT_CRAN"), "true")) local({
    root <- tempfile("prefix-worker-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    got <- callr::r(function(root) {
        Sys.setenv(CORTEZA_STATE_DIR = file.path(root, "state"),
                   R_USER_DATA_DIR = file.path(root, "data"),
                   R_USER_CONFIG_DIR = file.path(root, "config"),
                   R_USER_CACHE_DIR = file.path(root, "cache"))
        # corteza's own search tool is offered where a key is set; the
        # provider's is the one these jobs turn off.
        Sys.unsetenv("TAVILY_API_KEY")
        library(corteza)
        bodies <- list()
        resets <- list()
        put <- function(package, name, fn) utils::assignInNamespace(name, fn, package)
        put("llm.api", ".get_provider_config", function(provider) {
            list(provider = provider, api_key = "fake-auth",
                 base_url = "https://example.invalid", chat_path = "/messages",
                 default_model = "fixture")
        })
        setopt <- curl::handle_setopt
        put("curl", "handle_setopt", function(handle, ..., .list = list()) {
            args <- c(list(...), .list)
            if (!is.null(args$postfields)) {
                bodies[[length(bodies) + 1L]] <<- jsonlite::fromJSON(
                    as.character(args$postfields), simplifyVector = FALSE)
            }
            setopt(handle, ..., .list = .list)
        })
        emit <- function(fun, object) {
            fun(charToRaw(paste0("data: ", jsonlite::toJSON(object, auto_unbox = TRUE,
                                                            null = "null"), "\n\n")))
        }
        put("curl", "curl_fetch_stream", function(url, fun, handle) {
            emit(fun, list(type = "message_start", message = list(id = "msg",
                           model = "fixture", usage = list(input_tokens = 1L,
                                                           output_tokens = 0L))))
            emit(fun, list(type = "content_block_start", index = 0L,
                           content_block = list(type = "text", text = "done")))
            emit(fun, list(type = "content_block_stop", index = 0L))
            emit(fun, list(type = "message_delta", delta = list(stop_reason = "end_turn"),
                           usage = list(output_tokens = 1L)))
            list(status_code = 200L, headers = charToRaw("x-request-id: fixture\r\n"))
        })
        put("corteza", "log_event", function(event, ...) {
            if (identical(event, "request_prefix_reset")) {
                resets[[length(resets) + 1L]] <<- list(...)
            }
            invisible(NULL)
        })
        parent <- new_session(provider = "anthropic", system = "fixture",
                              web_search = TRUE)
        parent$cwd <- root
        parent$config <- list(context_edit = list(enabled = FALSE),
                              context_compact_pct = Inf)
        parent$model_map$cloud <- "fixture-model"
        spec <- corteza:::job_worker_spec(parent)
        spec$tools <- c("read_file", "write_file")
        state_dir <- file.path(root, "worker-state")
        corteza:::.job_worker_child_init(root, spec, state_dir)
        job <- function(task, tools = NULL) {
            origin <- if (is.null(tools)) list() else list(delegation = list(tools = tools))
            id <- corteza:::job_create(task, workspace = root, origin = origin)
            corteza:::.job_worker_child_run(id, task, spec, state_dir)
        }
        job("first job")
        job("second job")
        job("may not search", tools = "read_file")
        job("third job")
        list(bodies = bodies, resets = resets)
    }, list(root = root))
    expect_equal(length(got$bodies), 4L)
    b <- got$bodies
    expect_identical(prefix_of(b[[2L]]), prefix_of(b[[1L]]))
    expect_true(appended_only(b[[1L]], b[[2L]]))
    # The one that may not search sends no search tool, and the job
    # after it sends what the jobs before it sent.
    has_search <- function(body) {
        any(vapply(body$tools, function(t) identical(t$name, "web_search"), logical(1)))
    }
    expect_true(has_search(b[[1L]]))
    expect_false(has_search(b[[3L]]))
    expect_identical(prefix_of(b[[4L]]), prefix_of(b[[1L]]))
    # Its own tool limit is checked at the call: its list is the worker's.
    expect_identical(Filter(function(t) !identical(t$name, "web_search"), b[[3L]]$tools),
                     Filter(function(t) !identical(t$name, "web_search"), b[[1L]]$tools))
    reasons <- vapply(got$resets, function(r) if (is.null(r$reason)) "" else r$reason, "")
    expect_identical(reasons, c("job_search_off", "job_search_restored"))
})

# ---- A room session: no exit_plan_mode, room tools listed in any mode -------
local({
    s <- corteza::new_session(channel = "matrix", provider = "anthropic",
                              model_map = list(cloud = "fixture"), web_search = FALSE)
    s$send_room_file <- function(...) NULL
    names_of <- function(p) vapply(p$tools, function(t) t$name, "")
    p <- ns$session_request_prefix(s)
    expect_false("exit_plan_mode" %in% names_of(p))
    expect_false("task_create" %in% names_of(p))
    expect_true("send_room_file" %in% names_of(p))
    # The same prefix, from the cache, whatever the task list says: a
    # room has none.
    s$tasks <- list(list(text = "a", status = "pending"))
    expect_identical(ns$session_request_prefix(s)$key, p$key)
    expect_identical(ns$.prefix_notes(s)$notes, character())
    # A job's worker may not leave plan mode: never offered the tool.
    w <- corteza::new_session(channel = "console", provider = "anthropic",
                              model_map = list(cloud = "fixture"))
    w$plan_mode_locked <- TRUE
    expect_false("exit_plan_mode" %in% names_of(ns$session_request_prefix(w)))
    # A console session with a list of its own, as a monitor has, is not
    # offered it either, until it is in plan mode; then it keeps it.
    m <- corteza::new_session(channel = "console", provider = "anthropic",
                              model_map = list(cloud = "fixture"),
                              tools_filter = c("read_file", "grep_files"))
    expect_true(setequal(names_of(ns$session_request_prefix(m)),
                         c("read_file", "grep_files")))
    m$plan_mode <- TRUE
    expect_true("exit_plan_mode" %in% names_of(ns$session_request_prefix(m)))
    m$plan_mode <- FALSE
    expect_true("exit_plan_mode" %in% names_of(ns$session_request_prefix(m)))
    # A session that starts in plan mode (a subagent takes its parent's)
    # is offered it on any channel, and keeps it when plan mode ends.
    sub <- corteza::new_session(channel = "matrix", provider = "anthropic",
                                model_map = list(cloud = "fixture"), plan_mode = TRUE)
    first <- ns$session_request_prefix(sub)
    expect_true("exit_plan_mode" %in% names_of(first))
    sub$plan_mode <- FALSE
    expect_identical(ns$session_request_prefix(sub)$key, first$key)
})

# ---- The terminal's /clear builds the next session again ----------------------
# From the directory as it is now: a /trust yes since takes effect, and
# what a command set in this process carries over.
local({
    root <- tempfile("rebuild-")
    dir.create(file.path(root, "proj"), recursive = TRUE)
    proj <- normalizePath(file.path(root, "proj"), winslash = "/")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    old_config <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA_character_)
    Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config"))
    on.exit(if (is.na(old_config)) Sys.unsetenv("R_USER_CONFIG_DIR") else
        Sys.setenv(R_USER_CONFIG_DIR = old_config), add = TRUE)
    writeLines("MARK-PROJECT-INSTRUCTIONS", file.path(proj, "AGENTS.md"))
    # Each rebuild reads the briefing again. Hold that input constant so
    # crossing a minute in saber's generated timestamp cannot change it.
    reader <- ns$load_saber_briefing
    utils::assignInNamespace("load_saber_briefing", function(cwd) "Fixture briefing", "corteza")
    on.exit(utils::assignInNamespace("load_saber_briefing", reader, "corteza"), add = TRUE)
    s <- ns$session_setup("console", cwd = proj, provider = "anthropic",
                          model = "fixture", validate_api_key = FALSE)
    expect_false(isTRUE(s$project_trusted))
    expect_false(grepl("MARK-PROJECT-INSTRUCTIONS", s$system, fixed = TRUE))
    old_table <- s$tool_table
    s$config$approval_mode <- "deny"
    s$config$dry_run <- TRUE
    ns$project_trust_add(proj)
    ctx <- new.env(parent = emptyenv())
    ctx$session <- s
    ctx$cwd <- proj
    ctx$config <- s$config
    ctx$config_set <- "approval_mode"
    s$.prefix_seen <- list(plan_mode = TRUE, auto = FALSE, tasks = list())
    composed <- ns$session_request_prefix(s)
    expect_false(is.null(s$.prefix_base))
    ns$.repl_session_rebuild(ctx)
    expect_null(s$.prefix_base)
    expect_null(s$.request_prefix)
    expect_false(identical(ns$session_request_prefix(s)$system, composed$system))
    expect_true(isTRUE(s$project_trusted))
    expect_true(grepl("MARK-PROJECT-INSTRUCTIONS", s$system, fixed = TRUE))
    expect_false(identical(s$tool_table, old_table))
    # Set by a command: kept. Not set by one: the config's again.
    expect_identical(s$config$approval_mode, "deny")
    expect_false(isTRUE(s$config$dry_run))
    expect_identical(ctx$config, s$config)
    expect_null(s$.prefix_seen)

    # A talker keeps its guidance (Codex, #293 review): it is a source of
    # the session's context, ordered last, so a rebuild renders it where
    # the talker put it, once, and the manifest lists it.
    guidance <- trimws(ns$TALKER_GUIDANCE, which = "right")
    times <- function(text) {
        lengths(regmatches(text, gregexpr(guidance, text, fixed = TRUE)))
    }
    ns$talker_enable(s, list(enabled = TRUE))
    talker_system <- s$system
    expect_true(endsWith(talker_system, guidance))
    for (i in 1:2) {
        ns$.repl_session_rebuild(ctx)
        expect_equal(times(s$system), 1L, info = i)
        expect_identical(s$system, talker_system, info = i)
        expect_identical(s$system, saber::context_render(s$context_manifest),
                         info = i)
    }
})
