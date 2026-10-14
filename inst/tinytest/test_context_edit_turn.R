# Exercise turn() with the real dispatcher, checkpoint and journal, replacing
# only the remote model. No API keys, services, or live user state required.
local({
    original <- llm.api::agent
    on.exit(assignInNamespace("agent", original, "llm.api"), add = TRUE)
    old_log <- corteza:::set_log_enabled(FALSE)
    on.exit(corteza:::set_log_enabled(old_log), add = TRUE)
    dirs <- character()
    on.exit(unlink(dirs, recursive = TRUE), add = TRUE)
    seen <- new.env()
    seen$calls <- 0L
    fake <- function(prompt, tools, tool_handler, system, model, provider,
                     max_turns, verbose, history, history_callback = NULL,
                     checkpoint_callback = NULL, ...) {
        seen$calls <- seen$calls + 1L
        seen$system <- system
        seen$tools <- tools
        h <- c(history, list(list(role = "user", content = prompt)))
        append_round <- function(id, name, args) {
            if (provider == "anthropic") {
                h <<- c(h, list(list(role = "assistant", content = list(list(
                    type = "tool_use", id = id, name = name, input = args)))))
            } else {
                h <<- c(h, list(list(role = "assistant", content = "", tool_calls = list(list(
                    id = id, type = "function", `function` = list(name = name,
                        arguments = jsonlite::toJSON(args, auto_unbox = TRUE)))))))
            }
            history_callback(h)
            value <- tool_handler(name, args, context = list(agent_turn = length(h),
                call_index = 1L, call_count = 1L, assistant_text = "", provider = provider))
            if (provider == "anthropic") {
                h <<- c(h, list(list(role = "user", content = list(list(
                    type = "tool_result", tool_use_id = id, content = value)))))
            } else {
                h <<- c(h, list(list(role = "tool", tool_call_id = id, content = value)))
            }
            history_callback(h)
            replacement <- checkpoint_callback(h, list(agent_turn = length(h), usage = list()))
            if (!is.null(replacement)) {
                h <<- replacement$history
                history_callback(h)
            }
            value
        }
        result <- append_round("read1", "read_file", list(path = "synthetic"))
        seen$read <- result
        # The tool result ID is taken from the model-visible output itself.
        id <- sub("^\\[([^]]+)\\].*", "\\1", result)
        append_round("edit1", "edit_context", list(op = "replace", ids = id, text = "Verified: 3 rows."))
        h <- c(h, list(list(role = "assistant", content = "done")))
        history_callback(h)
        list(content = "done", history = h, usage = list(), turns = 3L)
    }
    assignInNamespace("agent", fake, "llm.api")
    for (provider in c("anthropic", "openai")) {
        for (channel in c("cli", "console", "matrix")) {
            s <- corteza::new_session(provider = provider, channel = channel,
                system = "KEEP THIS INSTRUCTION", web_search = FALSE,
                context_edit = list(enabled = TRUE, cadence = "always"))
            s$config <- list(context_compact_pct = 1000)
            s$context_window <- 100000L
            d <- tempfile("context-edit-turn-")
            dirs <- c(dirs, d)
            corteza:::journal_install(s, dir = d)
            executed <- 0L
            notified <- 0L
            s$on_context_edit <- function(event) notified <<- notified + 1L
            # Editing is intercepted, so it never reaches the external executor
            # or an approval callback, including in Matrix and plan mode.
            s$approval_cb <- function(...) TRUE
            s$plan_mode <- TRUE
            out <- corteza::turn("Inspect it", s, tools = list(list(name = "read_file",
                description = "read", input_schema = list(type = "object"))),
                tool_executor = function(name, args) {
                    stopifnot(name == "read_file")
                    executed <<- executed + 1L
                    list(content = list(list(type = "text", text = paste(rep("row", 100), collapse = " "))))
                })
            expect_equal(out$reply, "done")
            expect_equal(executed, 1L)
            expect_equal(notified, 1L)
            expect_true(startsWith(seen$read, "[r1]\n"))
            expect_true(any(vapply(seen$tools, function(t) identical(t$name, "edit_context"), logical(1))))
            expect_true(startsWith(seen$system, "KEEP THIS INSTRUCTION"))
            expect_identical(s$system, "KEEP THIS INSTRUCTION")
            calls <- llm.api::history_tool_calls(s$history)
            expect_true(grepl("Verified: 3 rows", calls[[1]]$result))
            expect_true(grepl("applied", calls[[2]]$result))
            original_calls <- llm.api::history_tool_calls(corteza:::journal_history(d))
            expect_true(grepl("row row row", original_calls[[1]]$result))
            expect_false(grepl("Verified: 3 rows", original_calls[[1]]$result))
        }
    }
    expect_equal(seen$calls, 6L)
})

# Provider fallback must remove the edit schema on the Responses shape and
# journal the bridge before native messages disappear from the live window.
local({
    corteza:::.fallback_reset()
    on.exit(corteza:::.fallback_reset(), add = TRUE)
    s <- corteza::new_session(provider = "anthropic", system = "instructions",
        context_edit = list(enabled = TRUE, cadence = "always"))
    s$fallback <- "test-model openai_codex"
    s$history <- list(list(role = "user", content = "question"),
        list(role = "assistant", content = list(list(type = "tool_use", id = "a",
            name = "read_file", input = list(path = "x")))),
        list(role = "user", content = list(list(type = "tool_result", tool_use_id = "a", content = "original"))),
        list(role = "assistant", content = "reply before fallback"))
    original_history <- s$history
    dir <- tempfile("context-edit-fallback-")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    corteza:::journal_install(s, dir = dir)
    corteza:::journal_snapshot(s)
    s$context_edit_base_system <- s$system
    s$context_edit_system <- corteza:::.context_edit_system(s$system, s)
    attempts <- 0L
    result <- corteza:::.agent_with_fallback(list(model = "test-primary", provider = "anthropic",
        prompt = "next", history = s$history, tools = list(corteza:::.context_edit_schema()),
        system = s$context_edit_system), s, .call = function(args) {
            attempts <<- attempts + 1L
            if (args$provider == "anthropic") stop("API error (429): rate limit")
            expect_equal(length(args$tools), 0L)
            expect_identical(args$system, "instructions")
            expect_false(corteza:::.context_edit_enabled(s))
            s$history <- c(args$history, list(list(role = "user", content = args$prompt)))
            corteza:::journal_snapshot(s)
            list(content = "fallback reply", history = s$history)
        })
    expect_equal(attempts, 2L)
    expect_identical(corteza:::journal_history(dir),
        c(original_history, list(list(role = "user", content = "next"))))
    expect_null(s$context_edit_provider)
})

# Runtime controls share one parser/action between CLI and rooms. Config merges
# the fields independently, and per-session settings take precedence.
local({
    root <- tempfile("context-edit-controls-")
    project <- file.path(root, "project")
    dir.create(file.path(project, ".corteza"), recursive = TRUE)
    previous <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA_character_)
    Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config"))
    old_cwd <- setwd(project)
    on.exit({
        setwd(old_cwd)
        if (is.na(previous)) Sys.unsetenv("R_USER_CONFIG_DIR") else Sys.setenv(R_USER_CONFIG_DIR = previous)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    global <- corteza:::corteza_config_path("config.json")
    dir.create(dirname(global), recursive = TRUE)
    writeLines('{"context_edit":{"enabled":true,"cadence":"always"}}', global)
    writeLines('{"context_edit":{"enabled":false}}', file.path(project, ".corteza", "config.json"))
    cfg <- corteza:::load_config(project)
    expect_identical(cfg$context_edit, list(enabled = FALSE, cadence = "always"))
    s <- corteza::new_session(context_edit = TRUE)
    s$config <- cfg
    expect_true(corteza:::.context_edit_enabled(s))
    expect_true(grepl("off", corteza:::.context_edit_command(s, "off")))
    expect_false(corteza:::.context_edit_enabled(s))
    for (body in c("/context-edit on", "//context-edit on", "@bot context-edit on")) {
        arg <- corteza:::.context_edit_parse_command(body)
        expect_equal(arg, "on")
        expect_true(grepl("on", corteza:::.context_edit_command(s, arg)))
    }
    expect_null(corteza:::.context_edit_parse_command("please context-edit on"))
    expect_equal(corteza:::.context_edit_parse_command("/context-edit"), "")
    expect_true(grepl("Usage", corteza:::.context_edit_command(s, "on extra")))
    s$provider <- "openai_codex"
    expect_true(grepl("unavailable", corteza:::.context_edit_command(s, "status")))

    expressions <- parse(system.file("bin", "corteza", package = "corteza"))
    definition <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
        identical(e[[2]], as.name("parse_args")), expressions)
    expect_equal(length(definition), 1L)
    env <- new.env(parent = globalenv())
    env$commandArgs <- function(...) "--context-edit"
    eval(definition[[1]], env)
    expect_true(env$parse_args()$context_edit)
    env$commandArgs <- function(...) character()
    expect_null(env$parse_args()$context_edit)

    # Drive the actual shared REPL dispatcher, not only the action helper.
    s$provider <- "anthropic"
    ctx <- new.env(parent = emptyenv())
    ctx$session <- s
    ctx$config <- cfg
    ctx$ws_enabled <- FALSE
    ctx$palette <- list(dim = "", reset = "")
    lines <- c("/context-edit off", "/context-edit status", "/context-edit on")
    n <- 0L
    ctx$read_input <- function(...) {
        n <<- n + 1L
        if (n <= length(lines)) lines[[n]] else character()
    }
    output <- capture.output(corteza:::run_repl_loop(ctx))
    expect_true(any(grepl("Context editing: off", output)))
    expect_true(any(grepl("Context editing: on", output)))
    expect_true(s$context_edit$enabled)
    expect_true(ctx$config$context_edit$enabled)

    # The real child initializer accepts an override without changing its cwd
    # config, so a parent can pass its effective setting across the process.
    child_state <- corteza:::.subagent_state
    saved_child <- as.list(child_state)
    on.exit({
        rm(list = ls(child_state, all.names = TRUE), envir = child_state)
        list2env(saved_child, envir = child_state)
    }, add = TRUE)
    corteza::subagent_turn_init(tools_filter = "read_file", system = "child",
        context_edit = list(enabled = TRUE, cadence = "always"))
    expect_true(corteza:::.context_edit_enabled(child_state$session))
    expect_equal(corteza:::.context_edit_config(child_state$session$config,
        child_state$session$context_edit)$cadence, "always")
    expect_false(child_state$session$config$context_edit$enabled)
})
