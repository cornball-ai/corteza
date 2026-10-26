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

# Two unaddressed image posts followed by addressed text must reach the model,
# and a later turn must still receive both images after context indexing.
local({
    original <- llm.api::agent
    on.exit(assignInNamespace("agent", original, "llm.api"), add = TRUE)
    root <- tempfile("context-image-turn-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    requests <- list()
    assignInNamespace("agent", function(prompt, history, provider,
                                         history_callback, ...) {
        h <- c(history, list(list(role = "user", content = prompt)))
        # Exercise the real provider conversion and JSON boundary. Only the
        # remote response is synthetic; no key, image file or API is needed.
        wire <- llm.api:::.llm_blocks(h, provider)
        requests[[length(requests) + 1L]] <<- jsonlite::fromJSON(
            jsonlite::toJSON(wire, auto_unbox = TRUE), simplifyVector = FALSE)
        h <- c(h, list(list(role = "assistant", content = "I can see both images.")))
        history_callback(h)
        list(content = "I can see both images.", history = h, usage = list(), turns = 1L)
    }, "llm.api")
    image <- llm.api::llm_image(data = as.raw(1:4), mime = "image/png")
    for (provider in c("anthropic", "openai")) {
        s <- corteza::new_session(provider = provider, channel = "matrix",
            system = "Fixture", web_search = FALSE,
            context_edit = list(enabled = TRUE, cadence = "always"))
        s$config <- list(context_compact_pct = 1000)
        s$context_window <- 100000L
        s$context_conversation <- TRUE
        s$context_archive_vault <- root
        s$history <- list(list(role = "user", content = llm.api::llm_content("First", image)),
                          list(role = "user", content = llm.api::llm_content("Second", image)))
        corteza:::journal_install(s, file.path(root, provider))
        for (prompt in c("What is shown?", "Tell me more.")) {
            reply <- corteza::turn(prompt, s, tools = list())
            expect_identical(reply$reply, "I can see both images.")
            wire <- tail(requests, 1)[[1]]
            for (i in 1:2) {
                blocks <- Filter(function(b) b$type %in% c("image", "image_url"), wire[[i]]$content)
                expect_equal(length(blocks), 1L)
                expect_inherits(s$history[[i]]$content, "llm_content")
            }
            expect_identical(corteza:::journal_load(s$journal_dir)$live$history, s$history)
        }
    }
    expect_equal(length(requests), 4L)
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

    # File configuration errors warn and disable the feature. Explicit
    # programmatic overrides remain strict (covered in test_context_edit.R).
    for (bad in c('"typo"', '{"enabled":"yes"}', '{"cadence":"sometimes"}')) {
        writeLines(paste0('{"context_edit":', bad, ',"port":1234}'),
            file.path(project, ".corteza", "config.json"))
        expect_warning(bad_cfg <- corteza:::load_config(project), "Invalid context_edit")
        expect_identical(bad_cfg$context_edit, list(enabled = FALSE, cadence = "pressure"))
        expect_equal(bad_cfg$port, 1234L)
    }
})

# Use the real llm.api agent loop, replacing only its provider request and
# credential lookup. This exercises dispatch indices, per-result callbacks,
# same-name/same-argument calls, multiple edit receipts and the next request's
# native history, including synthetic signed thinking blocks.
local({
    originals <- lapply(c(".agent_anthropic", ".agent_openai", ".get_provider_config"),
        get, envir = asNamespace("llm.api"))
    names(originals) <- c(".agent_anthropic", ".agent_openai", ".get_provider_config")
    on.exit({
        for (name in names(originals)) assignInNamespace(name, originals[[name]], "llm.api")
    }, add = TRUE)
    assignInNamespace(".get_provider_config", function(...) list(), "llm.api")
    dirs <- character()
    on.exit(unlink(dirs, recursive = TRUE), add = TRUE)
    for (provider in c("anthropic", "openai")) {
        round <- 0L
        checked <- 0L
        reads <- 0L
        visible_ids <- character()
        previous_assistant <- NULL
        fake <- function(messages, tools, system, model, config, ...) {
            round <<- round + 1L
            if (provider == "anthropic") {
                expect_equal(list(...)$thinking_budget_tokens, 1024L)
                expect_identical(messages[[1L]]$role, "user")
            }
            calls <- llm.api::history_tool_calls(messages)
            if (round == 2L) {
                expect_equal(length(calls), 2L)
                results <- vapply(calls, `[[`, character(1), "result")
                visible_ids <<- sub("^\\[([^]]+)\\].*", "\\1", results)
                expect_equal(length(unique(visible_ids)), 2L)
                expect_true(grepl("output 1", results[[1L]]))
                expect_true(grepl("output 2", results[[2L]]))
                requested <- list(
                    list(id = "bad", name = "edit_context", arguments = list(op = "drop", ids = "unknown")),
                    list(id = "drop-a", name = "edit_context", arguments = list(op = "drop", ids = visible_ids[[1L]])),
                    list(id = "note", name = "edit_context", arguments = list(op = "note", text = "Two reads completed.")))
            } else if (round == 3L) {
                expect_identical(vapply(calls, `[[`, character(1), "id"), c("read-b", "bad", "drop-a", "note"))
                expect_true(startsWith(calls[[1L]]$result, paste0("[", visible_ids[[2L]], "]\n")))
                expect_true(grepl("rejected", calls[[2L]]$result))
                expect_true(all(vapply(calls[3:4], function(call) grepl("applied", call$result), logical(1))))
                boundary <- corteza:::.context_edit_boundary(messages)
                expect_identical(messages[[boundary]], previous_assistant)
                # After Anthropic's same-role merge, no added text can precede
                # the current assistant's original leading thinking block.
                expect_false(identical(messages[[boundary - 1L]]$role, "assistant"))
                texts <- vapply(messages, corteza:::.compact_entry_body, character(1))
                note_index <- which(startsWith(texts, "[context note]\n"))
                expect_equal(length(note_index), 1L)
                expect_identical(messages[[note_index + 1L]], list(role = "user", content = "Inspect both files."))
                checked <<- checked + 1L
                requested <- list(list(id = "drop-b", name = "edit_context",
                    arguments = list(op = "drop", ids = visible_ids[[2L]])))
            } else if (round == 1L) {
                requested <- lapply(c("read-a", "read-b"), function(id) {
                    list(id = id, name = "read_file", arguments = list(path = "same-file"))
                })
            } else {
                expect_false(any(vapply(calls, function(call) call$id %in% c("read-a", "read-b"), logical(1))))
                return(list(text = "done", assistant_message = list(role = "assistant", content = "done"), tool_calls = list()))
            }
            if (provider == "anthropic") {
                assistant <- list(role = "assistant", content = c(list(
                    list(type = "thinking", thinking = "synthetic fixture", signature = "fixture-signature"),
                    list(type = "redacted_thinking", data = "fixture-data")),
                    lapply(requested, function(call) list(type = "tool_use", id = call$id,
                        name = call$name, input = call$arguments))))
            } else {
                assistant <- list(role = "assistant", content = "", tool_calls = lapply(requested, function(call) {
                    list(id = call$id, type = "function", `function` = list(name = call$name,
                        arguments = jsonlite::toJSON(call$arguments, auto_unbox = TRUE)))
                }))
            }
            previous_assistant <<- assistant
            list(text = "", assistant_message = assistant, tool_calls = requested)
        }
        assignInNamespace(if (provider == "anthropic") ".agent_anthropic" else ".agent_openai", fake, "llm.api")
        s <- corteza::new_session(provider = provider, system = "instructions", web_search = FALSE,
            thinking_budget_tokens = if (provider == "anthropic") 1024L else NULL,
            context_edit = list(enabled = TRUE, cadence = "always"))
        s$config <- list(context_compact_pct = 1000)
        s$context_window <- 100000L
        dir <- tempfile("context-edit-real-loop-")
        dirs <- c(dirs, dir)
        corteza:::journal_install(s, dir = dir)
        s$approval_cb <- function(...) TRUE
        result <- corteza::turn("Inspect both files.", s,
            tools = list(list(name = "read_file", description = "Read", input_schema = list(type = "object"))),
            tool_executor = function(name, args) {
                stopifnot(name == "read_file")
                reads <<- reads + 1L
                list(content = list(list(type = "text", text = paste("output", reads))))
            })
        expect_equal(round, 4L)
        expect_equal(checked, 1L)
        expect_equal(reads, 2L)
        expect_equal(result$reply, "done")
        bodies <- vapply(s$history, corteza:::.compact_entry_body, character(1))
        expect_equal(sum(startsWith(bodies, "Context workspace:")), 1L)
        record <- llm.api::history_tool_calls(corteza:::journal_history(dir))
        expect_identical(vapply(record[1:2], `[[`, character(1), "id"), c("read-a", "read-b"))
        expect_true(all(vapply(record, `[[`, logical(1), "completed")))
    }
})

# Indexing at turn start must not disable holders. Exercise the actual archival
# collapse and journal with only holder creation replaced by an offline fake.
local({
    original <- corteza:::archival_archive_turn
    on.exit(assignInNamespace("archival_archive_turn", original, "corteza"), add = TRUE)
    created <- 0L
    received <- NULL
    assignInNamespace("archival_archive_turn", function(history_slice, ...) {
        created <<- created + 1L
        received <<- history_slice
        list(subagent_id = "fixture-holder", summary = "Archived fixture.")
    }, "corteza")
    dir <- tempfile("context-edit-archival-")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    before <- list(list(role = "user", content = "earlier prompt"),
        list(role = "assistant", content = "earlier reply"))
    s <- corteza::new_session(history = before, context_edit = list(enabled = TRUE, cadence = "always"))
    corteza:::journal_install(s, dir = dir)
    mark <- corteza:::journal_mark(s)
    corteza:::.context_edit_prepare(s)
    expect_true(corteza:::journal_rewritten_since(s, mark))
    expect_false(corteza:::journal_rewritten_since(s, mark, ignore = "context_index"))
    indexed <- s$history
    added <- list(list(role = "user", content = "next prompt"), list(role = "assistant", content = "next reply"))
    s$history <- c(s$history, added)
    corteza:::journal_snapshot(s)
    cfg <- list(archival = list(enabled = TRUE, trigger = list(token_threshold = 1L)))
    corteza:::maybe_archive_turn(s, "next prompt", length(before), list(), cfg, "fixture-session", mark = mark)
    expect_equal(created, 1L)
    expect_identical(received, added)
    expect_identical(s$history[seq_along(indexed)], indexed)
    expect_true(startsWith(tail(s$history, 1)[[1]]$content, "[archived turn]"))
    expect_identical(corteza:::journal_history(dir), c(before, added))
    expect_true(corteza:::journal_rewritten_since(s, mark, ignore = "context_index"))

    # A structural rewrite after an index segment still prevents a second
    # collapse, because the old live offset no longer identifies the turn.
    corteza:::maybe_archive_turn(s, "next prompt", length(before), list(), cfg, "fixture-session", mark = mark)
    expect_equal(created, 1L)
})
