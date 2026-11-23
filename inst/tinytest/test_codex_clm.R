source("isolate.R", local = TRUE)

# Run the Codex agent loop and its serializer. Only the SSE transport is fake.
local({
    ns <- asNamespace("corteza")
    wire <- asNamespace("llm.api")
    old <- wire$.openai_codex_post_sse
    vars <- c("OPENAI_CODEX_ACCESS_TOKEN", "OPENAI_CODEX_ACCOUNT_ID")
    prev <- Sys.getenv(vars, unset = NA_character_)
    on.exit({
        assignInNamespace(".openai_codex_post_sse", old, "llm.api")
        Sys.unsetenv(vars)
        if (any(!is.na(prev))) do.call(Sys.setenv, as.list(prev[!is.na(prev)]))
        ns$.fallback_reset()
    }, add = TRUE)
    Sys.setenv(OPENAI_CODEX_ACCESS_TOKEN = "fixture", OPENAI_CODEX_ACCOUNT_ID = "fixture")
    ns$.fallback_reset()
    s <- corteza::new_session(provider = "openai_codex", system = "Fixture",
        model_map = list(cloud = "fixture"), web_search = FALSE,
        context_edit = list(enabled = TRUE, strategy = "clm", cadence = "always"))
    ns$journal_install(s, tempfile("codex-journal-"))
    tool <- function(id, name, args) {
        list(type = "function_call", id = paste0("fc_", id), call_id = id,
             name = name, arguments = args)
    }
    message_number <- 0L
    answer <- function(text) {
        message_number <<- message_number + 1L
        list(type = "message", id = paste0("msg_", message_number), role = "assistant", phase = "final_answer",
             content = list(list(type = "output_text", text = text)))
    }
    s$history <- list(list(role = "user", content = "Keep both files."),
        list(type = ".openai_codex_output", output = list(
            tool("keep", "read_file", "{}"), tool("retire", "read_file", "{}"))),
        list(type = "function_call_output", call_id = "keep", output = "One"),
        list(type = "function_call_output", call_id = "retire", output = "Two"),
        list(type = ".openai_codex_output", output = list(
            list(type = "reasoning", encrypted_content = "stale"), answer("Old reply"))))
    requests <- list()
    assignInNamespace(".openai_codex_post_sse", function(url, body, headers, on_delta = NULL) {
        requests[[length(requests) + 1L]] <<- body
        output <- if (length(requests) == 1L) {
            result <- Filter(function(i) identical(i$call_id, "retire") &&
                identical(i$type, "function_call_output"), body$input)[[1L]]
            id <- ns$.context_edit_label(result$output, "r")
            message <- Filter(function(i) identical(i$type, "message"), body$input)[[1L]]
            mid <- ns$.context_edit_label(message$content[[1L]]$text, "m")
            list(tool("edit", "edit_context", as.character(jsonlite::toJSON(
                list(op = "drop", ids = list(id)), auto_unbox = TRUE))),
                 tool("shorten", "edit_context", as.character(jsonlite::toJSON(
                     list(op = "replace", ids = list(mid), text = "Edited reply"), auto_unbox = TRUE))))
        } else list(answer("Done"))
        list(status = "completed", output = output,
             usage = list(input_tokens = 25L, output_tokens = 5L))
    }, "llm.api")
    expect_identical(corteza::turn("Continue.", s, tools = list())$reply, "Done")
    expect_equal(length(requests), 2L)
    items <- requests[[2L]]$input
    ids <- vapply(items, function(x) if (is.null(x$call_id)) "" else x$call_id, "")
    expect_equal(sum(ids == "keep"), 2L)
    expect_equal(sum(ids == "retire"), 0L)
    expect_equal(sum(ids == "edit"), 2L)
    txt <- as.character(ns$.history_json(items))
    expect_true(grepl("Context edit applied.", txt, fixed = TRUE))
    expect_false(grepl("stale", txt, fixed = TRUE))
    expect_true(grepl("Edited reply", txt, fixed = TRUE))
    expect_false(grepl("Old reply", txt, fixed = TRUE))
    expect_true(grepl("final_answer", txt, fixed = TRUE))
    expect_true(grepl("Keep both files.", txt, fixed = TRUE))

    # Use the approval worker's normal turn and wire format. Its first
    # ruling makes a CLM note. The next ruling sees both the note and
    # the person's revocation, with the original instruction present once.
    child <- ns$.subagent_state
    worker <- ns$.job_worker_state
    old_session <- child$session
    keys <- c("review_record", "monitor_run_fn", "monitor_scope", "conversation", "approval_timeout")
    old_worker <- mget(keys, worker, ifnotfound = rep(list(NULL), length(keys)))
    on.exit({
        child$session <- old_session
        list2env(old_worker, worker)
    }, add = TRUE)
    worker$review_record <- worker$monitor_run_fn <- NULL
    spec <- ns$job_worker_spec(s, "monitor")
    child$session <- corteza::new_session(provider = "openai_codex",
        model_map = list(cloud = "fixture"), system = spec$system, web_search = FALSE,
        context_edit = spec$context_edit, tools_filter = spec$tools)
    ns$journal_install(child$session, tempfile("reviewer-journal-"))
    requests <- list()
    assignInNamespace(".openai_codex_post_sse", function(url, body, headers, on_delta = NULL) {
        requests[[length(requests) + 1L]] <<- body
        output <- switch(length(requests),
            list(tool("note", "edit_context", '{"op":"note","text":"Verify scope."}')),
            list(answer("REQUEST: r1\nVERDICT: approve\nREASON: within scope")),
            list(answer("REQUEST: r2\nVERDICT: refuse\nREASON: permission revoked")))
        list(status = "completed", output = output,
             usage = list(input_tokens = 25L, output_tokens = 5L))
    }, "llm.api")
    lines <- c("Person: change the title.", "Person: stop changing the title.")
    q <- list(request_id = "r1", record = "test", conversation = lines[1L],
              tool = "write_file", args = list(path = "example.txt"))
    first <- ns$.job_monitor_child_ask(q, "review:r1")
    expect_null(first$error)
    expect_identical(first$window$id, "test")
    q$request_id <- "r2"
    q$conversation <- lines
    second <- ns$.job_monitor_child_ask(q, "review:r2")
    expect_null(second$error)
    expect_equal(length(requests), 3L)
    expect_true(grepl("VERDICT: refuse", second$reply, fixed = TRUE))
    body <- as.character(ns$.history_json(requests[[3L]]$input))
    for (needle in c(lines, "[context note]")) {
        expect_identical(lengths(regmatches(body, gregexpr(needle, body, fixed = TRUE))), 1L)
    }

    # The child reads its own journal setting. Each call uses a fresh
    # complete transcript when that setting is off.
    spec$init_fn <- function(spec) {
        child$session <- corteza::new_session(provider = "openai_codex",
            model_map = list(cloud = "fixture"), system = spec$system,
            web_search = FALSE, tools_filter = spec$tools, context_edit = spec$context_edit)
        child$session$config <- list(journal_enabled = FALSE)
    }
    ns$.job_worker_child_session(tempdir(), spec)
    requests <- list()
    assignInNamespace(".openai_codex_post_sse", function(url, body, headers, on_delta = NULL) {
        requests[[length(requests) + 1L]] <<- body
        list(status = "completed", output = list(answer("REQUEST: r2\nVERDICT: refuse\nREASON: stopped")),
             usage = list(input_tokens = 25L, output_tokens = 5L))
    }, "llm.api")
    for (i in 1:2) {
        result <- ns$.job_monitor_child_ask(q, "review:r2")
        expect_null(result$error)
        expect_null(result$window)
        expect_true(grepl("VERDICT: refuse", result$reply, fixed = TRUE))
    }
    expect_equal(length(requests), 2L)
    expect_false("edit_context" %in% vapply(requests[[2L]]$tools, function(t) t$name, ""))
    body <- as.character(ns$.history_json(requests[[2L]]$input))
    for (needle in lines) {
        expect_identical(lengths(regmatches(body, gregexpr(needle, body, fixed = TRUE))), 1L)
    }
})
