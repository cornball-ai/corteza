if (!"request_callback" %in% names(formals(llm.api::agent))) exit_file("older llm.api: request coverage unavailable")
local({
    root <- tempfile("audit-requests-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    saved <- list()
    replace <- function(package, name, fn) {
        key <- paste(package, name)
        saved[[key]] <<- list(package = package, name = name, fn = get(name, asNamespace(package)))
        utils::assignInNamespace(name, fn, package)
    }
    on.exit(for (item in saved) utils::assignInNamespace(item$name, item$fn, item$package), add = TRUE)
    replace("llm.api", ".get_provider_config", function(provider) list(provider = provider,
        api_key = "fake-auth", base_url = "https://example.invalid", chat_path = "/messages",
        default_model = "fixture"))
    requests <- list()
    payload <- NULL
    setopt <- curl::handle_setopt
    replace("curl", "handle_setopt", function(handle, ..., .list = list()) {
        args <- c(list(...), .list)
        if (!is.null(args$postfields)) {
            payload <<- as.character(args$postfields)
            requests[[length(requests) + 1L]] <<- payload
        }
        setopt(handle, ..., .list = .list)
    })
    calls <- 0L
    mode <- "normal"
    emit <- function(fun, object) fun(charToRaw(paste0("data: ",
        jsonlite::toJSON(object, auto_unbox = TRUE, null = "null"), "\n\n")))
    replace("curl", "curl_fetch_stream", function(url, fun, handle) {
        calls <<- calls + 1L
        if (mode == "fallback" && calls == 2L) stop("API error (429): fixture limit")
        emit(fun, list(type = "message_start", message = list(id = paste0("msg", calls),
            model = "served-fixture", usage = list(input_tokens = 7L, output_tokens = 0L))))
        if (calls == 1L) {
            for (i in 0:1) {
                emit(fun, list(type = "content_block_start", index = i,
                    content_block = list(type = "tool_use", id = paste0("call", i), name = "read_file",
                        input = list(path = paste0("fixture", i)))))
                emit(fun, list(type = "content_block_stop", index = i))
            }
        } else {
            emit(fun, list(type = "content_block_start", index = 0L,
                content_block = list(type = "text", text = "done")))
        }
        emit(fun, list(type = "message_delta", delta = list(stop_reason = if (calls == 1L) "tool_use" else "end_turn"),
            usage = list(output_tokens = 3L)))
        list(status_code = 200L, headers = charToRaw("x-request-id: fixture\r\n"))
    })
    replace("curl", "curl_fetch_memory", function(url, handle) {
        calls <<- calls + 1L
        list(status_code = 200L, headers = charToRaw("x-request-id: fallback\r\n"),
            content = charToRaw('{"id":"fallback","model":"served-openai","choices":[{"message":{"role":"assistant","content":"fallback done"},"finish_reason":"stop"}],"usage":{"prompt_tokens":5,"completion_tokens":2}}'))
    })
    opt <- options(corteza.policy = function(call) list(model = "cloud", approval = "allow", reason = "fixture"))
    on.exit(options(opt), add = TRUE)
    s <- corteza::new_session(provider = "anthropic", model_map = list(cloud = "fixture"), web_search = FALSE)
    s$audit_dir <- root
    s$journal_dir <- file.path(root, "journal")
    corteza:::journal_install(s, dir = s$journal_dir)
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = Inf,
        tool_output_caps = list(read_file = list(max_chars = 100, max_lines = 20)))
    raw <- paste(rep('token = "fixtureSecret123456789"', 100), collapse = "\n")
    executed <- 0L
    result <- corteza::turn("fixture", s, tools = list(), tool_executor = function(...) {
        executed <<- executed + 1L
        corteza:::ok(raw)
    })
    expect_equal(calls, 2L)
    expect_equal(executed, 2L)
    expect_equal(result$usage$total_tokens, 20)
    rows <- corteza:::auto_log_read(result$audit$stream)$records
    select <- function(type) Filter(function(x) identical(x$type, type), rows)
    starts <- select("request_start")
    expect_length(starts, 2L)
    expect_identical(starts[[1L]]$manifest$request$hash,
        digest::digest(charToRaw(requests[[1L]]), "sha256", serialize = FALSE))
    for (i in 1:2) expect_identical(corteza::audit_payload_read(starts[[i]]$manifest$request, FALSE), requests[[i]])
    expect_identical(select("turn_end")[[1L]]$request_coverage, "complete")
    expect_equal(select("turn_end")[[1L]]$usage$total_tokens, 20)
    expect_true(all(vapply(select("request_end"), function(x) identical(x$usage_source, "breakdown"), TRUE)))
    captures <- select("tool_payload")
    expect_length(captures, 2L)
    expect_identical(corteza::audit_payload_read(captures[[1L]]$full, FALSE), raw)
    expect_false(grepl("fixtureSecret123456789", corteza::audit_payload_read(captures[[1L]]$full), fixed = TRUE))
    expect_true(captures[[1L]]$truncated)
    expect_false(identical(captures[[1L]]$full$hash, captures[[1L]]$admitted$hash))
    # Both native result blocks survive in the second immutable request even
    # after an in-place mutation and journaled CLM/compaction replacements.
    body <- jsonlite::fromJSON(requests[[2L]], simplifyVector = FALSE)
    expect_length(body$messages[[3L]]$content, 2L)
    for (reason in c("context_edit", "threshold")) {
        before <- s$history
        after <- list(list(role = "user", content = paste("summary", reason)))
        corteza:::journal_rewrite(s, before, after, reason)
        s$history <- after
    }
    s$history[[1L]]$content <- "later mutation"
    expect_identical(corteza::audit_payload_read(starts[[2L]]$manifest$request, FALSE), requests[[2L]])
    expect_identical(corteza::audit_payload_read(starts[[2L]]$manifest$messages[[3L]], FALSE), body$messages[[3L]])
    expect_length(corteza:::journal_load(s$journal_dir)$segments, 2L)

    # Earlier requests served by Anthropic remain counted after an OpenAI
    # fallback. Its 429 attempt is unknown usage, never fabricated zero spend.
    corteza:::.fallback_reset()
    on.exit(corteza:::.fallback_reset(), add = TRUE)
    calls <- 0L
    mode <- "fallback"
    corteza:::journal_rewrite(s, s$history, list(), "fixture_reset")
    s$history <- list()
    s$fallback <- "fixture openai"
    recovered <- corteza::turn("fallback", s, tools = list(), tool_executor = function(...) corteza:::ok("result"))
    rows <- corteza:::auto_log_read(recovered$audit$stream)$records
    rows <- Filter(function(x) identical(x$turn_id, recovered$audit$turn_id), rows)
    ends <- select("request_end")
    expect_equal(calls, 3L)
    expect_identical(vapply(ends, `[[`, "", "provider"), c("anthropic", "openai"))
    expect_equal(recovered$usage$total_tokens, 17)
    expect_identical(recovered$usage_coverage, "partial")
    expect_length(select("request_error"), 1L)
    expect_identical(select("turn_end")[[1L]]$route$provider, "openai")
    openai <- Filter(function(x) identical(x$provider, "openai"), select("request_start"))[[1L]]
    sent <- jsonlite::fromJSON(corteza::audit_payload_read(openai$manifest$request, FALSE),
        simplifyVector = FALSE)
    instructions <- Filter(function(x) x$role %in% c("system", "developer"), sent$messages)
    expect_true(length(instructions) > 0L)
    expect_identical(corteza::audit_payload_read(openai$manifest$system, FALSE), instructions)
})
