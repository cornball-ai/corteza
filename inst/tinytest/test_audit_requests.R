# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

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
    # What each Anthropic response reports beside its 7 input tokens, and
    # what the OpenAI one reports. The cases at the end set them.
    cached <- list()
    openai_usage <- '{"prompt_tokens":5,"completion_tokens":2}'
    emit <- function(fun, object) fun(charToRaw(paste0("data: ",
        jsonlite::toJSON(object, auto_unbox = TRUE, null = "null"), "\n\n")))
    replace("curl", "curl_fetch_stream", function(url, fun, handle) {
        calls <<- calls + 1L
        if (mode == "fallback" && calls == 2L) stop("API error (429): fixture limit")
        emit(fun, list(type = "message_start", message = list(id = paste0("msg", calls),
            model = "served-fixture",
            usage = c(list(input_tokens = 7L, output_tokens = 0L), cached))))
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
            content = charToRaw(paste0('{"id":"fallback","model":"served-openai","choices":[{"message":{"role":"assistant","content":"fallback done"},"finish_reason":"stop"}],"usage":', openai_usage, '}')))
    })
    opt <- options(corteza.policy = function(call) list(model = "cloud", approval = "allow", reason = "fixture"))
    on.exit(options(opt), add = TRUE)
    # A system text of its own: the fallback case below reads it back from
    # the request another provider was sent.
    s <- corteza::new_session(provider = "anthropic", model_map = list(cloud = "fixture"), web_search = FALSE,
        system = "Fixture instructions.")
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

    # A failed observer write cannot lower the reported usage received by
    # /spent, jobs and subagent consumers. Coverage describes durable records.
    writer <- get(".audit_write", asNamespace("corteza"))
    failed <- 0L
    replace("corteza", ".audit_write", function(state, record) {
        if (identical(record$type, "request_end") && failed == 0L) {
            failed <<- failed + 1L
            stop("fixture audit write failure")
        }
        writer(state, record)
    })
    calls <- 0L
    partial <- suppressMessages(corteza::turn("record failure", s, tools = list(),
        tool_executor = function(...) corteza:::ok("result")))
    expect_equal(failed, 1L)
    expect_equal(calls, 2L)
    expect_equal(partial$usage$total_tokens, 20)
    expect_identical(partial$usage_coverage, "partial")
    rows <- Filter(function(x) identical(x$turn_id, partial$audit$turn_id),
        corteza::audit_read(partial$audit$stream, FALSE)$records)
    end <- select("turn_end")[[1L]]
    expect_equal(end$request_usage$total_tokens, 10)
    expect_equal(end$reported_usage$total_tokens, 20)
    expect_equal(end$usage$total_tokens, 20)
    expect_identical(end$usage_basis, "reported")
    expect_length(select("request_end"), 1L)
    utils::assignInNamespace(".audit_write", writer, "corteza")

    manifest <- get(".audit_request_manifest", asNamespace("corteza"))
    failed <- 0L
    replace("corteza", ".audit_request_manifest", function(...) {
        if (failed == 0L) {
            failed <<- failed + 1L
            stop("fixture manifest failure")
        }
        manifest(...)
    })
    calls <- 0L
    partial <- suppressMessages(corteza::turn("manifest failure", s, tools = list(),
        tool_executor = function(...) corteza:::ok("result")))
    rows <- Filter(function(x) identical(x$turn_id, partial$audit$turn_id),
        corteza::audit_read(partial$audit$stream, FALSE)$records)
    expect_equal(failed, 1L)
    expect_equal(calls, 2L)
    expect_length(select("request_start"), 1L)
    expect_identical(partial$usage_coverage, "partial")
    expect_equal(partial$usage$total_tokens, 20)
    utils::assignInNamespace(".audit_request_manifest", manifest, "corteza")

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

    # A session on an Anthropic provider caches its prompt unless a setting
    # says otherwise, and Anthropic counts cached input beside
    # `input_tokens`. A turn's total counts it, by the requests recorded
    # and by llm.api's own sum, so a token cap sees every token sent.
    fresh <- function() {
        corteza:::.fallback_reset()
        corteza:::journal_rewrite(s, s$history, list(), "fixture_reset")
        s$history <- list()
        calls <<- 0L
        length(requests)
    }
    turn_rows <- function(result) Filter(
        function(x) identical(x$turn_id, result$audit$turn_id),
        corteza:::auto_log_read(result$audit$stream)$records)
    sent_since <- function(n) unlist(requests[seq_along(requests) > n])
    mode <- "normal"
    s$fallback <- NULL
    cached <- list(cache_read_input_tokens = 1000L, cache_creation_input_tokens = 200L)
    before <- fresh()
    counted <- corteza::turn("cached", s, tools = list(), tool_executor = function(...) corteza:::ok("result"))
    expect_equal(calls, 2L)
    expect_true(all(grepl("cache_control", sent_since(before), fixed = TRUE)))
    expect_identical(counted$usage_coverage, "complete")
    expect_equal(counted$usage$input_tokens, 14)
    expect_equal(counted$usage$output_tokens, 6)
    expect_equal(counted$usage$cache_read_input_tokens, 2000)
    expect_equal(counted$usage$cache_creation_input_tokens, 400)
    expect_equal(counted$usage$total_tokens, 2420)
    rows <- turn_rows(counted)
    expect_equal(vapply(select("request_end"), function(x) x$usage$total_tokens, 0), c(1210, 1210))
    end <- select("turn_end")[[1L]]
    expect_equal(end$request_usage$total_tokens, 2420)
    expect_equal(end$reported_usage$total_tokens, 2420)
    # The turn's record of what it sent names the cache, though the
    # session set none.
    expect_null(s$cache)
    expect_identical(select("turn_config")[[1L]]$settings$cache, "5m")
    # The session's tally, which /spent prints and /auto's token cap reads.
    tally <- new.env()
    corteza:::session_accumulate_spend(tally, counted$usage)
    expect_equal(tally$spend$segments[[1L]]$total_tokens, 2420)
    expect_equal(tally$spend$segments[[1L]]$input_tokens, 2414)
    expect_equal(tally$spend$segments[[1L]]$output_tokens, 6)

    # "none" sends no marker. A response that reports nothing cached leaves
    # the total at input plus output.
    cached <- list()
    s$cache <- "none"
    before <- fresh()
    plain <- corteza::turn("no cache", s, tools = list(), tool_executor = function(...) corteza:::ok("result"))
    expect_false(any(grepl("cache_control", sent_since(before), fixed = TRUE)))
    expect_equal(plain$usage$total_tokens, 20)
    rows <- turn_rows(plain)
    expect_identical(select("turn_config")[[1L]]$settings$cache, "none")
    s$cache <- NULL

    # A request answered before the limit keeps its cached input in the
    # total after another provider finishes the turn. That provider counts
    # cached input inside its prompt tokens, so its 4 are not added again.
    cached <- list(cache_read_input_tokens = 1000L, cache_creation_input_tokens = 200L)
    openai_usage <- '{"prompt_tokens":5,"completion_tokens":2,"prompt_tokens_details":{"cached_tokens":4}}'
    mode <- "fallback"
    s$fallback <- "fixture openai"
    before <- fresh()
    mixed <- corteza::turn("cached fallback", s, tools = list(), tool_executor = function(...) corteza:::ok("result"))
    rows <- turn_rows(mixed)
    expect_identical(vapply(select("request_end"), `[[`, "", "provider"), c("anthropic", "openai"))
    expect_equal(vapply(select("request_end"), function(x) x$usage$total_tokens, 0), c(1210, 7))
    expect_equal(mixed$usage$total_tokens, 1217)
    corteza:::.fallback_reset()
})
