if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api unavailable")
local({
    root <- tempfile("audit-delivery-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- corteza::new_session("matrix")
    s$audit_dir <- root
    s$journal_enabled <- FALSE
    s$secret_values <- "fixture-secret-92801"
    client <- chat.api::chat_loopback()
    sent <- list()
    reply <- c("$first", "$second")
    failure <- NULL
    old <- get("chat_send", asNamespace("chat.api"))
    on.exit(utils::assignInNamespace("chat_send", old, "chat.api"), add = TRUE)
    utils::assignInNamespace("chat_send", function(chat, channel, text, ...) {
        sent[[length(sent) + 1L]] <<- text
        if (!is.null(failure)) stop(failure)
        reply
    }, "chat.api")
    reference <- list(stream = "child-stream", turn_id = "child:t1")
    expect_identical(corteza:::bot_reply_send(client, "room", "fixture-secret-92801",
        thread = "thread", session = s, audit = reference), reply)
    expect_identical(sent[[1L]], "[REDACTED]")
    read <- function() corteza:::auto_log_read(s$.audit_stream$path)$records
    rows <- read()
    expect_length(rows, 2L)
    end <- rows[[2L]]
    expect_identical(unlist(end$event_ids), reply)
    expect_identical(end$turn$turn_id, "child:t1")
    expect_identical(end$status, "confirmed")
    expect_identical(end$thread, "thread")
    expect_identical(end$text_hash, digest::digest("[REDACTED]", "sha256", serialize = FALSE))
    expect_false(identical(end$text_hash, end$generated_hash))
    expect_null(s$context_vault)
    reply <- character()
    expect_null(corteza:::bot_reply_send(client, "room", "nothing", session = s))
    expect_identical(tail(read(), 1L)[[1L]]$status, "unconfirmed")
    failure <- structure(list(message = "offline fixture", call = NULL),
        class = c("fixture_delivery_error", "error", "condition"))
    err <- tryCatch(corteza:::bot_reply_send(client, "room", "failed", session = s), error = identity)
    expect_inherits(err, "fixture_delivery_error")
    expect_identical(tail(read(), 1L)[[1L]]$condition_classes[[1L]], "fixture_delivery_error")
    expect_length(sent, 3L)
    expect_identical(tail(read(), 1L)[[1L]]$condition_message, "offline fixture")

    # Observation failure never retries a confirmed send. Recovery records the
    # failed turn writes as a gap before the successful delivery, without vaults.
    failure <- NULL
    reply <- "$delivered-once"
    runtime <- corteza:::.audit_runtime
    noticed <- runtime$noticed
    runtime$noticed <- FALSE
    on.exit(runtime$noticed <- noticed, add = TRUE)
    writer <- get(".audit_write", asNamespace("corteza"))
    on.exit(utils::assignInNamespace(".audit_write", writer, "corteza"), add = TRUE)
    blocked <- TRUE
    utils::assignInNamespace(".audit_write", function(state, record) {
        if (blocked) stop("fixture disk full")
        writer(state, record)
    }, "corteza")
    # Real turn() orchestration with the provider transport faked.
    agent <- get(".agent_anthropic", asNamespace("llm.api"))
    on.exit(utils::assignInNamespace(".agent_anthropic", agent, "llm.api"), add = TRUE)
    key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.setenv(ANTHROPIC_API_KEY = "fixture-key")
    on.exit(if (is.na(key)) Sys.unsetenv("ANTHROPIC_API_KEY") else
        Sys.setenv(ANTHROPIC_API_KEY = key), add = TRUE)
    calls <- 0L
    utils::assignInNamespace(".agent_anthropic", function(...) {
        calls <<- calls + 1L
        list(text = "delivered", tool_calls = list(),
            assistant_message = list(role = "assistant", content = list(list(type = "text", text = "delivered"))),
            usage = list(input_tokens = 3L, output_tokens = 1L))
    }, "llm.api")
    s$provider <- "anthropic"
    s$model_map <- list(cloud = "fixture")
    s$web_search <- FALSE
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = Inf)
    notices <- character()
    result <- withCallingHandlers(corteza::turn("fixture", s, tools = list()), message = function(m) {
        notices <<- c(notices, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    blocked <- FALSE
    corteza:::bot_reply_send(client, "room", result$reply, session = s, audit = result$audit)
    expect_equal(calls, 1L)
    expect_equal(sum(grepl("audit recording is failing", notices)), 1L)
    expect_length(sent, 4L)
    rows <- read()
    gap <- Filter(function(x) identical(x$type, "gap"), rows)
    expect_length(gap, 1L)
    expect_equal(gap[[1L]]$lost_records, 4,
        info = paste("lost records:", gap[[1L]]$lost_records))
    expect_identical(tail(rows, 1L)[[1L]]$event_ids[[1L]], "$delivered-once")
    expect_identical(tail(rows, 1L)[[1L]]$turn, result$audit)
})
