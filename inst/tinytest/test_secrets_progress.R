local({
    ns <- asNamespace("corteza")
    state <- ns$.secret_state
    old <- state$values
    on.exit(state$values <- old, add = TRUE)
    state$values <- character()
    secret <- "fixture-credential-12345"
    shapes <- c(sprintf('$ password : chr "%s"', secret),
                sprintf('{"access_token":"%s"}', secret),
                sprintf("API_KEY='%s'", secret),
                sprintf("Authorization: Bearer %s", secret))
    for (value in shapes) {
        out <- ns$.redact_text(value)
        expect_false(grepl(secret, out, fixed = TRUE))
        expect_true(grepl("[REDACTED]", out, fixed = TRUE))
    }
    expect_identical(ns$.redact_text(paste("echo", secret)), "echo [REDACTED]")
    obj <- list(password = secret, usage = list(input_tokens = 42L),
                content = list(list(text = secret)), signature = "signed")
    masked <- ns$.redact_object(obj)
    expect_identical(masked$password, "[REDACTED]")
    expect_identical(masked$usage, obj$usage)
    expect_identical(masked$signature, "signed")
    expect_identical(masked$content[[1]]$text, "[REDACTED]")
    thought <- list(type = "thinking", thinking = secret, signature = "signed")
    expect_identical(ns$.redact_object(thought), thought)
    ns$.redact_text('token = "none"')
    expect_identical(ns$.redact_text("none of the checks failed"), "none of the checks failed")
    chunks <- character()
    stream <- ns$.redact_stream(function(text) { chunks <<- c(chunks, text); TRUE })
    stream$push('password = "fixture-')
    expect_length(chunks, 0L)
    stream$push('credential-12345"\nDone')
    expect_false(any(grepl(secret, chunks, fixed = TRUE)))
    stream$flush()
    expect_identical(tail(chunks, 1L), "Done")
    expect_identical(jsonlite::fromJSON(ns$.redact_text(shapes[[2]]))$access_token,
                     "[REDACTED]")
    event <- list(outcome = "ran", success = TRUE,
                  call = list(tool = "run_r", args = list(code = "str(config)")),
                  result = shapes[[1]], diff = list(text = secret))
    expect_false(grepl(secret, ns$rooms_activity_record(event)$tool$output, fixed = TRUE))
    expect_false(any(grepl(secret, ns$cli_result_lines(event), fixed = TRUE)))
    s <- corteza::new_session()
    seen <- NULL
    corteza::add_observer(s, function(event) seen <<- event)
    ns$.fire_observers(s, event)
    expect_identical(seen$diff$text, "[REDACTED]")
    expect_false(grepl(secret, seen$result, fixed = TRUE))
    s$config <- list()
    s$approval_cb <- function(...) TRUE
    handler <- ns$.make_tool_handler(s, function(name, args) {
        list(content = list(list(type = "text", text = shapes[[1]])), isError = FALSE)
    })
    result <- handler("read_file", list(path = file.path(tempdir(), "fixture")))
    expect_false(grepl(secret, result, fixed = TRUE))
    expect_true(grepl("[REDACTED]", result, fixed = TRUE))

    now <- as.POSIXct("2026-01-01", tz = "UTC")
    ns$.progress_reset(s, now)
    expect_null(ns$.progress_record(s, event, now + 59))
    notice <- ns$.progress_record(s, event, now + 60)
    expect_true(grepl("2 tool calls completed", notice, fixed = TRUE))
    expect_null(ns$.progress_record(s, event, now + 61))
    event$outcome <- "start"
    event$call$model_context <- list(call_index = 1L, assistant_text = "The checks pass; checking the next file.")
    ns$.progress_record(s, event, now + 119)
    event$outcome <- "ran"
    expect_null(ns$.progress_record(s, event, now + 120))
    reply <- ns$.progress_reply(s, "[Max turns reached]")
    expect_true(grepl("not marked complete", reply, fixed = TRUE))
    expect_true(grepl("The checks pass", reply, fixed = TRUE))
    expect_identical(ns$.progress_reply(s, "Done."), "Done.")
    expect_true(nzchar(ns$rooms_activity_record(list(outcome = "progress", result = notice))$narration))
})
