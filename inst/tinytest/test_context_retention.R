# Synthetic conversations only. Count human-led turns rather than provider
# messages: one Anthropic tool batch adds a user-role result message.
local({
    root <- tempfile("retention-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    f <- function(name) get(name, asNamespace("corteza"))
    tool_pair <- function(provider, id, name = "read_file") {
        if (provider == "anthropic") list(
            list(role = "assistant", content = list(list(type = "tool_use",
                id = id, name = name, input = list(path = "fixture")))),
            list(role = "user", content = list(list(type = "tool_result",
                tool_use_id = id, content = paste("result", id))))) else list(
            list(role = "assistant", content = "", tool_calls = list(list(
                id = id, type = "function", `function` = list(name = name,
                arguments = '{"path":"fixture"}')))),
            list(role = "tool", tool_call_id = id, content = paste("result", id)))
    }
    history <- function(provider) {
        out <- list()
        for (i in 1:4) out <- c(out,
            list(list(role = "user", content = paste("question", i))),
            tool_pair(provider, paste0("call", i)),
            list(list(role = "assistant", content = paste("answer", i))))
        c(out, list(list(role = "user", content = "current question")))
    }
    make <- function(provider, keep = 2L, owner = TRUE) {
        s <- corteza::new_session(provider = provider,
            context_edit = list(enabled = TRUE, cadence = "always", retain_turns = keep))
        s$config <- list(context_compact_pct = 1000)
        s$context_conversation <- owner
        # Enable human eligibility without writing: this suite only drops tools.
        s$context_archive_vault <- root
        s$history <- history(provider)
        f("journal_install")(s, tempfile("journal-", tmpdir = root))
        s
    }
    targets <- function(s) f(".context_edit_targets")(s, s$history,
        f(".context_edit_boundary")(s$history))
    for (provider in c("anthropic", "openai")) {
        s <- make(provider)
        original <- s$history
        expect_equal(f(".context_retention_boundary")(s, original), 9L)
        f(".context_edit_prepare")(s)
        expect_identical(s$history[9:17], original[9:17])
        eligible <- targets(s)
        expect_equal(length(eligible), 6L) # two calls, two prompts, two replies
        calls <- llm.api::history_tool_calls(s$history)
        expect_true(all(startsWith(vapply(calls[1:2], `[[`, "", "result"), "[r")))
        expect_identical(vapply(calls[3:4], `[[`, "", "result"), c("result call3", "result call4"))
        expect_true(grepl("previous 2 human-led turns", f(".context_edit_system")("", s)))

        # Existing labels from an older setting stay protected, including
        # human labels. Raising retention between queue and commit rejects it.
        s <- make(provider, keep = 0)
        f(".context_edit_prepare")(s)
        all_targets <- targets(s)
        recent_id <- names(Filter(function(t) t$kind == "call" &&
            t$call$id == "call4", all_targets))
        pair <- tool_pair(provider, "edit", "edit_context")
        s$history <- c(s$history, pair[1])
        expect_true(grepl("queued", f(".context_edit_queue")(s,
            list(op = "drop", ids = recent_id))))
        s$history <- c(s$history, pair[2])
        s$context_edit$retain_turns <- 2L
        expect_error(f(".context_edit_queue")(s, list(op = "drop", ids = recent_id)),
                     "protected")
        expect_true(f(".context_edit_checkpoint")(s))
        expect_false(tail(s$context_edit_events, 1)[[1]]$applied)
        expect_true("call4" %in% vapply(llm.api::history_tool_calls(s$history), `[[`, "", "id"))
        expect_true(grepl("rejected", tail(llm.api::history_tool_calls(s$history), 1)[[1]]$result))

        # Workers inherit config but not conversation ownership or its floor.
        worker <- make(provider, keep = 10, owner = FALSE)
        f(".context_edit_prepare")(worker)
        expect_equal(f(".context_retention_turns")(worker), 0)
        expect_equal(sum(vapply(targets(worker), function(t) t$kind == "call", TRUE)), 4L)
        s <- make(provider, keep = 10)
        f(".context_edit_prepare")(s)
        expect_length(targets(s), 0L)
        expect_identical(s$history, original)

        # A tool-only user message does not advance the floor; a mixed message
        # does, conservatively protecting the whole human-bearing entry.
        h <- original[1:4]
        expect_equal(sum(vapply(h, f(".context_edit_human"), TRUE)), 1L)
        if (provider == "anthropic") {
            h[[3]]$content <- c(h[[3]]$content, list(list(type = "text", text = "Human follow-up")))
            expect_true(f(".context_edit_human")(h[[3]]))
        }

        # Drive turn() through the actual provider loop and checkpoint. An
        # older tool pair can retire while the last two full turns survive.
        local({
            backend <- if (provider == "anthropic") ".agent_anthropic" else ".agent_openai"
            key <- if (provider == "anthropic") "ANTHROPIC_API_KEY" else "OPENAI_API_KEY"
            old_key <- Sys.getenv(key, unset = NA)
            native <- get(backend, asNamespace("llm.api"))
            on.exit({
                utils::assignInNamespace(backend, native, "llm.api")
                if (is.na(old_key)) Sys.unsetenv(key) else
                    do.call(Sys.setenv, setNames(list(old_key), key))
            }, add = TRUE)
            do.call(Sys.setenv, setNames(list("offline-fixture"), key))
            seen <- list()
            utils::assignInNamespace(backend, function(messages, ...) {
                seen[[length(seen) + 1L]] <<- messages
                if (length(seen) == 1L) {
                    result <- llm.api::history_tool_calls(messages)[[1]]$result
                    id <- f(".context_edit_label")(result, "r")
                    args <- list(op = "drop", ids = list(id))
                    msg <- tool_pair(provider, "retire", "edit_context")[[1]]
                    if (provider == "anthropic") msg$content[[1]]$input <- args else
                        msg$tool_calls[[1]]$`function`$arguments <- jsonlite::toJSON(args, auto_unbox = TRUE)
                    return(list(text = "", tool_calls = list(list(id = "retire",
                        name = "edit_context", arguments = args)), assistant_message = msg,
                        usage = list(input_tokens = 1L, output_tokens = 1L)))
                }
                list(text = "Done", tool_calls = list(),
                     assistant_message = list(role = "assistant", content = "Done"),
                     usage = list(input_tokens = 1L, output_tokens = 1L))
            }, "llm.api")
            s <- make(provider, keep = 2)
            s$history <- s$history[-length(s$history)]
            s$context_archive_vault <- NULL
            s$model_map$cloud <- "fixture-model"
            s$web_search <- FALSE
            out <- suppressMessages(corteza::turn("current question", s, tools = list()))
            expect_equal(length(seen), 2L)
            expect_identical(out$reply, "Done")
            expect_true(tail(s$context_edit_events, 1)[[1]]$applied)
            expect_false("call1" %in% vapply(llm.api::history_tool_calls(seen[[2]]), `[[`, "", "id"))
            expect_true(all(vapply(original[9:16], function(m)
                any(vapply(seen[[2]], identical, TRUE, m)), TRUE)))
        })
    }
    for (bad in list(-1, 1.5, NA_real_, Inf, "10", TRUE, c(1, 2), 1e100)) {
        expect_error(f(".context_edit_config")(override = list(retain_turns = bad)), "retain_turns")
    }
    expect_equal(f(".context_edit_config")(override = list(retain_turns = 10))$retain_turns, 10)
})
