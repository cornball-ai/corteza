# Offline provider-native fixtures. The batch has two calls in one assistant
# message and, on Anthropic, two results in one user message.
ce_history <- function(provider = "anthropic", mixed = FALSE) {
    h <- list(list(role = "user", content = "Keep my exact words."))
    if (provider == "anthropic") {
        h <- c(h, list(
            list(role = "assistant", content = list(
                list(type = "text", text = "I will inspect both files."),
                list(type = "tool_use", id = "a", name = "read_file", input = list(path = "a")),
                list(type = "tool_use", id = "b", name = "read_file", input = list(path = "b")))),
            list(role = "user", content = list(
                list(type = "tool_result", tool_use_id = "a", content = paste(rep("alpha", 100), collapse = " ")),
                list(type = "tool_result", tool_use_id = "b", content = "beta")))))
        if (mixed) h[[3]]$content <- c(h[[3]]$content, list(list(type = "text", text = "Human follow-up")))
    } else {
        h <- c(h, list(
            list(role = "assistant", content = "I will inspect both files.", tool_calls = list(
                list(id = "a", type = "function", `function` = list(name = "read_file", arguments = '{"path":"a"}')),
                list(id = "b", type = "function", `function` = list(name = "read_file", arguments = '{"path":"b"}')))),
            list(role = "tool", tool_call_id = "a", content = paste(rep("alpha", 100), collapse = " ")),
            list(role = "tool", tool_call_id = "b", content = "beta")))
    }
    c(h, list(list(role = "assistant", content = "Both files exist."),
              list(role = "user", content = "Continue, preserving this too.")))
}

ce_call <- function(id = "edit", provider = "anthropic", op = "drop") {
    if (provider == "anthropic") {
        list(role = "assistant", content = list(list(type = "tool_use", id = id,
            name = "edit_context", input = list(op = op))))
    } else {
        list(role = "assistant", content = "", tool_calls = list(list(id = id,
            type = "function", `function` = list(name = "edit_context", arguments = "{}"))))
    }
}

ce_result <- function(id = "edit", provider = "anthropic", text = "queued") {
    if (provider == "anthropic") {
        list(role = "user", content = list(list(type = "tool_result", tool_use_id = id, content = text)))
    } else list(role = "tool", tool_call_id = id, content = text)
}

ce_session <- function(provider = "anthropic", history = ce_history(provider)) {
    s <- corteza::new_session(provider = provider, system = "PINNED INSTRUCTIONS",
        context_edit = list(enabled = TRUE, cadence = "always"))
    s$config <- list(context_compact_pct = 1000, context_warn_pct = 75)
    s$context_window <- 100000L
    s$history <- history
    d <- tempfile("context-edit-journal-")
    corteza:::journal_install(s, dir = d)
    s
}

ce_queue <- function(s, args, id = "edit") {
    s$history <- c(s$history, list(ce_call(id, s$provider, args$op)))
    text <- corteza:::.context_edit_queue(s, args)
    s$history <- c(s$history, list(ce_result(id, s$provider, text)))
    text
}

run_context_edit_tests <- function() {
    old_log <- corteza:::set_log_enabled(FALSE)
    on.exit(corteza:::set_log_enabled(old_log), add = TRUE)
    dirs <- character()
    on.exit(unlink(dirs, recursive = TRUE), add = TRUE)
    make <- function(...) {
        s <- ce_session(...)
        dirs <<- c(dirs, s$journal_dir)
        s
    }
    expect_identical(corteza:::.context_edit_config(), list(enabled = FALSE, cadence = "pressure"))
    expect_error(corteza::new_session(context_edit = list(cadence = "sometimes")), "cadence")
    expect_error(corteza::new_session(context_edit = NA), "override")
    off <- corteza::new_session()
    expect_false(corteza:::.context_edit_enabled(off))
    expect_identical(corteza:::.context_edit_tools(list(), off), list())
    expect_identical(corteza:::.context_edit_system("unchanged", off), "unchanged")
    expect_identical(corteza:::.context_edit_result(off, "unchanged", "x", NULL), "unchanged")
    off$context_edit <- TRUE
    off$provider <- "openai_codex"
    expect_false(corteza:::.context_edit_enabled(off))
    expect_true(grepl("unavailable", corteza:::.context_edit_status(off)))
    off$provider <- "anthropic"
    off$config <- list(journal_enabled = FALSE)
    expect_false(corteza:::.context_edit_enabled(off))
    expect_true(grepl("journal_enabled", corteza:::.context_edit_status(off)))

    dry <- make()
    dry$dry_run <- TRUE
    expect_true(grepl("dry run", corteza:::.context_edit_queue(dry,
        list(op = "note", text = "must not be stored"))))
    expect_null(dry$pending_context_edits)

    for (provider in c("anthropic", "openai")) {
        original <- ce_history(provider)
        s <- make(provider)
        corteza:::.context_edit_prepare(s)
        indexed <- s$history
        expect_identical(s$system, "PINNED INSTRUCTIONS")
        expect_identical(s$history[[1]], original[[1]])
        expect_true(startsWith(llm.api::history_tool_calls(s$history)[[1]]$result, "[r1]\n"))
        expect_true(grepl("[m3]", s$history[[length(s$history) - 1L]]$content, fixed = TRUE))
        mark <- corteza:::journal_mark(s)
        expect_true(grepl("queued", ce_queue(s, list(op = "drop", ids = "r1"))))
        before <- s$history
        expect_true(corteza:::.context_edit_checkpoint(s))
        calls <- llm.api::history_tool_calls(s$history)
        expect_identical(vapply(calls, `[[`, character(1), "id"), c("b", "edit"))
        expect_true(all(vapply(calls, `[[`, logical(1), "completed")))
        expect_identical(s$history[[1]], original[[1]])
        expect_identical(s$system, "PINNED INSTRUCTIONS")
        expect_true(any(vapply(s$history, identical, logical(1), original[[length(original)]])))
        expect_true(grepl("applied", tail(calls, 1)[[1]]$result))
        expect_identical(corteza:::journal_history(s$journal_dir),
            c(original, tail(before, 2)))
        expect_identical(corteza:::journal_turn_slice(s, mark, length(indexed)), tail(before, 2))
        raw <- jsonlite::fromJSON(file.path(s$journal_dir, "segment-0002.json"), simplifyVector = FALSE)
        expect_null(raw$history)
        expect_null(raw[["after"]])
        expect_true(length(raw$reverse_delta) > 0)
        expect_equal(length(raw$after_refs), length(s$history))
        expect_equal(raw$edits[[1]]$op, "drop")
        expect_identical(corteza:::journal_load(s$journal_dir)$segments[[2]]$history, before)

        # Replacement keeps the first invocation paired, removes the other,
        # and never overwrites human text or the current batch.
        s <- make(provider)
        corteza:::.context_edit_prepare(s)
        ce_queue(s, list(op = "replace", ids = c("r1", "r2"), text = "alpha and beta verified"))
        expect_true(corteza:::.context_edit_checkpoint(s))
        calls <- llm.api::history_tool_calls(s$history)
        expect_identical(vapply(calls, `[[`, character(1), "id"), c("a", "edit"))
        expect_equal(calls[[1]]$arguments, list(path = "a"))
        expect_true(grepl("alpha and beta verified", calls[[1]]$result))
        expect_true(s$context_edit_events[[1]]$tokens_after < s$context_edit_events[[1]]$tokens_before)

        # Older assistant text has its own ID. A note is replaced and moved
        # before the latest human prompt, without accumulating old notes or
        # joining the current assistant message on the Anthropic wire.
        s <- make(provider)
        corteza:::.context_edit_prepare(s)
        ce_queue(s, list(op = "drop", ids = "m3"))
        expect_true(corteza:::.context_edit_checkpoint(s))
        expect_false(any(grepl("Both files exist", vapply(s$history, corteza:::.compact_entry_body, character(1)))))
        ce_queue(s, list(op = "note", text = "Files verified. Next: tests."), "note1")
        expect_true(corteza:::.context_edit_checkpoint(s))
        ce_queue(s, list(op = "note", text = "Tests passed."), "note2")
        expect_true(corteza:::.context_edit_checkpoint(s))
        texts <- vapply(s$history, corteza:::.compact_entry_body, character(1))
        expect_equal(sum(startsWith(texts, "[context note]")), 1L)
        note_index <- which(startsWith(texts, "[context note]"))
        expect_identical(s$history[[note_index + 1L]], tail(original, 1L)[[1L]])

        # Compaction after edits and rollover still reconstruct the original
        # record once. Compacting first invalidates queued IDs rather than
        # changing what they target.
        whole <- corteza:::journal_history(s$journal_dir)
        after <- corteza:::compact_rewrite_history(s$history, length(s$history) - 2L, "summary")
        corteza:::journal_rewrite(s, s$history, after, "threshold")
        s$history <- after
        expect_identical(corteza:::journal_history(s$journal_dir), whole)
        resumed <- corteza::new_session(history = after)
        corteza:::journal_install(resumed, dir = s$journal_dir)
        expect_identical(corteza:::journal_history(s$journal_dir), whole)

        s <- make(provider)
        corteza:::.context_edit_prepare(s)
        ce_queue(s, list(op = "drop", ids = "r1"))
        after <- corteza:::compact_rewrite_history(s$history, length(s$history) - 2L, "summary")
        corteza:::journal_rewrite(s, s$history, after, "threshold")
        s$history <- after
        expect_true(corteza:::.context_edit_checkpoint(s))
        expect_false(tail(s$context_edit_events, 1)[[1]]$applied)
        expect_true(grepl("rejected", tail(llm.api::history_tool_calls(s$history), 1)[[1]]$result))
        expect_equal(tail(corteza:::journal_load(s$journal_dir)$segments, 1)[[1]]$reason,
            "context_edit_rejected")

        # Resume must preserve the exact displayed IDs, including assistant
        # text stored as blocks, and those IDs must still select their entries.
        s <- make(provider)
        reply <- length(s$history) - 1L
        if (provider == "anthropic") {
            s$history[[reply]]$content <- list(list(type = "text", text = "Both files exist."))
        }
        corteza:::.context_edit_prepare(s)
        indexed <- s$history
        restored <- make(provider, history = indexed)
        corteza:::.context_edit_prepare(restored)
        expect_identical(restored$history, indexed)
        expect_true(all(c("r1", "r2", "m3") %in% names(
            corteza:::.context_edit_targets(restored, restored$history, length(indexed) + 1L))))
        ce_queue(restored, list(op = "drop", ids = c("r1", "m3")))
        expect_true(corteza:::.context_edit_checkpoint(restored))
        expect_identical(vapply(llm.api::history_tool_calls(restored$history), `[[`, character(1), "id"),
            c("b", "edit"))

        # Supported provider changes and fallback provider overrides must not
        # change IDs or reset a pressure notice that has already been shown.
        state <- corteza:::.context_edit_state(s)
        state$pressure_open <- TRUE
        s$provider <- if (provider == "anthropic") "anthropic_claude" else "openai_compatible"
        corteza:::.context_edit_prepare(s)
        expect_identical(s$history, indexed)
        expect_true(corteza:::.context_edit_state(s)$pressure_open)
        s$context_edit_provider <- provider
        corteza:::.context_edit_prepare(s)
        expect_identical(s$history, indexed)
        s$context_edit_provider <- NULL
        corteza:::.context_edit_prepare(s)
        expect_identical(s$history, indexed)

        # Both invocations are pending in the same batch. Dispatch order can
        # differ from result arrival, so repeated names must use call identity.
        s <- make(provider, history = ce_history(provider)[1:2])
        corteza:::.context_edit_prepare(s)
        text_b <- corteza:::.context_edit_result(s, "second output", "read_file",
            list(agent_turn = 1L, call_index = 2L, call_count = 2L))
        text_a <- corteza:::.context_edit_result(s, "first output", "read_file",
            list(agent_turn = 1L, call_index = 1L, call_count = 2L))
        expect_false(identical(sub("\n.*", "", text_a), sub("\n.*", "", text_b)))
        s$history <- c(s$history, list(ce_result("a", provider, text_a), ce_result("b", provider, text_b),
            list(role = "assistant", content = "Finished.")))
        corteza:::.context_edit_prepare(s)
        calls <- llm.api::history_tool_calls(s$history)
        expect_identical(calls[[1]]$result, text_a)
        expect_identical(calls[[2]]$result, text_b)
        id_b <- sub("^\\[([^]]+)\\].*", "\\1", text_b)
        ce_queue(s, list(op = "drop", ids = id_b))
        expect_true(corteza:::.context_edit_checkpoint(s))
        expect_identical(vapply(llm.api::history_tool_calls(s$history), `[[`, character(1), "id"),
            c("a", "edit"))
    }

    # Human content sharing a result message is never labelled or editable.
    s <- make(history = ce_history(mixed = TRUE))
    before <- s$history[[3]]
    corteza:::.context_edit_prepare(s)
    expect_identical(s$history[[3]], before)
    expect_error(ce_queue(s, list(op = "drop", ids = "r1")), "protected")

    # Moonshot consumes its own search calls without invoking the handler;
    # these do not occupy the dispatch indices supplied to corteza.
    s <- make("openai", history = ce_history("openai")[1:2])
    s$provider <- "moonshot"
    server_call <- list(id = "server", type = "function", `function` =
        list(name = "$web_search", arguments = '{"search_id":"fixture"}'))
    s$history[[2]]$tool_calls <- c(list(server_call), s$history[[2]]$tool_calls)
    expect_identical(corteza:::.context_edit_active_call(s, "read_file",
        list(provider = "moonshot", call_index = 2L))$id, "b")
    expect_null(corteza:::.context_edit_active_call(s, "read_file"))
    expect_null(corteza:::.context_edit_active_call(s, "read_file", list(call_index = 10L)))
    expect_identical(corteza:::.context_edit_active_call(s, "read_file", list(call_id = "a"))$id, "a")

    # An ID in the current assistant batch is untouchable even once its
    # result has arrived. Invalid or conflicting intents cannot partly apply.
    s <- make()
    corteza:::.context_edit_prepare(s)
    ce_queue(s, list(op = "drop", ids = "r1"))
    current <- tail(llm.api::history_tool_calls(s$history), 1)[[1]]
    current_id <- corteza:::.context_edit_call_id(corteza:::.context_edit_state(s), current)
    expect_error(corteza:::.context_edit_queue(s, list(op = "drop", ids = current_id)), "protected")
    expect_error(corteza:::.context_edit_queue(s, list(op = "drop", ids = c("r1", "r1"))), "distinct")
    expect_error(corteza:::.context_edit_queue(s, list(op = "replace", ids = "r1", text = "")), "non-empty")
    corteza:::.context_edit_queue(s, list(op = "drop", ids = "r1"))
    before <- s$history
    corteza:::.context_edit_checkpoint(s)
    expect_equal(length(llm.api::history_tool_calls(s$history)), 3L)
    expect_false(tail(s$context_edit_events, 1)[[1]]$applied)

    # A rejected request and a valid request can share a batch. Only the
    # request actually queued gets an applied receipt.
    s <- make()
    corteza:::.context_edit_prepare(s)
    a <- ce_call("bad")$content[[1L]]
    b <- ce_call("good")$content[[1L]]
    s$history <- c(s$history, list(list(role = "assistant", content = list(a, b))))
    expect_error(corteza:::.context_edit_queue(s, list(op = "drop", ids = "unknown")), "protected")
    s$history <- c(s$history, list(ce_result("bad", text = "rejected: unknown")))
    corteza:::.context_edit_queue(s, list(op = "drop", ids = "r1"))
    s$history[[length(s$history)]]$content <- c(s$history[[length(s$history)]]$content,
        ce_result("good")$content)
    expect_true(corteza:::.context_edit_checkpoint(s))
    results <- llm.api::history_tool_calls(s$history)
    expect_equal(results[[length(results) - 1L]]$result, "rejected: unknown")
    expect_true(grepl("applied", tail(results, 1)[[1]]$result))

    # Pressure is opened only after the notice becomes visible to the next
    # request. Every-turn cadence is separately tested above.
    s <- make()
    s$context_edit$cadence <- "pressure"
    corteza:::.context_edit_prepare(s)
    expect_error(corteza:::.context_edit_queue(s, list(op = "note", text = "facts")), "pressure")
    s$context_window <- 10L
    s$history <- c(s$history, list(ce_call()))
    text <- corteza:::.context_edit_result(s, "result", "edit_context", list(agent_turn = 1L))
    expect_true(grepl("context pressure", text))
    expect_false(grepl("context pressure", corteza:::.context_edit_result(s, "second", "x", list(agent_turn = 1L))))
    expect_error(corteza:::.context_edit_queue(s, list(op = "note", text = "facts")), "pressure")
    s$history <- c(s$history, list(ce_result(text = text)))
    corteza:::.context_edit_checkpoint(s)
    expect_true(grepl("queued", ce_queue(s, list(op = "note", text = "facts"), "next")))
    expect_true(corteza:::.context_edit_checkpoint(s))
    expect_false(corteza:::.context_edit_state(s)$pressure_open)

    # A failed second journal write leaves the live history and committed
    # record unchanged. Retrying overwrites the unacknowledged edit segment.
    s <- make()
    corteza:::.context_edit_prepare(s)
    ce_queue(s, list(op = "drop", ids = "r1"))
    corteza:::journal_snapshot(s)
    before <- s$history
    whole <- corteza:::journal_history(s$journal_dir)
    pending <- s$pending_context_edits
    blocked <- file.path(s$journal_dir, "live.json.tmp")
    dir.create(blocked)
    expect_error(corteza:::.context_edit_checkpoint(s), "journal")
    expect_identical(s$history, before)
    expect_false(tail(s$context_edit_events, 1)[[1]]$applied)
    expect_true(grepl("journal", tail(s$context_edit_events, 1)[[1]]$error))
    expect_identical(corteza:::journal_history(s$journal_dir), whole)
    unlink(blocked, recursive = TRUE)
    s$pending_context_edits <- pending
    expect_true(corteza:::.context_edit_checkpoint(s))
    expect_identical(corteza:::journal_history(s$journal_dir), whole)
    expect_equal(length(corteza:::journal_load(s$journal_dir)$segments), 2L)

    # Restarted sessions retain visible IDs and adopt the previous working
    # note so the next note replaces it.
    s <- make()
    corteza:::.context_edit_prepare(s)
    ce_queue(s, list(op = "note", text = "old facts"))
    corteza:::.context_edit_checkpoint(s)
    restored <- make(history = s$history)
    corteza:::.context_edit_prepare(restored)
    ids <- names(corteza:::.context_edit_targets(restored, restored$history,
        length(restored$history) + 1L))
    expect_true("r1" %in% ids)
    ce_queue(restored, list(op = "note", text = "new facts"), "note-next")
    corteza:::.context_edit_checkpoint(restored)
    texts <- vapply(restored$history, corteza:::.compact_entry_body, character(1))
    expect_equal(sum(startsWith(texts, "[context note]")), 1L)
    expect_true(any(texts == "[context note]\nnew facts"))

    # Old edits do not rely on surviving entries remaining in live.json.
    # Even a host that replaces the live window cannot change their content.
    committed <- corteza:::journal_load(s$journal_dir)$segments
    s$history <- list(list(role = "user", content = "restored window"))
    corteza:::journal_snapshot(s)
    expect_identical(corteza:::journal_load(s$journal_dir)$segments, committed)
}
run_context_edit_tests()
