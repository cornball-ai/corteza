# Offline coverage of the two policies: doers retain the journal only;
# talkers file retired context before the journal commits the removal.
local({
    ns <- asNamespace("corteza")
    f <- function(name) get(name, ns)
    root <- tempfile("clm-bot-test-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    old_env <- Sys.getenv(c("R_USER_DATA_DIR", "R_USER_CONFIG_DIR", "R_USER_CACHE_DIR"), unset = NA)
    on.exit({
        for (name in names(old_env)) {
            if (is.na(old_env[[name]])) Sys.unsetenv(name) else
                do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
        }
    }, add = TRUE)
    Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
               R_USER_CONFIG_DIR = file.path(root, "config"),
               R_USER_CACHE_DIR = file.path(root, "cache"))
    pure <- list(enabled = TRUE, cadence = "always", strategy = "clm")
    expect_error(f(".context_edit_config")(override = list(strategy = "unknown")), "strategy")
    make <- function(provider = "anthropic", vault = NULL) {
        s <- corteza::new_session(provider = provider, system = "Pinned instructions",
                                  context_edit = pure)
        s$config <- list(context_compact_pct = 1, archival = list(enabled = TRUE))
        s$model_map$cloud <- "fixture"
        s$context_window <- 100L
        s$history <- list(list(role = "user", content = "Earlier topic, original words."),
                          list(role = "assistant", content = "Earlier answer."),
                          list(role = "user", content = "Current request: keep this."))
        s$job_owner <- "bot"
        s$job_key <- "room/thread"
        s$room_id <- "!room:example.invalid"
        s$talker <- !is.null(vault)
        s$context_conversation <- !is.null(vault)
        s$context_archive_vault <- vault
        f("journal_install")(s, tempfile("journal-", tmpdir = root))
        f(".context_edit_prepare")(s)
        s
    }
    batch <- function(s, id = "edit") {
        if (s$provider == "anthropic") {
            list(role = "assistant", content = list(list(type = "tool_use",
                id = id, name = "edit_context", input = list())))
        } else list(role = "assistant", content = "", tool_calls = list(list(
            id = id, type = "function", `function` = list(name = "edit_context", arguments = "{}"))))
    }
    queue <- function(s, ids, op = "drop", text = "Earlier topic finished.") {
        s$history <- c(s$history, list(batch(s)))
        f(".context_edit_queue")(s, list(op = op, ids = ids, text = text))
        result <- if (s$provider == "anthropic") {
            list(role = "user", content = list(list(type = "tool_result",
                tool_use_id = "edit", content = "queued")))
        } else list(role = "tool", tool_call_id = "edit", content = "queued")
        s$history <- c(s$history, list(result))
    }
    rendered <- function(s) paste(vapply(s$history, f(".compact_entry_body"), ""), collapse = "\n")

    # Pure means no threshold OR forced overflow summary and no holder collapse.
    s <- make()
    before <- s$history
    expect_false(f("maybe_compact_turn_session")(s, s$config, threshold = 0, min_messages = 1))
    expect_identical(s$history, before)
    f("maybe_archive_turn")(s, "job", 0L, list(reply = "done"), s$config, "id")
    expect_identical(s$history, before)
    expect_false(any(startsWith(names(f(".context_edit_state")(s)$messages), "u")))
    expect_null(f(".context_archive_write")(s, before, list()))
    s$provider <- "openai_codex"
    expect_error(f(".context_edit_require")(s), "supported provider")
    s$provider <- "anthropic"
    s$journal_enabled <- FALSE
    expect_error(f(".context_edit_require")(s), "enabled journal")

    # A role-specific policy reaches the actual child initializer; it does not
    # alter the parent, reviewer, monitor, or give the doer a vault callback.
    parent <- corteza::new_session(provider = "anthropic", system = "parent")
    parent$cwd <- root
    parent$config <- list(context_edit = list(enabled = FALSE),
                          jobs = list(doer = list(context_edit = pure)))
    parent$model_map$cloud <- "fixture"
    spec <- f("job_worker_spec")(parent)
    expect_identical(spec$context_edit, pure)
    expect_false(f(".context_edit_config")(parent$config, parent$context_edit)$enabled)
    expect_null(f("job_worker_spec")(parent, "reviewer")$context_edit)
    expect_null(f("job_worker_spec")(parent, "monitor")$context_edit)
    expect_warning(f("talker_enable")(parent, list(context_edit = pure)), "explicit context_vault")
    expect_false(f(".context_edit_enabled")(parent))
    expect_false(f(".context_edit_pure")(parent))
    # Actual child process, isolated library/config/cwd, with no model call.
    child <- callr::r(function(spec, root) {
        library(corteza)
        corteza:::.job_worker_child_init(root, spec, file.path(root, "worker"))
        s <- corteza:::.subagent_state$session
        list(config = corteza:::.context_edit_config(s$config, s$context_edit),
             pure = corteza:::.context_edit_pure(s),
             archives = corteza:::.context_edit_archives(s),
             journal = !is.null(corteza:::journal_dir(s)))
    }, list(spec = spec, root = root))
    expect_identical(child$config, pure)
    expect_true(child$pure)
    expect_false(child$archives)
    expect_true(child$journal)

    lifecycle <- callr::r(function(spec, root) {
        library(corteza)
        dir.create(file.path(root, ".corteza"))
        writeLines('{"context_compact_pct":1,"archival":{"enabled":true}}',
                   file.path(root, ".corteza", "config.json"))
        corteza:::.job_worker_child_init(root, spec, file.path(root, "lifecycle"))
        s <- corteza:::.subagent_state$session
        s$context_window <- 1L
        called <- 0L
        checkpoints <- 0L
        overflow <- FALSE
        utils::assignInNamespace("chat", function(...) stop("unexpected automatic summary"), "llm.api")
        utils::assignInNamespace("archival_should_trigger", function(...) stop("unexpected holder archival"), "corteza")
        utils::assignInNamespace("agent", function(prompt, history, history_callback,
                                             checkpoint_callback, ...) {
            called <<- called + 1L
            h <- c(history, list(list(role = "user", content = prompt),
                                 list(role = "assistant", content = "Completed job.")))
            history_callback(h)
            checkpoints <<- checkpoints + 1L
            change <- checkpoint_callback(h, list(agent_turn = 1L, usage = list()))
            if (!is.null(change)) h <- change$history
            list(content = "Completed job.", history = h, turns = 1L,
                 truncated = overflow,
                 truncation_reason = if (overflow) "model_context_window_exceeded",
                 usage = list())
        }, "llm.api")
        a <- corteza::subagent_turn_prompt("First job.")
        b <- corteza::subagent_turn_prompt("Second job.")
        overflow <- TRUE
        exhausted <- tryCatch({
            corteza::subagent_turn_prompt("Third job.")
            "no error"
        }, error = conditionMessage)
        list(replies = c(a$reply, b$reply), called = called,
             checkpoints = checkpoints, exhausted = exhausted,
             history = s$history, vault = s$context_archive_vault)
    }, list(spec = spec, root = root))
    expect_equal(lifecycle$replies, rep("Completed job.", 2L))
    expect_equal(lifecycle$called, 3L)
    expect_equal(lifecycle$checkpoints, 3L)
    expect_true(grepl("Pure CLM exhausted", lifecycle$exhausted, fixed = TRUE))
    expect_null(lifecycle$vault)
    expect_true(any(vapply(lifecycle$history, function(m) identical(m$content, "First job."), logical(1))))

    # Unsupported fallbacks never silently turn pure CLM off.
    s <- make()
    s$fallback <- c("fixture openai_codex", "fixture openai")
    old_limits <- as.list(f(".fallback_state"))
    on.exit({
        env <- f(".fallback_state")
        rm(list = ls(env, all.names = TRUE), envir = env)
        list2env(old_limits, envir = env)
    }, add = TRUE)
    calls <- character()
    result <- f(".agent_with_fallback")(
        list(model = "fixture", provider = "anthropic", history = s$history), s,
        .call = function(args) {
            calls <<- c(calls, args$provider)
            if (args$provider == "anthropic") stop("HTTP 429 rate limit")
            list(content = "done", history = args$history)
        })
    expect_identical(calls, c("anthropic", "openai"))
    f(".fallback_reset")()

    if (requireNamespace("pensar", quietly = TRUE)) {
        vault <- file.path(root, "vault")
        pensar::init_vault(vault, rproj = FALSE, agent_instructions = FALSE)
        for (provider in c("anthropic", "openai")) {
            s <- make(provider, vault)
            ids <- names(f(".context_edit_state")(s)$messages)
            uid <- ids[startsWith(ids, "u")]
            mid <- ids[startsWith(ids, "m")]
            expect_equal(length(uid), 1L)
            expect_equal(length(mid), 1L)
            s$history <- c(s$history, list(batch(s, "invalid")))
            expect_error(f(".context_edit_queue")(s,
                list(op = "replace", ids = uid, text = "Invented user words")), "never replaced")
            s$history <- head(s$history, -1L)
            queue(s, c(uid, mid))
            original <- s$history
            ops <- s$pending_context_edits
            expect_true(f(".context_edit_checkpoint")(s))
            expect_false(grepl("Earlier topic, original words", rendered(s), fixed = TRUE))
            expect_true(grepl("Current request: keep this", rendered(s), fixed = TRUE))
            expect_true(grepl("Filed:", rendered(s), fixed = TRUE))
            paths <- list.files(file.path(vault, "raw", "chats"), pattern = "[.]json$", full.names = TRUE)
            records <- lapply(paths, f(".journal_read_json"))
            expect_true(any(vapply(records, function(r) {
                grepl("Earlier topic, original words", paste(vapply(r$history,
                    f(".compact_entry_body"), ""), collapse = "\n"), fixed = TRUE)
            }, logical(1))))
            expect_true(grepl("Earlier topic, original words", paste(vapply(
                f("journal_history")(f("journal_dir")(s)), f(".compact_entry_body"), ""),
                collapse = "\n"), fixed = TRUE))
            n <- length(paths)
            f(".context_archive_write")(s, original, ops)
            expect_equal(length(list.files(file.path(vault, "raw", "chats"), pattern = "[.]json$")), n)
        }

        # Retiring one tool preserves its exact call/result and long readable
        # output. Updating a working note also files its previous contents.
        s <- make(vault = vault)
        long <- paste0(paste(rep("evidence", 10000L), collapse = " "), " END OF EVIDENCE")
        s$history <- c(head(s$history, 2L), list(
            list(role = "assistant", content = list(list(type = "tool_use",
                id = "read", name = "read_file", input = list(path = "source.txt")))),
            list(role = "user", content = list(list(type = "tool_result",
                tool_use_id = "read", content = long))),
            list(role = "user", content = "Current request.")))
        f(".context_edit_prepare")(s)
        queue(s, unlist(f(".context_edit_state")(s)$calls, use.names = FALSE),
              op = "replace", text = "Evidence retained in vault.")
        before <- s$history
        original_pair <- before[3:4]
        pending <- s$pending_context_edits
        path <- f(".context_archive_write")(s, before, pending)
        expect_true(any(grepl("END OF EVIDENCE", readLines(path), fixed = TRUE)))
        native <- sub("[.]md$", ".json", path)
        expect_identical(f(".journal_read_json")(native)$history, original_pair)
        expect_true(f(".context_edit_checkpoint")(s))
        expect_false(grepl("END OF EVIDENCE", rendered(s), fixed = TRUE))
        expect_true(grepl("Evidence retained in vault.", rendered(s), fixed = TRUE))
        queue(s, character(), op = "note", text = "Previous working note.")
        expect_true(f(".context_edit_checkpoint")(s))
        queue(s, character(), op = "note", text = "Updated working note.")
        path <- f(".context_archive_write")(s, s$history, s$pending_context_edits)
        expect_true(any(grepl("Previous working note.", readLines(path), fixed = TRUE)))

        adopted <- file.path(root, "adopted")
        pensar::init_vault(adopted, adopt = TRUE)
        s <- make(vault = adopted)
        expect_warning(f(".context_conversation_enable")(s, pure, adopted), "read-only")
        expect_false(f(".context_edit_enabled")(s))
        expect_false(f(".context_edit_archives")(s))
        s <- make(vault = adopted)
        queue(s, names(f(".context_edit_state")(s)$messages)[1L])
        before <- s$history
        expect_true(f(".context_edit_checkpoint")(s))
        expect_identical(head(s$history, -1L), head(before, -1L))
        expect_true(grepl("read-only", rendered(s), fixed = TRUE))
        expect_false(dir.exists(file.path(adopted, "raw")))

        # A vault write failure commits a rejection receipt, preserving every
        # original entry. A later checkpoint can retry the same edit.
        bad <- file.path(root, "blocked-vault")
        dir.create(bad)
        writeLines("schema", file.path(bad, "schema.md"))
        for (provider in c("anthropic", "openai")) {
            unlink(file.path(bad, "raw"), recursive = TRUE)
            writeLines("not a directory", file.path(bad, "raw"))
            s <- make(provider, bad)
            id <- names(f(".context_edit_state")(s)$messages)[1L]
            queue(s, id)
            before <- s$history
            mark <- f("journal_mark")(s)
            expect_true(suppressWarnings(f(".context_edit_checkpoint")(s)))
            expect_identical(head(s$history, -1L), head(before, -1L))
            expect_true(grepl("Context edit rejected:", rendered(s), fixed = TRUE))
            expect_false(tail(s$context_edit_events, 1L)[[1L]]$applied)
            expect_false(f("journal_rewritten_since")(s, mark,
                ignore = "context_edit_rejected"))
            expect_identical(s$pending_context_edits, list())
            unlink(file.path(bad, "raw"))
            queue(s, id)
            expect_true(f(".context_edit_checkpoint")(s))
            expect_true(tail(s$context_edit_events, 1L)[[1L]]$applied)
            expect_true(grepl("Filed:", rendered(s), fixed = TRUE))
        }

        # Drive turn() through a real dispatch/checkpoint with a failed filing.
        # The model's next step receives the rejection and can finish its reply.
        local({
            unlink(file.path(bad, "raw"), recursive = TRUE)
            writeLines("not a directory", file.path(bad, "raw"))
            s <- make(vault = bad)
            ids <- names(f(".context_edit_state")(s)$messages)
            args <- list(op = "drop", ids = ids[startsWith(ids, "m")])
            original_agent <- llm.api::agent
            on.exit(utils::assignInNamespace("agent", original_agent, "llm.api"), add = TRUE)
            continued <- FALSE
            utils::assignInNamespace("agent", function(prompt, history, tool_handler,
                history_callback, checkpoint_callback, ...) {
                h <- c(history, list(list(role = "user", content = prompt),
                    list(role = "assistant", content = list(list(type = "tool_use",
                        id = "failed-filing", name = "edit_context", input = args)))))
                history_callback(h)
                value <- tool_handler("edit_context", args, context = list(
                    provider = "anthropic", call_index = 1L, call_count = 1L, agent_turn = 1L))
                h <- c(h, list(list(role = "user", content = list(list(
                    type = "tool_result", tool_use_id = "failed-filing", content = value)))))
                history_callback(h)
                change <- checkpoint_callback(h, list(agent_turn = 1L, usage = list()))
                h <- change$history
                calls <- llm.api::history_tool_calls(h)
                continued <<- grepl("Context edit rejected:", tail(calls, 1L)[[1L]]$result,
                                   fixed = TRUE)
                h <- c(h, list(list(role = "assistant", content = "The conversation continues.")))
                history_callback(h)
                list(content = "The conversation continues.", history = h, usage = list(), turns = 2L)
            }, "llm.api")
            result <- suppressWarnings(corteza::turn("Continue after filing.", s, tools = list()))
            expect_true(continued)
            expect_equal(result$reply, "The conversation continues.")
            expect_true(grepl("Earlier answer.", rendered(s), fixed = TRUE))
            expect_false(tail(s$context_edit_events, 1L)[[1L]]$applied)
        })

        # A failed journal write leaves an idempotent archive, but keeps the
        # live window. Retrying that edit never creates a second raw record.
        s <- make(vault = vault)
        uid <- names(f(".context_edit_state")(s)$messages)[1L]
        queue(s, uid)
        before <- s$history
        pending <- s$pending_context_edits
        block <- file.path(f("journal_dir")(s), "live.json.tmp")
        dir.create(block)
        expect_error(suppressWarnings(f(".context_edit_checkpoint")(s)), "journal")
        expect_identical(s$history, before)
        n <- length(list.files(file.path(vault, "raw", "chats"), pattern = "[.]json$"))
        unlink(block, recursive = TRUE)
        s$pending_context_edits <- pending
        expect_true(f(".context_edit_checkpoint")(s))
        expect_equal(length(list.files(file.path(vault, "raw", "chats"), pattern = "[.]json$")), n)

        # Restart restores the edited window and its consumed event IDs in
        # one envelope, so startup backfill cannot revive the retired turn.
        s$seen_event_ids <- c("$earlier", "$current")
        s$rehydrate_checked <- TRUE
        f(".context_room_snapshot")(s)
        restored <- corteza::new_session(provider = "anthropic", system = "restored")
        cfg <- list(talker = list(enabled = TRUE, context_vault = vault))
        expect_true(f(".context_room_restore")(restored, f("journal_dir")(s), cfg))
        expect_identical(restored$history, s$history)
        expect_identical(restored$seen_event_ids, s$seen_event_ids)
        expect_true(restored$rehydrate_checked)
        f(".context_room_clear")(s)
        expect_true(f(".context_room_restore")(restored, f("journal_dir")(s), cfg))
        expect_equal(length(restored$history), 0L)

        if (requireNamespace("chat.api", quietly = TRUE)) {
            lo <- chat.api::chat_loopback()
            room <- "!room:example.invalid"
            human <- chat.api::chat_identity("human")
            old_id <- chat.api::chat_send(lo, room, "Earlier topic, original words.", identity = human)
            reply_id <- chat.api::chat_send(lo, room, "Earlier answer.")
            current_id <- chat.api::chat_send(lo, room, "Current request: keep this.", identity = human)
            originals <- lapply(c("bot_chat_client", "bot_room_cwd"), f)
            names(originals) <- c("bot_chat_client", "bot_room_cwd")
            on.exit(for (name in names(originals)) {
                utils::assignInNamespace(name, originals[[name]], "corteza")
            }, add = TRUE)
            utils::assignInNamespace("bot_chat_client", function(...) lo, "corteza")
            utils::assignInNamespace("bot_room_cwd", function(...) root, "corteza")
            cfg <- list(user = "test", user_id = "@bot:example.invalid", provider = "ollama",
                        model = "fixture", talker = list(enabled = TRUE, model = "fixture",
                        context_edit = pure, context_vault = vault))
            reg <- f("bot_new_session_registry")()
            live <- f("bot_get_or_create_session")(reg, room, cfg, system = "Pinned")
            live$history <- list(list(role = "user", content = "Earlier topic, original words."),
                                 list(role = "assistant", content = "Earlier answer."),
                                 list(role = "user", content = "Current request: keep this."))
            live$seen_event_ids <- c(old_id, reply_id, current_id)
            live$rehydrate_checked <- TRUE
            f(".context_room_snapshot")(live)
            f(".context_edit_prepare")(live)
            ids <- names(f(".context_edit_state")(live)$messages)
            queue(live, ids)
            expect_true(f(".context_edit_checkpoint")(live))
            chat.api::chat_send(lo, room, "New main-timeline message.", identity = human)
            chat.api::chat_send(lo, room, "Earlier topic, original words.", identity = human,
                                thread = "$separate-thread")
            restarted <- f("bot_new_session_registry")()
            expect_equal(f("bot_backfill_sessions")(lo, restarted, cfg, system = "Pinned"), 1L)
            main <- get(room, restarted)
            thread <- get(f("bot_session_key")(room, "$separate-thread"), restarted)
            expect_false(grepl("Earlier topic, original words.", rendered(main), fixed = TRUE))
            expect_true(grepl("New main-timeline message.", rendered(main), fixed = TRUE))
            expect_true(grepl("Earlier topic, original words.", rendered(thread), fixed = TRUE))
            expect_false(grepl("Current request", rendered(thread), fixed = TRUE))
            cleared <- f("bot_reset_session")(restarted, room, cfg, "$clear", "Cleared.", system = "Pinned")
            expect_equal(length(cleared$history), 0L)
            expect_true("$clear" %in% cleared$seen_event_ids)

            # An unsplit bot owns its conversation without becoming a
            # talker or changing its model/tools. Its workers never file.
            cfg$talker <- NULL
            cfg$user_id <- "@unsplit:example.invalid"
            cfg$context_edit <- pure
            cfg$context_vault <- vault
            unified <- f("bot_get_or_create_session")(
                f("bot_new_session_registry")(), room, cfg, system = "Pinned")
            expect_false(isTRUE(unified$talker))
            expect_identical(unified$model_map$cloud, "fixture")
            expect_false(identical(unified$tools_filter, f("TALKER_TOOLS")))
            expect_true(f(".context_edit_pure")(unified))
            expect_true(f(".context_edit_archives")(unified))
            unified$history <- list(list(role = "user", content = "Unsplit earlier topic."),
                                   list(role = "assistant", content = "Earlier reply."),
                                   list(role = "user", content = "Latest topic."))
            unified$seen_event_ids <- c("$old-unsplit", "$new-unsplit")
            f(".context_edit_prepare")(unified)
            queue(unified, names(f(".context_edit_state")(unified)$messages))
            expect_true(f(".context_edit_checkpoint")(unified))
            expect_true(grepl("Filed:", rendered(unified), fixed = TRUE))
            expect_false(grepl("Unsplit earlier topic.", rendered(unified), fixed = TRUE))
            f(".context_room_snapshot")(unified)
            again <- f("bot_get_or_create_session")(
                f("bot_new_session_registry")(), room, cfg, system = "Pinned")
            expect_identical(again$history, unified$history)
            expect_identical(again$seen_event_ids, unified$seen_event_ids)
            expect_identical(f("job_worker_spec")(unified)$context_edit, pure)
            expect_null(f("job_worker_spec")(unified)$context_vault)
            expect_null(f("job_worker_spec")(unified, "reviewer")$context_edit)
            expect_null(f("job_worker_spec")(unified, "monitor")$context_edit)

            # Invalid conversation configuration must not escape the real
            # room factory and terminate the bot's poll loop.
            no_schema <- file.path(root, "no-schema")
            dir.create(no_schema)
            for (split in c(FALSE, TRUE)) {
                for (invalid in list(NULL, no_schema, adopted, 42)) {
                    broken <- cfg
                    broken$context_vault <- invalid
                    if (split) broken$talker <- list(enabled = TRUE, model = "fixture")
                    registry <- f("bot_new_session_registry")()
                    expect_warning(room_session <- f("bot_get_or_create_session")(
                        registry, room, broken, system = "Pinned"),
                        "Conversation context editing disabled")
                    expect_false(f(".context_edit_enabled")(room_session))
                    expect_false(f(".context_edit_pure")(room_session))
                    expect_false(f(".context_edit_archives")(room_session))
                    expect_identical(get(room, registry), room_session)
                    expect_identical(f("bot_get_or_create_session")(
                        registry, room, broken, system = "Pinned"), room_session)
                }
                broken <- cfg
                if (split) {
                    broken$talker <- list(enabled = TRUE, model = "fixture",
                                         context_edit = "typo")
                } else {
                    broken$context_edit <- "typo"
                }
                expect_warning(room_session <- f("bot_get_or_create_session")(
                    f("bot_new_session_registry")(), room, broken, system = "Pinned"),
                    "Conversation context editing disabled")
                expect_false(f(".context_edit_enabled")(room_session))
            }
        }
    }
})
