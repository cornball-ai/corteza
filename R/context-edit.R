# Model-requested edits to completed conversation entries. Instructions and
# tool schemas are separate request parameters and never enter this module.

.context_edit_config <- function(config = NULL, override = NULL) {
    value <- config$context_edit %||% list()
    if (!is.list(value)) {
        stop("context_edit must be a list", call. = FALSE)
    }
    if (is.logical(override) && length(override) == 1L && !is.na(override)) {
        override <- list(enabled = override)
    }
    if (!is.null(override)) {
        if (!is.list(override)) {
            stop("context_edit override must be a list or logical", call. = FALSE)
        }
        value <- utils::modifyList(value, override)
    }
    value <- utils::modifyList(list(enabled = FALSE, cadence = "pressure"), value)
    if (!is.logical(value$enabled) || length(value$enabled) != 1L ||
        is.na(value$enabled) || !is.character(value$cadence) ||
        length(value$cadence) != 1L || is.na(value$cadence) ||
        !value$cadence %in% c("pressure", "always")) {
        stop("context_edit needs enabled = TRUE/FALSE and cadence = pressure/always",
             call. = FALSE)
    }
    value
}

.context_edit_supported <- function(session) {
    provider <- session$context_edit_provider %||% session$provider
    provider %in% c("anthropic", "anthropic_claude", "openai",
                            "openai_compatible", "moonshot", "ollama") &&
        !identical(.history_shape(session$history), "responses")
}

.context_edit_enabled <- function(session) {
    is.environment(session) &&
        isTRUE(.context_edit_config(session$config, session$context_edit)$enabled) &&
        .context_edit_supported(session) && journal_enabled(session)
}

.context_edit_state <- function(session) {
    key <- list(session$sessionId, session$context_edit_provider %||% session$provider,
                journal_dir(session))
    state <- session$.context_edit
    if (is.null(state) || !identical(state$key, key)) {
        state <- new.env(parent = emptyenv())
        state$key <- key
        # A restored window can still display IDs from its previous process.
        # Allocate beyond those so a stale reference cannot select a new entry.
        rendered <- vapply(session$history %||% list(), .compact_entry_body, character(1))
        labels <- unlist(regmatches(rendered, gregexpr("\\[[rm][0-9]+\\]", rendered)))
        numbers <- suppressWarnings(as.numeric(gsub("[^0-9]", "", labels)))
        state$next_id <- max(c(0, numbers[is.finite(numbers)]))
        state$calls <- list()
        state$messages <- list()
        notes <- Filter(function(m) .context_edit_plain(m) &&
            startsWith(.compact_entry_body(m), "[context note]\n"),
            session$history %||% list())
        state$note <- if (length(notes)) tail(notes, 1L)[[1L]] else NULL
        state$pressure_open <- FALSE
        state$notice_pending <- FALSE
        session$.context_edit <- state
        session$pending_context_edits <- list()
    }
    state
}

.context_edit_id <- function(state, prefix) {
    state$next_id <- state$next_id + 1L
    paste0(prefix, format(state$next_id, scientific = FALSE, trim = TRUE))
}

.context_edit_call_id <- function(state, call) {
    key <- digest::digest(list(call$id, call$name, call$arguments))
    if (is.null(state$calls[[key]])) {
        state$calls[[key]] <- .context_edit_id(state, "r")
    }
    state$calls[[key]]
}

.context_edit_boundary <- function(history) {
    which_assistant <- which(vapply(history, function(m) {
        identical(m$role, "assistant")
    }, logical(1)))
    if (length(which_assistant)) tail(which_assistant, 1L) else 1L
}

# A mixed human/tool-result message is protected as a whole. Never infer that
# human text is synthetic merely because it starts with a harness marker.
.context_edit_human <- function(message) {
    identical(message$role, "user") &&
        !(is.list(message$content) && length(message$content) > 0L &&
          all(vapply(message$content, function(b) {
              identical(b$type, "tool_result")
          }, logical(1))))
}

.context_edit_plain <- function(message) {
    identical(message$role, "assistant") && !length(message$tool_calls) &&
        (is.character(message$content) ||
         (is.list(message$content) && length(message$content) > 0L &&
          all(vapply(message$content, function(b) identical(b$type, "text"),
                     logical(1)))))
}

.context_edit_targets <- function(session, history, boundary) {
    state <- .context_edit_state(session)
    targets <- list()
    for (call in llm.api::history_tool_calls(history)) {
        if (!isTRUE(call$completed) || call$call_message_index >= boundary ||
            call$result_message_index >= boundary ||
            .context_edit_human(history[[call$result_message_index]])) next
        id <- .context_edit_call_id(state, call)
        targets[[id]] <- list(kind = "call", call = call)
    }
    for (id in names(state$messages)) {
        found <- which(vapply(history, identical, logical(1), state$messages[[id]]))
        if (length(found) == 1L && found < boundary) {
            targets[[id]] <- list(kind = "message", index = found)
        }
    }
    targets
}

.context_edit_result_content <- function(history, call, content) {
    i <- call$result_message_index
    if (identical(call$provider_shape, "openai")) {
        history[[i]]$content <- content
    } else {
        for (j in seq_along(history[[i]]$content)) {
            if (identical(history[[i]]$content[[j]]$type, "tool_result") &&
                identical(history[[i]]$content[[j]]$tool_use_id, call$id)) {
                history[[i]]$content[[j]]$content <- content
            }
        }
    }
    history
}

# Label a rendered copy, then journal it before changing the live history.
# Only plain assistant messages and completed tool outputs get editable IDs.
.context_edit_index <- function(session, history, boundary) {
    state <- .context_edit_state(session)
    for (call in llm.api::history_tool_calls(history)) {
        if (!isTRUE(call$completed) || call$result_message_index >= boundary ||
            .context_edit_human(history[[call$result_message_index]])) next
        id <- .context_edit_call_id(state, call)
        if (!startsWith(call$result, paste0("[", id, "]\n"))) {
            # Keep non-text output blocks intact when adding a label.
            i <- call$result_message_index
            if (identical(call$provider_shape, "anthropic")) {
                block <- Filter(function(b) identical(b$tool_use_id, call$id),
                                history[[i]]$content)[[1L]]
                content <- block$content
                if (is.list(content)) {
                    content <- c(list(list(type = "text", text = paste0("[", id, "]\n"))), content)
                } else {
                    content <- paste0("[", id, "]\n", content)
                }
            } else {
                content <- paste0("[", id, "]\n", call$result)
            }
            history <- .context_edit_result_content(history, call, content)
        }
    }
    for (i in seq_len(max(0L, boundary - 1L))) {
        m <- history[[i]]
        if (!.context_edit_plain(m) || identical(m, state$note) ||
            startsWith(.compact_entry_body(m), "[context note]\n") ||
            any(vapply(state$messages, identical, logical(1), m))) next
        id <- .context_edit_id(state, "m")
        if (is.character(m$content)) {
            m$content <- paste0("[", id, "]\n", m$content)
        } else {
            m$content <- c(list(list(type = "text", text = paste0("[", id, "]\n"))), m$content)
        }
        history[[i]] <- m
        state$messages[[id]] <- m
    }
    history
}

.context_edit_prepare <- function(session) {
    session$pending_context_edits <- list()
    session$context_edit_events <- list()
    if (!.context_edit_enabled(session)) return(invisible(NULL))
    if (is.null(journal_dir(session))) {
        # Embedded new_session()/turn() callers may have no transcript ID.
        # Give them a unique durable journal without touching the transcript.
        journal_install(session, dir = tempfile("context-",
            tmpdir = sessions_dir(DEFAULT_AGENT_ID), fileext = ".journal"))
    }
    state <- .context_edit_state(session)
    state$notice_batch <- NULL
    state$notice_pending <- FALSE
    before <- session$history %||% list()
    after <- .context_edit_index(session, before, length(before) + 1L)
    if (!identical(before, after)) {
        journal_rewrite(session, before, after, "context_index", edits = list())
        session$history <- after
    }
}

.context_edit_schema <- function() {
    list(name = "edit_context",
         description = paste("Manage completed conversation entries by visible ID.",
             "drop removes entries (tool calls and results together).",
             "replace summarizes entries, keeping the first ID and its tool call if any.",
             "note replaces the working note with concise task facts, decisions, and next actions.",
             "Instructions, human messages, and the current tool batch are protected.",
             "Edits are queued and validated after the complete tool batch."),
         input_schema = list(type = "object", properties = list(
             op = list(type = "string", enum = c("drop", "replace", "note")),
             ids = list(type = "array", items = list(type = "string")),
             text = list(type = "string")), required = "op",
             additionalProperties = FALSE))
}

.context_edit_tools <- function(tools, session) {
    tools <- Filter(function(t) !identical(t$name, "edit_context"), tools)
    if (.context_edit_enabled(session)) c(tools, list(.context_edit_schema())) else tools
}

.context_edit_system <- function(system, session) {
    if (!.context_edit_enabled(session)) return(system)
    paste(system, paste(
        "\n# Conversation context management",
        "Tool outputs have [rN] IDs; earlier assistant text has [mN] IDs.",
        "Use edit_context to drop spent material or replace it with concise task-specific summaries.",
        "Use note for task facts, decisions, and next actions; it stays near the recent conversation.",
        "Keep useful evidence and unresolved constraints. Never target instructions or human messages.",
        "The current assistant/tool batch is unavailable until a later batch.",
        if (.context_edit_config(session$config, session$context_edit)$cadence == "pressure") {
            "Edit only after a context-pressure notice. Threshold compaction remains the fallback."
        } else "Edits are allowed at each checkpoint.", sep = "\n"), sep = "\n")
}

.context_edit_pct <- function(session, extra = "") {
    limit <- session$context_window %||% session$config$context_window %||%
        context_limit_for_model(.resolve_model(session), provider = session$provider)
    if (is.null(limit) || !is.finite(limit) || limit <= 0) return(0)
    used <- estimate_live_context_tokens(list(history = session$history),
        system_prompt = session$context_edit_system %||% session$system,
        tools = session$context_edit_tools %||% list())
    100 * (used + nchar(extra) / 4) / limit
}

.context_edit_result <- function(session, text, name, context) {
    if (!.context_edit_enabled(session)) return(text)
    state <- .context_edit_state(session)
    calls <- llm.api::history_tool_calls(session$history)
    pending <- Filter(function(c) !isTRUE(c$completed) &&
        c$call_message_index == .context_edit_boundary(session$history) &&
        identical(unsanitize_tool_name(c$name), name), calls)
    if (length(pending)) {
        id <- .context_edit_call_id(state, pending[[1L]])
        text <- paste0("[", id, "]\n", text)
    }
    pct <- .context_edit_pct(session, text)
    batch <- context$agent_turn %||% session$turn_number
    if (pct >= (session$config$context_warn_pct %||% 75) &&
        !identical(state$notice_batch, batch)) {
        text <- paste0(text, sprintf(
            "\n[context pressure: %.0f%%; use edit_context on older [rN]/[mN] entries before threshold compaction]", pct))
        state$notice_pending <- TRUE
        state$notice_batch <- batch
    }
    text
}

.context_edit_queue <- function(session, args) {
    if (!.context_edit_enabled(session)) stop("Context editing is disabled", call. = FALSE)
    if (isTRUE(session$dry_run) || isTRUE(session$config$dry_run)) {
        return("[dry run: context edit not queued]")
    }
    state <- .context_edit_state(session)
    cfg <- .context_edit_config(session$config, session$context_edit)
    if (cfg$cadence == "pressure" && !isTRUE(state$pressure_open)) {
        stop("Context edits require a context-pressure notice first", call. = FALSE)
    }
    op <- args$op
    if (!is.character(op) || length(op) != 1L || !op %in% c("drop", "replace", "note")) {
        stop("op must be drop, replace, or note", call. = FALSE)
    }
    ids <- unlist(args$ids, use.names = FALSE)
    if (op != "note" && (!is.character(ids) || !length(ids) ||
        anyNA(ids) || anyDuplicated(ids))) stop("Supply distinct editable ids", call. = FALSE)
    if (op == "note" && length(ids)) stop("note takes text, not ids", call. = FALSE)
    if (op != "drop" && (!is.character(args$text) || length(args$text) != 1L ||
        is.na(args$text) || !nzchar(trimws(args$text)))) stop("Supply non-empty text", call. = FALSE)
    targets <- .context_edit_targets(session, session$history,
                                      .context_edit_boundary(session$history))
    if (length(ids) && !all(ids %in% names(targets))) {
        stop("Unknown, stale, or protected context id", call. = FALSE)
    }
    # Queue immutable targets, not just IDs, so compaction cannot retarget an edit.
    requests <- Filter(function(c) !isTRUE(c$completed) && c$name == "edit_context" &&
        c$call_message_index == .context_edit_boundary(session$history),
        llm.api::history_tool_calls(session$history))
    session$pending_context_edits <- c(session$pending_context_edits,
        list(list(op = op, ids = ids, text = args$text, targets = targets[ids],
                  request_id = if (length(requests)) requests[[1L]]$id else NULL)))
    "Context edit queued; the checkpoint will validate and apply it."
}

.context_edit_rewrite <- function(session, history, pending) {
    boundary <- .context_edit_boundary(history)
    targets <- .context_edit_targets(session, history, boundary)
    ids <- unlist(lapply(pending, `[[`, "ids"), use.names = FALSE)
    if (anyDuplicated(ids)) stop("Overlapping context edits in one batch", call. = FALSE)
    for (edit in pending) {
        if (!all(edit$ids %in% names(targets)) ||
            !identical(targets[edit$ids], edit$targets)) {
            stop("Context edit became stale after compaction", call. = FALSE)
        }
    }
    state <- .context_edit_state(session)
    drop_calls <- character()
    drop_messages <- integer()
    note <- state$note
    messages <- state$messages
    for (edit in pending) {
        if (edit$op == "note") {
            note <- list(role = "assistant", content = paste0("[context note]\n", edit$text))
            next
        }
        remove <- edit$ids
        if (edit$op == "replace") {
            id <- edit$ids[[1L]]
            target <- targets[[id]]
            text <- paste0("[", id, "]\n[context summary]\n", edit$text)
            if (target$kind == "call") {
                history <- .context_edit_result_content(history, target$call, text)
            } else {
                history[[target$index]]$content <- text
                messages[[id]] <- history[[target$index]]
            }
            remove <- remove[-1L]
        }
        for (id in remove) {
            target <- targets[[id]]
            if (target$kind == "call") {
                drop_calls <- c(drop_calls, target$call$id)
            } else {
                drop_messages <- c(drop_messages, target$index)
                messages[[id]] <- NULL
            }
        }
    }
    out <- list()
    for (i in seq_along(history)) {
        # Relocate only the harness-owned note, keeping it next to the tail.
        if (i == boundary && !is.null(note)) out <- c(out, list(note))
        m <- history[[i]]
        if (i < boundary && !is.null(state$note) && identical(m, state$note)) next
        if (i %in% drop_messages) next
        if (i < boundary && identical(m$role, "tool") && m$tool_call_id %in% drop_calls) next
        if (i < boundary && identical(m$role, "assistant")) {
            if (length(m$tool_calls)) {
                m$tool_calls <- Filter(function(c) !c$id %in% drop_calls, m$tool_calls)
                if (!length(m$tool_calls)) m$tool_calls <- NULL
            }
            if (is.list(m$content)) {
                m$content <- Filter(function(b) !(identical(b$type, "tool_use") &&
                    b$id %in% drop_calls), m$content)
                substantive <- vapply(m$content, function(b) {
                    !b$type %in% c("thinking", "redacted_thinking")
                }, logical(1))
                if (!any(substantive) && !length(m$tool_calls)) next
            } else if (!nzchar(m$content %||% "") && !length(m$tool_calls)) next
        }
        if (i < boundary && identical(m$role, "user") && !.context_edit_human(m)) {
            m$content <- Filter(function(b) !b$tool_use_id %in% drop_calls, m$content)
            if (!length(m$content)) next
        }
        out <- c(out, list(m))
    }
    list(history = out, note = note, messages = messages)
}

.context_edit_receipt <- function(session, history, text, request_ids) {
    boundary <- .context_edit_boundary(history)
    for (call in llm.api::history_tool_calls(history)) {
        if (call$call_message_index == boundary && call$id %in% request_ids && call$completed) {
            id <- .context_edit_call_id(.context_edit_state(session), call)
            history <- .context_edit_result_content(history, call, paste0("[", id, "]\n", text))
        }
    }
    history
}

.context_edit_checkpoint <- function(session) {
    pending <- session$pending_context_edits %||% list()
    session$pending_context_edits <- list()
    if (!.context_edit_enabled(session)) return(FALSE)
    state <- .context_edit_state(session)
    previous_messages <- state$messages
    committed <- FALSE
    on.exit(if (!committed) state$messages <- previous_messages, add = TRUE)
    # A notice in this batch permits edits on the next model request.
    state$pressure_open <- isTRUE(state$notice_pending)
    state$notice_pending <- FALSE
    before <- session$history
    after <- before
    rewritten <- NULL
    rejected <- NULL
    request_ids <- unlist(lapply(pending, `[[`, "request_id"), use.names = FALSE)
    if (length(pending)) {
        rewritten <- tryCatch(.context_edit_rewrite(session, before, pending),
            error = function(e) { rejected <<- conditionMessage(e); NULL })
        if (is.null(rewritten)) {
            after <- .context_edit_receipt(session, before,
                paste("Context edit rejected:", rejected), request_ids)
        } else {
            after <- .context_edit_receipt(session, rewritten$history,
                "Context edit applied.", request_ids)
        }
    }
    if (!is.null(rewritten)) state$messages <- rewritten$messages
    after <- .context_edit_index(session, after, .context_edit_boundary(after))
    if (identical(before, after)) return(FALSE)
    ops <- lapply(pending, function(e) e[c("op", "ids", "text")])
    # An I/O error propagates from the checkpoint. The old history stays live.
    if (is.null(journal_dir(session))) {
        stop("Context editing requires a session journal", call. = FALSE)
    }
    tryCatch(journal_rewrite(session, before, after,
        if (!is.null(rewritten)) "context_edit" else "context_index", edits = ops),
        error = function(e) {
            if (length(pending)) {
                .context_edit_publish(session, list(ops = ops,
                    ids = unlist(lapply(ops, `[[`, "ids"), use.names = FALSE),
                    tokens_before = estimate_history_tokens(before),
                    tokens_after = estimate_history_tokens(before), applied = FALSE,
                    error = conditionMessage(e),
                    cadence = .context_edit_config(session$config, session$context_edit)$cadence))
            }
            stop(e)
        })
    session$history <- after
    committed <- TRUE
    if (!is.null(rewritten)) {
        state$note <- rewritten$note
        state$pressure_open <- FALSE
    }
    if (length(pending)) {
        event <- list(ops = ops, ids = unlist(lapply(ops, `[[`, "ids"), use.names = FALSE),
            tokens_before = estimate_live_context_tokens(list(history = before)),
            tokens_after = estimate_live_context_tokens(list(history = after)),
            applied = !is.null(rewritten), error = rejected,
            cadence = .context_edit_config(session$config, session$context_edit)$cadence)
        .context_edit_publish(session, event)
    }
    TRUE
}

.context_edit_publish <- function(session, event) {
    do.call(log_event, c(list(event = "context_edit"), event))
    session$context_edit_events <- c(session$context_edit_events, list(event))
    if (is.function(session$on_context_edit)) {
        tryCatch(session$on_context_edit(event), error = function(e) {
            log_event("context_edit_notification_failed", error = conditionMessage(e), level = "warn")
        })
    }
    invisible(NULL)
}

.context_edit_event_text <- function(event) {
    if (!isTRUE(event$applied)) return(paste("Context edit rejected:", event$error))
    sprintf("Context edited: %s; history %.0f -> %.0f tokens.",
        paste(vapply(event$ops, `[[`, character(1), "op"), collapse = ", "),
        event$tokens_before, event$tokens_after)
}

.context_edit_status <- function(session) {
    cfg <- .context_edit_config(session$config, session$context_edit)
    status <- if (cfg$enabled) "on" else "off"
    if (cfg$enabled && !.context_edit_supported(session)) {
        status <- "unavailable for this provider history shape"
    } else if (cfg$enabled && !journal_enabled(session)) {
        status <- "unavailable: journal_enabled is false"
    }
    sprintf("Context editing: %s (cadence: %s).", status, cfg$cadence)
}

.context_edit_command <- function(session, arg = "status") {
    if (!arg %in% c("on", "off", "status", "")) {
        return("Usage: /context-edit on|off|status")
    }
    if (arg %in% c("on", "off")) {
        cfg <- .context_edit_config(session$config, session$context_edit)
        cfg$enabled <- identical(arg, "on")
        session$context_edit <- cfg
        session$config$context_edit <- cfg
        session$pending_context_edits <- list()
    }
    .context_edit_status(session)
}

.context_edit_parse_command <- function(body) {
    command <- tolower(sub("^/+", "", bot_command_text(body)))
    if (!grepl("^context-edit($|[[:space:]])", command)) return(NULL)
    trimws(sub("^context-edit", "", command))
}
