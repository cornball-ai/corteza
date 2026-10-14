# Pure CLM is a session policy, independent of where retired content goes.
# Only a conversation owner with an explicit vault can retire human turns.
.context_edit_pure <- function(session) {
    cfg <- .context_edit_config(session$config, session$context_edit)
    isTRUE(cfg$enabled) && identical(cfg$strategy, "clm")
}

.context_edit_archives <- function(session) {
    isTRUE(session$context_conversation) && !is.null(session$context_archive_vault)
}

# Both a split talker and an unsplit room session own conversations. Worker
# initialization never calls this, so vault filing cannot follow inheritance.
.context_conversation_enable <- function(session, editing = NULL, vault = NULL) {
    base <- .context_edit_config(session$config, session$context_edit)
    effective <- .context_edit_config(list(context_edit = base), editing)
    if ((!is.null(vault) && (!is.character(vault) || length(vault) != 1L ||
         is.na(vault) || !nzchar(vault))) ||
        (isTRUE(effective$enabled) && identical(effective$strategy, "clm") && is.null(vault))) {
        stop("A pure CLM conversation needs an explicit context_vault", call. = FALSE)
    }
    session$context_edit <- effective
    session$context_archive_vault <- vault
    session$context_conversation <- TRUE
    invisible(session)
}

.context_edit_last_human <- function(history) {
    tail(c(0L, which(vapply(history, .context_edit_human, logical(1)))), 1L)
}

# Mixed human/tool-result messages stay protected: deleting them would also
# delete results whose calls may still be present. Ordinary multimodal human
# messages can be archived verbatim in the native JSON record.
.context_edit_archivable_human <- function(message) {
    .context_edit_human(message) &&
        !(is.list(message$content) && any(vapply(message$content, function(b) {
            identical(b$type, "tool_result")
        }, logical(1))))
}

.context_edit_require <- function(session) {
    if (.context_edit_pure(session) && !.context_edit_enabled(session)) {
        stop("Pure CLM requires a supported provider and an enabled journal",
             call. = FALSE)
    }
}

# Keep source entries, including paired tool calls, before drop/replace.
# Whole batch messages provide surrounding evidence when a single result is
# retired; the IDs and operations identify exactly what the edit targeted.
.context_archive_records <- function(session, history, pending) {
    indices <- integer()
    for (edit in pending) {
        for (target in edit$targets) {
            indices <- c(indices, if (target$kind == "call") {
                c(target$call$call_message_index, target$call$result_message_index)
            } else target$index)
        }
        if (edit$op == "note") {
            note <- .context_edit_state(session)$note
            indices <- c(indices, which(vapply(history, identical, logical(1), note)))
        }
    }
    history[sort(unique(indices))]
}

# Immutable, content-addressed raw records. This deliberately does not use
# pensar::ingest(), whose vault_commit() can stage other work and auto-push.
# Index/log bookkeeping still goes through pensar's public APIs.
.context_archive_write <- function(session, history, pending) {
    if (!.context_edit_archives(session)) return(NULL)
    records <- .context_archive_records(session, history, pending)
    if (!length(records)) return(NULL)
    if (!requireNamespace("pensar", quietly = TRUE)) {
        stop("Conversation context filing requires pensar", call. = FALSE)
    }
    vault <- normalizePath(path.expand(session$context_archive_vault), mustWork = TRUE)
    if (!file.exists(file.path(vault, "schema.md"))) {
        stop("Conversation context vault has no schema.md: ", vault, call. = FALSE)
    }
    if (isTRUE(pensar::status(vault)$adopted)) {
        stop("Conversation context vault is adopted read-only: ", vault, call. = FALSE)
    }
    record <- list(owner = session$job_owner, session = session$job_key,
                   room = session$room_id,
                   edits = lapply(pending, function(e) e[c("op", "ids", "text")]),
                   history = records)
    # Hash the portable serialization so restart/retry has the same identity.
    payload <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null",
                                na = "null", digits = NA)
    id <- digest::digest(payload, algo = "sha256", serialize = FALSE)
    base <- paste0("clm-", id)
    dir <- file.path(vault, "raw", "chats")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    native <- file.path(dir, paste0(base, ".json"))
    path <- file.path(dir, paste0(base, ".md"))
    if (!file.exists(native)) {
        .journal_write_json(record, native)
    } else if (!identical(jsonlite::toJSON(.journal_read_json(native),
        auto_unbox = TRUE, null = "null", na = "null", digits = NA), payload)) {
        stop("Context archive identity collision: ", native, call. = FALSE)
    }
    if (!file.exists(path)) {
        text <- c("---", "title: Retired conversation context", "type: chat",
                  paste0("source: ", jsonlite::toJSON(session$job_key %||% "session",
                                                     auto_unbox = TRUE)),
                  paste0("date: ", Sys.Date()), "tags: [context-retirement]", "---", "",
                  "# Retired conversation context", "",
                  paste0("Native record: [JSON](", base, ".json)"), "",
                  "## Filing decisions", "",
                  vapply(record$edits, function(e) paste(e$op,
                      paste(e$ids, collapse = ", "), e$text %||% ""), ""), "",
                  "## Original context", "",
                  vapply(records, function(m) {
                      sprintf("[%s]: %s", m$role %||% "unknown", .compact_entry_body(m))
                  }, ""))
        tmp <- paste0(path, ".tmp")
        on.exit(unlink(tmp), add = TRUE)
        writeLines(text, tmp, useBytes = TRUE)
        if (!isTRUE(file.rename(tmp, path))) {
            stop("Could not commit context archive: ", path, call. = FALSE)
        }
        pensar::log_entry(paste("Filed context", base), operation = "ingest", vault = vault)
    }
    pensar::update_index(vault)
    path
}
