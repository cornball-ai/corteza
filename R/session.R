# Session management for corteza
# Full compatibility with openclaw session format
#
# Storage format (matches openclaw / pi-coding-agent):
#   ~/.corteza/agents/main/sessions/sessions.json  - Session metadata store
#   ~/.corteza/agents/main/sessions/{id}.jsonl     - Transcript per session
#
# JSONL transcript format:
#   Line 1: {"type":"session","version":2,"id":"...","timestamp":"...","cwd":"..."}
#   Lines 2+: {"role":"user|assistant","content":[{"type":"text","text":"..."}],...}

# Constants
SESSION_VERSION <- 2L
DEFAULT_AGENT_ID <- "main"

#' Get the sessions directory (openclaw-compatible)
#' @param agent_id Agent ID (default: "main")
#' @return Path to sessions directory
#' @noRd
sessions_dir <- function(agent_id = DEFAULT_AGENT_ID) {
    file.path(path.expand("~/.corteza"), "agents", agent_id, "sessions")
}

#' Get path to sessions store (metadata for all sessions)
#' @param agent_id Agent ID
#' @return Path to sessions.json
#' @noRd
sessions_store_path <- function(agent_id = DEFAULT_AGENT_ID) {
    file.path(sessions_dir(agent_id), "sessions.json")
}

#' Generate a new session ID (UUID format like openclaw)
#' @return Character string UUID
#' @noRd
session_id <- function() {
    # Generate UUID v4
    hex <- c(0:9, letters[1:6])
    paste0(
           paste0(sample(hex, 8, replace = TRUE), collapse = ""), "-",
           paste0(sample(hex, 4, replace = TRUE), collapse = ""), "-",
           "4", paste0(sample(hex, 3, replace = TRUE), collapse = ""), "-",
           sample(c("8", "9", "a", "b"), 1), paste0(sample(hex, 3, replace = TRUE),
            collapse = ""), "-",
           paste0(sample(hex, 12, replace = TRUE), collapse = "")
    )
}

#' Get path to session transcript file
#' @param id Session ID
#' @param agent_id Agent ID
#' @return Path to transcript JSONL file
#' @noRd
session_transcript_path <- function(id, agent_id = DEFAULT_AGENT_ID) {
    file.path(sessions_dir(agent_id), paste0(id, ".jsonl"))
}

# Store operations ----

#' Load all session metadata from store
#' @param agent_id Agent ID
#' @return Named list of session entries (keyed by session key)
#' @noRd
store_load <- function(agent_id = DEFAULT_AGENT_ID) {
    path <- sessions_store_path(agent_id)

    if (!file.exists(path)) {
        return(list())
    }

    tryCatch(
             jsonlite::fromJSON(path, simplifyVector = FALSE),
             error = function(e) list()
    )
}

#' Save session metadata store
#' @param store Named list of session entries
#' @param agent_id Agent ID
#' @return Invisible path to store file
#' @noRd
store_save <- function(store, agent_id = DEFAULT_AGENT_ID) {
    dir <- sessions_dir(agent_id)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, mode = "0700")
    }

    path <- sessions_store_path(agent_id)
    writeLines(jsonlite::toJSON(store, auto_unbox = TRUE, pretty = TRUE), path)
    invisible(path)
}

#' Update a single session in the store
#' @param session_key Session key (identifier in store)
#' @param updates Named list of fields to update
#' @param agent_id Agent ID
#' @return Updated session entry
#' @noRd
store_update <- function(session_key, updates, agent_id = DEFAULT_AGENT_ID) {
    store <- store_load(agent_id)

    if (is.null(store[[session_key]])) {
        store[[session_key]] <- list(sessionId = updates$sessionId %||% session_id())
    }

    # Merge updates
    for (key in names(updates)) {
        store[[session_key]][[key]] <- updates[[key]]
    }

    store[[session_key]]$updatedAt <- as.numeric(Sys.time()) * 1000 # milliseconds

    store_save(store, agent_id)
    store[[session_key]]
}

# Session operations ----

#' Create a new session
#' @param provider LLM provider name
#' @param model Model name
#' @param cwd Working directory for session
#' @param session_key Session key (default: generates new)
#' @param agent_id Agent ID
#' @return Session list object
#' @noRd
session_new <- function(provider = "anthropic", model = NULL, cwd = getwd(),
                        session_key = NULL, agent_id = DEFAULT_AGENT_ID) {
    id <- session_id()
    now <- as.numeric(Sys.time()) * 1000
    session_key <- session_key %||% paste0("corteza:", id)

    session <- list(
                    sessionId = id,
                    sessionKey = session_key,
                    createdAt = now,
                    updatedAt = now,
                    provider = provider,
                    model = model,
                    cwd = normalizePath(cwd, mustWork = FALSE),
                    inputTokens = 0L,
                    outputTokens = 0L,
                    totalTokens = 0L,
                    compactionCount = 0L,
                    memoryFlushCompactionCount = 0L,
                    messages = list()
    )

    # Write session header to transcript
    transcript_write_header(id, cwd, agent_id)

    # Save to store
    store_update(session_key, list(
                                   sessionId = id,
                                   sessionFile = session_transcript_path(id, agent_id),
                                   createdAt = now,
                                   modelProvider = provider,
                                   model = model,
                                   inputTokens = 0L,
                                   outputTokens = 0L,
                                   totalTokens = 0L,
                                   compactionCount = 0L,
                                   memoryFlushCompactionCount = 0L
        ), agent_id)

    session
}

#' Save session metadata
#' @param session Session object
#' @param agent_id Agent ID
#' @return Invisible session key
#' @noRd
session_save <- function(session, agent_id = DEFAULT_AGENT_ID) {
    session_key <- session$sessionKey %||% paste0("corteza:", session$sessionId)

    store_update(session_key, list(
                                   sessionId = session$sessionId,
                                   sessionFile = session_transcript_path(session$sessionId, agent_id),
                                   modelProvider = session$provider,
                                   model = session$model,
                                   inputTokens = session$inputTokens %||% 0L,
                                   outputTokens = session$outputTokens %||% 0L,
                                   totalTokens = session$totalTokens %||% 0L,
                                   compactionCount = session$compactionCount %||% 0L,
                                   memoryFlushCompactionCount = session$memoryFlushCompactionCount %||% 0L
        ), agent_id)

    invisible(session_key)
}

#' Load session from disk
#' @param session_key Session key
#' @param agent_id Agent ID
#' @param from_compaction If TRUE, only load messages after last compaction
#' @return Session object, or NULL if not found
#' @noRd
session_load <- function(session_key, agent_id = DEFAULT_AGENT_ID,
                         from_compaction = TRUE) {
    store <- store_load(agent_id)
    entry <- store[[session_key]]

    if (is.null(entry)) {
        return(NULL)
    }

    id <- entry$sessionId
    if (is.null(id)) {
        return(NULL)
    }

    # Build session object from store entry + transcript
    session <- list(
                    sessionId = id,
                    sessionKey = session_key,
                    createdAt = entry$createdAt,
                    updatedAt = entry$updatedAt,
                    provider = entry$modelProvider,
                    model = entry$model,
                    cwd = entry$cwd,
                    inputTokens = entry$inputTokens %||% 0L,
                    outputTokens = entry$outputTokens %||% 0L,
                    totalTokens = entry$totalTokens %||% 0L,
                    compactionCount = entry$compactionCount %||% 0L,
                    memoryFlushCompactionCount = entry$memoryFlushCompactionCount %||% 0L,
                    messages = transcript_load(id, agent_id,
            from_compaction = from_compaction)
    )

    session
}

#' List sessions
#' @param agent_id Agent ID
#' @param n Maximum number of sessions to return (most recent first)
#' @return List of session summaries
#' @noRd
session_list <- function(agent_id = DEFAULT_AGENT_ID, n = 20) {
    store <- store_load(agent_id)

    if (length(store) == 0) {
        return(list())
    }

    # Convert to list and sort by updatedAt
    sessions <- lapply(names(store), function(key) {
        entry <- store[[key]]
        entry$sessionKey <- key
        entry$messages <- transcript_count(entry$sessionId, agent_id)
        entry
    })

    # Sort by updatedAt descending
    updated_times <- vapply(sessions, function(s) s$updatedAt %||% 0,
                            numeric(1))
    sessions <- sessions[order(updated_times, decreasing = TRUE)]

    # Limit to n
    head(sessions, n)
}

#' Get the latest session
#' @param agent_id Agent ID
#' @return Session object, or NULL if no sessions exist
#' @noRd
session_latest <- function(agent_id = DEFAULT_AGENT_ID) {
    sessions <- session_list(agent_id, n = 1)

    if (length(sessions) == 0) {
        return(NULL)
    }

    session_load(sessions[[1]]$sessionKey, agent_id)
}

#' Add a message to a session (in memory only)
#' @param session Session object
#' @param role Message role
#' @param content Message content (string)
#' @return Updated session object
#' @noRd
session_add_message <- function(session, role, content) {
    # Store in pi-coding-agent compatible format
    msg <- list(
                role = role,
                content = list(list(type = "text", text = content))
    )

    session$messages <- c(session$messages, list(msg))
    session
}

# Transcript operations ----

#' Write session header to transcript file
#' @param id Session ID
#' @param cwd Working directory
#' @param agent_id Agent ID
#' @return Invisible path to transcript file
#' @noRd
transcript_write_header <- function(id, cwd, agent_id = DEFAULT_AGENT_ID) {
    path <- session_transcript_path(id, agent_id)

    dir <- dirname(path)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, mode = "0700")
    }

    # Only write if file doesn't exist
    if (file.exists(path)) {
        return(invisible(path))
    }

    header <- list(
                   type = "session",
                   version = SESSION_VERSION,
                   id = id,
                   timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                   cwd = normalizePath(cwd, mustWork = FALSE)
    )

    json <- jsonlite::toJSON(header, auto_unbox = TRUE)
    cat(json, "\n", file = path, append = FALSE)

    invisible(path)
}

#' Append a message to session transcript (openclaw format)
#' @param session Session object
#' @param role Message role (user/assistant)
#' @param content Message content (string)
#' @param provider LLM provider (for assistant messages)
#' @param model Model name (for assistant messages)
#' @param usage Token usage list (for assistant messages)
#' @param agent_id Agent ID
#' @return Invisible path to transcript file
#' @noRd
transcript_append <- function(session, role, content, provider = NULL,
                              model = NULL, usage = NULL,
                              agent_id = DEFAULT_AGENT_ID) {
    path <- session_transcript_path(session$sessionId, agent_id)

    # Ensure header exists
    if (!file.exists(path)) {
        transcript_write_header(session$sessionId, session$cwd %||% getwd(),
                                agent_id)
    }

    # Build message in pi-coding-agent format
    msg <- list(
                role = role,
                content = list(list(type = "text", text = content))
    )

    if (role == "assistant") {
        msg$stopReason <- "stop"
        msg$api <- "openai-responses"
        msg$provider <- provider %||% session$provider %||% "corteza"
        msg$model <- model %||% session$model %||% "unknown"
        msg$usage <- usage %||% list(
                                     input = 0L,
                                     output = 0L,
                                     cacheRead = 0L,
                                     cacheWrite = 0L,
                                     totalTokens = 0L,
                                     cost = list(
                input = 0,
                output = 0,
                cacheRead = 0,
                cacheWrite = 0,
                total = 0
            )
        )
        msg$timestamp <- as.numeric(Sys.time()) * 1000
    }

    json <- jsonlite::toJSON(msg, auto_unbox = TRUE)
    cat(json, "\n", file = path, append = TRUE)

    invisible(path)
}

#' Load transcript from disk
#' @param id Session ID
#' @param agent_id Agent ID
#' @param from_compaction If TRUE, only load messages after last compaction
#' @return List of messages
#' @noRd
transcript_load <- function(id, agent_id = DEFAULT_AGENT_ID,
                            from_compaction = TRUE) {
    path <- session_transcript_path(id, agent_id)

    if (!file.exists(path)) {
        return(list())
    }

    lines <- readLines(path, warn = FALSE)
    lines <- lines[nchar(trimws(lines)) > 0]

    if (length(lines) == 0) {
        return(list())
    }

    # Parse all lines
    entries <- lapply(lines, function(line) {
        tryCatch(
                 jsonlite::fromJSON(line, simplifyVector = FALSE),
                 error = function(e) NULL
        )
    })

    entries <- Filter(Negate(is.null), entries)

    # Separate header from messages
    messages <- list()
    for (entry in entries) {
        if (identical(entry$type, "session")) {
            # This is the header, skip
            next
        }
        if (!is.null(entry$role)) {
            messages <- c(messages, list(entry))
        }
    }

    # TODO: Handle compaction markers when implemented
    # For now, return all messages

    messages
}

#' Count messages in transcript
#' @param id Session ID
#' @param agent_id Agent ID
#' @return Number of messages (excluding header)
#' @noRd
transcript_count <- function(id, agent_id = DEFAULT_AGENT_ID) {
    path <- session_transcript_path(id, agent_id)

    if (!file.exists(path)) {
        return(0L)
    }

    lines <- readLines(path, warn = FALSE)
    # Subtract 1 for header line
    max(0L, length(lines) - 1L)
}

#' Append compaction marker to transcript
#' @param session Session object
#' @param summary The compaction summary text
#' @param agent_id Agent ID
#' @return Invisible path to transcript file
#' @noRd
transcript_compact <- function(session, summary, agent_id = DEFAULT_AGENT_ID) {
    # Compaction in pi-coding-agent format is an assistant message with special marker
    # For now, just append as a regular assistant message
    # TODO: Match exact compaction format from pi-coding-agent
    transcript_append(
                      session,
                      role = "assistant",
                      content = paste0("[Compaction Summary]\n\n", summary),
                      provider = "corteza",
                      model = "compaction",
                      agent_id = agent_id
    )
}

#' Format session list for display
#' @param sessions List of session summaries from session_list()
#' @return Character string for printing
#' @noRd
format_session_list <- function(sessions) {
    if (length(sessions) == 0) {
        return("No sessions found.")
    }

    safe_str <- function(x, default = "?") {
        if (is.null(x) || length(x) == 0 || identical(x, list())) {
            default
        } else {
            as.character(x)
        }
    }

    format_time <- function(ms) {
        if (is.null(ms) || !is.numeric(ms)) {
            return("?")
        }
        format(as.POSIXct(ms / 1000, origin = "1970-01-01"), "%Y-%m-%d %H:%M")
    }

    lines <- vapply(sessions, function(s) {
        if (is.null(s)) return("")
        compactions <- if ((s$compactionCount %||% 0) > 0) {
            sprintf(" [%d compactions]", s$compactionCount)
        } else ""
        sprintf("  %s  %s  %d msgs  %s/%s%s",
                safe_str(s$sessionKey, "?"),
                format_time(s$updatedAt),
            if (is.numeric(s$messages)) s$messages else 0L,
                safe_str(s$modelProvider, "?"),
                safe_str(s$model, "default"),
                compactions)
    }, character(1))

    lines <- lines[nchar(lines) > 0]
    paste(c("Sessions:", lines), collapse = "\n")
}

# Trace storage ----

#' Get path to trace file for a session
#' @param session_id Session ID
#' @param agent_id Agent ID
#' @return Path to trace file
#' @noRd
trace_path <- function(session_id, agent_id = DEFAULT_AGENT_ID) {
    file.path(sessions_dir(agent_id), paste0(session_id, "_trace.jsonl"))
}

#' Add a trace entry for a tool execution
#' @param session_id Session ID
#' @param tool Tool name
#' @param args Tool arguments
#' @param result Result text
#' @param success TRUE if successful
#' @param elapsed_ms Execution time in milliseconds
#' @param approved_by How tool was approved
#' @param turn Conversation turn number
#' @param agent_id Agent ID
#' @return Invisible path to trace file
#' @noRd
trace_add <- function(session_id, tool, args, result, success, elapsed_ms,
                      approved_by = NULL, turn = NULL,
                      agent_id = DEFAULT_AGENT_ID) {
    path <- trace_path(session_id, agent_id)

    dir <- dirname(path)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }

    # Truncate large values
    args_summary <- lapply(args, function(x) {
        if (is.character(x) && nchar(x) > 200) {
            paste0(substr(x, 1, 197), "...")
        } else {
            x
        }
    })

    result_summary <- if (is.character(result) && nchar(result) > 500) {
        paste0(substr(result, 1, 497), "...")
    } else {
        result
    }

    entry <- list(
                  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                  turn = turn,
                  tool = tool,
                  args = args_summary,
                  result = result_summary,
                  success = success,
                  elapsed_ms = elapsed_ms,
                  approved_by = approved_by
    )

    json <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")
    cat(json, "\n", file = path, append = TRUE)

    invisible(path)
}

#' Load trace for a session
#' @param session_id Session ID
#' @param agent_id Agent ID
#' @param n Maximum number of entries to return (NULL for all)
#' @return List of trace entries
#' @noRd
trace_load <- function(session_id, agent_id = DEFAULT_AGENT_ID, n = NULL) {
    path <- trace_path(session_id, agent_id)

    if (!file.exists(path)) {
        return(list())
    }

    lines <- readLines(path, warn = FALSE)

    if (length(lines) == 0) {
        return(list())
    }

    if (!is.null(n) && n < length(lines)) {
        lines <- tail(lines, n)
    }

    lapply(lines, function(line) {
        tryCatch(
                 jsonlite::fromJSON(line, simplifyVector = FALSE),
                 error = function(e) NULL
        )
    })
}

#' Format trace for display
#' @param trace List of trace entries from trace_load()
#' @param show_args Whether to show arguments
#' @return Character string for printing
#' @noRd
format_trace <- function(trace, show_args = FALSE) {
    if (length(trace) == 0) {
        return("No tool calls recorded.")
    }

    lines <- vapply(trace, function(entry) {
        if (is.null(entry)) return("")

        status <- if (isTRUE(entry$success)) "OK" else "ERR"
        time_str <- if (!is.null(entry$elapsed_ms)) {
            sprintf("%dms", entry$elapsed_ms)
        } else {
            "?"
        }

        base <- sprintf("  [%s] %s %s (%s)",
                        status, entry$tool, time_str,
                        substr(entry$timestamp, 12, 19))

        if (show_args && length(entry$args) > 0) {
            args_str <- paste(names(entry$args), "=",
                              vapply(entry$args, function(x) {
                if (is.character(x)) {
                    s <- if (nchar(x) > 30) paste0(substr(x, 1, 27), "...") else x
                    sprintf('"%s"', s)
                } else {
                    as.character(x)
                }
            }, character(1)),
                              collapse = ", ")
            base <- paste0(base, "\n    ", args_str)
        }

        base
    }, character(1))

    lines <- lines[nchar(lines) > 0]
    paste(c("Tool execution trace:", lines), collapse = "\n")
}

# Backward compatibility - project-local sessions ----
# These functions allow using project-local .corteza/sessions/ if needed

#' Get project-local sessions directory
#' @param cwd Working directory
#' @return Path to sessions directory
#' @noRd
sessions_dir_local <- function(cwd = getwd()) {
    file.path(cwd, ".corteza", "sessions")
}

