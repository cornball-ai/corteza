# The sessions of `corteza acp` (R/acp.R): made, resumed, listed,
# closed, and put in a mode.
#
# Each is a corteza session in the directory the client names, built as
# chat() builds one (session_setup(): the config, the trust decision, the
# tool table, the system text), with the terminal's records: a session
# record and a transcript (R/session.R), and a journal. Sessions in one
# process share none of their R objects: each one's run_r runs in a
# worker process of its own (run_r_mode "worker", R/run-r-worker.R),
# and keeps its own cut results (`handle_store`).

# How many sessions a session/list page holds.
.ACP_LIST_PAGE <- 50L

# A directory the client names: absolute, and there. corteza creates
# none.
.acp_cwd <- function(cwd) {
    if (!is.character(cwd) || length(cwd) != 1L || is.na(cwd) || !nzchar(cwd)) {
        .acp_refuse("cwd must name a directory.")
    }
    if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", cwd)) {
        .acp_refuse("cwd must be an absolute path.")
    }
    if (!dir.exists(cwd)) {
        .acp_refuse(sprintf("There is no directory %s, and corteza creates none.",
                            .sanitize_inline(cwd, max_chars = 300L)))
    }
    normalizePath(cwd, winslash = "/", mustWork = TRUE)
}

# corteza connects to no MCP server a client names. A request that names
# one is refused, by each server's name alone: the spec asks an agent
# not to repeat a server's environment or headers anywhere.
.acp_no_mcp <- function(servers) {
    if (is.null(servers) || !length(servers)) {
        return(invisible(NULL))
    }
    names <- vapply(servers, function(s) {
        name <- if (is.list(s)) s$name
        if (is.character(name) && length(name) == 1L && !is.na(name)) {
            .sanitize_inline(name, max_chars = 60L)
        } else {
            "(unnamed)"
        }
    }, character(1))
    .acp_refuse(sprintf(paste("corteza connects to no MCP server a client names,",
                              "and this request names %s. Send an empty",
                              "mcpServers list."),
                        paste0("'", names, "'", collapse = ", ")))
}

.acp_session_get <- function(st, sid) {
    session <- if (is.character(sid) && length(sid) == 1L) st$sessions[[sid]]
    if (is.null(session)) {
        .acp_refuse(sprintf("No session %s is open.",
                            .sanitize_inline(paste(sid, collapse = " "), max_chars = 80L)), -32002L)
    }
    session
}

.acp_session_new <- function(st, params) {
    cwd <- .acp_cwd(params$cwd)
    .acp_no_mcp(params$mcpServers)
    route <- .session_model_pair(st$provider, st$model, load_config(cwd))
    # Built before the record is made, so a session that cannot start (a
    # provider with no key) leaves no record behind.
    session <- .acp_session_build(st, cwd, route)
    disk <- session_new(route$provider, route$model, cwd, surface = "acp")
    .acp_session_open(st, session, disk)
    list(sessionId = disk$sessionId, modes = .acp_session_modes(session))
}

# A conversation this directory started, taken up again: its history
# from the transcript, as --resume takes it. Nothing is replayed to the
# client.
.acp_session_resume <- function(st, params) {
    sid <- params$sessionId
    cwd <- .acp_cwd(params$cwd)
    .acp_no_mcp(params$mcpServers)
    if (!is.character(sid) || length(sid) != 1L || !nzchar(sid)) {
        .acp_refuse("sessionId must name a session.")
    }
    open <- st$sessions[[sid]]
    if (!is.null(open)) {
        .acp_same_dir(sid, open$cwd, cwd)
        return(list(modes = .acp_session_modes(open)))
    }
    key <- NULL
    for (entry in session_entries()) {
        if (identical(entry$sessionId, sid)) {
            key <- entry$sessionKey
            break
        }
    }
    disk <- if (!is.null(key)) session_load(key)
    if (is.null(disk)) {
        .acp_refuse(sprintf("There is no session %s to resume.",
                            .sanitize_inline(sid, max_chars = 80L)), -32002L)
    }
    .acp_same_dir(sid, disk$cwd, cwd)
    route <- .session_model_pair(st$provider, st$model, load_config(cwd),
                                 saved = disk)
    history <- lapply(disk$messages %||% list(), function(m) {
        text <- if (is.list(m$content) && length(m$content) &&
                      !is.null(m$content[[1L]]$text)) {
            m$content[[1L]]$text
        } else {
            m$content
        }
        list(role = m$role, content = text)
    })
    session <- .acp_session_build(st, cwd, route, history)
    .acp_session_open(st, session, disk)
    list(modes = .acp_session_modes(session))
}

# A conversation belongs to the directory it started in (R/session.R).
.acp_same_dir <- function(sid, dir, cwd) {
    if (!identical(session_cwd(dir), session_cwd(cwd))) {
        .acp_refuse(sprintf("Session %s belongs to %s, not to %s.",
                            .sanitize_inline(sid, max_chars = 80L),
                            .sanitize_inline(dir %||% "another directory",
                    max_chars = 300L),
                            .sanitize_inline(cwd, max_chars = 300L)))
    }
    invisible(TRUE)
}

# The session's own corteza session, for its directory. It is built in
# that directory, as the terminal builds one where it starts: a skill's
# R file, sourced as the tool table is made, reads relative paths from
# there and not from the launcher's directory, or from a running
# session's when a client opens this one during a turn. The caller's
# directory is the working directory again afterwards.
.acp_session_build <- function(st, cwd, route, history = NULL) {
    here <- getwd()
    on.exit(setwd(here), add = TRUE)
    setwd(cwd)
    session <- session_setup("cli", cwd = cwd, provider = route$provider,
                             model = route$model, tools = st$tools,
                             history = history)
    session$config$run_r_mode <- "worker"
    # A config that asked for run_r in corteza's own process is told
    # why not, in the session's first update (.acp_notes_send()). The
    # files are read as load_config() reads them, the project's only
    # where it is trusted; the default fills in nothing here.
    asked <- project_config(cwd)$run_r_mode %||%
    load_config_file(corteza_config_path("config.json"))$run_r_mode
    if (identical(asked, "in_process")) {
        session$acp_notes <- paste("corteza: run_r runs in a worker process",
                                   "of this session's own, not in corteza's",
                                   "process as your config's run_r_mode asks,",
                                   "so sessions share no R objects.")
    }
    session$handle_store <- new.env(parent = emptyenv())
    # The approval mode the config chose, which the client may come back
    # to (.acp_mode_ids()).
    session$acp_config_mode <- session$config$approval_mode %||% "ask"
    session
}

# Give the session its record and its ties to the client.
.acp_session_open <- function(st, session, disk) {
    sid <- disk$sessionId
    session$sessionId <- sid
    session$disk_session <- disk
    session$provider <- session$provider %||% disk$provider
    # A resumed conversation's task list, as --resume takes it.
    session$tasks <- disk$tasks %||% list()
    journal_install(session, agent_id = DEFAULT_AGENT_ID)
    session$approval_cb <- .acp_approval_cb(st, sid, session$cwd)
    session$on_delta <- function(text) .acp_on_delta(st, sid, text)
    session$stop_requested <- function() .acp_stop(st, sid)
    add_observer(session, function(event) .acp_on_tool(st, sid, event))
    review_apply(session)
    st$sessions[[sid]] <- session
    # The commands it takes (R/acp-commands.R), and what its opening has
    # to say.
    updates <- c(list(.acp_commands_update()),
                 lapply(session$acp_notes, .acp_text_chunk))
    for (update in updates) {
        st$notes <- c(st$notes, list(list(sid = sid, update = update)))
    }
    invisible(session)
}

# A session's opening updates, sent once its answer has gone, since the
# client learns the session's id from the answer.
.acp_notes_send <- function(st) {
    notes <- st$notes
    st$notes <- list()
    for (note in notes) {
        .acp_update(st, note$sid, note$update)
    }
    invisible(NULL)
}

# The modes a session offers: "ask" and "review", and the config's own
# approval_mode where it is "allow" or "deny", so the mode the client
# shows is the one in force.
.acp_mode_ids <- function(session) {
    own <- session$acp_config_mode
    c("ask", "review",
        if (identical(own, "allow") || identical(own, "deny")) own)
}

.acp_mode_of <- function(session) {
    mode <- session$config$approval_mode %||% "ask"
    if (mode %in% .acp_mode_ids(session)) {
        mode
    } else {
        "ask"
    }
}

.acp_session_modes <- function(session) {
    .acp_modes(.acp_mode_of(session), .acp_mode_ids(session))
}

# session/set_mode: one of .acp_mode_ids(). For a session whose prompt
# runs, the next call meets the new mode: the gate is read per call.
.acp_set_mode <- function(st, params) {
    session <- .acp_session_get(st, params$sessionId)
    mode <- params$modeId
    ids <- .acp_mode_ids(session)
    if (!is.character(mode) || length(mode) != 1L || !mode %in% ids) {
        .acp_refuse(sprintf("modeId must be one of %s.",
                            paste0("'", ids, "'", collapse = ", ")))
    }
    session$config$approval_mode <- mode
    review_apply(session)
    .acp_update(st, session$sessionId,
                list(sessionUpdate = "current_mode_update", currentModeId = mode))
    .acp_object()
}

# session/list: the sessions started in `cwd`, or in any directory
# without one, newest first, a page at a time. The cursor is where the
# next page starts.
.acp_session_list <- function(st, params) {
    cwd <- params$cwd
    if (!is.null(cwd)) {
        cwd <- .acp_cwd(cwd)
    }
    from <- 0L
    if (!is.null(params$cursor)) {
        from <- suppressWarnings(as.integer(params$cursor))
        if (length(from) != 1L || is.na(from) || from < 0L) {
            .acp_refuse("cursor is not one this agent gave.")
        }
    }
    entries <- session_list(n = from + .ACP_LIST_PAGE + 1L,
                            cwd = cwd %||% getwd(), all = is.null(cwd))
    rows <- utils::tail(entries, max(0L, length(entries) - from))
    more <- length(rows) > .ACP_LIST_PAGE
    rows <- utils::head(rows, .ACP_LIST_PAGE)
    out <- list(sessions = lapply(rows, function(e) {
        list(sessionId = e$sessionId, cwd = e$cwd %||% "",
             title = e$title %||% NULL,
             updatedAt = .acp_time(e$updatedAt %||% e$createdAt))
    }))
    if (more) {
        out$nextCursor <- as.character(from + .ACP_LIST_PAGE)
    }
    out
}

# A record's time (milliseconds since 1970) as ISO 8601, or NULL.
.acp_time <- function(ms) {
    ms <- suppressWarnings(as.numeric(ms))
    if (length(ms) != 1L || is.na(ms)) {
        return(NULL)
    }
    format(as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC"),
           "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# session/close: the session's work stops, its workers end, its record
# is saved, and it is forgotten. A session whose prompt runs is closed
# once the prompt has its `cancelled` answer (NULL: answered then). An
# unknown or closed session answers as a closed one.
.acp_session_close <- function(st, params, id) {
    sid <- params$sessionId
    live <- st$live
    if (!is.null(live) && identical(live$sid, sid)) {
        live$cancelled <- TRUE
        st$after <- c(st$after, list(list(id = id, sid = sid)))
        return(NULL)
    }
    .acp_cancel(st, sid)
    .acp_forget(st, sid)
    .acp_object()
}

.acp_forget <- function(st, sid) {
    session <- if (is.character(sid) && length(sid) == 1L) st$sessions[[sid]]
    if (is.null(session)) {
        return(invisible(NULL))
    }
    st$sessions[[sid]] <- NULL
    tryCatch(.run_r_worker_close(session), error = function(e) NULL)
    # The reviewer's process, where review mode started one.
    tryCatch(job_monitor_close(session), error = function(e) NULL)
    tryCatch(session_save(session$disk_session), error = function(e) NULL)
    invisible(NULL)
}
