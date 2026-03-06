# Subagent System
# Spawn, query, and manage child agents for parallel/specialized tasks

# Subagent registry (package-level environment)
.subagent_registry <- new.env(parent = emptyenv())

# Default configuration
SUBAGENT_DEFAULTS <- list(
                          max_concurrent = 3L,
                          timeout_minutes = 30L,
                          allow_nested = FALSE,
                          default_tools = c("base::readLines", "base::writeLines", "bash",
        "grep_files"),
                          base_port = 7851L
)

#' Get subagent configuration
#'
#' @param config Config list from load_config()
#' @return Subagent config with defaults applied
#' @noRd
get_subagent_config <- function(config = list()) {
    cfg <- config$subagents %||% list()

    list(
         enabled = cfg$enabled %||% TRUE,
         max_concurrent = cfg$max_concurrent %||% SUBAGENT_DEFAULTS$max_concurrent,
         timeout_minutes = cfg$timeout_minutes %||% SUBAGENT_DEFAULTS$timeout_minutes,
         allow_nested = cfg$allow_nested %||% SUBAGENT_DEFAULTS$allow_nested,
         default_tools = cfg$default_tools %||% SUBAGENT_DEFAULTS$default_tools,
         base_port = cfg$base_port %||% SUBAGENT_DEFAULTS$base_port
    )
}

#' Find a free port
#'
#' @param base Starting port number
#' @param max_tries Number of ports to try
#' @return Available port number
#' @noRd
find_free_port <- function(base = 7851L, max_tries = 100L) {
    for (i in seq_len(max_tries)) {
        port <- base + i - 1L
        conn <- tryCatch(
                         socketConnection("localhost", port, open = "r+b",
                blocking = TRUE, timeout = 1),
                         error = function(e) NULL
        )
        if (is.null(conn)) {
            return(port) # Port is free
        }
        close(conn)
    }
    stop("No free ports found in range ", base, "-", base + max_tries - 1,
         call. = FALSE)
}

#' Wait for a port to be available
#'
#' @param port Port number
#' @param timeout_secs Timeout in seconds
#' @return TRUE if port became available, FALSE otherwise
#' @noRd
wait_for_port <- function(port, timeout_secs = 10L) {
    start_time <- Sys.time()

    while (difftime(Sys.time(), start_time, units = "secs") < timeout_secs) {
        conn <- tryCatch(
                         socketConnection("localhost", port, open = "r+b",
                blocking = TRUE, timeout = 1),
                         error = function(e) NULL
        )
        if (!is.null(conn)) {
            close(conn)
            return(TRUE)
        }
        Sys.sleep(0.25)
    }

    FALSE
}

#' Generate subagent session key
#'
#' @param parent_key Parent session key
#' @return Subagent session key
#' @noRd
subagent_session_key <- function(parent_key) {
    id <- session_id()
    sprintf("agent:main:subagent:%s", id)
}

#' Create system prompt for subagent
#'
#' @param task Task description
#' @param parent_context Context from parent
#' @return System prompt string
#' @noRd
subagent_system_prompt <- function(task, parent_context = NULL) {
    prompt <- paste0(
                     "You are a specialized subagent spawned to complete a specific task.\n\n",
                     "## Your Task\n", task, "\n\n",
                     "## Guidelines\n",
                     "- Stay focused on the assigned task\n",
                     "- Do not initiate new conversations\n",
                     "- Be concise in responses\n",
                     "- Report completion clearly\n",
                     "- You cannot spawn additional subagents\n"
    )

    if (!is.null(parent_context)) {
        prompt <- paste0(prompt, "\n## Parent Context\n", parent_context)
    }

    prompt
}

#' Spawn a subagent
#'
#' Creates a new agent process with its own MCP server.
#'
#' @param task Task description
#' @param model Optional model override
#' @param tools Optional tool filter (character vector)
#' @param parent_session Parent session object
#' @param config Config list
#' @return Subagent ID
#' @export
subagent_spawn <- function(task, model = NULL, tools = NULL,
                           parent_session = NULL, config = NULL) {
    if (is.null(config)) {
        config <- load_config(getwd())
    }

    subcfg <- get_subagent_config(config)

    # Check if subagents are enabled
    if (!isTRUE(subcfg$enabled)) {
        stop("Subagents are disabled in configuration", call. = FALSE)
    }

    # Check concurrent limit
    active_count <- length(ls(.subagent_registry))
    if (active_count >= subcfg$max_concurrent) {
        stop(sprintf("Maximum concurrent subagents reached (%d)",
                     subcfg$max_concurrent),
             call. = FALSE)
    }

    # Check for nested spawning
    if (!is.null(parent_session$is_subagent) &&
        isTRUE(parent_session$is_subagent)) {
        if (!isTRUE(subcfg$allow_nested)) {
            stop("Nested subagent spawning is not allowed", call. = FALSE)
        }
    }

    # Find free port
    port <- find_free_port(subcfg$base_port)

    # Create session key
    if (!is.null(parent_session)) {
        parent_key <- parent_session$sessionKey
    } else {
        parent_key <- "llamar:main"
    }
    session_key <- subagent_session_key(parent_key)
    id <- sub("^agent:main:subagent:", "", session_key)

    # Store in session metadata
    store_update(session_key, list(
                                   sessionId = id,
                                   spawnedBy = parent_key,
                                   task = task,
                                   port = port,
                                   status = "starting",
                                   createdAt = as.numeric(Sys.time()) * 1000
        ))

    # Build serve command
    tools_arg <- if (!is.null(tools)) {
        sprintf(', tools = c(%s)', paste0('"', tools, '"', collapse = ", "))
    } else if (!is.null(subcfg$default_tools)) {
        sprintf(', tools = c(%s)',
                paste0('"', subcfg$default_tools, '"', collapse = ", "))
    } else {
        ""
    }

    if (!is.null(parent_session)) {
        cwd <- parent_session$cwd
    } else {
        cwd <- getwd()
    }
    cmd <- sprintf('llamaR::serve(port = %d, cwd = "%s", agent_id = "subagent-%s"%s)',
                   port, cwd, id, tools_arg)

    # Spawn server process
    log_file <- file.path(tempdir(), sprintf("subagent-%s.log", id))
    system2("Rscript", c("-e", shQuote(cmd)), wait = FALSE,
            stdout = log_file, stderr = log_file)

    # Wait for server to start
    if (!wait_for_port(port, timeout_secs = 10L)) {
        store_update(session_key, list(status = "failed"))
        stop("Subagent failed to start (timeout)", call. = FALSE)
    }

    # Update status
    store_update(session_key, list(status = "running"))

    # Register locally
    .subagent_registry[[id]] <- list(
                                     id = id,
                                     session_key = session_key,
                                     port = port,
                                     task = task,
                                     model = model,
                                     started_at = Sys.time(),
                                     timeout = Sys.time() + subcfg$timeout_minutes * 60
    )

    log_event("subagent_spawn", subagent_id = id, port = port, task = task)

    id
}

#' Query a subagent
#'
#' Sends a prompt to a running subagent and returns the response.
#'
#' @param id Subagent ID
#' @param prompt Prompt to send
#' @param timeout Timeout in seconds
#' @return Response text
#' @export
subagent_query <- function(id, prompt, timeout = 60L) {
    info <- .subagent_registry[[id]]
    if (is.null(info)) {
        stop("Subagent not found: ", id, call. = FALSE)
    }

    # Check if expired
    if (Sys.time() > info$timeout) {
        subagent_kill(id)
        stop("Subagent expired: ", id, call. = FALSE)
    }

    # Connect to subagent's MCP server
    conn <- tryCatch(
                     mcp_connect(port = info$port, name = "parent"),
                     error = function(e) {
        stop("Failed to connect to subagent: ", e$message, call. = FALSE)
    }
    )
    on.exit(mcp_close(conn))

    # Use chat tool to send prompt
    result <- tryCatch(
                       mcp_call(conn, "chat", list(prompt = prompt)),
                       error = function(e) {
        stop("Subagent query failed: ", e$message, call. = FALSE)
    }
    )

    log_event("subagent_query", subagent_id = id, prompt_length = nchar(prompt))

    result$text %||% ""
}

#' Kill a subagent
#'
#' Terminates a running subagent.
#'
#' @param id Subagent ID
#' @return Invisible TRUE
#' @export
subagent_kill <- function(id) {
    info <- .subagent_registry[[id]]
    if (is.null(info)) {
        return(invisible(FALSE))
    }

    # Update store status
    store_update(info$session_key, list(
                                        status = "completed",
                                        completedAt = as.numeric(Sys.time()) * 1000
        ))

    # Try to gracefully close the connection
    tryCatch({
        conn <- mcp_connect(port = info$port, name = "killer")
        mcp_close(conn)
    }, error = function(e) {
        # Ignore connection errors during shutdown
    })

    # Remove from registry
    rm(list = id, envir = .subagent_registry)

    log_event("subagent_kill", subagent_id = id)

    invisible(TRUE)
}

#' List active subagents
#'
#' @return List of subagent info objects
#' @export
subagent_list <- function() {
    ids <- ls(.subagent_registry)
    if (length(ids) == 0) {
        return(list())
    }

    lapply(ids, function(id) {
        info <- .subagent_registry[[id]]
        list(
             id = info$id,
             task = info$task,
             port = info$port,
             started_at = info$started_at,
             time_remaining = as.numeric(difftime(info$timeout, Sys.time(),
                    units = "mins"))
        )
    })
}

#' Clean up expired subagents
#'
#' @return Number of subagents cleaned up
#' @noRd
subagent_cleanup <- function() {
    ids <- ls(.subagent_registry)
    cleaned <- 0L

    for (id in ids) {
        info <- .subagent_registry[[id]]
        if (Sys.time() > info$timeout) {
            subagent_kill(id)
            cleaned <- cleaned + 1L
        }
    }

    cleaned
}

#' Format subagent list for display
#'
#' @param agents List from subagent_list()
#' @return Character string for display
#' @noRd
format_subagent_list <- function(agents) {
    if (length(agents) == 0) {
        return("No active subagents.")
    }

    lines <- c("Active subagents:")

    for (a in agents) {
        time_str <- if (a$time_remaining > 0) {
            sprintf("%.1f min remaining", a$time_remaining)
        } else {
            "expired"
        }
        lines <- c(lines, sprintf("  [%s] port %d - %s (%s)",
                                  a$id, a$port, a$task, time_str))
    }

    paste(lines, collapse = "\n")
}

