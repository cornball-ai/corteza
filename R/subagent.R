# Subagent system.
#
# Each subagent is a private `callr::r_session` with corteza loaded
# inside it. Same "we own both ends" reasoning as the CLI/worker
# split: there's no external client to target, so there's nothing to
# gain from running an MCP server inside the child. We keep a session
# handle in .subagent_registry, drive the agent loop via session$run(),
# and close it on kill.
#
# Each child owns a persistent turn-session. subagent_query forwards
# a prompt through that session; history accumulates across queries,
# tool calls resolve against the child's in-process skill registry.

#' Subagent registry (package-level environment).
#' @noRd
.subagent_registry <- new.env(parent = emptyenv())

#' Child-side state holder. Populated by [subagent_turn_init()] inside
#' each spawned child; read by [subagent_turn_prompt()]. The parent's
#' instance of this env is unused — child processes have their own
#' corteza namespace.
#' @noRd
.subagent_state <- new.env(parent = emptyenv())

#' Initialize the child-side turn session.
#'
#' Called once per child just after [worker_init()]. Creates a
#' `new_session()` configured with the subagent's provider/model/tools
#' and stores it where [subagent_turn_prompt()] can find it. Subagents
#' deny all tool approvals by default so a subagent can't run bash
#' without the parent opting in.
#'
#' @param provider LLM provider name (see [new_session()]).
#' @param model Optional model override.
#' @param tools_filter Optional character vector of tool names to
#'   expose. NULL uses the subagent config defaults.
#' @param system Optional system prompt string.
#' @param max_turns Max tool-use turns per query.
#' @return Invisible TRUE.
#' @keywords internal
#' @export
subagent_turn_init <- function(provider = "anthropic", model = NULL,
                               tools_filter = NULL, system = NULL,
                               max_turns = 10L) {
    session <- new_session(
        channel = "console",
        provider = provider,
        tools_filter = tools_filter,
        system = system,
        max_turns = as.integer(max_turns)
    )
    if (!is.null(model)) session$model_map$cloud <- model
    .subagent_state$session <- session
    invisible(TRUE)
}

#' Forward a prompt to the child-side turn session.
#'
#' @param prompt User prompt (character).
#' @return Reply text (character).
#' @keywords internal
#' @export
subagent_turn_prompt <- function(prompt) {
    if (is.null(.subagent_state$session)) {
        stop("Subagent turn session not initialized", call. = FALSE)
    }
    result <- turn(prompt, .subagent_state$session)
    as.character(result$reply %||% "")
}

SUBAGENT_DEFAULTS <- list(
    max_concurrent = 3L,
    timeout_minutes = 30L,
    allow_nested = FALSE,
    default_tools = c("read_file", "bash", "grep_files")
)

#' Get subagent configuration.
#' @param config Config list from load_config().
#' @return Subagent config with defaults applied.
#' @noRd
get_subagent_config <- function(config = list()) {
    cfg <- config$subagents %||% list()
    list(
        enabled = cfg$enabled %||% TRUE,
        max_concurrent = cfg$max_concurrent %||% SUBAGENT_DEFAULTS$max_concurrent,
        timeout_minutes = cfg$timeout_minutes %||% SUBAGENT_DEFAULTS$timeout_minutes,
        allow_nested = cfg$allow_nested %||% SUBAGENT_DEFAULTS$allow_nested,
        default_tools = cfg$default_tools %||% SUBAGENT_DEFAULTS$default_tools
    )
}

#' Generate subagent session key.
#' @param parent_key Parent session key.
#' @return Subagent session key.
#' @noRd
subagent_session_key <- function(parent_key) {
    id <- session_id()
    sprintf("agent:main:subagent:%s", id)
}

#' Spawn a subagent.
#'
#' Starts a fresh `callr::r_session` with corteza loaded and its tool
#' registry set up. Stores the handle in the package-level registry
#' keyed by subagent id.
#'
#' @param task Task description (stored for bookkeeping; not yet fed
#'   into an agent loop — see TODO on subagent_query).
#' @param model Optional model override (reserved for later use).
#' @param tools Optional tool filter (character vector).
#' @param parent_session Parent session object; read for
#'   nested-spawning control and session-key derivation.
#' @param config Config list.
#' @return Subagent ID (character).
#' @importFrom callr r_session
#' @export
subagent_spawn <- function(task, model = NULL, tools = NULL,
                           parent_session = NULL, config = NULL) {
    if (is.null(config)) {
        config <- load_config(getwd())
    }
    subcfg <- get_subagent_config(config)
    if (!isTRUE(subcfg$enabled)) {
        stop("Subagents are disabled in configuration", call. = FALSE)
    }
    active_count <- length(ls(.subagent_registry))
    if (active_count >= subcfg$max_concurrent) {
        stop(sprintf("Maximum concurrent subagents reached (%d)",
                     subcfg$max_concurrent),
             call. = FALSE)
    }
    if (!is.null(parent_session$is_subagent) &&
        isTRUE(parent_session$is_subagent)) {
        if (!isTRUE(subcfg$allow_nested)) {
            stop("Nested subagent spawning is not allowed", call. = FALSE)
        }
    }

    cwd <- if (!is.null(parent_session$cwd)) parent_session$cwd else getwd()

    parent_key <- if (!is.null(parent_session)) {
        parent_session$sessionKey
    } else {
        "corteza:main"
    }
    session_key <- subagent_session_key(parent_key)
    id <- sub("^agent:main:subagent:", "", session_key)

    store_update(session_key, list(
        sessionId = id,
        spawnedBy = parent_key,
        task = task,
        status = "starting",
        createdAt = as.numeric(Sys.time()) * 1000
    ))

    # Spin up the child session and initialize corteza inside it.
    session <- tryCatch(
        callr::r_session$new(wait = TRUE),
        error = function(e) {
            store_update(session_key, list(status = "failed"))
            stop("Failed to start subagent session: ", conditionMessage(e),
                 call. = FALSE)
        }
    )
    # Compose the child's system prompt: focus on the task, forbid
    # conversational drift and (if nested is disabled) recursive
    # spawning. Parent context is appended verbatim when present.
    system_prompt <- paste0(
        "You are a specialized subagent spawned for a specific task.\n",
        "- Stay focused on the assigned task\n",
        "- Do not initiate new conversations\n",
        "- Be concise in responses\n",
        "- Report completion clearly\n",
        if (!isTRUE(subcfg$allow_nested))
            "- You cannot spawn additional subagents\n" else "",
        "\n## Task\n", task
    )
    effective_tools <- if (is.null(tools)) subcfg$default_tools else tools
    spawn_model <- model
    spawn_provider <- "anthropic"  # child inherits parent config via env; override later if needed

    tryCatch(
        session$run(
            function(cwd, provider, model, tools_filter, system, max_turns) {
                library(corteza)
                corteza::worker_init(cwd = cwd)
                corteza::subagent_turn_init(
                    provider = provider,
                    model = model,
                    tools_filter = tools_filter,
                    system = system,
                    max_turns = max_turns
                )
            },
            list(cwd = cwd, provider = spawn_provider, model = spawn_model,
                 tools_filter = effective_tools, system = system_prompt,
                 max_turns = 10L)
        ),
        error = function(e) {
            try(session$close(), silent = TRUE)
            store_update(session_key, list(status = "failed"))
            stop("Failed to initialize subagent: ", conditionMessage(e),
                 call. = FALSE)
        }
    )

    store_update(session_key, list(status = "running"))
    .subagent_registry[[id]] <- list(
        id = id,
        session_key = session_key,
        session = session,
        task = task,
        tools = tools,
        model = model,
        started_at = Sys.time(),
        timeout = Sys.time() + subcfg$timeout_minutes * 60
    )
    log_event("subagent_spawn", subagent_id = id, task = task)
    id
}

#' Query a subagent.
#'
#' Sends a prompt to a running subagent. Inside the child it runs
#' through [turn()] with the child's persistent turn session: the LLM
#' replies, any tool calls it makes resolve against the child's
#' in-process skill registry, and history accumulates across queries.
#'
#' @param id Subagent ID.
#' @param prompt Prompt to send.
#' @param timeout Timeout in seconds (currently advisory; callr-level
#'   hard timeouts are future work).
#' @return Reply text (character).
#' @export
subagent_query <- function(id, prompt, timeout = 60L) {
    info <- .subagent_registry[[id]]
    if (is.null(info)) {
        stop("Subagent not found: ", id, call. = FALSE)
    }
    if (Sys.time() > info$timeout) {
        subagent_kill(id)
        stop("Subagent expired: ", id, call. = FALSE)
    }

    reply <- tryCatch(
        info$session$run(
            function(p) corteza::subagent_turn_prompt(p),
            list(p = prompt)
        ),
        error = function(e) {
            stop("Subagent query failed: ", conditionMessage(e), call. = FALSE)
        }
    )
    log_event("subagent_query", subagent_id = id, prompt_length = nchar(prompt))
    as.character(reply)
}

#' Kill a subagent.
#' @param id Subagent ID.
#' @return Invisible TRUE if killed, FALSE if not found.
#' @export
subagent_kill <- function(id) {
    info <- .subagent_registry[[id]]
    if (is.null(info)) {
        return(invisible(FALSE))
    }
    store_update(info$session_key, list(
        status = "completed",
        completedAt = as.numeric(Sys.time()) * 1000
    ))
    tryCatch(info$session$close(), error = function(e) NULL)
    rm(list = id, envir = .subagent_registry)
    log_event("subagent_kill", subagent_id = id)
    invisible(TRUE)
}

#' List active subagents.
#' @return List of subagent info objects.
#' @export
subagent_list <- function() {
    ids <- ls(.subagent_registry)
    if (length(ids) == 0L) return(list())
    lapply(ids, function(id) {
        info <- .subagent_registry[[id]]
        list(
            id = info$id,
            task = info$task,
            started_at = info$started_at,
            time_remaining = as.numeric(difftime(info$timeout, Sys.time(),
                                                 units = "mins"))
        )
    })
}

#' Clean up expired subagents.
#' @return Number of subagents cleaned up.
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

#' Format subagent list for display.
#' @param agents List from subagent_list().
#' @return Character string for display.
#' @noRd
format_subagent_list <- function(agents) {
    if (length(agents) == 0L) return("No active subagents.")
    lines <- c("Active subagents:")
    for (a in agents) {
        time_str <- if (a$time_remaining > 0) {
            sprintf("%.1f min remaining", a$time_remaining)
        } else {
            "expired"
        }
        lines <- c(lines, sprintf("  [%s] %s (%s)",
                                  a$id, a$task, time_str))
    }
    paste(lines, collapse = "\n")
}
