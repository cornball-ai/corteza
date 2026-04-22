# Subagent system.
#
# Each subagent is a private `callr::r_session` with corteza loaded
# inside it. Same "we own both ends" reasoning as the CLI/worker
# split: there's no external client to target, so there's nothing to
# gain from running an MCP server inside the child. We keep a session
# handle in .subagent_registry, invoke tools on it via session$run(),
# and close it on kill.
#
# subagent_query() is currently a thin stub; full agent-loop semantics
# (a running `turn()` inside the child processing prompts) would need
# more plumbing than the transport swap. Left as a TODO; the transport
# change is the part that removes MCP duplication from corteza.

#' Subagent registry (package-level environment).
#' @noRd
.subagent_registry <- new.env(parent = emptyenv())

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
    tryCatch(
        session$run(
            function(cwd) {
                library(corteza)
                corteza::worker_init(cwd = cwd)
            },
            list(cwd = cwd)
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
#' Sends a prompt to a running subagent. The current implementation
#' routes the prompt through `worker_dispatch("run_r", ...)` — i.e. the
#' subagent evaluates the prompt as R code. True agent-loop query
#' (prompt goes through `turn()` inside the child, LLM replies,
#' optional tool calls) is a TODO.
#'
#' @param id Subagent ID.
#' @param prompt Prompt / code to send.
#' @param timeout Timeout in seconds.
#' @return Response text.
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

    result <- tryCatch(
        info$session$run(
            function(code) corteza::worker_dispatch("run_r",
                                                    list(code = code)),
            list(code = prompt)
        ),
        error = function(e) {
            stop("Subagent query failed: ", conditionMessage(e), call. = FALSE)
        }
    )
    log_event("subagent_query", subagent_id = id, prompt_length = nchar(prompt))
    text_parts <- vapply(result$content %||% list(), function(c) {
        if (identical(c$type, "text")) c$text else ""
    }, character(1L))
    paste(text_parts, collapse = "\n")
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
