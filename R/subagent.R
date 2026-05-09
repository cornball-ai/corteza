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

#' Per-process monotonic counter for short subagent ids.
#'
#' Subagents are short-lived and never outlive the parent process, so a
#' per-process counter that never reuses values gives the user a one-
#' or two-character handle (`/ask 1`) without the cognitive overhead of
#' a UUID prefix. Killing #2 leaves a gap; we never recycle.
#' @noRd
.subagent_counter <- new.env(parent = emptyenv())
.subagent_counter$n <- 0L

#' Pull the next short-id sequence number.
#' @noRd
next_subagent_seq <- function() {
    .subagent_counter$n <- .subagent_counter$n + 1L
    .subagent_counter$n
}

#' Resolve a user-supplied identifier to a canonical subagent id.
#'
#' Accepts three forms:
#' 1. Sequence number (e.g. `"1"` or `1`): matches the `seq` field in
#'    the registry. Always tried first when the input is all digits.
#' 2. Exact UUID: matches as-is.
#' 3. UUID prefix: matches if exactly one registered id starts with the
#'    input. Two or more matches raise an "ambiguous" error.
#'
#' Returns the canonical id string, or NULL when nothing matches.
#' @param input Character or integer identifier.
#' @return Canonical id (character) or NULL.
#' @noRd
resolve_subagent_id <- function(input) {
    if (length(input) != 1L) {
        return(NULL)
    }
    s <- as.character(input)
    if (!nzchar(s)) {
        return(NULL)
    }
    ids <- ls(.subagent_registry)
    if (length(ids) == 0L) {
        return(NULL)
    }
    # All-digits: try seq match first. Fall through to id matching if
    # nothing matches (covers the rare case of a UUID that happens to
    # start with digits).
    if (grepl("^[0-9]+$", s)) {
        target <- as.integer(s)
        for (id in ids) {
            if (identical(.subagent_registry[[id]]$seq, target)) {
                return(id)
            }
        }
    }
    # Exact id match.
    if (s %in% ids) {
        return(s)
    }
    # UUID prefix.
    matches <- ids[startsWith(ids, s)]
    if (length(matches) == 1L) {
        return(matches[1])
    }
    if (length(matches) > 1L) {
        stop(sprintf("Ambiguous subagent id '%s' matches: %s",
                     s, paste(matches, collapse = ", ")),
             call. = FALSE)
    }
    NULL
}

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
#' @param depth Archival depth this child sits at (0 means a direct
#'   child of the CLI parent). Used by recursion in
#'   [subagent_turn_prompt()] to avoid archiving past the configured
#'   depth_cap.
#' @return Invisible TRUE.
#' @keywords internal
#' @export
subagent_turn_init <- function(provider = "anthropic", model = NULL,
                               tools_filter = NULL, system = NULL,
                               max_turns = 10L, depth = 0L) {
    session <- new_session(
        channel = "console",
        provider = provider,
        tools_filter = tools_filter,
        system = system,
        max_turns = as.integer(max_turns)
    )
    if (!is.null(model)) session$model_map$cloud <- model
    .subagent_state$session <- session
    .subagent_state$depth <- as.integer(depth)
    .subagent_state$subagent_id <- NULL
    invisible(TRUE)
}

#' Set this child's subagent id post-spawn.
#'
#' Called from [subagent_spawn()] right after [subagent_turn_init()] so
#' the child knows its own id when archival inside the child needs to
#' pass `parent_session_id`.
#' @param id Subagent id assigned by the parent.
#' @return Invisible TRUE.
#' @keywords internal
#' @export
subagent_turn_set_id <- function(id) {
    .subagent_state$subagent_id <- as.character(id)
    invisible(TRUE)
}

#' Seed the child's turn-session history with an externally-built slice.
#'
#' Used by the archival runtime: the parent spawns a holder subagent,
#' then ships the just-finished turn's history into the holder via this
#' function so the holder owns the full transcript while the parent
#' keeps only `{summary, subagent_id}`.
#' @param history List of message entries.
#' @return Invisible TRUE.
#' @keywords internal
#' @export
subagent_seed_history <- function(history) {
    if (is.null(.subagent_state$session)) {
        stop("Subagent turn session not initialized", call. = FALSE)
    }
    .subagent_state$session$history <- history
    invisible(TRUE)
}

#' Forward a prompt to the child-side turn session.
#'
#' Captures the pre-turn history length so that, if archival is enabled
#' and this query qualifies, the child can recursively archive its own
#' turn into a sub-subagent (capped by depth_cap).
#'
#' @param prompt User prompt (character).
#' @return Reply text (character).
#' @keywords internal
#' @export
subagent_turn_prompt <- function(prompt) {
    if (is.null(.subagent_state$session)) {
        stop("Subagent turn session not initialized", call. = FALSE)
    }
    pre_len <- length(.subagent_state$session$history %||% list())
    result <- turn(prompt, .subagent_state$session)

    cfg <- tryCatch(load_config(getwd()), error = function(e) list())
    arc_cfg <- cfg$archival %||% list()
    depth <- .subagent_state$depth %||% 0L
    cap <- arc_cfg$trigger$depth_cap %||% 3L
    if (isTRUE(arc_cfg$enabled) && depth < cap) {
        post_len <- length(.subagent_state$session$history %||% list())
        if (post_len > pre_len) {
            slice <- .subagent_state$session$history[(pre_len + 1L):post_len]
            max_turns_hit <- isTRUE(grepl("Max turns",
                                          as.character(result$reply %||% "")))
            if (archival_should_trigger(arc_cfg, slice, depth = depth,
                                        max_turns_hit = max_turns_hit) &&
                !archival_slice_has_unfinished_tool_use(slice)) {
                archived <- archival_archive_turn(
                    turn_session = .subagent_state$session,
                    prompt = prompt, history_slice = slice,
                    arc_cfg = arc_cfg, depth = depth,
                    parent_session_id = .subagent_state$subagent_id,
                    parent_provider = .subagent_state$session$provider %||%
                        "anthropic",
                    parent_model = .subagent_state$session$model_map$cloud,
                    config = cfg
                )
                if (!is.null(archived)) {
                    keep <- .subagent_state$session$history[seq_len(pre_len)]
                    user_msg <- slice[[1]]
                    if (!identical(user_msg$role %||% "", "user")) {
                        user_msg <- list(role = "user", content = prompt)
                    }
                    archived_assistant <- list(
                        role = "assistant",
                        content = sprintf(
                            "[archived turn]\nsubagent_id: %s\n\n%s",
                            archived$subagent_id, archived$summary
                        )
                    )
                    .subagent_state$session$history <- c(
                        keep, list(user_msg), list(archived_assistant)
                    )
                }
            }
        }
    }

    as.character(result$reply %||% "")
}

SUBAGENT_DEFAULTS <- list(
    max_concurrent = 3L,
    timeout_minutes = 30L,
    allow_nested = FALSE,
    default_tools = c("read_file", "grep_files", "r_help", "web_search", "fetch_url")
)

SUBAGENT_PRESETS <- list(
    investigate = c("read_file", "grep_files", "r_help", "web_search", "fetch_url"),
    work = c("read_file", "grep_files", "r_help", "web_search", "fetch_url",
             "bash", "write_file", "replace_in_file", "list_files",
             "git_status", "git_diff", "git_log", "run_r"),
    minimal = c("read_file", "grep_files")
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

#' Resolve a subagent preset to a tool vector.
#' @param preset Character: "investigate", "work", "minimal", or NULL.
#' @param tools Optional explicit tool vector (overrides preset).
#' @param default_tools Fallback when both preset and tools are NULL. Pass
#'   `subcfg$default_tools` so user config wins over the hard-coded default.
#' @return Character vector of tool names.
#' @noRd
resolve_subagent_tools <- function(preset = NULL, tools = NULL,
                                   default_tools = SUBAGENT_DEFAULTS$default_tools) {
    if (!is.null(tools)) {
        return(tools)
    }
    if (is.null(preset)) {
        return(default_tools)
    }
    preset_tools <- SUBAGENT_PRESETS[[preset]]
    if (is.null(preset_tools)) {
        stop(sprintf("Unknown subagent preset: '%s'. Use: %s",
                     preset,
                     paste(names(SUBAGENT_PRESETS), collapse = ", ")),
             call. = FALSE)
    }
    preset_tools
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
#'   into an agent loop).
#' @param model Optional model override (reserved for later use).
#' @param tools Optional explicit tool filter (character vector).
#'   Overrides `preset` when provided.
#' @param preset Preset name: `"investigate"` (read/search only, default),
#'   `"work"` (investigate + bash + write/edit), or `"minimal"`
#'   (read_file + grep_files only).
#' @param parent_session Parent session object; read for
#'   nested-spawning control and session-key derivation.
#' @param config Config list.
#' @return Subagent ID (character).
#' @importFrom callr r_session
#' @export
subagent_spawn <- function(task, model = NULL, tools = NULL,
                           preset = NULL,
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
    # spawning.
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
    effective_tools <- resolve_subagent_tools(
        preset = preset, tools = tools,
        default_tools = subcfg$default_tools
    )
    # Default provider/model from parent session when available, else config/env.
    spawn_provider <- parent_session$provider %||%
        getOption("corteza.provider", "anthropic")
    spawn_model <- model %||%
        parent_session$model_map$cloud %||%
        getOption("corteza.model", NULL)

    # Archival depth: parent depth + 1. Caller stamps
    # `parent_session$archival_depth` before calling spawn so the child
    # knows its own depth for recursion gating.
    child_depth <- as.integer((parent_session$archival_depth %||% 0L) + 1L)

    tryCatch(
        session$run(
            function(cwd, provider, model, tools_filter, system, max_turns,
                     depth, id) {
                library(corteza)
                corteza::worker_init(cwd = cwd)
                corteza::subagent_turn_init(
                    provider = provider,
                    model = model,
                    tools_filter = tools_filter,
                    system = system,
                    max_turns = max_turns,
                    depth = depth
                )
                corteza::subagent_turn_set_id(id)
            },
            list(cwd = cwd, provider = spawn_provider, model = spawn_model,
                 tools_filter = effective_tools, system = system_prompt,
                 max_turns = 10L, depth = child_depth, id = id)
        ),
        error = function(e) {
            try(session$close(), silent = TRUE)
            store_update(session_key, list(status = "failed"))
            stop("Failed to initialize subagent: ", conditionMessage(e),
                 call. = FALSE)
        }
    )

    store_update(session_key, list(status = "running"))
    seq <- next_subagent_seq()
    .subagent_registry[[id]] <- list(
        id = id,
        seq = seq,
        session_key = session_key,
        session = session,
        task = task,
        tools = tools,
        model = model,
        started_at = Sys.time(),
        timeout = Sys.time() + subcfg$timeout_minutes * 60,
        depth = child_depth
    )
    log_event("subagent_spawn", subagent_id = id, seq = seq, task = task,
              depth = child_depth)
    id
}

#' Query a subagent.
#'
#' Sends a prompt to a running subagent. Inside the child it runs
#' through [turn()] with the child's persistent turn session: the LLM
#' replies, any tool calls it makes resolve against the child's
#' in-process skill registry, and history accumulates across queries.
#'
#' @param id Subagent identifier. Accepts the canonical UUID, a unique
#'   UUID prefix, or the per-session sequence number printed by
#'   `subagent_list()` / `/agents`.
#' @param prompt Prompt to send.
#' @param timeout Timeout in seconds (currently advisory; callr-level
#'   hard timeouts are future work).
#' @return Reply text (character).
#' @export
subagent_query <- function(id, prompt, timeout = 60L) {
    canonical <- resolve_subagent_id(id)
    if (is.null(canonical)) {
        stop("Subagent not found: ", id, call. = FALSE)
    }
    info <- .subagent_registry[[canonical]]
    if (Sys.time() > info$timeout) {
        subagent_kill(canonical)
        stop("Subagent expired: ", canonical, call. = FALSE)
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
    log_event("subagent_query", subagent_id = canonical,
              prompt_length = nchar(prompt))
    as.character(reply)
}

#' Kill a subagent.
#' @param id Subagent identifier (UUID, prefix, or sequence number).
#' @return Invisible TRUE if killed, FALSE if not found.
#' @export
subagent_kill <- function(id) {
    canonical <- tryCatch(resolve_subagent_id(id), error = function(e) NULL)
    if (is.null(canonical)) {
        return(invisible(FALSE))
    }
    info <- .subagent_registry[[canonical]]
    if (is.null(info)) {
        return(invisible(FALSE))
    }
    store_update(info$session_key, list(
        status = "completed",
        completedAt = as.numeric(Sys.time()) * 1000
    ))
    tryCatch(info$session$close(), error = function(e) NULL)
    rm(list = canonical, envir = .subagent_registry)
    log_event("subagent_kill", subagent_id = canonical)
    invisible(TRUE)
}

#' List active subagents.
#' @return List of subagent info objects.
#' @export
subagent_list <- function() {
    ids <- ls(.subagent_registry)
    if (length(ids) == 0L) return(list())
    out <- lapply(ids, function(id) {
        info <- .subagent_registry[[id]]
        list(
            id = info$id,
            seq = info$seq,
            task = info$task,
            started_at = info$started_at,
            time_remaining = as.numeric(difftime(info$timeout, Sys.time(),
                                                 units = "mins"))
        )
    })
    # Sort by seq ascending so the user-visible numbering is stable.
    seqs <- vapply(out, function(a) a$seq %||% 0L, integer(1))
    out[order(seqs)]
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
#'
#' Shows the per-session sequence number first (the user-typeable
#' shortcut) followed by the canonical id and task. `query_subagent` /
#' `kill_subagent` accept either form.
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
        seq_str <- if (!is.null(a$seq)) sprintf("%d", a$seq) else "?"
        id_short <- substr(a$id, 1L, 8L)
        lines <- c(lines, sprintf("  [%s] %s (%s) %s",
                                  seq_str, a$task, time_str, id_short))
    }
    paste(c(lines, "",
            "Use the sequence number, the 8-char prefix, or the full id with /ask and /kill."),
          collapse = "\n")
}
