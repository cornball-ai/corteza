# Subagent system.
#
# Each subagent is a private `callr::r_session` with corteza loaded
# inside it. We own both ends, so there's no external client to target
# and nothing to gain from running an MCP server inside the child: the
# parent drives the child's agent loop directly via session$run(). We
# keep a session handle in .subagent_registry and close it on kill.
#
# Each child owns a persistent turn-session. subagent_query forwards
# a prompt through that session; history accumulates across queries,
# tool calls resolve against the child's in-process skill registry.

#' Subagent registry (package-level environment).
#' @noRd
.subagent_registry <- new.env(parent = emptyenv())

#' Retired-subagent spend accumulator (package-level environment).
#'
#' A killed subagent's registry entry is removed, so its cumulative
#' usage would vanish from any live sum. Before [subagent_kill()] drops
#' an entry it rolls the entry's totals in here, so [subagent_spend_total()]
#' (and therefore `/spent` and the MCP spend cap) keep counting it for
#' the life of the process. Like the registry it is process-level and
#' never reset -- spend is reported per run.
#' @noRd
.subagent_spend_retired <- new.env(parent = emptyenv())
.subagent_spend_retired$cost <- 0
.subagent_spend_retired$input_tokens <- 0L
.subagent_spend_retired$output_tokens <- 0L
.subagent_spend_retired$total_tokens <- 0L
.subagent_spend_retired$query_count <- 0L
.subagent_spend_retired$n_agents <- 0L
.subagent_spend_retired$cost_missing <- FALSE

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
        stop(sprintf("Ambiguous subagent id '%s' matches: %s", s,
                     paste(matches, collapse = ", ")),
             call. = FALSE)
    }
    NULL
}

#' Child-side state holder. Populated by [subagent_turn_init()] inside
#' each spawned child; read by [subagent_turn_prompt()]. The parent's
#' instance of this env is unused -- child processes have their own
#' corteza namespace.
#' @noRd
.subagent_state <- new.env(parent = emptyenv())

#' Initialize the child-side turn session.
#'
#' Called once per child just after [worker_init()]. Creates a
#' `new_session()` configured with the subagent's provider/model/tools
#' and stores it where [subagent_turn_prompt()] can find it. The
#' child's `approval_cb` denies by default: subagents have no
#' interactive approval channel back to the parent or user, and tool
#' permissions are fixed at spawn time via `tools_filter` (derived
#' from the parent's `preset` or explicit `tools` argument to
#' [subagent_spawn()]). There is no way to grant additional capability
#' mid-run.
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
                               max_turns = 10L, depth = 0L, plan_mode = FALSE) {
    session <- new_session(channel = "console", provider = provider,
                           tools_filter = tools_filter, system = system,
                           max_turns = as.integer(max_turns),
                           plan_mode = isTRUE(plan_mode))
    if (!is.null(model)) {
        session$model_map$cloud <- model
    }
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
    # Mark this child as an archive holder: its seeded history is the
    # whole point of the subagent, so context compaction must not
    # touch it.
    .subagent_state$kind <- "archive_holder"
    .subagent_state$protected_history_len <- length(history)
    invisible(TRUE)
}

#' Validate a subagent return name.
#'
#' Accepts a single syntactic R name or a `.h_NNN` handle id, and
#' rejects anything else (`x$y`, `a b`, ...) so a malformed request
#' fails clearly instead of silently resolving to nothing.
#' @noRd
.valid_return_name <- function(x) {
    is.character(x) && length(x) == 1L && nzchar(x) &&
    identical(make.names(x), x)
}

#' Resolve a return name inside the child process.
#'
#' Handle store first, then a top-level globalenv binding. A single
#' name only, no expression evaluation.
#' @return list(found = logical, value = resolved object or NULL)
#' @noRd
.resolve_return_value <- function(name) {
    if (name %in% list_handles()) {
        return(list(found = TRUE, value = get_handle(name)))
    }
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
        return(list(found = TRUE,
                    value = get(name, envir = globalenv(), inherits = FALSE)))
    }
    list(found = FALSE, value = NULL)
}

#' Format a subagent turn result for the parent.
#'
#' When the child resolved a value (`final_found`), stash it in the
#' parent's handle store and append a `summary` + `[stored as .h_NNN]`
#' block, mirroring `tool_run_r()`. The flag, not `is.null(final)`,
#' gates this so a legitimately NULL result still returns a handle. A
#' `final_note` (bad or missing return name) is appended as plain text.
#' Runs parent-side, so `with_handle()` mints the handle in the parent
#' store where later `run_r` calls see it.
#' @noRd
.format_subagent_reply <- function(res) {
    reply <- as.character(res$reply %||% "")
    if (isTRUE(res$final_found)) {
        stashed <- with_handle(res$final)
        block <- sprintf("%s\n\n[stored as %s]", stashed$summary,
                         stashed$handle)
        return(paste(c(reply[nzchar(reply)], block), collapse = "\n\n"))
    }
    if (!is.null(res$final_note)) {
        return(paste(c(reply[nzchar(reply)], res$final_note),
                     collapse = "\n\n"))
    }
    reply
}

#' Forward a prompt to the child-side turn session.
#'
#' Captures the pre-turn history length so that, if archival is enabled
#' and this query qualifies, the child can recursively archive its own
#' turn into a sub-subagent (capped by depth_cap).
#'
#' @param prompt User prompt (character).
#' @param return_name Optional single name or `.h_NNN` handle. When
#'   set, after the turn the child resolves it (handle store, then
#'   globalenv) and ships the value back as `$final` so the parent can
#'   stash it by handle. A bad or unresolved name yields `$final_note`.
#' @return A list with `$reply` (character, the LLM reply text),
#'   `$usage` (list with `input_tokens`, `output_tokens`, `total_tokens`,
#'   and optionally `cost` -- provider-dependent), and, when
#'   `return_name` is set, `$final` (the resolved value) or
#'   `$final_note` (why nothing was returned). Callers extract the
#'   reply and accumulate usage into the parent-side registry.
#' @keywords internal
#' @export
subagent_turn_prompt <- function(prompt, return_name = NULL) {
    if (is.null(.subagent_state$session)) {
        stop("Subagent turn session not initialized", call. = FALSE)
    }
    pre_len <- length(.subagent_state$session$history %||% list())
    result <- turn(prompt, .subagent_state$session)

    # Persist this turn's history slice to disk. The transcript is the
    # durable record (disk space is cheap); the in-memory history will
    # be compacted later. Each entry is appended as a separate line so
    # tool calls and results stay distinguishable.
    post_len_disk <- length(.subagent_state$session$history %||% list())
    if (post_len_disk > pre_len && !is.null(.subagent_state$subagent_id)) {
        sub_id <- .subagent_state$subagent_id
        agent_id <- paste0("subagent-", sub_id)
        disk_sess <- list(sessionId = sub_id, cwd = getwd(),
                          provider = .subagent_state$session$provider,
                          model = .subagent_state$session$model_map$cloud)
        disk_slice <- .subagent_state$session$history[
            (pre_len + 1L):post_len_disk]
        for (entry in disk_slice) {
            role <- entry$role %||% "user"
            body <- archival_history_entry_to_text(entry)
            tryCatch(
                     transcript_append(disk_sess, role, body,
                                       provider = disk_sess$provider,
                                       model = disk_sess$model,
                                       agent_id = agent_id),
                     error = function(e) {
                log_event("subagent_transcript_append_failed",
                          subagent_id = sub_id,
                          error = conditionMessage(e), level = "warn")
            })
        }
    }

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

    # Context compaction. Runs after the turn (never mid-turn) and
    # after archival has had its shot at the slice. The on-disk
    # transcript already holds the full record; compaction only
    # rewrites the in-memory history sent to the model on the next
    # query. Archive holders are skipped via the kind marker.
    tryCatch(
             maybe_compact_turn_session(
                                        .subagent_state$session, cfg,
                                        kind = .subagent_state$kind),
             error = function(e) {
        log_event("subagent_compact_failed",
                  reason = "unexpected_error",
                  error = conditionMessage(e), level = "warn")
    })

    # Resolve an optional return value to ship back as a handle. Done
    # child-side so the object travels by callr serialization, never
    # through tool-call arguments or the transcript.
    final <- NULL
    final_found <- FALSE
    final_note <- NULL
    if (!is.null(return_name)) {
        if (!.valid_return_name(return_name)) {
            final_note <- sprintf(
                                  "return_name '%s' is not a simple name or .h_NNN handle; no value returned.",
                                  as.character(return_name)[1])
        } else {
            resolved <- .resolve_return_value(return_name)
            if (resolved$found) {
                final <- resolved$value
                final_found <- TRUE
            } else {
                final_note <- sprintf(
                                      "return_name '%s' not found in the subagent session; no value returned.",
                                      return_name)
            }
        }
    }

    list(reply = as.character(result$reply %||% ""),
         usage = result$usage %||% list(),
         final = final, final_found = final_found, final_note = final_note)
}

SUBAGENT_DEFAULTS <- list(
                          max_concurrent = 3L,
                          timeout_minutes = 30L,
                          allow_nested = FALSE,
                          default_tools = c("read_file", "grep_files", "r_help", "web_search",
        "fetch_url")
)

SUBAGENT_PRESETS <- list(
                         investigate = c("read_file", "grep_files", "r_help", "web_search",
        "fetch_url"),
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
        stop(sprintf("Unknown subagent preset: '%s'. Use: %s", preset,
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
#' Permissions: subagents have no interactive approval channel back
#' to the parent or user. The child's `approval_cb` denies by default
#' and there is no mid-run escalation path. Whatever capability the
#' child needs must be granted at spawn time through `preset` or
#' `tools`. If a task may need shell, write, or network capability,
#' pick a preset that includes it (or pass an explicit `tools` list);
#' otherwise the child should report that it is blocked rather than
#' retry.
#'
#' @param task Task description (stored for bookkeeping; not yet fed
#'   into an agent loop).
#' @param model Optional model override (reserved for later use).
#' @param tools Optional explicit tool filter (character vector).
#'   Overrides `preset` when provided. Fixed for the lifetime of the
#'   child -- cannot be expanded after spawn.
#' @param preset Preset name (fixed for the lifetime of the child).
#'   `"investigate"` (default): `read_file`, `grep_files`, `r_help`,
#'   `web_search`, `fetch_url`. `"work"`: investigate + `bash`,
#'   `write_file`, `replace_in_file`, `list_files`, `git_status`,
#'   `git_diff`, `git_log`, `run_r`. `"minimal"`: `read_file`,
#'   `grep_files`.
#' @param parent_session Parent session object; read for
#'   nested-spawning control and session-key derivation.
#' @param config Config list.
#' @return Subagent ID (character).
#' @examples
#' \donttest{
#' if (interactive()) {
#'     # Spawns a callr::r_session child loaded with corteza; the
#'     # registry is in-memory and dies with the parent R session, so
#'     # we wrap in interactive() to keep R CMD check from leaving
#'     # children behind.
#'     id <- subagent_spawn("look up the package version",
#'                          preset = "minimal")
#'     subagent_kill(id)
#' }
#' }
#' @importFrom callr r_session
#' @export
subagent_spawn <- function(task, model = NULL, tools = NULL, preset = NULL,
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

    if (!is.null(parent_session$cwd)) {
        cwd <- parent_session$cwd
    } else {
        cwd <- getwd()
    }

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

    # Plan mode is inherited: if the parent is in plan mode, the child
    # is too, so a spawned subagent can't launder a write through a
    # child process.
    child_plan_mode <- isTRUE(parent_session$plan_mode)

    tryCatch(
             session$run(
                         function(cwd, provider, model, tools_filter, system, max_turns,
                                  depth, id, plan_mode) {
        library(corteza)
        corteza::worker_init(cwd = cwd)
        corteza::subagent_turn_init(
                                    provider = provider,
                                    model = model,
                                    tools_filter = tools_filter,
                                    system = system,
                                    max_turns = max_turns,
                                    depth = depth,
                                    plan_mode = plan_mode
        )
        corteza::subagent_turn_set_id(id)
    },
                         list(cwd = cwd, provider = spawn_provider, model = spawn_model,
                              tools_filter = effective_tools, system = system_prompt,
                              max_turns = 10L, depth = child_depth, id = id,
                              plan_mode = child_plan_mode)
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
                                     model = spawn_model,
                                     provider = spawn_provider,
                                     started_at = Sys.time(),
                                     timeout = Sys.time() + subcfg$timeout_minutes * 60,
                                     depth = child_depth,
                                     # Usage counters (accumulated across queries; cost is NA when
                                     # the provider doesn't surface it).
                                     cumulative_input_tokens = 0L,
                                     cumulative_output_tokens = 0L,
                                     cumulative_total_tokens = 0L,
                                     cumulative_cost = NA_real_,
                                     # TRUE once any query returns tokens but no
                                     # cost, so the spend rollup reads as a floor.
                                     cost_missing = FALSE,
                                     query_count = 0L
    )
    # Initialize the durable transcript file. Disk space is cheap;
    # context is expensive -- the in-memory child history may later be
    # compacted, but the on-disk transcript is append-only and never
    # rewritten. Header writes are idempotent.
    tryCatch(
             transcript_write_header(id, cwd, agent_id = paste0("subagent-", id)),
             error = function(e) {
        log_event("subagent_transcript_init_failed", subagent_id = id,
                  error = conditionMessage(e), level = "warn")
    })

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
#' With `wait = FALSE` the call returns immediately after firing the
#' prompt; the parent collects the reply later with [subagent_collect()].
#' A subagent can only carry one in-flight async query at a time:
#' firing a second one while the first is pending raises an error.
#'
#' @param id Subagent identifier. Accepts the canonical UUID, a unique
#'   UUID prefix, or the per-session sequence number printed by
#'   `subagent_list()` / `/agents`.
#' @param prompt Prompt to send.
#' @param wait If TRUE (default), block until the child replies and
#'   return the reply text. If FALSE, fire the prompt and return the
#'   canonical id invisibly; caller must collect via
#'   [subagent_collect()].
#' @param timeout Timeout in seconds (currently advisory; callr-level
#'   hard timeouts are future work).
#' @param return_name Optional single name or `.h_NNN` handle for a
#'   value the child should hand back. When set, the child must have
#'   left the result bound under that name (e.g. via `run_r`); the
#'   resolved value is stashed in the parent handle store and the reply
#'   gains a `[stored as .h_NNN]` block referencing it. Requires a
#'   subagent with `run_r` (the `work` preset). For `wait = FALSE` the
#'   name is captured now and applied when collected.
#' @return Reply text (character) when `wait = TRUE`, with a handle
#'   block appended when `return_name` resolved. Canonical id
#'   (character, invisibly) when `wait = FALSE`.
#' @examples
#' \dontrun{
#' # Requires LLM credentials in the child's environment.
#' id <- subagent_spawn("read R/skill.R and summarize", preset = "minimal")
#' subagent_query(id, "what does this file do?", wait = TRUE)
#' subagent_kill(id)
#' }
#' @export
subagent_query <- function(id, prompt, wait = TRUE, timeout = 60L,
                           return_name = NULL) {
    canonical <- resolve_subagent_id(id)
    if (is.null(canonical)) {
        stop("Subagent not found: ", id, call. = FALSE)
    }
    info <- .subagent_registry[[canonical]]
    if (Sys.time() > info$timeout) {
        subagent_kill(canonical)
        stop("Subagent expired: ", canonical, call. = FALSE)
    }

    # One-in-flight invariant: a session can carry only one outstanding
    # callr call at a time, so both wait paths must refuse to stack on
    # top of a pending async query.
    # `[[ ]]` not `$`: list `$` prefix-matches, so info$pending would
    # silently return info$pending_started_at when pending itself has
    # been NULL-stripped from the list.
    pending <- info[["pending"]]
    if (!is.null(pending)) {
        snippet <- substr(pending, 1L, 60L)
        stop(sprintf("Subagent %s is busy with: %s", canonical, snippet),
             call. = FALSE)
    }

    if (!isTRUE(wait)) {
        tryCatch(
                 info$session$call(
                                   function(p, rn) corteza::subagent_turn_prompt(p, rn),
                                   list(p = prompt, rn = return_name)
            ),
                 error = function(e) {
            stop("Subagent query failed to start: ",
                 conditionMessage(e), call. = FALSE)
        }
        )
        info$pending <- prompt
        info$pending_started_at <- Sys.time()
        .subagent_registry[[canonical]] <- info
        log_event("subagent_query_async", subagent_id = canonical,
                  prompt_length = nchar(prompt))
        return(invisible(canonical))
    }

    turn_result <- tryCatch(
                            info$session$run(
            function(p, rn) corteza::subagent_turn_prompt(p, rn),
            list(p = prompt, rn = return_name)
        ),
                            error = function(e) {
        stop("Subagent query failed: ", conditionMessage(e), call. = FALSE)
    })
    info <- subagent_accumulate_usage(info, turn_result$usage)
    .subagent_registry[[canonical]] <- info
    log_event("subagent_query", subagent_id = canonical,
              prompt_length = nchar(prompt))
    .format_subagent_reply(turn_result)
}

#' Collect the result of a previously-fired async subagent query.
#'
#' Pairs with `subagent_query(..., wait = FALSE)`. Returns the reply
#' text once the child finishes its turn, or NULL while the query is
#' still running. Result is read exactly once: after a successful
#' collect the pending slot is cleared, so the next async query can
#' fire.
#'
#' @param id Subagent identifier (UUID, prefix, or sequence number).
#' @param wait If TRUE (default), block up to `timeout` seconds waiting
#'   for the child to finish. If FALSE, poll once and return
#'   immediately.
#' @param timeout Maximum seconds to block when `wait = TRUE`. On
#'   timeout the child is left running; caller may collect again later
#'   or kill explicitly.
#' @return Reply text (character) when ready; NULL when still running.
#' @examples
#' \dontrun{
#' id <- subagent_spawn("background research")
#' subagent_query(id, "what's in DESCRIPTION?", wait = FALSE)
#' # ... do other work ...
#' subagent_collect(id, wait = TRUE, timeout = 30)
#' subagent_kill(id)
#' }
#' @export
subagent_collect <- function(id, wait = TRUE, timeout = 60L) {
    canonical <- resolve_subagent_id(id)
    if (is.null(canonical)) {
        stop("Subagent not found: ", id, call. = FALSE)
    }
    info <- .subagent_registry[[canonical]]
    if (is.null(info[["pending"]])) {
        stop("No pending query for subagent ", canonical, call. = FALSE)
    }
    if (isTRUE(wait)) {
        timeout_ms <- as.integer(timeout * 1000L)
    } else {
        timeout_ms <- 0L
    }
    state <- info$session$poll_process(timeout_ms)
    if (state != "ready") {
        return(invisible(NULL))
    }
    # callr's r_session: poll_process == "ready" means a message is
    # available; read() returns the callr_session_result with $result
    # (the call's return value) and $error (NULL when successful).
    msg <- info$session$read()
    info$pending <- NULL
    info$pending_started_at <- NULL
    if (is.null(msg$error)) {
        info <- subagent_accumulate_usage(info, msg$result$usage)
    }
    .subagent_registry[[canonical]] <- info
    if (!is.null(msg$error)) {
        stop("Subagent query failed: ", conditionMessage(msg$error),
             call. = FALSE)
    }
    log_event("subagent_collect", subagent_id = canonical)
    .format_subagent_reply(msg$result)
}

#' Kill a subagent.
#' @param id Subagent identifier (UUID, prefix, or sequence number).
#' @return Invisible TRUE if killed, FALSE if not found.
#' @examples
#' # Unknown id is a silent no-op (returns FALSE), so this is safe to
#' # run during R CMD check without a live subagent.
#' subagent_kill("no-such-id")
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
    # Best-effort external side effects first. These run before the
    # spend is retired so that nothing here can throw between retiring
    # the spend and removing the entry -- otherwise the agent would be
    # counted twice (live registry + retired) by subagent_spend_total().
    tryCatch(
             store_update(info$session_key, list(status = "completed",
                completedAt = as.numeric(Sys.time()) * 1000)),
             error = function(e) NULL)
    tryCatch(info$session$close(), error = function(e) NULL)
    # Retire spend and drop the entry together: subagent_retire_spend()
    # is pure in-memory arithmetic and rm() of an existing binding does
    # not throw, so the agent moves from live to retired atomically and
    # is counted exactly once.
    subagent_retire_spend(info)
    rm(list = canonical, envir = .subagent_registry)
    log_event("subagent_kill", subagent_id = canonical)
    invisible(TRUE)
}

#' Accumulate per-turn usage into a registry entry.
#'
#' `usage` is the `$usage` field returned by `subagent_turn_prompt()`
#' (originally from `llm.api::agent`). Missing fields are treated as
#' zero -- for providers that don't return cost (moonshot, ollama),
#' `cumulative_cost` stays NA.
#' @noRd
subagent_accumulate_usage <- function(info, usage) {
    if (is.null(usage)) {
        return(info)
    }
    add_int <- function(prev, new) {
        if (is.null(new) || is.na(new)) {
            prev
        } else {
            prev + as.integer(new)
        }
    }
    info$cumulative_input_tokens <- add_int(info$cumulative_input_tokens %||% 0L,
        usage$input_tokens)
    info$cumulative_output_tokens <- add_int(info$cumulative_output_tokens %||% 0L,
        usage$output_tokens)
    info$cumulative_total_tokens <- add_int(info$cumulative_total_tokens %||% 0L,
        usage$total_tokens)
    if (!is.null(usage$cost) && !is.na(usage$cost)) {
        prev <- info$cumulative_cost
        info$cumulative_cost <- if (is.na(prev)) {
            as.numeric(usage$cost)
        } else {
            prev + as.numeric(usage$cost)
        }
    } else if (.spend_usage_has_tokens(usage)) {
        # A query that consumed tokens but came back without a price
        # (cost-blind provider) makes the running cost a floor, not a
        # precise figure. A zero-token query leaves the flag alone.
        info$cost_missing <- TRUE
    }
    info$query_count <- (info$query_count %||% 0L) + 1L
    info
}

#' Roll a dying subagent's spend into the retired accumulator.
#'
#' Called by [subagent_kill()] before the registry entry is removed so
#' the agent's cost is not lost to the process-run total. Idempotent
#' only in the sense that it is called exactly once per kill.
#' @param info A registry entry list.
#' @return Invisible TRUE.
#' @noRd
subagent_retire_spend <- function(info) {
    r <- .subagent_spend_retired
    r$input_tokens <- r$input_tokens + (info$cumulative_input_tokens %||% 0L)
    r$output_tokens <- r$output_tokens + (info$cumulative_output_tokens %||% 0L)
    r$total_tokens <- r$total_tokens + (info$cumulative_total_tokens %||% 0L)
    r$query_count <- r$query_count + (info$query_count %||% 0L)
    r$n_agents <- r$n_agents + 1L
    cc <- info$cumulative_cost
    if (is.null(cc) || is.na(cc)) {
        if ((info$cumulative_total_tokens %||% 0L) > 0L) {
            r$cost_missing <- TRUE
        }
    } else {
        r$cost <- r$cost + as.numeric(cc)
    }
    if (isTRUE(info$cost_missing)) {
        r$cost_missing <- TRUE
    }
    invisible(TRUE)
}

#' Process-run subagent spend total (live registry + retired).
#'
#' Sums the cumulative usage of every live subagent plus the spend of
#' those already killed (the retired accumulator). Reads only the
#' in-process registry -- no callr round-trips -- so it is cheap enough
#' for `/spent` and the per-spawn MCP cap check. Cost is a floor when
#' any contributor lacked a price (`cost_missing`).
#' @return list(cost, input_tokens, output_tokens, total_tokens,
#'   query_count, n_agents, cost_missing).
#' @noRd
subagent_spend_total <- function() {
    acc <- list(cost = 0, input_tokens = 0L, output_tokens = 0L,
                total_tokens = 0L, query_count = 0L, n_agents = 0L,
                cost_missing = FALSE)
    for (id in ls(.subagent_registry)) {
        e <- .subagent_registry[[id]]
        acc$input_tokens <- acc$input_tokens + (e$cumulative_input_tokens %||% 0L)
        acc$output_tokens <- acc$output_tokens + (e$cumulative_output_tokens %||% 0L)
        acc$total_tokens <- acc$total_tokens + (e$cumulative_total_tokens %||% 0L)
        acc$query_count <- acc$query_count + (e$query_count %||% 0L)
        acc$n_agents <- acc$n_agents + 1L
        cc <- e$cumulative_cost
        if (is.null(cc) || is.na(cc)) {
            if ((e$cumulative_total_tokens %||% 0L) > 0L) {
                acc$cost_missing <- TRUE
            }
        } else {
            acc$cost <- acc$cost + as.numeric(cc)
        }
        if (isTRUE(e$cost_missing)) {
            acc$cost_missing <- TRUE
        }
    }
    r <- .subagent_spend_retired
    acc$input_tokens <- acc$input_tokens + r$input_tokens
    acc$output_tokens <- acc$output_tokens + r$output_tokens
    acc$total_tokens <- acc$total_tokens + r$total_tokens
    acc$query_count <- acc$query_count + r$query_count
    acc$n_agents <- acc$n_agents + r$n_agents
    acc$cost <- acc$cost + r$cost
    if (isTRUE(r$cost_missing)) {
        acc$cost_missing <- TRUE
    }
    acc
}

#' Best-effort live context-token count for an idle subagent.
#'
#' Calls into the child via `r_session$run()` to compute the same
#' `context_usage_pct()` math the compaction policy uses. Returns NA
#' on any failure (busy child, callr error, etc.) so the caller can
#' display `?` instead of crashing `/agents`.
#' @noRd
subagent_live_token_count <- function(info) {
    if (!is.null(info[["pending"]])) {
        return(list(tokens = NA_integer_, limit = NA_integer_))
    }
    result <- tryCatch(
                       info$session$run(function() {
        subagent_state <- utils::getFromNamespace(".subagent_state", "corteza")
        skills_as_api_tools <- utils::getFromNamespace("skills_as_api_tools",
            "corteza")
        sess <- subagent_state$session
        if (is.null(sess)) {
            return(list(tokens = NA_integer_, limit = NA_integer_,
                        model = NULL))
        }
        # Match the model the child actually runs with: explicit
        # model_map$cloud first, otherwise the provider default.
        # Without this fallback, child sessions spawned with the
        # default model report `ctx ?` because there's no explicit
        # model name to look up a limit for.
        model <- sess$model_map$cloud %||%
        corteza::default_provider_model(sess$provider)
        tools <- tryCatch(skills_as_api_tools(sess$tools_filter),
                          error = function(e) NULL)
        list(
             tokens = corteza::estimate_live_context_tokens(
                list(history = sess$history %||% list()),
                system_prompt = sess$system, tools = tools),
             limit = if (is.null(model)) NA_integer_ else
             corteza::context_limit_for_model(model),
             model = model)
    }),
                       error = function(e) list(tokens = NA_integer_, limit = NA_integer_,
            model = NULL))
    result
}

#' List active subagents.
#'
#' Returns a list of info objects per agent: id/seq/task/started_at/
#' time_remaining/pending plus model/age/cumulative usage and a
#' best-effort live token count for idle agents (`NA` for busy).
#' @return List of subagent info objects.
#' @examples
#' # Empty when no subagent has been spawned yet -- safe to call any time.
#' subagent_list()
#' @export
subagent_list <- function() {
    ids <- ls(.subagent_registry)
    if (length(ids) == 0L) {
        return(list())
    }
    out <- lapply(ids, function(id) {
        info <- .subagent_registry[[id]]
        live <- subagent_live_token_count(info)
        age_seconds <- as.numeric(difftime(Sys.time(), info$started_at,
                units = "secs"))
        # Display the actual model the child runs with -- explicit
        # info$model first, otherwise the resolved default for the
        # provider (live$model, which subagent_live_token_count
        # already computed inside the child). Falls back to provider
        # then "?" only if neither is known.
        resolved_model <- info$model %||% live$model %||%
        default_provider_model(info$provider) %||%
        info$provider %||% "?"
        # `[[ ]]` for pending fields: list `$` prefix-matches, so
        # info$pending would silently return info$pending_started_at
        # whenever pending itself has been NULL-stripped.
        list(
             id = info$id,
             seq = info$seq,
             task = info$task,
             model = resolved_model,
             started_at = info$started_at,
             age_seconds = age_seconds,
             time_remaining = as.numeric(difftime(info$timeout, Sys.time(),
                    units = "mins")),
             live_tokens = live$tokens,
             context_limit = live$limit,
             cumulative_input_tokens = info$cumulative_input_tokens %||% 0L,
             cumulative_output_tokens = info$cumulative_output_tokens %||% 0L,
             cumulative_total_tokens = info$cumulative_total_tokens %||% 0L,
             cumulative_cost = info$cumulative_cost %||% NA_real_,
             query_count = info$query_count %||% 0L,
             pending = info[["pending"]],
             pending_started_at = info[["pending_started_at"]]
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
    if (length(agents) == 0L) {
        return("No active subagents.")
    }
    lines <- c("Active subagents:")
    for (a in agents) {
        time_str <- if (a$time_remaining > 0) {
            sprintf("%.1f min remaining", a$time_remaining)
        } else {
            "expired"
        }
        if (!is.null(a$seq)) {
            seq_str <- sprintf("%d", a$seq)
        } else {
            seq_str <- "?"
        }
        id_short <- substr(a$id, 1L, 8L)
        # `[[ ]]` not `$`: list `$` prefix-matches, so a$pending would
        # silently return a$pending_started_at whenever pending itself
        # is NULL.
        pending <- a[["pending"]]
        state_str <- if (!is.null(pending)) {
            snippet <- substr(pending, 1L, 40L)
            if (nchar(pending) > 40L) {
                snippet <- paste0(snippet, "...")
            }
            sprintf(" busy: %s", snippet)
        } else {
            " idle"
        }
        # Model / age / live ctx / cumulative tokens / cost. Live ctx
        # is "?" when the child is busy (callr can't ask it
        # mid-turn). Cost is "?" when the provider doesn't surface it.
        model_str <- as.character(a$model %||% "?")
        age_str <- format_age(a$age_seconds %||% 0)
        ctx_str <- format_live_ctx(a$live_tokens, a$context_limit)
        tok_str <- sprintf("%s in / %s out",
                           format_tokens(a$cumulative_input_tokens %||% 0L),
                           format_tokens(a$cumulative_output_tokens %||% 0L))
        cost_str <- if (is.na(a$cumulative_cost)) {
            "?"
        } else {
            sprintf("$%.4f", a$cumulative_cost)
        }
        meta <- sprintf("(%s \u00B7 %s \u00B7 %s \u00B7 %s \u00B7 %s)",
                        model_str, age_str, ctx_str, tok_str, cost_str)
        lines <- c(lines, sprintf("  [%s] %s %s (%s)%s %s",
                                  seq_str, a$task, meta, time_str,
                                  state_str, id_short))
    }
    paste(c(lines, "",
            "Use the sequence number, the 8-char prefix, or the full id with /ask, /collect, and /kill."),
          collapse = "\n")
}

