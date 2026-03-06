# Workspace Environment + Registry
# Managed runtime state: live values with provenance metadata
#
# Two package-level environments:
#   .workspace      - live R values
#   .workspace_meta - provenance metadata per object
#   .workspace_state - turn counter
#
# Each metadata entry tracks: name, class, origin, turn, timestamp,
# deps, byte_size, stale, pinned

.workspace <- new.env(parent = emptyenv())
.workspace_meta <- new.env(parent = emptyenv())
.workspace_state <- new.env(parent = emptyenv())
.workspace_state$turn <- 0L

# CRUD ----

#' Store a value in the workspace
#'
#' @param name Character name for the object
#' @param value The R object to store
#' @param origin List with tool/args provenance (default: empty)
#' @param deps Character vector of workspace names this depends on
#' @param pinned If TRUE, object survives pruning
#' @return Invisible name
#' @noRd
ws_put <- function(name, value, origin = list(), deps = character(),
                   pinned = FALSE) {
    .workspace[[name]] <- value

    meta <- list(
                 name = name,
                 class = class(value)[1],
                 origin = origin,
                 turn = .workspace_state$turn,
                 timestamp = Sys.time(),
                 deps = deps,
                 byte_size = as.integer(object.size(value)),
                 stale = FALSE,
                 pinned = pinned
    )
    .workspace_meta[[name]] <- meta

    invisible(name)
}

#' Get a value from the workspace
#'
#' @param name Object name
#' @return The stored value, or NULL if not found
#' @noRd
ws_get <- function(name) {
    if (exists(name, envir = .workspace, inherits = FALSE)) {
        .workspace[[name]]
    } else {
        NULL
    }
}

#' Get metadata for a workspace object
#'
#' @param name Object name
#' @return Provenance list, or NULL if not found
#' @noRd
ws_meta <- function(name) {
    if (exists(name, envir = .workspace_meta, inherits = FALSE)) {
        .workspace_meta[[name]]
    } else {
        NULL
    }
}

#' Set metadata for a workspace object (for testing)
#'
#' @param name Object name
#' @param meta Metadata list
#' @return Invisible name
#' @noRd
ws_set_meta <- function(name, meta) {
    .workspace_meta[[name]] <- meta
    invisible(name)
}

#' Remove an object from the workspace
#'
#' @param name Object name
#' @return Invisible logical (TRUE if removed)
#' @noRd
ws_remove <- function(name) {
    existed <- ws_exists(name)
    if (existed) {
        rm(list = name, envir = .workspace)
        rm(list = name, envir = .workspace_meta)
    }
    invisible(existed)
}

#' Check if an object exists in the workspace
#'
#' @param name Object name
#' @return Logical
#' @noRd
ws_exists <- function(name) {
    exists(name, envir = .workspace, inherits = FALSE)
}

#' List workspace objects as a data.frame
#'
#' @return data.frame with name, class, turn, byte_size, stale, pinned
#' @noRd
ws_list <- function() {
    nms <- ws_names()
    if (length(nms) == 0) {
        return(data.frame(
                          name = character(),
                          class = character(),
                          turn = integer(),
                          byte_size = integer(),
                          stale = logical(),
                          pinned = logical(),
                          stringsAsFactors = FALSE
            ))
    }

    rows <- lapply(nms, function(nm) {
        m <- .workspace_meta[[nm]]
        data.frame(
                   name = m$name,
                   class = m$class,
                   turn = m$turn,
                   byte_size = m$byte_size,
                   stale = m$stale,
                   pinned = m$pinned,
                   stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

#' Get workspace object names
#'
#' @return Character vector
#' @noRd
ws_names <- function() {
    ls(.workspace)
}

#' Get total workspace size in bytes
#'
#' @return Integer
#' @noRd
ws_size <- function() {
    nms <- ws_names()
    if (length(nms) == 0) {
        return(0L)
    }
    sum(vapply(nms, function(nm) {
        m <- .workspace_meta[[nm]]
        m$byte_size %||% 0L
    }, integer(1)))
}

# Turn counter ----

#' Set the current turn number
#' @param n Integer turn number
#' @noRd
ws_set_turn <- function(n) {
    .workspace_state$turn <- as.integer(n)
}

#' Get the current turn number
#' @return Integer
#' @noRd
ws_current_turn <- function() {
    .workspace_state$turn
}

# Invalidation ----

#' Mark a workspace object as stale
#'
#' Propagates staleness to dependents (objects that list this name in deps).
#' Uses a visited set to prevent cycles.
#'
#' @param name Object name to invalidate
#' @return Invisible character vector of all invalidated names
#' @noRd
ws_invalidate <- function(name) {
    invalidated <- character()
    visited <- character()

    invalidate_recursive <- function(nm) {
        if (nm %in% visited) {
            return()
        }
        visited <<- c(visited, nm)

        meta <- ws_meta(nm)
        if (is.null(meta)) {
            return()
        }

        meta$stale <- TRUE
        .workspace_meta[[nm]] <- meta
        invalidated <<- c(invalidated, nm)

        # Find dependents: objects whose deps include nm
        all_names <- ws_names()
        for (other in all_names) {
            other_meta <- .workspace_meta[[other]]
            if (nm %in% other_meta$deps) {
                invalidate_recursive(other)
            }
        }
    }

    invalidate_recursive(name)
    invisible(invalidated)
}

#' Mark a workspace object as fresh (not stale)
#'
#' @param name Object name
#' @return Invisible logical
#' @noRd
ws_mark_fresh <- function(name) {
    meta <- ws_meta(name)
    if (is.null(meta)) {
        return(invisible(FALSE))
    }
    meta$stale <- FALSE
    .workspace_meta[[name]] <- meta
    invisible(TRUE)
}

#' Clear the entire workspace
#'
#' @return Invisible NULL
#' @noRd
ws_clear <- function() {
    rm(list = ls(.workspace), envir = .workspace)
    rm(list = ls(.workspace_meta), envir = .workspace_meta)
    .workspace_state$turn <- 0L
    invisible(NULL)
}

# Auto-capture ----

#' No-capture tool list
#' @noRd
ws_no_capture_tools <- function() {
    c("base::writeLines", "memory_store", "memory_recall", "memory_get",
        "spawn_subagent", "query_subagent", "list_subagents", "kill_subagent")
}

#' Capture a tool result into the workspace
#'
#' Stores tool results with provenance. Skips no-capture tools,
#' errors, and oversized results.
#'
#' @param tool_name Tool that produced the result
#' @param args Tool arguments
#' @param result Result text (character)
#' @param turn Current turn number
#' @param max_size Max result size in bytes to capture (default 50000)
#' @return Invisible name if captured, NULL if skipped
#' @noRd
ws_capture_tool_result <- function(tool_name, args, result, turn,
                                   max_size = 50000L) {
    # Skip no-capture tools
    if (tool_name %in% ws_no_capture_tools()) {
        return(invisible(NULL))
    }

    # Skip errors
    if (!is.character(result) || length(result) == 0) {
        return(invisible(NULL))
    }
    if (startsWith(result, "Error:")) {
        return(invisible(NULL))
    }

    # Skip oversized results
    if (nchar(result) > max_size) {
        return(invisible(NULL))
    }

    # Generate key name
    key <- if (tool_name == "base::readLines" && !is.null(args$con)) {
        paste0("file:", args$con)
    } else if (tool_name == "grep_files" && !is.null(args$pattern)) {
        paste0("grep:", args$pattern)
    } else if (tool_name == "bash" && !is.null(args$command)) {
        cmd_short <- substr(args$command, 1, 40)
        paste0("bash:", cmd_short)
    } else if (tool_name == "web_search" && !is.null(args$query)) {
        paste0("search:", args$query)
    } else {
        paste0(tool_name, ":turn", turn)
    }

    ws_set_turn(turn)
    ws_put(key, result, origin = list(tool = tool_name, args = args))

    invisible(key)
}

#' Invalidate cached file reads when a file is written
#'
#' @param path File path that was written
#' @return Invisible character vector of invalidated names
#' @noRd
ws_invalidate_file <- function(path) {
    key <- paste0("file:", path)
    if (ws_exists(key)) {
        ws_invalidate(key)
    } else {
        invisible(character())
    }
}

# Persistence ----

#' Save workspace to disk
#'
#' @param session_id Session ID
#' @param agent_id Agent ID (default: "main")
#' @return Invisible path to saved files
#' @noRd
ws_save <- function(session_id, agent_id = "main") {
    dir <- sessions_dir(agent_id)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, mode = "0700")
    }

    rds_path <- file.path(dir, paste0(session_id, "_workspace.rds"))
    json_path <- file.path(dir, paste0(session_id, "_workspace.json"))

    nms <- ws_names()
    if (length(nms) == 0) {
        # Clean up any existing files
        unlink(c(rds_path, json_path))
        return(invisible(NULL))
    }

    # Save values (skip non-serializable objects)
    values <- list()
    skipped <- character()
    for (nm in nms) {
        val <- tryCatch({
            # Test serializability by round-tripping
            serialize(.workspace[[nm]], NULL)
            .workspace[[nm]]
        }, error = function(e) {
            skipped <<- c(skipped, nm)
            NULL
        })
        if (!is.null(val)) {
            values[[nm]] <- val
        }
    }

    if (length(skipped) > 0) {
        warning("Workspace: skipped non-serializable objects: ",
                paste(skipped, collapse = ", "))
    }

    saveRDS(values, rds_path)

    # Save metadata as JSON
    meta_list <- lapply(nms[!nms %in% skipped], function(nm) {
        m <- .workspace_meta[[nm]]
        m$timestamp <- format(m$timestamp, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
        m
    })
    names(meta_list) <- nms[!nms %in% skipped]

    state <- list(turn = .workspace_state$turn, objects = meta_list)
    writeLines(
               jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE,
                                null = "null"),
               json_path
    )

    invisible(rds_path)
}

#' Load workspace from disk
#'
#' @param session_id Session ID
#' @param agent_id Agent ID (default: "main")
#' @return Invisible logical (TRUE if loaded)
#' @noRd
ws_load <- function(session_id, agent_id = "main") {
    dir <- sessions_dir(agent_id)
    rds_path <- file.path(dir, paste0(session_id, "_workspace.rds"))
    json_path <- file.path(dir, paste0(session_id, "_workspace.json"))

    if (!file.exists(rds_path) || !file.exists(json_path)) {
        return(invisible(FALSE))
    }

    ws_clear()

    # Load values
    values <- tryCatch(readRDS(rds_path), error = function(e) NULL)
    if (is.null(values)) {
        return(invisible(FALSE))
    }

    # Load metadata
    state <- tryCatch(
                      jsonlite::fromJSON(json_path, simplifyVector = FALSE),
                      error = function(e) NULL
    )
    if (is.null(state)) {
        return(invisible(FALSE))
    }

    # Restore turn
    .workspace_state$turn <- state$turn %||% 0L

    # Restore objects
    for (nm in names(values)) {
        .workspace[[nm]] <- values[[nm]]

        meta <- state$objects[[nm]]
        if (!is.null(meta)) {
            # Convert timestamp back to POSIXct
            meta$timestamp <- as.POSIXct(meta$timestamp,
                format = "%Y-%m-%dT%H:%M:%OS",
                tz = "UTC")
            .workspace_meta[[nm]] <- meta
        }
    }

    invisible(TRUE)
}

#' Prune old/large workspace objects
#'
#' Removes non-pinned objects older than max_age_turns, then removes
#' largest non-pinned objects until total size is under max_total_bytes.
#'
#' @param max_age_turns Max age in turns (default 50)
#' @param max_total_bytes Max total workspace size (default 5MB)
#' @return Invisible character vector of pruned names
#' @noRd
ws_prune <- function(max_age_turns = 50L, max_total_bytes = 5e6) {
    pruned <- character()
    current <- ws_current_turn()

    # Pass 1: remove old non-pinned objects
    for (nm in ws_names()) {
        meta <- .workspace_meta[[nm]]
        if (isTRUE(meta$pinned)) {
            next
        }
        age <- current - (meta$turn %||% 0L)
        if (age > max_age_turns) {
            ws_remove(nm)
            pruned <- c(pruned, nm)
        }
    }

    # Pass 2: remove largest non-pinned objects until under budget
    while (ws_size() > max_total_bytes) {
        nms <- ws_names()
        if (length(nms) == 0) {
            break
        }

        # Find largest non-pinned
        sizes <- vapply(nms, function(nm) {
            m <- .workspace_meta[[nm]]
            if (isTRUE(m$pinned)) return(0L)
            m$byte_size %||% 0L
        }, integer(1))

        if (max(sizes) == 0L) {
            break # only pinned objects left
        }

        largest <- nms[which.max(sizes)]
        ws_remove(largest)
        pruned <- c(pruned, largest)
    }

    invisible(pruned)
}

