# Context Engine
# R as the thalamic filter: decides what reaches the LLM's attention.
#
# Target: pack ~100K tokens (400KB) of the best possible context every turn.
# Async pre-computation runs in dead time while user thinks/types.
#
# Components:
#   .context_engine$conversation  - data.frame: queryable conversation index
#   .context_engine$file_index    - named list: path -> lines (in-memory project)
#   .context_engine$symbol_index  - basalt symbols() output, cached
#   .context_engine$payload       - pre-computed context, ready to splice
#   .context_engine$bg_process    - callr::r_bg handle (NULL when idle)
#   .context_engine$cwd           - project working directory
#   .context_engine$config        - config snapshot

.context_engine <- new.env(parent = emptyenv())

# Lifecycle ----

#' Initialize the context engine
#'
#' Loads file index, basalt symbols, and prepares for payload assembly.
#'
#' @param cwd Project working directory
#' @param config Config list from load_config()
#' @return Invisible NULL
#' @noRd
ce_init <- function(cwd, config = list()) {
    .context_engine$cwd <- cwd
    .context_engine$config <- config
    .context_engine$bg_process <- NULL
    .context_engine$payload <- NULL
    .context_engine$dirty <- TRUE

    # Initialize conversation index
    .context_engine$conversation <- data.frame(
        turn = integer(),
        role = character(),
        content = character(),
        tokens = integer(),
        tool_calls = I(list()),
        files_touched = I(list()),
        timestamp = as.POSIXct(character()),
        stringsAsFactors = FALSE
    )

    # Build in-memory file index
    ce_index_files(cwd)

    # Cache basalt symbols (best-effort)
    ce_update_symbols(cwd)

    invisible(NULL)
}

#' Shut down the context engine
#'
#' Cleans up background processes.
#'
#' @return Invisible NULL
#' @noRd
ce_shutdown <- function() {
    proc <- .context_engine$bg_process
    if (!is.null(proc) && inherits(proc, "r_process") && proc$is_alive()) {
        proc$kill()
    }
    .context_engine$bg_process <- NULL
    invisible(NULL)
}

# Conversation indexing ----

#' Index a conversation turn
#'
#' Adds a turn to the in-memory conversation data.frame.
#'
#' @param turn Integer turn number
#' @param role "user" or "assistant"
#' @param content Message text
#' @param tool_calls Character vector of tool names called (NULL for none)
#' @param files_touched Character vector of file paths touched (NULL for none)
#' @return Invisible turn number
#' @noRd
ce_index_turn <- function(turn, role, content, tool_calls = NULL,
                          files_touched = NULL) {
    tokens <- ceiling(nchar(content) / 4L)

    row <- data.frame(
                      turn = as.integer(turn),
                      role = role,
                      content = content,
                      tokens = tokens,
                      tool_calls = I(list(tool_calls %||% character())),
                      files_touched = I(list(files_touched %||% character())),
                      timestamp = Sys.time(),
                      stringsAsFactors = FALSE
    )

    conv <- .context_engine$conversation
    .context_engine$conversation <- rbind(conv, row)
    .context_engine$dirty <- TRUE

    invisible(turn)
}

#' Get the conversation data.frame
#'
#' @return data.frame with turn, role, content, tokens, tool_calls,
#'   files_touched, timestamp
#' @noRd
ce_conversation <- function() {
    .context_engine$conversation
}

#' Search conversation by keyword
#'
#' @param query Character string to search for
#' @return data.frame subset of matching turns
#' @noRd
ce_search_conversation <- function(query) {
    conv <- .context_engine$conversation
    if (nrow(conv) == 0) {
        return(conv)
    }

    hits <- grep(query, conv$content, ignore.case = TRUE)
    conv[hits,, drop = FALSE]
}

#' Total tokens in conversation
#'
#' @return Integer
#' @noRd
ce_conversation_tokens <- function() {
    conv <- .context_engine$conversation
    if (nrow(conv) == 0) {
        return(0L)
    }
    sum(conv$tokens)
}

# File index ----

#' Index project files into memory
#'
#' Reads all text files matching patterns into a named list for instant grep.
#'
#' @param cwd Project directory
#' @param patterns Glob patterns to match
#' @param max_file_size Max file size in bytes (skip larger files)
#' @return Invisible count of indexed files
#' @noRd
ce_index_files <- function(cwd,
                           patterns = c("R/*.R", "inst/**/*.R", "*.md", "*.json", "man/*.Rd"),
                           max_file_size = 100000L) {
    index <- list()

    for (pat in patterns) {
        files <- Sys.glob(file.path(cwd, pat))
        for (f in files) {
            # Skip .git, binary, oversized
            if (grepl(".git/", f, fixed = TRUE)) {
                next
            }
            sz <- tryCatch(file.size(f), error = function(e) Inf)
            if (sz > max_file_size) {
                next
            }

            lines <- tryCatch(readLines(f, warn = FALSE),
                              error = function(e) NULL)
            if (!is.null(lines)) {
                # Store with path relative to cwd
                rel <- sub(paste0("^", normalizePath(cwd, mustWork = FALSE),
                                  "/?"), "", normalizePath(f, mustWork = FALSE))
                index[[rel]] <- lines
            }
        }
    }

    .context_engine$file_index <- index
    invisible(length(index))
}

#' Update specific files in the index
#'
#' Re-reads only the specified files. Call after tool writes.
#'
#' @param paths Character vector of file paths (absolute or relative)
#' @return Invisible count of updated files
#' @noRd
ce_update_files <- function(paths) {
    cwd <- .context_engine$cwd %||% getwd()
    updated <- 0L

    for (p in paths) {
        if (startsWith(p, "/")) {
            abs_path <- p
        } else {
            abs_path <- file.path(cwd, p)
        }
        if (!file.exists(abs_path)) {
            # File deleted: remove from index
            # Can't normalizePath a deleted file, use raw relative
            rel <- if (startsWith(p, "/")) {
                sub(paste0("^", normalizePath(cwd, mustWork = FALSE),
                           "/?"), "", p)
            } else {
                p
            }
            if (nchar(rel) > 0) {
                .context_engine$file_index[[rel]] <- NULL
            }
            next
        }

        lines <- tryCatch(readLines(abs_path, warn = FALSE),
                          error = function(e) NULL)
        if (!is.null(lines)) {
            rel <- sub(paste0("^", normalizePath(cwd, mustWork = FALSE),
                              "/?"), "", normalizePath(abs_path, mustWork = FALSE))
            .context_engine$file_index[[rel]] <- lines
            updated <- updated + 1L
        }
    }

    .context_engine$dirty <- TRUE
    invisible(updated)
}

#' In-memory grep across file index
#'
#' Search all indexed files for a pattern. Microseconds, no disk IO.
#'
#' @param pattern Regex pattern
#' @param file_glob Optional glob to filter files (e.g., "*.R")
#' @return data.frame with file, line_number, text columns
#' @noRd
ce_grep <- function(pattern, file_glob = NULL) {
    index <- .context_engine$file_index
    if (is.null(index) || length(index) == 0) {
        return(data.frame(file = character(), line_number = integer(),
                          text = character(), stringsAsFactors = FALSE))
    }

    # Filter files by glob if specified
    files <- names(index)
    if (!is.null(file_glob)) {
        # Convert glob to regex
        glob_regex <- utils::glob2rx(file_glob)
        files <- files[grepl(glob_regex, basename(files))]
    }

    results <- list()
    for (f in files) {
        lines <- index[[f]]
        hits <- grep(pattern, lines)
        if (length(hits) > 0) {
            results <- c(results, list(data.frame(
                        file = f,
                        line_number = hits,
                        text = lines[hits],
                        stringsAsFactors = FALSE
                    )))
        }
    }

    if (length(results) == 0) {
        return(data.frame(file = character(), line_number = integer(),
                          text = character(), stringsAsFactors = FALSE))
    }
    do.call(rbind, results)
}

#' Get file contents from index
#'
#' @param path Relative file path
#' @return Character vector of lines, or NULL if not indexed
#' @noRd
ce_file <- function(path) {
    if (is.null(path) || length(path) == 0 || nchar(path) == 0) {
        return(NULL)
    }
    idx <- .context_engine$file_index
    if (is.null(idx) || !path %in% names(idx)) {
        return(NULL)
    }
    idx[[path]]
}

#' Count of indexed files and total lines
#'
#' @return Named integer vector: files, lines, chars
#' @noRd
ce_file_stats <- function() {
    index <- .context_engine$file_index
    if (is.null(index) || length(index) == 0) {
        return(c(files = 0L, lines = 0L, chars = 0L))
    }
    n_lines <- sum(vapply(index, length, integer(1)))
    n_chars <- sum(vapply(index, function(x) sum(nchar(x)), numeric(1)))
    c(files = length(index), lines = n_lines, chars = as.integer(n_chars))
}

# Symbol/AST layer (via basalt) ----

#' Update symbol index from basalt
#'
#' @param cwd Project directory
#' @return Invisible logical (TRUE if updated)
#' @noRd
ce_update_symbols <- function(cwd) {
    if (!requireNamespace("basalt", quietly = TRUE)) {
        .context_engine$symbol_index <- NULL
        return(invisible(FALSE))
    }

    .context_engine$symbol_index <- tryCatch(
        basalt::symbols(cwd),
        error = function(e) NULL
    )
    invisible(!is.null(.context_engine$symbol_index))
}

#' Get function definitions from symbol index
#'
#' @return data.frame with name, file, line, exported columns, or NULL
#' @noRd
ce_definitions <- function() {
    idx <- .context_engine$symbol_index
    if (is.null(idx)) {
        return(NULL)
    }
    idx$defs
}

#' Get function call graph from symbol index
#'
#' @return data.frame with caller, callee, file, line columns, or NULL
#' @noRd
ce_call_graph <- function() {
    idx <- .context_engine$symbol_index
    if (is.null(idx)) {
        return(NULL)
    }
    idx$calls
}

#' Find callers of a function across the project
#'
#' @param fn Function name
#' @return data.frame of callers, or empty data.frame
#' @noRd
ce_blast_radius <- function(fn) {
    if (!requireNamespace("basalt", quietly = TRUE)) {
        return(data.frame(caller = character(), project = character(),
                          file = character(), line = integer(),
                          stringsAsFactors = FALSE))
    }
    cwd <- .context_engine$cwd %||% getwd()
    tryCatch(
             basalt::blast_radius(fn, project = cwd),
             error = function(e) {
        data.frame(caller = character(), project = character(),
                   file = character(), line = integer(),
                   stringsAsFactors = FALSE)
    }
    )
}

#' Find relevant code for a prompt using symbol index
#'
#' Matches prompt keywords against function names in the symbol index,
#' then returns the files and surrounding code.
#'
#' @param prompt User prompt
#' @param max_results Max number of matching functions to return
#' @return Character string of relevant code snippets
#' @noRd
ce_related_code <- function(prompt, max_results = 10L) {
    defs <- ce_definitions()
    if (is.null(defs) || nrow(defs) == 0) {
        return("")
    }

    # Extract keywords from prompt
    words <- unique(tolower(strsplit(prompt, "[^a-zA-Z0-9_]+")[[1]]))
    words <- words[nchar(words) > 2]
    if (length(words) == 0) {
        return("")
    }

    # Score each definition by keyword overlap
    scores <- vapply(seq_len(nrow(defs)), function(i) {
        name <- tolower(defs$name[i])
        # Split function name on _ and . for partial matching
        name_parts <- strsplit(name, "[_.]")[[1]]
        sum(vapply(words, function(w) {
            if (grepl(w, name, fixed = TRUE)) return(1)
            if (any(grepl(w, name_parts, fixed = TRUE))) return(0.5)
            0
        }, numeric(1)))
    }, numeric(1))

    # Get top matches
    top_idx <- head(order(scores, decreasing = TRUE), max_results)
    top_idx <- top_idx[scores[top_idx] > 0]
    if (length(top_idx) == 0) {
        return("")
    }

    # Retrieve code snippets from file index
    snippets <- character()
    for (i in top_idx) {
        file <- defs$file[i]
        line <- defs$line[i]
        fn_name <- defs$name[i]

        lines <- ce_file(file)
        if (is.null(lines)) {
            next
        }

        # Get function body: start at definition, read until next definition
        # or end of reasonable block
        start <- max(1, line)
        end <- min(length(lines), line + 30)

        # Try to find end of function (simple heuristic: closing brace at col 1)
        for (j in (line + 1):min(length(lines), line + 100)) {
            if (grepl("^\\}", lines[j])) {
                end <- j
                break
            }
        }

        snippet <- paste(sprintf("%s:%d-%d %s()", file, start, end, fn_name),
                         paste(lines[start:end], collapse = "\n"),
                         sep = "\n")
        snippets <- c(snippets, snippet)
    }

    paste(snippets, collapse = "\n\n")
}

# Payload assembly ----

#' Compute the full context payload
#'
#' Assembles system prompt + project context + workspace state,
#' targeting ~100K tokens total.
#'
#' @param prompt Current user prompt (for relevance scoring)
#' @param system_base Base system prompt (from load_context)
#' @param tools_json Tool definitions as JSON string
#' @return List with system (character), tokens_used (integer)
#' @noRd
ce_compute_payload <- function(prompt, system_base, tools_json = "") {
    target_tokens <- 100000L

    # Fixed costs
    system_base_tokens <- ceiling(nchar(system_base) / 4L)
    tool_tokens <- ceiling(nchar(tools_json) / 4L)
    conv_tokens <- ce_conversation_tokens()

    # Budget remaining for dynamic context
    budget_tokens <- target_tokens - system_base_tokens - tool_tokens -
    conv_tokens
    budget_tokens <- max(budget_tokens, 5000L) # floor: always have some budget
    budget_chars <- budget_tokens * 4L

    # Assemble dynamic context, ranked by relevance
    parts <- character()

    # 1. basalt briefing (if available, most bang per token)
    briefing <- ce_get_briefing()
    if (nchar(briefing) > 0) {
        parts <- c(parts, "## Project Briefing", "", briefing, "")
    }

    # 2. Relevant code from symbol index
    related <- ce_related_code(prompt, max_results = 8L)
    if (nchar(related) > 0) {
        parts <- c(parts, "## Relevant Code", "", related, "")
    }

    # 3. Files recently touched in conversation
    recent_files <- ce_recent_files(n = 5L)
    if (length(recent_files) > 0) {
        file_context <- ce_format_files(recent_files,
                                        max_chars = budget_chars %/% 3)
        if (nchar(file_context) > 0) {
            parts <- c(parts, "## Recently Touched Files", "", file_context, "")
        }
    }

    # 4. Workspace state
    ws_budget <- min(budget_chars %/% 4, 40000L)
    ws_context <- ws_format_context(
                                    ws_retrieve(prompt, budget_chars = ws_budget)
    )
    if (nchar(ws_context) > 0) {
        parts <- c(parts, ws_context, "")
    }

    # Combine and enforce budget
    dynamic_context <- paste(parts, collapse = "\n")
    if (nchar(dynamic_context) > budget_chars) {
        dynamic_context <- substr(dynamic_context, 1, budget_chars)
    }

    enriched_system <- if (nchar(dynamic_context) > 0) {
        paste(system_base, "\n\n", dynamic_context)
    } else {
        system_base
    }

    tokens_used <- ceiling(nchar(enriched_system) / 4L) + tool_tokens +
    conv_tokens

    list(
         system = enriched_system,
         tokens_used = tokens_used,
         budget_remaining = target_tokens - tokens_used
    )
}

#' Get basalt briefing for current project
#'
#' @return Character string (empty if basalt not available)
#' @noRd
ce_get_briefing <- function() {
    if (!requireNamespace("basalt", quietly = TRUE)) {
        return("")
    }

    cwd <- .context_engine$cwd %||% getwd()
    project <- basename(cwd)

    tryCatch({
        text <- basalt::briefing(project = project)
        if (is.character(text) && nchar(text) > 0) text else ""
    }, error = function(e) "")
}

#' Get files recently touched in conversation
#'
#' @param n Max number of files to return
#' @return Character vector of relative file paths
#' @noRd
ce_recent_files <- function(n = 5L) {
    conv <- .context_engine$conversation
    if (nrow(conv) == 0) {
        return(character())
    }

    # Collect all files_touched from recent turns, most recent first
    all_files <- character()
    for (i in rev(seq_len(nrow(conv)))) {
        ft <- conv$files_touched[[i]]
        if (length(ft) > 0) {
            all_files <- c(all_files, ft)
        }
    }

    # Unique, preserving order (most recent first)
    unique_files <- unique(all_files)
    head(unique_files, n)
}

#' Format file contents for context injection
#'
#' @param paths Character vector of relative file paths
#' @param max_chars Max total characters
#' @return Character string
#' @noRd
ce_format_files <- function(paths, max_chars = 20000L) {
    parts <- character()
    used <- 0L

    for (p in paths) {
        lines <- ce_file(p)
        if (is.null(lines)) {
            next
        }

        content <- paste(lines, collapse = "\n")
        if (used + nchar(content) > max_chars) {
            # Truncate this file to fit
            remaining <- max_chars - used
            if (remaining < 200) {
                break
            }
            content <- paste0(substr(content, 1, remaining - 20),
                              "\n... (truncated)")
        }

        parts <- c(parts, sprintf("### %s\n```\n%s\n```", p, content))
        used <- used + nchar(content)
    }

    paste(parts, collapse = "\n\n")
}

#' Get pre-computed payload (fast path)
#'
#' Returns cached payload if available, otherwise NULL.
#'
#' @return List with system and tokens_used, or NULL
#' @noRd
ce_get_payload <- function() {
    .context_engine$payload
}

#' Fast re-rank with actual prompt
#'
#' If pre-computed payload exists, does a fast adjustment.
#' Otherwise, computes from scratch.
#'
#' @param prompt User prompt
#' @param system_base Base system prompt
#' @param tools_json Tool definitions as JSON
#' @return List with system and tokens_used
#' @noRd
ce_rerank <- function(prompt, system_base, tools_json = "") {
    # For now, always compute fresh. Pre-computed payload is a future
    # optimization once we verify the synchronous path is fast enough.
    ce_compute_payload(prompt, system_base, tools_json)
}

# Async pre-computation ----

#' Check if pre-computation should run
#'
#' Skip when the assistant just asked a question or presented options.
#' User's next message is probably a short selection.
#'
#' @param assistant_response Last assistant response text
#' @return Logical
#' @noRd
ce_should_precompute <- function(assistant_response) {
    if (is.null(assistant_response) || length(assistant_response) == 0 ||
        nchar(assistant_response) < 200) {
        return(FALSE)
    }

    lines <- strsplit(assistant_response, "\n")[[1]]
    last_line <- trimws(tail(lines, 1))

    # Ends with question
    if (grepl("\\?\\s*$", last_line)) {
        return(FALSE)
    }

    # Contains enumerated options
    if (any(grepl("^\\s*[1-9ABC][\\.\\)]\\s", lines))) {
        return(FALSE)
    }

    TRUE
}

#' Kick off background pre-computation
#'
#' Runs payload assembly in a callr::r_bg process.
#' Falls back to synchronous if callr not available.
#'
#' @param system_base Base system prompt
#' @param tools_json Tool definitions as JSON
#' @return Invisible NULL
#' @noRd
ce_precompute <- function(system_base, tools_json = "") {
    if (!isTRUE(.context_engine$dirty)) {
        return(invisible(NULL))
    }

    # Don't stack background jobs
    if (!is.null(.context_engine$bg_process)) {
        if (inherits(.context_engine$bg_process, "r_process") &&
            .context_engine$bg_process$is_alive()) {
            return(invisible(NULL))
        }
        ce_poll()
    }

    # For now: synchronous pre-computation
    # The async callr path will be added once we verify this works
    if (!requireNamespace("callr", quietly = TRUE)) {
        # Synchronous fallback: just mark as ready
        .context_engine$dirty <- FALSE
        return(invisible(NULL))
    }

    # Async path: serialize state and compute in background
    # (Placeholder for Phase 8 of implementation)
    .context_engine$dirty <- FALSE
    invisible(NULL)
}

#' Poll background process for results
#'
#' @return Invisible NULL
#' @noRd
ce_poll <- function() {
    proc <- .context_engine$bg_process
    if (is.null(proc)) {
        return(invisible(NULL))
    }

    if (inherits(proc, "r_process") && !proc$is_alive()) {
        result <- tryCatch(proc$get_result(), error = function(e) NULL)
        if (!is.null(result)) {
            .context_engine$payload <- result
        }
        .context_engine$bg_process <- NULL
    }

    invisible(NULL)
}

# Utility ----

#' Extract tool names from an llm.api result
#'
#' @param result Result from llm.api::agent()
#' @return Character vector of tool names called
#' @noRd
ce_extract_tool_calls <- function(result) {
    if (is.null(result) || is.null(result$history)) {
        return(character())
    }

    tools <- character()
    for (msg in result$history) {
        if (!is.null(msg$tool_calls)) {
            for (tc in msg$tool_calls) {
                tools <- c(tools, tc$name %||% tc[["function"]]$name)
            }
        }
    }
    unique(tools)
}

#' Extract file paths touched by tool calls
#'
#' Heuristic: look for file path arguments in tool calls.
#'
#' @param result Result from llm.api::agent()
#' @return Character vector of file paths
#' @noRd
ce_extract_files_touched <- function(result) {
    if (is.null(result) || is.null(result$history)) {
        return(character())
    }

    files <- character()
    for (msg in result$history) {
        if (!is.null(msg$tool_calls)) {
            for (tc in msg$tool_calls) {
                args <- tc$arguments %||% tc[["function"]]$arguments
                if (is.character(args)) {
                    args <- tryCatch(
                                     jsonlite::fromJSON(args, simplifyVector = FALSE),
                                     error = function(e) list()
                    )
                }
                # Common file path argument names
                for (key in c("path", "con", "file", "filename")) {
                    if (!is.null(args[[key]]) && is.character(args[[key]])) {
                        files <- c(files, args[[key]])
                    }
                }
            }
        }
    }
    unique(files)
}

