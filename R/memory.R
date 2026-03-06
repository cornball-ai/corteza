# Memory Management
# Enhanced Markdown memory with tags and search

#' Get memory file path
#'
#' @param scope "project" or "global"
#' @param cwd Working directory for project scope
#' @return Path to MEMORY.md file
#' @noRd
memory_path <- function(scope = c("project", "global"), cwd = getwd()) {
    scope <- match.arg(scope)
    if (scope == "global") {
        file.path(get_workspace_dir(), "MEMORY.md")
    } else {
        file.path(cwd, ".llamar", "MEMORY.md")
    }
}

#' Parse tags from text
#'
#' Extracts hashtags from text (e.g., "#r #style #config")
#'
#' @param text Text to parse
#' @return Character vector of tags (without #)
#' @noRd
parse_tags <- function(text) {
    matches <- gregexpr("#[a-zA-Z0-9_-]+", text)
    tags <- regmatches(text, matches)[[1]]
    if (length(tags) == 0) {
        return(character())
    }
    # Remove # prefix
    gsub("^#", "", tags)
}

#' Remove tags from text
#'
#' @param text Text with tags
#' @return Text with tags removed
#' @noRd
strip_tags <- function(text) {
    trimws(gsub("#[a-zA-Z0-9_-]+", "", text))
}

#' Format a memory entry
#'
#' @param fact The fact to remember
#' @param tags Character vector of tags
#' @param timestamp POSIXct timestamp (default: now)
#' @return Formatted memory line
#' @noRd
format_memory_entry <- function(fact, tags = character(),
                                timestamp = Sys.time()) {
    date_str <- format(timestamp, "%Y-%m-%d")
    tag_str <- if (length(tags) > 0) {
        paste0(" ", paste0("#", tags, collapse = " "))
    } else {
        ""
    }
    sprintf("- %s (%s)%s", fact, date_str, tag_str)
}

#' Auto-categorize a fact
#'
#' Attempts to determine the category based on content.
#'
#' @param fact The fact text
#' @return Category name: "Preferences", "Facts", "Context", or NULL
#' @noRd
auto_categorize <- function(fact) {
    fact_lower <- tolower(fact)

    # Preference patterns
    if (grepl("prefer|like|want|use|always|never|style", fact_lower)) {
        return("Preferences")
    }

    # Fact patterns
    if (grepl("is|are|has|stores|located|path|key|api|config", fact_lower)) {
        return("Facts")
    }

    # Context patterns
    if (grepl("working on|project|currently|building|developing", fact_lower)) {
        return("Context")
    }

    NULL
}

#' Store a memory entry
#'
#' Appends a fact to MEMORY.md with optional tags and auto-categorization.
#'
#' @param fact The fact to remember (may include #tags)
#' @param tags Additional tags (combined with parsed tags)
#' @param category Category to file under (auto-detected if NULL)
#' @param scope "project" or "global"
#' @param cwd Working directory
#' @return Invisible TRUE on success
#' @noRd
memory_store <- function(fact, tags = character(), category = NULL,
                         scope = c("project", "global"), cwd = getwd()) {
    scope <- match.arg(scope)
    path <- memory_path(scope, cwd)

    # Parse tags from fact text
    parsed_tags <- parse_tags(fact)
    clean_fact <- strip_tags(fact)
    all_tags <- unique(c(tags, parsed_tags))

    # Auto-categorize if not specified
    if (is.null(category)) {
        category <- auto_categorize(clean_fact) %||% "Facts"
    }

    # Format the entry
    entry <- format_memory_entry(clean_fact, all_tags)

    # Read existing content
    if (file.exists(path)) {
        lines <- readLines(path, warn = FALSE)
    } else {
        # Create directory if needed
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        lines <- c(
                   "# Memory",
                   "",
                   "## Preferences",
                   "",
                   "## Facts",
                   "",
                   "## Context",
                   ""
        )
    }

    # Find the category section
    section_pattern <- sprintf("^## %s", category)
    section_idx <- grep(section_pattern, lines, ignore.case = TRUE)

    if (length(section_idx) == 0) {
        # Category doesn't exist, add it
        lines <- c(lines, "", sprintf("## %s", category), entry)
    } else {
        # Find insertion point (after section header, before next section or EOF)
        insert_idx <- section_idx[1] + 1

        # Skip blank lines after header
        while (insert_idx <= length(lines) && lines[insert_idx] == "") {
            insert_idx <- insert_idx + 1
        }

        # Find end of section (next ## or EOF)
        end_idx <- insert_idx
        while (end_idx <= length(lines) && !grepl("^##", lines[end_idx])) {
            end_idx <- end_idx + 1
        }

        # Insert at end of section content
        if (end_idx > length(lines)) {
            lines <- c(lines, entry)
        } else {
            lines <- c(lines[1:(end_idx - 1)], entry,
                       lines[end_idx:length(lines)])
        }
    }

    # Write back
    writeLines(lines, path)
    invisible(TRUE)
}

#' Search memory for matching entries
#'
#' Searches MEMORY.md files for entries matching a query.
#'
#' @param query Search query (supports simple keywords)
#' @param scope "project", "global", or "both"
#' @param cwd Working directory
#' @return List of matching entries with metadata
#' @noRd
memory_search <- function(query, scope = c("both", "project", "global"),
                          cwd = getwd()) {
    scope <- match.arg(scope)

    paths <- character()
    if (scope %in% c("both", "global")) {
        paths <- c(paths, memory_path("global", cwd))
    }
    if (scope %in% c("both", "project")) {
        paths <- c(paths, memory_path("project", cwd))
    }

    results <- list()

    for (path in paths) {
        if (!file.exists(path)) {
            next
        }

        lines <- readLines(path, warn = FALSE)
        if (grepl("workspace", path)) {
            scope_name <- "global"
        } else {
            scope_name <- "project"
        }

        current_section <- NULL

        for (i in seq_along(lines)) {
            line <- lines[i]

            # Track current section
            if (grepl("^## ", line)) {
                current_section <- sub("^## ", "", line)
                next
            }

            # Skip non-entry lines
            if (!grepl("^- ", line)) {
                next
            }

            # Check if line matches query
            if (grepl(query, line, ignore.case = TRUE)) {
                # Parse the entry
                entry_text <- sub("^- ", "", line)

                # Extract date if present
                date_match <- regmatches(entry_text,
                    regexec("\\(([0-9]{4}-[0-9]{2}-[0-9]{2})\\)", entry_text))[[1]]
                if (length(date_match) > 1) {
                    date <- date_match[2]
                } else {
                    date <- NULL
                }

                # Extract tags
                tags <- parse_tags(entry_text)

                # Clean text (remove date and tags)
                clean_text <- gsub("\\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "",
                                   entry_text)
                clean_text <- strip_tags(clean_text)
                clean_text <- trimws(clean_text)

                results <- c(results, list(list(
                            text = clean_text,
                            tags = tags,
                            date = date,
                            section = current_section,
                            scope = scope_name,
                            line = i,
                            raw = line
                        )))
            }
        }
    }

    results
}

#' Search memory by tag
#'
#' @param tag Tag to search for (without #)
#' @param scope "project", "global", or "both"
#' @param cwd Working directory
#' @return List of matching entries
#' @noRd
memory_search_tag <- function(tag, scope = c("both", "project", "global"),
                              cwd = getwd()) {
    scope <- match.arg(scope)
    # Search for the hashtag pattern
    memory_search(sprintf("#%s", tag), scope, cwd)
}

#' List all tags in memory
#'
#' @param scope "project", "global", or "both"
#' @param cwd Working directory
#' @return Character vector of unique tags
#' @noRd
memory_list_tags <- function(scope = c("both", "project", "global"),
                             cwd = getwd()) {
    scope <- match.arg(scope)

    paths <- character()
    if (scope %in% c("both", "global")) {
        paths <- c(paths, memory_path("global", cwd))
    }
    if (scope %in% c("both", "project")) {
        paths <- c(paths, memory_path("project", cwd))
    }

    all_tags <- character()

    for (path in paths) {
        if (!file.exists(path)) {
            next
        }
        content <- paste(readLines(path, warn = FALSE), collapse = "\n")
        all_tags <- c(all_tags, parse_tags(content))
    }

    sort(unique(all_tags))
}

#' Format search results for display
#'
#' @param results List from memory_search()
#' @return Character string for display
#' @noRd
format_memory_results <- function(results) {
    if (length(results) == 0) {
        return("No matching memories found.")
    }

    lines <- character()
    for (r in results) {
        if (r$scope == "global") {
            scope_marker <- "[G]"
        } else {
            scope_marker <- "[P]"
        }
        if (!is.null(r$section)) {
            section_marker <- sprintf("[%s]", r$section)
        } else {
            section_marker <- ""
        }
        tag_str <- if (length(r$tags) > 0) {
            sprintf(" #%s", paste(r$tags, collapse = " #"))
        } else {
            ""
        }
        if (!is.null(r$date)) {
            date_str <- sprintf(" (%s)", r$date)
        } else {
            date_str <- ""
        }

        lines <- c(lines, sprintf("%s%s %s%s%s",
                                  scope_marker, section_marker, r$text, date_str, tag_str))
    }

    paste(lines, collapse = "\n")
}

# ============================================================================
# Daily Memory Logs
# ============================================================================

#' Get daily memory log directory
#'
#' @return Path to memory log directory (~/.llamar/workspace/memory/)
#' @noRd
memory_log_dir <- function() {
    dir <- file.path(get_workspace_dir(), "memory")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir
}

#' Get path to daily memory log
#'
#' @param date Date (default: today)
#' @return Path to memory/YYYY-MM-DD.md
#' @noRd
memory_log_path <- function(date = Sys.Date()) {
    file.path(memory_log_dir(), sprintf("%s.md", format(date, "%Y-%m-%d")))
}

#' Write content to daily memory log
#'
#' Appends to the daily log, creating with header if new.
#'
#' @param content Text to append
#' @param date Date (default: today)
#' @return Invisible path to log file
#' @noRd
memory_log_write <- function(content, date = Sys.Date()) {
    path <- memory_log_path(date)

    if (!file.exists(path)) {
        header <- sprintf("# Memory Log: %s\n", format(date, "%Y-%m-%d"))
        writeLines(header, path)
    }

    cat(content, "\n", file = path, append = TRUE, sep = "")
    invisible(path)
}

#' List all daily memory logs
#'
#' @return Character vector of YYYY-MM-DD.md file paths, sorted newest-first
#' @noRd
memory_log_list <- function() {
    dir <- memory_log_dir()
    files <- list.files(dir, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}\\.md$",
                        full.names = TRUE)
    sort(files, decreasing = TRUE)
}

#' Load all daily memory logs
#'
#' @return Single string with all logs concatenated, or NULL if none
#' @noRd
memory_log_load_all <- function() {
    files <- memory_log_list()
    if (length(files) == 0) {
        return(NULL)
    }

    parts <- character()
    for (f in files) {
        content <- paste(readLines(f, warn = FALSE), collapse = "\n")
        if (nchar(trimws(content)) > 0) {
            parts <- c(parts, content, "")
        }
    }

    if (length(parts) == 0) {
        return(NULL)
    }
    paste(parts, collapse = "\n")
}

# ============================================================================
# SQLite-based Memory Index
# ============================================================================

#' Get path to memory database
#'
#' @param agent_id Agent identifier (default: "default")
#' @return Path to SQLite file
#' @noRd
memory_db_path <- function(agent_id = "default") {
    dir <- file.path(get_workspace_dir(), "memory")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.path(dir, sprintf("%s.sqlite", agent_id))
}

#' Initialize memory database schema
#'
#' Creates tables and FTS5 virtual table if they don't exist.
#'
#' @param con SQLite connection
#' @return Invisible NULL
#' @noRd
memory_db_init_schema <- function(con) {
    # Metadata table
    sql_meta <- paste0(
                       "CREATE TABLE IF NOT EXISTS meta (",
                       "key TEXT PRIMARY KEY,",
                       "value TEXT NOT NULL)")
    DBI::dbExecute(con, sql_meta)

    # Files table - tracks what's indexed
    sql_files <- paste0(
                        "CREATE TABLE IF NOT EXISTS files (",
                        "path TEXT PRIMARY KEY,",
                        "source TEXT NOT NULL DEFAULT 'memory',",
                        "hash TEXT NOT NULL,",
                        "mtime INTEGER NOT NULL,",
                        "size INTEGER NOT NULL,",
                        "indexed_at INTEGER NOT NULL)")
    DBI::dbExecute(con, sql_files)

    # Chunks table - text chunks (no embedding columns)
    sql_chunks <- paste0(
                         "CREATE TABLE IF NOT EXISTS chunks (",
                         "id TEXT PRIMARY KEY,",
                         "path TEXT NOT NULL,",
                         "source TEXT NOT NULL DEFAULT 'memory',",
                         "start_line INTEGER NOT NULL,",
                         "end_line INTEGER NOT NULL,",
                         "hash TEXT NOT NULL,",
                         "text TEXT NOT NULL,",
                         "updated_at INTEGER NOT NULL)")
    DBI::dbExecute(con, sql_chunks)

    # FTS5 virtual table for full-text search
    tryCatch({
        DBI::dbExecute(con, paste0(
                                   "CREATE VIRTUAL TABLE IF NOT EXISTS chunks_fts ",
                                   "USING fts5(id, text, content=chunks, content_rowid=rowid)"))

        # Auto-sync triggers: keep chunks_fts in sync with chunks
        DBI::dbExecute(con, paste0(
                                   "CREATE TRIGGER IF NOT EXISTS chunks_ai AFTER INSERT ON chunks BEGIN ",
                                   "INSERT INTO chunks_fts(rowid, id, text) ",
                                   "VALUES (new.rowid, new.id, new.text); END"))

        DBI::dbExecute(con, paste0(
                                   "CREATE TRIGGER IF NOT EXISTS chunks_ad AFTER DELETE ON chunks BEGIN ",
                                   "INSERT INTO chunks_fts(chunks_fts, rowid, id, text) ",
                                   "VALUES('delete', old.rowid, old.id, old.text); END"))

        DBI::dbExecute(con, paste0(
                                   "CREATE TRIGGER IF NOT EXISTS chunks_au AFTER UPDATE ON chunks BEGIN ",
                                   "INSERT INTO chunks_fts(chunks_fts, rowid, id, text) ",
                                   "VALUES('delete', old.rowid, old.id, old.text); ",
                                   "INSERT INTO chunks_fts(rowid, id, text) ",
                                   "VALUES (new.rowid, new.id, new.text); END"))
    }, error = function(e) {
        log_msg("FTS5 index creation failed:", e$message)
    })

    invisible(NULL)
}

#' Open memory database connection
#'
#' @param agent_id Agent identifier
#' @param read_only Open in read-only mode (ignored for SQLite, kept for API compat)
#' @return SQLite connection
#' @noRd
memory_db_open <- function(agent_id = "default", read_only = FALSE) {
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("RSQLite package required for memory indexing", call. = FALSE)
    }

    path <- memory_db_path(agent_id)
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

    if (!read_only) {
        memory_db_init_schema(con)
    }

    con
}

#' Close memory database connection
#'
#' @param con SQLite connection
#' @noRd
memory_db_close <- function(con) {
    DBI::dbDisconnect(con)
}

#' Compute hash of text content
#'
#' @param text Text to hash
#' @return MD5 hash string
#' @noRd
memory_hash <- function(text) {
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("digest package required for memory hashing", call. = FALSE)
    }
    digest::digest(text, algo = "md5", serialize = FALSE)
}

#' Chunk text into segments
#'
#' Splits text into overlapping chunks for indexing.
#'
#' @param text Character vector of lines
#' @param chunk_size Target lines per chunk
#' @param overlap Lines of overlap between chunks
#' @return List of chunk objects with start_line, end_line, text
#' @noRd
memory_chunk_text <- function(text, chunk_size = 50, overlap = 10) {
    if (length(text) == 0) {
        return(list())
    }

    chunks <- list()
    start <- 1

    while (start <= length(text)) {
        end <- min(start + chunk_size - 1, length(text))
        chunk_text <- paste(text[start:end], collapse = "\n")

        chunks <- c(chunks, list(list(
                                      start_line = start,
                                      end_line = end,
                                      text = chunk_text
                )))

        if (end >= length(text)) {
            break
        }
        start <- end - overlap + 1
    }

    chunks
}

#' Index a single file into memory database
#'
#' @param con DuckDB connection
#' @param path Path to file
#' @param source Source identifier (e.g., "memory", "session", "claude")
#' @param force Re-index even if unchanged
#' @return Number of chunks indexed
#' @noRd
memory_index_file <- function(con, path, source = "memory", force = FALSE) {
    if (!file.exists(path)) {
        return(0)
    }

    info <- file.info(path)
    mtime <- as.integer(info$mtime)
    size <- info$size

    # Check if already indexed and unchanged
    if (!force) {
        existing <- DBI::dbGetQuery(con,
                                    "SELECT hash, mtime FROM files WHERE path = ?",
                                    params = list(path))

        if (nrow(existing) > 0 && existing$mtime >= mtime) {
            return(0) # Already up to date
        }
    }

    # Read and hash content
    lines <- readLines(path, warn = FALSE)
    content <- paste(lines, collapse = "\n")
    hash <- memory_hash(content)

    # Check hash (content might be same even if mtime changed)
    if (!force) {
        existing <- DBI::dbGetQuery(con,
                                    "SELECT hash FROM files WHERE path = ?",
                                    params = list(path))
        if (nrow(existing) > 0 && existing$hash == hash) {
            # Update mtime but skip re-indexing
            DBI::dbExecute(con,
                           "UPDATE files SET mtime = ?, indexed_at = ? WHERE path = ?",
                           params = list(mtime, as.integer(Sys.time()), path))
            return(0)
        }
    }

    # Delete old chunks for this file
    DBI::dbExecute(con, "DELETE FROM chunks WHERE path = ?",
                   params = list(path))

    # Chunk and index
    chunks <- memory_chunk_text(lines)
    now <- as.integer(Sys.time())

    sql_insert_chunk <- paste0(
                               "INSERT OR REPLACE INTO chunks ",
                               "(id, path, source, start_line, end_line, hash, text, updated_at) ",
                               "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    )

    for (chunk in chunks) {
        chunk_id <- sprintf("%s:%d-%d", basename(path), chunk$start_line,
                            chunk$end_line)
        chunk_hash <- memory_hash(chunk$text)

        DBI::dbExecute(con, sql_insert_chunk, params = list(
                chunk_id, path, source,
                chunk$start_line, chunk$end_line,
                chunk_hash, chunk$text, now
            ))
    }

    # Update files table
    sql_insert_file <- paste0(
                              "INSERT OR REPLACE INTO files ",
                              "(path, source, hash, mtime, size, indexed_at) ",
                              "VALUES (?, ?, ?, ?, ?, ?)"
    )
    DBI::dbExecute(con, sql_insert_file,
                   params = list(path, source, hash, mtime, size, now))

    length(chunks)
}

#' Extract text from Claude Code JSONL session
#'
#' @param path Path to JSONL file
#' @return Character vector of "Role: text" lines
#' @noRd
claude_session_to_text <- function(path) {
    if (!file.exists(path)) {
        return(character())
    }

    lines <- readLines(path, warn = FALSE)
    result <- character()

    for (line in lines) {
        if (nchar(trimws(line)) == 0) {
            next
        }

        record <- tryCatch(
                           jsonlite::fromJSON(line, simplifyVector = FALSE),
                           error = function(e) NULL
        )
        if (is.null(record)) {
            next
        }

        # Claude Code format: type is "user" or "assistant"
        # openclaw format: type is "message" with message.role
        msg_type <- record$type
        message <- record$message

        if (is.null(message)) {
            next
        }

        role <- NULL
        if (msg_type %in% c("user", "assistant")) {
            role <- msg_type
        } else if (msg_type == "message" && !is.null(message$role)) {
            role <- message$role
        }

        if (is.null(role) || !role %in% c("user", "assistant")) {
            next
        }

        # Extract text content
        content <- message$content
        if (is.null(content)) {
            next
        }

        text_parts <- character()
        if (is.character(content)) {
            text_parts <- content
        } else if (is.list(content)) {
            for (block in content) {
                if (is.list(block) && block$type == "text" &&
                    !is.null(block$text)) {
                    text_parts <- c(text_parts, block$text)
                }
            }
        }

        if (length(text_parts) > 0) {
            if (role == "user") {
                label <- "User"
            } else {
                label <- "Assistant"
            }
            result <- c(result,
                        sprintf("%s: %s", label, paste(text_parts, collapse = " ")))
        }
    }

    result
}

#' Index Claude Code session into memory database
#'
#' @param con DuckDB connection
#' @param path Path to JSONL session file
#' @param force Re-index even if unchanged
#' @return Number of chunks indexed
#' @noRd
memory_index_claude_session <- function(con, path, force = FALSE) {
    if (!file.exists(path)) {
        return(0)
    }

    info <- file.info(path)
    mtime <- as.integer(info$mtime)
    size <- info$size

    # Check if already indexed
    if (!force) {
        existing <- DBI::dbGetQuery(con,
                                    "SELECT mtime FROM files WHERE path = ?",
                                    params = list(path))
        if (nrow(existing) > 0 && existing$mtime >= mtime) {
            return(0)
        }
    }

    # Extract text
    lines <- claude_session_to_text(path)
    if (length(lines) == 0) {
        return(0)
    }

    content <- paste(lines, collapse = "\n")
    hash <- memory_hash(content)

    # Delete old chunks
    DBI::dbExecute(con, "DELETE FROM chunks WHERE path = ?",
                   params = list(path))

    # Chunk and index
    chunks <- memory_chunk_text(lines, chunk_size = 30, overlap = 5)
    now <- as.integer(Sys.time())

    sql_insert_chunk <- paste0(
                               "INSERT OR REPLACE INTO chunks ",
                               "(id, path, source, start_line, end_line, hash, text, updated_at) ",
                               "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    )

    for (chunk in chunks) {
        chunk_id <- sprintf("%s:%d-%d", basename(path), chunk$start_line,
                            chunk$end_line)
        chunk_hash <- memory_hash(chunk$text)

        DBI::dbExecute(con, sql_insert_chunk, params = list(
                chunk_id, path, "claude",
                chunk$start_line, chunk$end_line,
                chunk_hash, chunk$text, now
            ))
    }

    # Update files table
    sql_insert_file <- paste0(
                              "INSERT OR REPLACE INTO files ",
                              "(path, source, hash, mtime, size, indexed_at) ",
                              "VALUES (?, ?, ?, ?, ?, ?)"
    )
    DBI::dbExecute(con, sql_insert_file,
                   params = list(path, "claude", hash, mtime, size, now))

    length(chunks)
}

#' Find all Claude Code session files
#'
#' @param base_dir Base directory (default: ~/.claude/projects)
#' @return Character vector of JSONL file paths
#' @noRd
find_claude_sessions <- function(base_dir = NULL) {
    if (is.null(base_dir)) {
        base_dir <- file.path(Sys.getenv("HOME"), ".claude", "projects")
    }
    if (!dir.exists(base_dir)) {
        return(character())
    }

    list.files(base_dir, pattern = "\\.jsonl$", recursive = TRUE,
               full.names = TRUE)
}

#' Import Claude Code history into memory index
#'
#' @param agent_id Agent identifier
#' @param base_dir Claude projects directory
#' @param verbose Print progress
#' @return List with indexed count and skipped count
#' @export
memory_import_claude <- function(agent_id = "default", base_dir = NULL,
                                 verbose = TRUE) {
    sessions <- find_claude_sessions(base_dir)
    if (length(sessions) == 0) {
        if (verbose) {
            message("No Claude Code sessions found")
        }
        return(list(indexed = 0, skipped = 0, total = 0))
    }

    if (verbose) {
        message(sprintf("Found %d Claude Code session files", length(sessions)))
    }

    con <- memory_db_open(agent_id)
    on.exit(memory_db_close(con))

    indexed <- 0
    skipped <- 0

    for (i in seq_along(sessions)) {
        path <- sessions[i]
        if (verbose && i %% 50 == 0) {
            message(sprintf("  Processing %d/%d...", i, length(sessions)))
        }

        chunks <- tryCatch(
                           memory_index_claude_session(con, path),
                           error = function(e) {
            if (verbose) message(sprintf("  Error indexing %s: %s",
                    basename(path), e$message))
            0
        }
        )

        if (chunks > 0) {
            indexed <- indexed + 1
        } else {
            skipped <- skipped + 1
        }
    }

    if (verbose) {
        message(sprintf("Indexed %d sessions (%d skipped/unchanged)", indexed,
                        skipped))
    }

    list(indexed = indexed, skipped = skipped, total = length(sessions))
}

#' Search memory using full-text search
#'
#' @param query Search query
#' @param agent_id Agent identifier
#' @param limit Maximum results
#' @param source Filter by source (NULL for all)
#' @return Data frame of matching chunks
#' @export
memory_search_fts <- function(query, agent_id = "default", limit = 20,
                              source = NULL) {
    con <- memory_db_open(agent_id, read_only = TRUE)
    on.exit(memory_db_close(con))

    # Build query with optional source filter
    source_filter <- if (!is.null(source)) {
        "AND c.source = ?"
    } else {
        ""
    }

    sql <- paste0(
                  "SELECT c.id, c.path, c.source, c.start_line, c.end_line, c.text, ",
                  "chunks_fts.rank AS score ",
                  "FROM chunks_fts ",
                  "JOIN chunks c ON c.id = chunks_fts.id ",
                  "WHERE chunks_fts MATCH ? ",
                  source_filter, " ",
                  "ORDER BY chunks_fts.rank ",
                  "LIMIT ?")

    params <- if (!is.null(source)) {
        list(query, source, limit)
    } else {
        list(query, limit)
    }

    DBI::dbGetQuery(con, sql, params = params)
}

#' Get memory index statistics
#'
#' @param agent_id Agent identifier
#' @return List with file counts, chunk counts, etc.
#' @export
memory_stats <- function(agent_id = "default") {
    con <- memory_db_open(agent_id, read_only = TRUE)
    on.exit(memory_db_close(con))

    files <- DBI::dbGetQuery(con, paste0(
            "SELECT source, COUNT(*) as count, SUM(size) as total_size ",
            "FROM files GROUP BY source"
        ))

    chunks <- DBI::dbGetQuery(con, paste0(
            "SELECT source, COUNT(*) as count ",
            "FROM chunks GROUP BY source"
        ))

    list(
         files = files,
         chunks = chunks,
         total_files = sum(files$count),
         total_chunks = sum(chunks$count)
    )
}

