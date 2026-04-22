# MCP Tool Implementations
# Actual implementations of tools exposed by the MCP server

# Shared helpers ----

tool_config <- function() {
    load_config(getwd())
}

tool_resolve_path <- function(path = ".") {
    target <- path %||% "."
    if (nchar(trimws(target)) == 0) {
        target <- "."
    }
    normalizePath(path.expand(target), mustWork = FALSE)
}

tool_check_path <- function(path, operation = "access") {
    full_path <- tool_resolve_path(path)
    validation <- validate_path(full_path, tool_config(), operation = operation)

    list(ok = validation$ok, message = validation$message, path = full_path)
}

tool_read_text <- function(path) {
    info <- file.info(path)
    size <- info$size[[1]]

    if (is.na(size) || size <= 0) {
        return("")
    }

    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    readChar(con, nchars = size, useBytes = TRUE)
}

tool_write_text <- function(path, text, append = FALSE) {
    if (isTRUE(append)) {
        mode <- "ab"
    } else {
        mode <- "wb"
    }
    con <- file(path, open = mode)
    on.exit(close(con), add = TRUE)
    writeChar(text %||% "", con, eos = NULL, useBytes = TRUE)
    invisible(TRUE)
}

format_numbered_lines <- function(lines, start = 1L) {
    if (length(lines) == 0) {
        return("")
    }

    width <- nchar(as.character(start + length(lines) - 1L))
    numbered <- sprintf(paste0("%", width, "d | %s"),
                        seq.int(start, length.out = length(lines)),
                        lines)
    paste(numbered, collapse = "\n")
}

git_run <- function(args, path = ".") {
    repo_path <- tool_resolve_path(path)
    output <- tryCatch(
                       system2("git", c("-C", repo_path, args), stdout = TRUE, stderr = TRUE),
                       error = function(e) structure(paste("Error:", e$message), status = 1L)
    )

    list(
         status = attr(output, "status") %||% 0L,
         text = paste(output, collapse = "\n")
    )
}

git_repo_available <- function(path = ".") {
    result <- git_run(c("rev-parse", "--is-inside-work-tree"), path = path)
    identical(trimws(result$text), "true") && result$status == 0L
}

# File tools ----

#' List files in a directory.
#'
#' @param path (character) Directory to inspect.
#' @param pattern (character) Regex pattern to filter file names.
#' @param recursive (logical) Recurse into subdirectories.
#' @param all_files (logical) Include hidden files.
#' @param limit (integer) Maximum number of entries to return.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_list_files <- function(path = ".", pattern = NULL, recursive = FALSE,
                            all_files = FALSE, limit = 200L) {
    checked <- tool_check_path(path %||% ".", operation = "read")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    if (!dir.exists(path)) {
        return(err(paste("Directory not found:", path)))
    }

    recursive <- isTRUE(recursive)
    all_files <- isTRUE(all_files)
    limit <- as.integer(limit %||% 200L)
    if (is.na(limit) || limit < 1) {
        limit <- 200L
    }

    entries <- list.files(
                          path = path,
                          pattern = pattern %||% NULL,
                          all.files = all_files,
                          recursive = recursive,
                          full.names = TRUE,
                          include.dirs = TRUE,
                          no.. = TRUE
    )
    entries <- sort(entries)

    if (length(entries) == 0) {
        return(ok(paste("No files found in", path)))
    }

    prefix <- if (endsWith(path, .Platform$file.sep)) path else {
        paste0(path, .Platform$file.sep)
    }

    display <- vapply(entries, function(entry) {
        rel <- if (startsWith(entry, prefix)) {
            substr(entry, nchar(prefix) + 1L, nchar(entry))
        } else {
            basename(entry)
        }
        if (dir.exists(entry)) {
            paste0(rel, "/")
        } else {
            rel
        }
    }, character(1))

    truncated <- length(display) > limit
    if (truncated) {
        display <- display[seq_len(limit)]
    }

    header <- sprintf("Directory: %s", path)
    if (truncated) {
        header <- paste0(header, sprintf("\nShowing first %d entries.", limit))
    }

    ok(paste(c(header, "", display), collapse = "\n"))
}

#' Read file contents, optionally with line numbers.
#'
#' @param path (character) Path to the file.
#' @param from (integer) Starting line number (1-based).
#' @param lines (integer) Number of lines to read.
#' @param line_numbers (logical) Prefix each line with its line number.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_read_file <- function(path, from = 1L, lines = NULL, line_numbers = TRUE) {
    checked <- tool_check_path(path, operation = "read")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    if (!file.exists(path)) {
        return(err(paste("File not found:", path)))
    }
    if (dir.exists(path)) {
        return(err(paste("Path is a directory, not a file:", path)))
    }

    lines_read <- tryCatch(readLines(path, warn = FALSE),
                           error = function(e) structure(e$message, class = "tool_read_error"))
    if (inherits(lines_read, "tool_read_error")) {
        return(err(paste("Read error:", unclass(lines_read))))
    }

    total <- length(lines_read)
    if (total == 0L) {
        return(ok(paste(c(sprintf("File: %s", path), "(empty file)"),
                        collapse = "\n")))
    }

    from <- as.integer(from %||% 1L)
    if (is.na(from) || from < 1L) from <- 1L

    count <- lines
    if (!is.null(count)) count <- as.integer(count)

    if (from > total) {
        return(ok(sprintf("File: %s\nLines: %d-%d of %d\n(no content in requested range)",
                          path, from, total, total)))
    }

    end <- if (is.null(count) || is.na(count)) {
        total
    } else {
        min(total, from + max(count, 1L) - 1L)
    }

    selected <- lines_read[from:end]
    body <- if (isFALSE(line_numbers)) {
        paste(selected, collapse = "\n")
    } else {
        format_numbered_lines(selected, start = from)
    }

    ok(paste(
             c(
                sprintf("File: %s", path),
                sprintf("Lines: %d-%d of %d", from, end, total),
                "",
                body
            ),
             collapse = "\n"
        ))
}

#' Write text to a file.
#'
#' Creates parent directories by default.
#'
#' @param path (character) Path to the file.
#' @param content (character) Text to write.
#' @param append (logical) Append instead of overwrite.
#' @param create_dirs (logical) Create parent directories if needed.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_write_file <- function(path, content, append = FALSE, create_dirs = TRUE) {
    checked <- tool_check_path(path, operation = "write")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    parent <- tool_check_path(dirname(path), operation = "write")
    if (!parent$ok) {
        return(err(parent$message))
    }

    create_dirs <- !isFALSE(create_dirs)
    if (!dir.exists(dirname(path))) {
        if (!create_dirs) {
            return(err(paste("Parent directory does not exist:", dirname(path))))
        }
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    }

    content <- content %||% ""
    append <- isTRUE(append)

    write_error <- tryCatch({
        tool_write_text(path, content, append = append)
        NULL
    }, error = function(e) e$message)
    if (!is.null(write_error)) {
        return(err(paste("Write error:", write_error)))
    }

    ok(sprintf("%s %d byte(s) to %s",
            if (append) "Appended" else "Wrote",
               nchar(content, type = "bytes"),
               path))
}

#' Replace exact text in a file without rewriting the whole file manually.
#'
#' @param path (character) Path to the file.
#' @param old_text (character) Exact text to replace.
#' @param new_text (character) Replacement text.
#' @param all (logical) Replace all matches instead of exactly one.
#' @param expected_count (integer) Fail unless this many matches are found.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_replace_in_file <- function(path, old_text, new_text, all = FALSE,
                                 expected_count = NULL) {
    checked <- tool_check_path(path, operation = "write")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    if (!file.exists(path)) {
        return(err(paste("File not found:", path)))
    }
    if (dir.exists(path)) {
        return(err(paste("Path is a directory, not a file:", path)))
    }

    old_text <- old_text %||% ""
    new_text <- new_text %||% ""
    replace_all <- isTRUE(all)

    if (nchar(old_text) == 0) {
        return(err("old_text must not be empty"))
    }

    original <- tryCatch(tool_read_text(path),
                         error = function(e) structure(e$message, class = "tool_read_error"))
    if (inherits(original, "tool_read_error")) {
        return(err(paste("Read error:", unclass(original))))
    }

    matches <- gregexpr(old_text, original, fixed = TRUE)[[1]]
    if (length(matches) == 1L && identical(matches[[1]], -1L)) {
        return(err("old_text not found"))
    }

    match_count <- length(matches)
    if (!is.null(expected_count)) {
        expected_count <- as.integer(expected_count)
        if (!is.na(expected_count) && expected_count != match_count) {
            return(err(sprintf("Expected %d match(es), found %d",
                               expected_count, match_count)))
        }
    } else if (!replace_all && match_count != 1L) {
        return(err(sprintf(
                           "old_text matched %d times; set all=TRUE or expected_count",
                           match_count
                )))
    }

    updated <- if (replace_all) {
        gsub(old_text, new_text, original, fixed = TRUE)
    } else {
        sub(old_text, new_text, original, fixed = TRUE)
    }

    write_error <- tryCatch({
        tool_write_text(path, updated, append = FALSE)
        NULL
    }, error = function(e) e$message)
    if (!is.null(write_error)) {
        return(err(paste("Write error:", write_error)))
    }

    ok(sprintf("Updated %s (%d replacement%s)",
               path,
            if (replace_all) match_count else 1L,
            if ((if (replace_all) match_count else 1L) == 1L) "" else "s"))
}

# Search ----

#' Search file contents with regex pattern.
#'
#' @param pattern (character) Regex pattern to search.
#' @param path (character) Directory to search.
#' @param file_pattern (character) File glob pattern.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_grep_files <- function(pattern, path = ".", file_pattern = "*.R") {
    checked <- tool_check_path(path %||% ".", operation = "read")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    file_pattern <- file_pattern %||% "*.R"

    files <- Sys.glob(file.path(path, file_pattern))
    if (length(files) == 0) {
        return(ok("No files to search"))
    }

    results <- character()
    for (f in files) {
        lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
        if (is.null(lines)) {
            next
        }

        hits <- grep(pattern, lines)
        if (length(hits) > 0) {
            for (i in hits) {
                results <- c(results, sprintf("%s:%d: %s", f, i, lines[i]))
            }
        }
    }

    if (length(results) == 0) {
        return(ok("No matches found"))
    }
    ok(paste(results, collapse = "\n"))
}

# Code execution ----

#' Execute R code in the session's global environment.
#'
#' New bindings are auto-captured into the workspace cache. Large
#' result values (data frames, matrices, long vectors, objects over
#' ~10 KB) are stashed via `with_handle()` and returned as a `str()`
#' summary plus a short `.h_NNN` handle the LLM can reference in a
#' later `run_r` call or inspect with `read_handle`.
#'
#' @param code (character) R code to execute.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_run_r <- function(code) {
    # Snapshot globalenv before eval for workspace auto-capture
    before <- ls(globalenv())

    # Active handles are visible in `code` as regular R names.
    eval_env <- handle_eval_env(parent = globalenv())

    # Evaluate in a two-step dance: get the withVisible() result first
    # (so we have the value, not just its printed representation), then
    # separately capture what the print of that value would look like.
    # Using <<- inside nested capture.output/tryCatch frames is fragile.
    outcome <- tryCatch({
        r <- withVisible(eval(parse(text = code), envir = eval_env))
        printed <- if (isTRUE(r$visible)) {
            utils::capture.output(print(r$value))
        } else {
            character(0)
        }
        list(ok = TRUE, value = r$value, visible = r$visible,
             printed = paste(printed, collapse = "\n"))
    }, error = function(e) {
        list(ok = FALSE, message = paste("Error:", e$message))
    })

    if (!isTRUE(outcome$ok)) {
        return(ok(outcome$message))
    }

    # Large visible results get stashed as handles so the LLM sees a
    # summary instead of the full print.
    text <- if (isTRUE(outcome$visible) && .is_large_result(outcome$value)) {
        stashed <- with_handle(outcome$value)
        sprintf("%s\n\n[stored as %s]", stashed$summary, stashed$handle)
    } else {
        outcome$printed
    }

    # Auto-capture new globalenv bindings into the workspace. Variables
    # assigned inside eval_env don't leak to globalenv, so this only
    # fires when the user explicitly uses `<<-` or `assign()`.
    new_names <- setdiff(ls(globalenv()), before)
    origin <- list(tool = "run_r", args = list(code = code))
    for (nm in new_names) {
        val <- get(nm, envir = globalenv())
        if (object.size(val) < 10e6) {
            deps <- tryCatch({
                fn <- eval(parse(text = paste0("function() {", code, "}")))
                referenced <- codetools::findGlobals(fn)
                intersect(referenced, ws_names())
            }, error = function(e) character())
            ws_put(nm, val, origin = origin, deps = deps)
        }
    }

    ok(text)
}

#' Execute R code in a clean subprocess via littler.
#'
#' Use for scripts that modify packages, run tests, or need isolation
#' from the server.
#'
#' @param code (character) R code to execute.
#' @param timeout (integer) Timeout in seconds.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_run_r_script <- function(code, timeout = 30L) {
    timeout <- timeout %||% 30L

    # Write code to temp file (avoids shell escaping issues)
    tmp <- tempfile(fileext = ".R")
    on.exit(unlink(tmp))
    writeLines(code, tmp)

    result <- tryCatch({
        out <- system2("r", c("-f", tmp), stdout = TRUE, stderr = TRUE,
                       timeout = timeout)
        paste(out, collapse = "\n")
    }, error = function(e) {
        paste("Error:", e$message)
    })
    ok(result)
}

#' Run a bash shell command.
#'
#' Use background=true for long-running servers or processes.
#'
#' @param command (character) Shell command to execute.
#' @param timeout (integer) Timeout in seconds.
#' @param background (logical) Run in background and return immediately.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_bash <- function(command, timeout = 30L, background = FALSE) {
    tool_shell_impl(
        list(command = command, timeout = timeout, background = background),
        "bash"
    )
}

#' Run a Windows cmd.exe command.
#'
#' Use background=true for long-running processes.
#'
#' @param command (character) cmd.exe command to execute.
#' @param timeout (integer) Timeout in seconds.
#' @param background (logical) Run in background and return immediately.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_cmd <- function(command, timeout = 30L, background = FALSE) {
    tool_shell_impl(
        list(command = command, timeout = timeout, background = background),
        "cmd"
    )
}

# Resolve bash to an explicit path on Windows. Without this, PATH order
# often picks up C:\Windows\System32\bash.exe (the WSL launcher stub),
# which fails for users without a provisioned WSL distro. Prefer Rtools
# first (the likely install for anyone building R packages), then Git
# for Windows, then plain "bash" as a last-resort PATH lookup.
.find_bash_exe <- function() {
    if (.Platform$OS.type != "windows") return("bash")
    rtools_home <- Sys.getenv("RTOOLS45_HOME",
                              Sys.getenv("RTOOLS44_HOME", ""))
    candidates <- c(
                    if (nzchar(rtools_home)) file.path(rtools_home, "usr", "bin", "bash.exe"),
                    "C:/rtools45/usr/bin/bash.exe",
                    "C:/rtools44/usr/bin/bash.exe",
                    "C:/Program Files/Git/bin/bash.exe",
                    "C:/Program Files (x86)/Git/bin/bash.exe"
    )
    for (p in candidates) {
        if (file.exists(p)) return(p)
    }
    "bash"
}

# Unified shell handler. shell_name is "bash" (Unix/Windows with Rtools)
# or "cmd" (Windows fallback). Windows bash is resolved to an absolute
# path to avoid picking up the WSL launcher stub in System32.
tool_shell_impl <- function(args, shell_name) {
    cmd <- args$command
    timeout <- args$timeout %||% 30
    background <- isTRUE(args$background)
    command_check <- validate_command(cmd)

    if (!command_check$ok) {
        return(err(command_check$message))
    }

    shell_exe <- switch(
                        shell_name,
                        bash = .find_bash_exe(),
                        cmd = "cmd",
                        stop(sprintf("Unknown shell %s", shell_name), call. = FALSE)
    )

    exe_args <- switch(
                       shell_name,
                       bash = c("-lc", cmd),
                       cmd = c("/c", cmd)
    )

    if (background) {
        proc <- processx::process$new(
                                      shell_exe, exe_args,
                                      stdout = "|", stderr = "|", cleanup_tree = TRUE
        )
        id <- bg_register(cmd, proc)
        return(ok(sprintf(
                          "Started background process [%s] (pid %d)\nCheck with: bg_status tool",
                          id, proc$get_pid()
                )))
    }

    # Windows cmd.exe does not need shQuote and doesn't understand -lc.
    exe_args_fg <- switch(
                          shell_name,
                          bash = c("-lc", shQuote(cmd)),
                          cmd = c("/c", cmd)
    )

    result <- tryCatch({
        out <- system2(shell_exe, exe_args_fg, stdout = TRUE,
                       stderr = TRUE, timeout = timeout)
        paste(out, collapse = "\n")
    }, error = function(e) {
        paste("Error:", e$message)
    })
    ok(result)
}

# Background process registry ----

.bg_processes <- new.env(parent = emptyenv())

bg_register <- function(cmd, proc) {
    id <- sprintf("bg_%d", length(ls(.bg_processes)) + 1L)
    .bg_processes[[id]] <- list(
                                id = id,
                                command = substr(cmd, 1, 80),
                                process = proc,
                                started = Sys.time()
    )
    id
}

#' Check status and output of background processes.
#'
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_bg_status <- function() {
    ids <- ls(.bg_processes)
    if (length(ids) == 0) {
        return(ok("No background processes."))
    }

    lines <- vapply(ids, function(id) {
        entry <- .bg_processes[[id]]
        proc <- entry$process
        alive <- proc$is_alive()
        status <- if (alive) "running" else paste("exited",
            proc$get_exit_status())
        elapsed <- round(as.numeric(difftime(Sys.time(), entry$started,
                    units = "secs")))

        # Read available output
        out <- ""
        if (!alive) {
            out <- tryCatch(proc$read_all_output(), error = function(e) "")
            err_out <- tryCatch(proc$read_all_error(), error = function(e) "")
            if (nchar(err_out) > 0) out <- paste(out, err_out, sep = "\n")
        } else {
            out <- tryCatch(proc$read_output(), error = function(e) "")
        }

        tail_out <- if (nchar(out) > 500) {
            paste0("...\n", substr(out, nchar(out) - 499, nchar(out)))
        } else {
            out
        }

        sprintf("[%s] %s | %s | %ds | pid %d%s",
                id, entry$command, status, elapsed, proc$get_pid(),
            if (nchar(tail_out) > 0) paste0("\n", tail_out) else "")
    }, character(1))

    ok(paste(lines, collapse = "\n\n"))
}

#' Kill a background process by id.
#'
#' @param id (character) Process id (e.g. bg_1).
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_bg_kill <- function(id) {
    if (!exists(id, envir = .bg_processes, inherits = FALSE)) {
        return(err(sprintf("No background process with id '%s'", id)))
    }
    entry <- .bg_processes[[id]]
    if (entry$process$is_alive()) {
        entry$process$kill_tree()
        ok(sprintf("Killed process [%s] (pid %d)", id, entry$process$get_pid()))
    } else {
        ok(sprintf("Process [%s] already exited with status %d",
                   id, entry$process$get_exit_status()))
    }
}

# R-specific ----

#' Get R package documentation via saber (exports, function help).
#'
#' @param topic (character) Package or function name.
#' @param package (character) Package to search in (optional).
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_r_help <- function(topic, package = NULL) {
    pkg <- package

    # Accept pkg::fn notation in the topic as a convenience
    if (is.null(pkg) && grepl("::", topic, fixed = TRUE)) {
        parts <- strsplit(topic, "::", fixed = TRUE)[[1]]
        pkg <- parts[1]
        topic <- parts[2]
    }

    tryCatch({
        # Bare package name: return the exports table
        if (is.null(pkg) && topic %in% rownames(installed.packages())) {
            out <- capture.output(print(saber::pkg_exports(topic)))
            return(ok(paste(out, collapse = "\n")))
        }

        # Function: resolve its package if not given
        if (is.null(pkg)) {
            for (e in search()) {
                if (exists(topic, where = e, mode = "function")) {
                    pkg <- sub("^package:", "", e)
                    break
                }
            }
        }

        if (is.null(pkg) || pkg == ".GlobalEnv") {
            return(err(paste("Could not find package for:", topic)))
        }

        md <- saber::pkg_help(topic, pkg)
        ok(md)
    }, error = function(e) {
        err(paste("Help error:", e$message))
    })
}

#' List installed R packages, optionally filtered by name.
#'
#' @param pattern (character) Case-insensitive package-name filter.
#' @param limit (integer) Maximum number of packages to return.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_installed_packages <- function(pattern = NULL, limit = 100L) {
    limit <- as.integer(limit %||% 100L)
    if (is.na(limit) || limit < 1L) {
        limit <- 100L
    }

    pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")],
                          stringsAsFactors = FALSE)
    pkgs <- pkgs[order(pkgs$Package),, drop = FALSE]

    if (!is.null(pattern) && nchar(pattern) > 0) {
        keep <- grepl(pattern, pkgs$Package, ignore.case = TRUE)
        pkgs <- pkgs[keep,, drop = FALSE]
    }

    if (nrow(pkgs) == 0) {
        return(ok("No installed packages matched."))
    }

    truncated <- nrow(pkgs) > limit
    shown <- head(pkgs, limit)
    body <- sprintf("%-30s %s", shown$Package, shown$Version)

    header <- sprintf("Installed packages: %d match(es)", nrow(pkgs))
    if (truncated) {
        header <- paste0(header, sprintf(" (showing first %d)", limit))
    }

    ok(paste(c(header, "", body), collapse = "\n"))
}

# Web ----

#' Search the web using Tavily API.
#'
#' @param query (character) Search query.
#' @param max_results (integer) Max results to return.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_web_search <- function(query, max_results = 5L) {
    max_results <- max_results %||% 5L

    api_key <- Sys.getenv("TAVILY_API_KEY")
    if (nchar(api_key) == 0) {
        return(err("TAVILY_API_KEY not set in .Renviron"))
    }

    tryCatch({
        body <- list(
                     api_key = api_key,
                     query = query,
                     max_results = max_results,
                     include_answer = TRUE
        )

        h <- curl::new_handle()
        curl::handle_setopt(h,
                            customrequest = "POST",
                            postfields = jsonlite::toJSON(body, auto_unbox = TRUE)
        )
        curl::handle_setheaders(h, "Content-Type" = "application/json")

        resp <- curl::curl_fetch_memory("https://api.tavily.com/search",
                                        handle = h)

        if (resp$status_code >= 400) {
            return(err(paste("Tavily API error:", resp$status_code)))
        }

        data <- jsonlite::fromJSON(rawToChar(resp$content),
                                   simplifyVector = FALSE)

        # Format results
        parts <- character()

        # Include AI-generated answer if available
        if (!is.null(data$answer) && nchar(data$answer) > 0) {
            parts <- c(parts, "Answer:", data$answer, "")
        }

        parts <- c(parts, "Results:")
        for (r in data$results) {
            parts <- c(parts, sprintf("- %s", r$title))
            parts <- c(parts, sprintf("  %s", r$url))
            if (!is.null(r$content)) {
                snippet <- substr(r$content, 1, 200)
                if (nchar(r$content) > 200) snippet <- paste0(snippet, "...")
                parts <- c(parts, sprintf("  %s", snippet))
            }
            parts <- c(parts, "")
        }

        ok(paste(parts, collapse = "\n"))
    }, error = function(e) {
        err(paste("Search error:", e$message))
    })
}

#' Fetch the contents of a URL and return the response body.
#'
#' @param url (character) URL to fetch.
#' @param max_chars (integer) Maximum number of characters to return.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_fetch_url <- function(url, max_chars = 8000L) {
    max_chars <- as.integer(max_chars %||% 8000L)
    if (is.na(max_chars) || max_chars < 1L) {
        max_chars <- 8000L
    }

    tryCatch({
        h <- curl::new_handle()
        curl::handle_setopt(h, followlocation = TRUE)
        resp <- curl::curl_fetch_memory(url, handle = h)

        text <- tryCatch(rawToChar(resp$content),
                         error = function(e) paste(resp$content, collapse = " "))
        if (nchar(text) > max_chars) {
            text <- paste0(substr(text, 1, max_chars),
                           "\n[truncated by max_chars]")
        }

        ok(paste(
                 c(
                    sprintf("URL: %s", url),
                    sprintf("Status: %d", resp$status_code),
                    "",
                    text
                ),
                 collapse = "\n"
            ))
    }, error = function(e) {
        err(paste("Fetch error:", e$message))
    })
}

# Git ----

#' Show git working tree status.
#'
#' @param path (character) Repository path.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_git_status <- function(path = ".") {
    repo_path <- path %||% "."
    if (!git_repo_available(repo_path)) {
        return(err("Not inside a git repository"))
    }

    result <- git_run(c("status", "--short", "--branch"), path = repo_path)
    if (result$status != 0L) {
        return(err(result$text))
    }

    ok(result$text)
}

#' Show git diff for the current repository.
#'
#' @param ref (character) Diff against this ref.
#' @param path (character) Repository path or file path filter when combined with file_path.
#' @param file_path (character) Optional file path filter within the repository.
#' @param staged (logical) Diff staged changes instead of the worktree.
#' @param context_lines (integer) Number of context lines around changes.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_git_diff <- function(ref = "HEAD", path = ".", file_path = "",
                          staged = FALSE, context_lines = 3L) {
    repo_path <- path %||% "."
    if (!git_repo_available(repo_path)) {
        return(err("Not inside a git repository"))
    }

    ref <- trimws(ref %||% "HEAD")
    file_path <- trimws(file_path %||% "")
    staged <- isTRUE(staged)
    context_lines <- as.integer(context_lines %||% 3L)
    if (is.na(context_lines) || context_lines < 0L) {
        context_lines <- 3L
    }

    cmd <- c("diff", "--no-ext-diff", "--find-renames",
             sprintf("--unified=%d", context_lines))
    if (staged) {
        cmd <- c(cmd, "--cached")
    }
    if (nchar(ref) > 0) {
        cmd <- c(cmd, ref)
    }
    if (nchar(file_path) > 0) {
        cmd <- c(cmd, "--", file_path)
    }

    result <- git_run(cmd, path = repo_path)
    if (result$status != 0L) {
        return(err(result$text))
    }
    if (nchar(trimws(result$text)) == 0) {
        return(ok("No diff."))
    }

    ok(result$text)
}

#' Show recent git commits.
#'
#' @param n (integer) Number of commits to return.
#' @param ref (character) Optional ref to log from.
#' @param path (character) Repository path.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_git_log <- function(n = 10L, ref = "HEAD", path = ".") {
    repo_path <- path %||% "."
    if (!git_repo_available(repo_path)) {
        return(err("Not inside a git repository"))
    }

    n <- as.integer(n %||% 10L)
    if (is.na(n) || n < 1L) {
        n <- 10L
    }
    ref <- trimws(ref %||% "HEAD")

    cmd <- c("log", "--oneline", "--decorate", sprintf("-n%d", n))
    if (nchar(ref) > 0) {
        cmd <- c(cmd, ref)
    }

    result <- git_run(cmd, path = repo_path)
    if (result$status != 0L) {
        return(err(result$text))
    }

    ok(result$text)
}

# Subagent tools ----

#' Spawn a specialized subagent for a task.
#'
#' Use for parallel work or tasks requiring focused attention. Note that
#' parent_session is not wired through the skill handler and defaults to
#' NULL; the legacy registration forwarded ctx$session here.
#'
#' @param task (character) Task description for the subagent.
#' @param model (character) Optional model override.
#' @param tools (character vector) Optional tool filter (list of tool names).
#' @param parent_session (list) Parent session object (typically NULL when
#'   invoked as a skill).
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_spawn_subagent <- function(task, model = NULL, tools = NULL,
                                parent_session = NULL) {
    tryCatch({
        id <- subagent_spawn(
                             task = task,
                             model = model,
                             tools = tools,
                             parent_session = parent_session
        )
        ok(sprintf("Spawned subagent %s for: %s", id, task))
    }, error = function(e) {
        err(paste("Spawn failed:", e$message))
    })
}

#' Send a prompt to a running subagent and get the response.
#'
#' @param id (character) Subagent ID.
#' @param prompt (character) Prompt to send.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_query_subagent <- function(id, prompt) {
    tryCatch({
        result <- subagent_query(id, prompt)
        ok(result)
    }, error = function(e) {
        err(paste("Query failed:", e$message))
    })
}

#' List all active subagents.
#'
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_list_subagents <- function() {
    agents <- subagent_list()
    ok(format_subagent_list(agents))
}

#' Terminate a running subagent.
#'
#' @param id (character) Subagent ID to terminate.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_kill_subagent <- function(id) {
    success <- subagent_kill(id)
    if (success) {
        ok(sprintf("Subagent %s terminated", id))
    } else {
        err(sprintf("Subagent not found: %s", id))
    }
}

# Skill Registration ----

#' Register all built-in skills
#'
#' Creates skill specs for all built-in tools and registers them.
#' Called on package load.
#'
#' @return Invisible character vector of registered skill names
#' @noRd
register_builtin_skills <- function() {
    # File tools
    register_skill_from_fn("read_file", tool_read_file)
    register_skill_from_fn("write_file", tool_write_file)
    register_skill_from_fn("replace_in_file", tool_replace_in_file)
    register_skill_from_fn("list_files", tool_list_files)

    # Search
    register_skill_from_fn("grep_files", tool_grep_files)

    # Code execution
    register_skill_from_fn("run_r", tool_run_r)
    register_skill_from_fn("read_handle", tool_read_handle)
    register_skill_from_fn("run_r_script", tool_run_r_script)

    # Shell tool: prefer bash everywhere for cross-OS consistency. On
    # Windows we register bash only if we can find a real bash (Rtools
    # or Git for Windows); otherwise fall back to cmd so minimal-install
    # Windows users still have a working shell tool.
    use_bash <- .Platform$OS.type != "windows" ||
        file.exists(.find_bash_exe())
    if (use_bash) {
        register_skill_from_fn("bash", tool_bash)
    } else {
        register_skill_from_fn("cmd", tool_cmd)
    }

    # Background process management
    register_skill_from_fn("bg_status", tool_bg_status)
    register_skill_from_fn("bg_kill", tool_bg_kill)

    # R-specific
    register_skill_from_fn("r_help", tool_r_help)
    register_skill_from_fn("installed_packages", tool_installed_packages)

    # Web
    register_skill_from_fn("web_search", tool_web_search)
    register_skill_from_fn("fetch_url", tool_fetch_url)

    # Git
    register_skill_from_fn("git_status", tool_git_status)
    register_skill_from_fn("git_diff", tool_git_diff)
    register_skill_from_fn("git_log", tool_git_log)

    # Subagent tools
    register_skill_from_fn("spawn_subagent", tool_spawn_subagent)
    register_skill_from_fn("query_subagent", tool_query_subagent)
    register_skill_from_fn("list_subagents", tool_list_subagents)
    register_skill_from_fn("kill_subagent", tool_kill_subagent)

    invisible(list_skills())
}

# Dispatcher ----

#' Call a tool by name
#'
#' Delegates to the skill system. Falls back to legacy dispatch if skill not found.
#'
#' @param name Tool name
#' @param args List of arguments
#' @param ctx Optional context (cwd, session, etc.)
#' @param timeout Timeout in seconds (default 30)
#' @param dry_run If TRUE, validate only without executing
#' @return MCP tool result
#' @noRd
call_tool <- function(name, args, ctx = list(), timeout = 30L,
                      dry_run = FALSE) {
    args <- args %||% list()

    # Try skill system first
    skill <- get_skill(name)
    if (!is.null(skill)) {
        return(skill_run(skill, args, ctx, timeout, dry_run))
    }

    # Fallback: unknown tool
    err(paste("Unknown tool:", name))
}

