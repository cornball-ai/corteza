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

tool_list_files <- function(args) {
    checked <- tool_check_path(args$path %||% ".", operation = "read")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    if (!dir.exists(path)) {
        return(err(paste("Directory not found:", path)))
    }

    pattern <- args$pattern
    recursive <- isTRUE(args$recursive)
    all_files <- isTRUE(args$all_files)
    limit <- as.integer(args$limit %||% 200L)
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

tool_read_file <- function(args) {
    checked <- tool_check_path(args$path, operation = "read")
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

    lines <- tryCatch(readLines(path, warn = FALSE),
                      error = function(e) structure(e$message, class = "tool_read_error"))
    if (inherits(lines, "tool_read_error")) {
        return(err(paste("Read error:", unclass(lines))))
    }

    total <- length(lines)
    if (total == 0) {
        return(ok(paste(c(sprintf("File: %s", path), "(empty file)"),
                        collapse = "\n")))
    }

    from <- as.integer(args$from %||% 1L)
    if (is.na(from) || from < 1L) {
        from <- 1L
    }

    count <- args$lines
    if (!is.null(count)) {
        count <- as.integer(count)
    }

    if (from > total) {
        return(ok(sprintf("File: %s\nLines: %d-%d of %d\n(no content in requested range)",
                          path, from, total, total)))
    }

    end <- if (is.null(count) || is.na(count)) {
        total
    } else {
        min(total, from + max(count, 1L) - 1L)
    }

    selected <- lines[from:end]
    body <- if (isFALSE(args$line_numbers)) {
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

tool_write_file <- function(args) {
    checked <- tool_check_path(args$path, operation = "write")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    parent <- tool_check_path(dirname(path), operation = "write")
    if (!parent$ok) {
        return(err(parent$message))
    }

    create_dirs <- !isFALSE(args$create_dirs)
    if (!dir.exists(dirname(path))) {
        if (!create_dirs) {
            return(err(paste("Parent directory does not exist:", dirname(path))))
        }
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    }

    content <- args$content %||% ""
    append <- isTRUE(args$append)

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

tool_replace_in_file <- function(args) {
    checked <- tool_check_path(args$path, operation = "write")
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

    old_text <- args$old_text %||% ""
    new_text <- args$new_text %||% ""
    replace_all <- isTRUE(args$all)

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
    expected_count <- args$expected_count
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

tool_grep_files <- function(args) {
    pattern <- args$pattern
    checked <- tool_check_path(args$path %||% ".", operation = "read")
    if (!checked$ok) {
        return(err(checked$message))
    }

    path <- checked$path
    file_pattern <- args$file_pattern %||% "*.R"

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

tool_run_r <- function(args) {
    code <- args$code

    # Snapshot globalenv before eval for workspace auto-capture
    before <- ls(globalenv())

    result <- tryCatch({
        out <- capture.output(eval(parse(text = code), envir = globalenv()))
        paste(out, collapse = "\n")
    }, error = function(e) {
        paste("Error:", e$message)
    })

    # Auto-capture new bindings into workspace
    new_names <- setdiff(ls(globalenv()), before)
    origin <- list(tool = "run_r", args = args)
    for (nm in new_names) {
        val <- get(nm, envir = globalenv())
        if (object.size(val) < 10e6) {
            # Detect dependencies via codetools (best-effort)
            deps <- tryCatch({
                fn <- eval(parse(text = paste0("function() {", code, "}")))
                referenced <- codetools::findGlobals(fn)
                intersect(referenced, ws_names())
            }, error = function(e) character())
            ws_put(nm, val, origin = origin, deps = deps)
        }
    }

    ok(result)
}

tool_run_r_script <- function(args) {
    code <- args$code
    timeout <- args$timeout %||% 30L

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

tool_bash <- function(args) {
    tool_shell_impl(args, "bash")
}

tool_cmd <- function(args) {
    tool_shell_impl(args, "cmd")
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

tool_bg_status <- function(args) {
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

tool_bg_kill <- function(args) {
    id <- args$id
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

tool_r_help <- function(args) {
    topic <- args$topic
    pkg <- args$package

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

tool_installed_packages <- function(args) {
    pattern <- args$pattern
    limit <- as.integer(args$limit %||% 100L)
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

tool_web_search <- function(args) {
    query <- args$query
    max_results <- args$max_results %||% 5L

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

tool_fetch_url <- function(args) {
    url <- args$url
    max_chars <- as.integer(args$max_chars %||% 8000L)
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

tool_git_status <- function(args) {
    repo_path <- args$path %||% "."
    if (!git_repo_available(repo_path)) {
        return(err("Not inside a git repository"))
    }

    result <- git_run(c("status", "--short", "--branch"), path = repo_path)
    if (result$status != 0L) {
        return(err(result$text))
    }

    ok(result$text)
}

tool_git_diff <- function(args) {
    repo_path <- args$path %||% "."
    if (!git_repo_available(repo_path)) {
        return(err("Not inside a git repository"))
    }

    ref <- trimws(args$ref %||% "HEAD")
    file_path <- trimws(args$file_path %||% "")
    staged <- isTRUE(args$staged)
    context_lines <- as.integer(args$context_lines %||% 3L)
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

tool_git_log <- function(args) {
    repo_path <- args$path %||% "."
    if (!git_repo_available(repo_path)) {
        return(err("Not inside a git repository"))
    }

    n <- as.integer(args$n %||% 10L)
    if (is.na(n) || n < 1L) {
        n <- 10L
    }
    ref <- trimws(args$ref %||% "HEAD")

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
    register_skill(skill_spec(
                              name = "read_file",
                              description = "Read a text file with optional line ranges and line numbers.",
                              params = list(
                path = list(type = "string",
                            description = "Path to the file",
                            required = TRUE),
                from = list(type = "integer",
                            description = "Start line number (1-based)"),
                lines = list(type = "integer",
                             description = "Number of lines to return"),
                line_numbers = list(type = "boolean",
                                    description = "Include line numbers in the output (default: true)")
            ),
                              handler = function(args, ctx) tool_read_file(args)
        ))

    register_skill(skill_spec(
                              name = "write_file",
                              description = "Write text to a file. Creates parent directories by default.",
                              params = list(
                path = list(type = "string",
                            description = "Path to the file",
                            required = TRUE),
                content = list(type = "string",
                               description = "Text to write",
                               required = TRUE),
                append = list(type = "boolean",
                              description = "Append instead of overwrite"),
                create_dirs = list(type = "boolean",
                                   description = "Create parent directories if needed (default: true)")
            ),
                              handler = function(args, ctx) tool_write_file(args)
        ))

    register_skill(skill_spec(
                              name = "replace_in_file",
                              description = "Replace exact text in a file without rewriting the whole file manually.",
                              params = list(
                path = list(type = "string",
                            description = "Path to the file",
                            required = TRUE),
                old_text = list(type = "string",
                                description = "Exact text to replace",
                                required = TRUE),
                new_text = list(type = "string",
                                description = "Replacement text",
                                required = TRUE),
                all = list(type = "boolean",
                           description = "Replace all matches instead of exactly one"),
                expected_count = list(type = "integer",
                                      description = "Fail unless this many matches are found")
            ),
                              handler = function(args, ctx) tool_replace_in_file(args)
        ))

    register_skill(skill_spec(
                              name = "list_files",
                              description = "List files in a directory.",
                              params = list(
                path = list(type = "string",
                            description = "Directory to inspect"),
                pattern = list(type = "string",
                               description = "Regex pattern to filter file names"),
                recursive = list(type = "boolean",
                                 description = "Recurse into subdirectories"),
                all_files = list(type = "boolean",
                                 description = "Include hidden files"),
                limit = list(type = "integer",
                             description = "Maximum number of entries to return")
            ),
                              handler = function(args, ctx) tool_list_files(args)
        ))

    # Search
    register_skill(skill_spec(
                              name = "grep_files",
                              description = "Search file contents with regex pattern",
                              params = list(
                pattern = list(type = "string",
                               description = "Regex pattern to search",
                               required = TRUE),
                path = list(type = "string",
                            description = "Directory to search (default: .)"),
                file_pattern = list(type = "string",
                                    description = "File glob pattern (default: *.R)")
            ),
                              handler = function(args, ctx) tool_grep_files(args)
        ))

    # Code execution
    register_skill(skill_spec(
                              name = "run_r",
                              description = "Execute R code and return result",
                              params = list(
                code = list(type = "string",
                            description = "R code to execute", required = TRUE)
            ),
                              handler = function(args, ctx) tool_run_r(args)
        ))

    register_skill(skill_spec(
                              name = "run_r_script",
                              description = "Execute R code in a clean subprocess via littler. Use for scripts that modify packages, run tests, or need isolation from the server.",
                              params = list(
                code = list(type = "string",
                            description = "R code to execute", required = TRUE),
                timeout = list(type = "integer",
                               description = "Timeout in seconds (default: 30)")
            ),
                              handler = function(args, ctx) tool_run_r_script(args)
        ))

    # Shell tool: prefer bash everywhere for cross-OS consistency. On
    # Windows we register bash only if we can find a real bash (Rtools
    # or Git for Windows); otherwise fall back to cmd so minimal-install
    # Windows users still have a working shell tool.
    use_bash <- .Platform$OS.type != "windows" ||
        file.exists(.find_bash_exe())
    if (use_bash) {
        register_skill(skill_spec(
                                  name = "bash",
                                  description = "Run a bash shell command. Use background=true for long-running servers or processes.",
                                  params = list(
                    command = list(type = "string",
                                   description = "Shell command to execute",
                                   required = TRUE),
                    timeout = list(type = "integer",
                                   description = "Timeout in seconds (default: 30)"),
                    background = list(type = "boolean",
                                      description = "Run in background and return immediately (default: false)")
                ),
                                  handler = function(args, ctx) tool_bash(args)
            ))
    } else {
        register_skill(skill_spec(
                                  name = "cmd",
                                  description = "Run a Windows cmd.exe command. Use background=true for long-running processes.",
                                  params = list(
                    command = list(type = "string",
                                   description = "cmd.exe command to execute",
                                   required = TRUE),
                    timeout = list(type = "integer",
                                   description = "Timeout in seconds (default: 30)"),
                    background = list(type = "boolean",
                                      description = "Run in background and return immediately (default: false)")
                ),
                                  handler = function(args, ctx) tool_cmd(args)
            ))
    }

    # Background process management
    register_skill(skill_spec(
                              name = "bg_status",
                              description = "Check status and output of background processes",
                              params = list(),
                              handler = function(args, ctx) tool_bg_status(args)
        ))

    register_skill(skill_spec(
                              name = "bg_kill",
                              description = "Kill a background process by id",
                              params = list(
                id = list(type = "string",
                          description = "Process id (e.g. bg_1)",
                          required = TRUE)
            ),
                              handler = function(args, ctx) tool_bg_kill(args)
        ))

    # R-specific
    register_skill(skill_spec(
                              name = "r_help",
                              description = "Get R package documentation via saber (exports, function help)",
                              params = list(
                topic = list(type = "string",
                             description = "Package or function name",
                             required = TRUE),
                package = list(type = "string",
                               description = "Package to search in (optional)")
            ),
                              handler = function(args, ctx) tool_r_help(args)
        ))

    register_skill(skill_spec(
                              name = "installed_packages",
                              description = "List installed R packages, optionally filtered by name.",
                              params = list(
                pattern = list(type = "string",
                               description = "Case-insensitive package-name filter"),
                limit = list(type = "integer",
                             description = "Maximum number of packages to return")
            ),
                              handler = function(args, ctx) tool_installed_packages(args)
        ))

    # Web
    register_skill(skill_spec(
                              name = "web_search",
                              description = "Search the web using Tavily API",
                              params = list(
                query = list(type = "string", description = "Search query",
                             required = TRUE),
                max_results = list(type = "integer",
                                   description = "Max results to return (default: 5)")
            ),
                              handler = function(args, ctx) tool_web_search(args)
        ))

    register_skill(skill_spec(
                              name = "fetch_url",
                              description = "Fetch the contents of a URL and return the response body.",
                              params = list(
                url = list(type = "string",
                           description = "URL to fetch",
                           required = TRUE),
                max_chars = list(type = "integer",
                                 description = "Maximum number of characters to return")
            ),
                              handler = function(args, ctx) tool_fetch_url(args)
        ))

    # Git
    register_skill(skill_spec(
                              name = "git_status",
                              description = "Show git working tree status.",
                              params = list(
                path = list(type = "string",
                            description = "Repository path (default: current directory)")
            ),
                              handler = function(args, ctx) tool_git_status(args)
        ))

    register_skill(skill_spec(
                              name = "git_diff",
                              description = "Show git diff for the current repository.",
                              params = list(
                ref = list(type = "string",
                           description = "Diff against this ref (default: HEAD)"),
                path = list(type = "string",
                            description = "Repository path or file path filter when combined with file_path"),
                file_path = list(type = "string",
                                 description = "Optional file path filter within the repository"),
                staged = list(type = "boolean",
                              description = "Diff staged changes instead of the worktree"),
                context_lines = list(type = "integer",
                                     description = "Number of context lines around changes")
            ),
                              handler = function(args, ctx) tool_git_diff(args)
        ))

    register_skill(skill_spec(
                              name = "git_log",
                              description = "Show recent git commits.",
                              params = list(
                n = list(type = "integer",
                         description = "Number of commits to return"),
                ref = list(type = "string",
                           description = "Optional ref to log from (default: HEAD)"),
                path = list(type = "string",
                            description = "Repository path (default: current directory)")
            ),
                              handler = function(args, ctx) tool_git_log(args)
        ))

    # Subagent tools
    register_skill(skill_spec(
                              name = "spawn_subagent",
                              description = "Spawn a specialized subagent for a task. Use for parallel work or tasks requiring focused attention.",
                              params = list(
                task = list(type = "string",
                            description = "Task description for the subagent",
                            required = TRUE),
                model = list(type = "string",
                             description = "Optional model override"),
                tools = list(type = "array",
                             items = list(type = "string"),
                             description = "Optional tool filter (list of tool names)")
            ),
                              handler = function(args, ctx) {
        tryCatch({
            id <- subagent_spawn(
                                 task = args$task,
                                 model = args$model,
                                 tools = args$tools,
                                 parent_session = ctx$session
            )
            ok(sprintf("Spawned subagent %s for: %s", id, args$task))
        }, error = function(e) {
            err(paste("Spawn failed:", e$message))
        })
    }
        ))

    register_skill(skill_spec(
                              name = "query_subagent",
                              description = "Send a prompt to a running subagent and get the response.",
                              params = list(
                id = list(type = "string", description = "Subagent ID",
                          required = TRUE),
                prompt = list(type = "string", description = "Prompt to send",
                              required = TRUE)
            ),
                              handler = function(args, ctx) {
        tryCatch({
            result <- subagent_query(args$id, args$prompt)
            ok(result)
        }, error = function(e) {
            err(paste("Query failed:", e$message))
        })
    }
        ))

    register_skill(skill_spec(
                              name = "list_subagents",
                              description = "List all active subagents.",
                              params = list(),
                              handler = function(args, ctx) {
        agents <- subagent_list()
        ok(format_subagent_list(agents))
    }
        ))

    register_skill(skill_spec(
                              name = "kill_subagent",
                              description = "Terminate a running subagent.",
                              params = list(
                id = list(type = "string",
                          description = "Subagent ID to terminate",
                          required = TRUE)
            ),
                              handler = function(args, ctx) {
        success <- subagent_kill(args$id)
        if (success) {
            ok(sprintf("Subagent %s terminated", args$id))
        } else {
            err(sprintf("Subagent not found: %s", args$id))
        }
    }
        ))

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

