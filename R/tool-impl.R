# MCP Tool Implementations
# Actual implementations of tools exposed by the MCP server

# Shared helpers ----

tool_config <- function() {
    config <- load_config(getwd())
    # Process-level path confinement, used to pin a subagent to its
    # project (see PRESET_ALLOWED_PATHS in R/subagent.R). Each subagent
    # is its own R process, so the option cannot leak into the parent.
    # This can only ever tighten: validate_path() treats a NULL
    # allowed_paths as unrestricted, so setting one is strictly a
    # narrowing. denied_paths is untouched either way.
    confined <- getOption("corteza.allowed_paths", NULL)
    if (!is.null(confined)) {
        config$allowed_paths <- confined
    }
    config
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

# The subset of `paths` the session may read, each judged by where it
# resolves (symlinks followed). The config is loaded once for the batch.
tool_readable_paths <- function(paths) {
    if (!length(paths)) {
        return(paths)
    }
    cfg <- tool_config()
    keep <- vapply(paths, function(p) {
        isTRUE(validate_path(tool_resolve_path(p), cfg, operation = "read")$ok)
    }, logical(1))
    paths[keep]
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
                        seq.int(start, length.out = length(lines)), lines)
    paste(numbered, collapse = "\n")
}

# Run git with `args` in `path`. No shell is involved: each element of
# `args` reaches git as one argument, whatever it contains. This used to
# go through system2(), which builds a shell command line, so a ref of
# "HEAD; touch x" ran `touch` -- command execution through tools that
# policy classes as reads.
#
# Nor does git run a program of the repository's choosing. A repository's
# own config (.git/config, which anything with write access can edit)
# can name programs that git starts during an ordinary status, diff, or
# add: an fsmonitor hook, clean/smudge filters, the post-index-change
# hook, gpg for signatures, a transport for a lazy fetch. Tools that
# policy classes as reads, and the job snapshot, must not be a way to
# run them, so every call here turns them off (GIT_SAFE_CONFIG,
# GIT_SAFE_ENV, git_programs_off()). The cost: a file under a clean
# filter (git-lfs) is compared and snapshotted as its raw content, a
# partial clone does not fetch missing objects, and a submodule is
# compared by its commit, not by the state of its working tree.
#
# Pathspecs are literal: ":(top)x" and "*.R" name files, they are not
# pathspec magic or globs. `stdin` is a file to feed git; `bytes = TRUE`
# returns stdout as a raw vector in `bytes` (for NUL-separated output)
# instead of `text`.
#
# The filters are turned off for every command, not only the ones that
# look like they read file content. Git re-hashes a recently modified
# file whenever it writes an index, so `write-tree` runs a clean filter
# as readily as `add` does.
git_run <- function(args, path = ".", env = NULL, stdin = NULL, bytes = FALSE) {
    repo_path <- tool_resolve_path(path)
    off <- git_programs_off(repo_path)
    if (is.null(off)) {
        return(list(status = 1L, bytes = raw(), text = GIT_CONFIG_ERROR))
    }
    git_exec(c(rbind("-c", c(GIT_SAFE_CONFIG, off)), "--no-pager",
               "--literal-pathspecs", "-C", repo_path, args), env = env,
             stdin = stdin, bytes = bytes)
}

# No fsmonitor hook, no hooks directory, no gpg, no transport.
GIT_SAFE_CONFIG <- c("core.fsmonitor=", "core.hooksPath=/dev/null",
                     "log.showSignature=false", "protocol.allow=never")

GIT_CONFIG_ERROR <- paste("Error: git was not run, because the programs",
                          "this repository's configuration names could not",
                          "be turned off (unreadable git configuration, or",
                          "a filter driver or protocol with \"=\" in its",
                          "name)")

# No index refresh written back (which would fire post-index-change), no
# fetch of objects a partial clone lacks, no credential prompt.
GIT_SAFE_ENV <- c(GIT_OPTIONAL_LOCKS = "0", GIT_NO_LAZY_FETCH = "1",
                  GIT_TERMINAL_PROMPT = "0")

# Start git with exactly `args`. With `bytes`, stdout goes through a
# file, since an R string cannot hold the NUL bytes in `-z` output.
git_exec <- function(args, env = NULL, stdin = NULL, bytes = FALSE) {
    out <- "|"
    if (bytes) {
        out <- tempfile("corteza-git-")
        on.exit(unlink(out), add = TRUE)
    }
    res <- tryCatch(
                    processx::run("git", args, error_on_status = FALSE, stdout = out,
                                  stderr = if (bytes) NULL else "|",
                                  stderr_to_stdout = !bytes, stdin = stdin,
                                  env = c("current", GIT_SAFE_ENV, env)),
                    error = function(e) {
        list(status = 1L, stdout = paste("Error:", conditionMessage(e)))
    })
    status <- as.integer(res$status %||% 1L)
    if (bytes) {
        got <- if (file.exists(out)) {
            readBin(out, "raw", file.size(out))
        } else {
            raw()
        }
        return(list(status = status, bytes = got, text = ""))
    }
    list(status = status, text = sub("\r?\n$", "", res$stdout %||% ""))
}

# NUL-separated git output as a character vector. Names are kept as the
# bytes git printed: a tab, a newline, or a quote in a filename arrives
# as itself, where git's default output would quote and escape it.
git_split_nul <- function(bytes) {
    n <- sum(bytes == as.raw(0L))
    if (n == 0L) {
        return(character())
    }
    readBin(bytes, "character", n = n)
}

# `-c` settings for what GIT_SAFE_CONFIG cannot name in advance, because
# the names are the configuration's own:
# - every filter driver is emptied, so no clean, smudge, or process
#   command runs. Git has no switch for this;
# - every protocol allowed by name is refused. `protocol.allow=never`
#   only sets the default, and `protocol.ext.allow=always` with a remote
#   of "ext::<command>" is a program a lazy fetch would start.
# NULL when that cannot be done: the configuration is unreadable, or a
# name holds "=", which `-c name=value` cannot express.
git_programs_off <- function(repo_path) {
    if (!dir.exists(repo_path)) {
        # Git will not start there either; let it say so itself.
        return(character())
    }
    res <- git_exec(c("-C", repo_path, "config", "-z", "--name-only",
                      "--get-regexp",
                      paste0("^(filter\\..*\\.(clean|smudge|process|required)",
                             "|protocol\\..*\\.allow)$")),
                    bytes = TRUE)
    # Exit 1 is git's "nothing matched".
    if (res$status == 1L) {
        return(character())
    }
    if (res$status != 0L) {
        return(NULL)
    }
    keys <- unique(git_split_nul(res$bytes))
    if (any(grepl("=", keys, fixed = TRUE, useBytes = TRUE))) {
        return(NULL)
    }
    is_filter <- startsWith(keys, "filter.")
    drivers <- unique(sub("^filter\\.(.*)\\.[^.]*$", "\\1", keys[is_filter],
                          useBytes = TRUE))
    c(unlist(lapply(drivers, function(d) {
        c(paste0("filter.", d, c(".clean=", ".smudge=", ".process=")),
            paste0("filter.", d, ".required=false"))
    })), paste0(keys[!is_filter], rep("=never", sum(!is_filter))))
}

git_repo_available <- function(path = ".") {
    result <- git_run(c("rev-parse", "--is-inside-work-tree"), path = path)
    if (identical(result$text, GIT_CONFIG_ERROR)) {
        # Not the same as "no repository here": say why git did not run.
        return(structure(FALSE, reason = GIT_CONFIG_ERROR))
    }
    identical(trimws(result$text), "true") && result$status == 0L
}

# Path confinement for the git tools, which used to apply none: with a
# `path` argument they read any repository on the machine, whatever
# allowed_paths said. Returns list(ok, message, path, scope). `scope` is
# a pathspec limiting git to the directory when the repository's root
# lies outside what the session may read (a session confined to a
# subdirectory of a repo), and NULL otherwise.
git_tool_repo <- function(path = ".") {
    checked <- tool_check_path(path %||% ".", operation = "read")
    if (!checked$ok) {
        return(list(ok = FALSE, message = checked$message))
    }
    available <- git_repo_available(checked$path)
    if (!available) {
        return(list(ok = FALSE, message = attr(available, "reason") %||%
                    "Not inside a git repository"))
    }
    top <- git_run(c("rev-parse", "--show-toplevel"), path = checked$path)
    scope <- NULL
    if (top$status != 0L ||
        !validate_path(tool_resolve_path(top$text), tool_config(),
                       operation = "read")$ok) {
        scope <- c("--", ".")
    }
    list(ok = TRUE, message = NULL, path = checked$path, scope = scope)
}

# A ref from the model must be a ref, not an option. Without a shell an
# argument can no longer run a command, but one starting with "-" would
# still be read by git as a flag: --output=<file> makes `git diff` write
# a file, --no-index makes it read paths outside the repository.
#
# Nor may it name a path. "HEAD:b/secret.R" is a file's content at a
# commit, and `git diff HEAD:b/secret.R -- a/x.R` prints it, from
# anywhere in the repository, whatever directory the session is
# confined to. No commit, branch, tag, or range needs a colon.
git_ref_ok <- function(ref) {
    !startsWith(ref, "-") && !grepl(":", ref, fixed = TRUE)
}

GIT_REF_MESSAGE <- paste("ref must be a commit, branch, tag, or range,",
                         "not an option or a <rev>:<path>")

# Why `ref` may not be used, or NULL when it may.
#
# The spelling rules of git_ref_ok() are not enough. A file's content
# has an object id of its own, and a tag can point at one, so
# `git diff <blob> -- a/x.R` prints a file from anywhere in the
# repository with no colon in sight. What counts is what the ref
# resolves to: git expands it (a name, a range, "^A", "A^!") into object
# ids, and every one of them has to be a commit, or a tag of one. A
# diff or log between commits is limited by its pathspec; a blob or a
# tree is not.
git_ref_problem <- function(ref, repo_path) {
    if (!nzchar(ref)) {
        return(NULL)
    }
    if (!git_ref_ok(ref)) {
        return(GIT_REF_MESSAGE)
    }
    parsed <- git_run(c("rev-parse", "--revs-only", ref), path = repo_path)
    ids <- sub("^\\^", "", strsplit(parsed$text, "\n", fixed = TRUE)[[1L]])
    if (parsed$status != 0L || !length(ids) ||
        !all(grepl("^[0-9a-f]{40,64}$", ids))) {
        return(sprintf("'%s' is not a revision in this repository", ref))
    }
    feed <- tempfile("corteza-refs-")
    on.exit(unlink(feed), add = TRUE)
    writeLines(paste0(ids, "^{commit}"), feed)
    types <- git_run(c("cat-file", "--batch-check=%(objecttype)"),
                     path = repo_path, stdin = feed)
    found <- strsplit(types$text, "\n", fixed = TRUE)[[1L]]
    if (types$status != 0L || length(found) != length(ids) ||
        !all(found == "commit")) {
        return(paste("ref must name commits (a commit, branch, tag of a",
                     "commit, or range of them), not a file or a tree"))
    }
    NULL
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

    entries <- list.files(path = path, pattern = pattern %||% NULL,
                          all.files = all_files, recursive = recursive,
                          full.names = TRUE, include.dirs = TRUE, no.. = TRUE)
    # A recursive listing follows symlinked directories; entries that
    # resolve outside what the session may read are not listed. Checked
    # in sorted order and only until the display limit is passed, so a
    # huge tree costs no more checks than it shows.
    entries <- sort(entries)
    kept <- character()
    i <- 0L
    while (length(kept) <= limit && i < length(entries)) {
        chunk <- entries[seq.int(i + 1L, min(i + limit + 1L, length(entries)))]
        kept <- c(kept, tool_readable_paths(chunk))
        i <- i + length(chunk)
    }
    entries <- kept

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
    if (is.na(from) || from < 1L) {
        from <- 1L
    }

    count <- lines
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

    # Read prior content so we can show the user a diff after the
    # write. Empty string when the file is new or unreadable; that
    # path produces an all-green diff, which is what we want.
    old_content <- ""
    if (file.exists(path) && !dir.exists(path)) {
        old_content <- tryCatch(tool_read_text(path), error = function(e) "")
    }

    write_error <- tryCatch({
        tool_write_text(path, content, append = append)
        NULL
    }, error = function(e) e$message)
    if (!is.null(write_error)) {
        return(err(paste("Write error:", write_error)))
    }

    summary <- sprintf("%s %d byte(s) to %s",
        if (append) "Appended" else "Wrote",
                       nchar(content, type = "bytes"), path)
    # Append mode writes after existing content; the on-disk file now
    # has old_content + content. Reflect that in the displayed diff so
    # the user sees what was actually written, not a misleading
    # whole-file overwrite preview.
    if (append) {
        new_for_diff <- paste0(old_content, content)
    } else {
        new_for_diff <- content
    }
    diff <- compute_unified_diff(old_content, new_for_diff, path)
    ok_with_diff(summary, diff)
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

    if (replace_all) {
        replacements <- match_count
    } else {
        replacements <- 1L
    }
    summary <- sprintf("Updated %s (%d replacement%s)",
                       path,
                       replacements,
        if (replacements == 1L) "" else "s")
    diff <- compute_unified_diff(original, updated, path)
    ok_with_diff(summary, diff)
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
    # Checking `path` is not enough: the glob is the model's too, and
    # "../other/*.R" or a symlink walks out of it. Every file the glob
    # produced is checked where it really resolves; one outside what the
    # session may read is dropped without being opened.
    files <- tool_readable_paths(files)
    files <- files[!dir.exists(files)]
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

#' Execute R code in a persistent environment.
#'
#' By default, code runs in the host's global environment, preserving the
#' original \code{run_r} contract. Pass a private persistent environment to
#' give an agent or capability its own R scope. Scoped evaluation retains
#' assignments and handles but does not copy bindings into the host workspace
#' cache.
#'
#' @param code (character) R code to execute.
#' @param envir (environment) Persistent environment in which assignments
#'   should land. Defaults to \code{globalenv()} for backwards compatibility.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_run_r <- function(code, envir = globalenv()) {
    stopifnot(is.environment(envir))
    capture_workspace <- identical(envir, globalenv())
    if (isTRUE(capture_workspace)) {
        before <- ls(envir)
    } else {
        before <- character()
    }
    handle_store <- handle_store_for(envir)
    # Active handles are visible in `code` as regular R names.
    eval_env <- handle_eval_env(parent = envir, store = handle_store)

    # Evaluate like a console: stdout, messages, and warnings interleave
    # in the order the code emits them, then the print of a visible final
    # value. Everything streams to one connection so a `message()` before
    # a `cat()` prints before it, not after. A warning is muffled only
    # under the default `warn < 2`; with `options(warn = 2)` it is left to
    # become an error, and code after it does not run. Output written
    # before an error survives, ahead of the `Error:` line.
    stream_file <- tempfile("run_r_stream")
    stream_con <- file(stream_file, open = "wt")
    sink(stream_con)
    sink_open <- TRUE
    con_open <- TRUE
    close_stream <- function() {
        if (sink_open) {
            sink()
            sink_open <<- FALSE
        }
        if (con_open) {
            close(stream_con)
            con_open <<- FALSE
        }
    }
    # Release the sink, connection, and temp file however this call exits.
    # The supervised worker aborts an over-deadline call with an interrupt,
    # which unwinds past the explicit close_stream() below without running
    # it -- leaking a sink and connection into the persistent worker on every
    # timeout. on.exit fires on interrupts as well as errors and normal
    # returns; the flags keep it idempotent with the normal-path close.
    on.exit({
        close_stream()
        unlink(stream_file)
    }, add = TRUE)
    eval_error <- NULL
    r <- tryCatch(
                  withCallingHandlers(
                                      withVisible(eval(parse(text = code), envir = eval_env)),
                                      message = function(m) {
        cat(conditionMessage(m), file = stream_con, sep = "")
        invokeRestart("muffleMessage")
    },
                                      warning = function(w) {
        if (getOption("warn") < 2) {
            cat("Warning: ", conditionMessage(w), "\n", file = stream_con,
                sep = "")
            invokeRestart("muffleWarning")
        }
    }
        ),
                  error = function(e) {
        eval_error <<- e
        NULL
    }
    )
    close_stream()
    streams <- suppressWarnings(readLines(stream_file))

    if (!is.null(eval_error)) {
        text <- paste(c(streams, paste("Error:", conditionMessage(eval_error))),
                      collapse = "\n")
        result <- ok(admit_tool_result(text, tool = "run_r", store = handle_store))
        result$r_error <- TRUE
        return(result)
    }
    outcome <- list(value = r$value, visible = isTRUE(r$visible))
    outcome$printed <- if (outcome$visible) {
        paste(utils::capture.output(print(outcome$value)), collapse = "\n")
    } else {
        character(0)
    }

    # Large visible results get stashed as handles so the LLM sees a
    # summary instead of the full print.
    text <- if (outcome$visible && .is_large_result(outcome$value)) {
        stashed <- with_handle(outcome$value, store = handle_store)
        sprintf("%s\n\n[stored as %s]", stashed$summary, stashed$handle)
    } else {
        outcome$printed
    }
    text <- paste(c(streams, text), collapse = "\n")

    if (isTRUE(capture_workspace)) {
        # Auto-capture new bindings into the workspace. Hidden names
        # (`.foo`, `.h_NNN`) are excluded by default ls() rules.
        new_names <- setdiff(ls(envir), before)
        origin <- list(tool = "run_r", args = list(code = code))
        for (nm in new_names) {
            val <- get(nm, envir = envir)
            if (object.size(val) < 10e6) {
                deps <- tryCatch({
                    fn <- eval(parse(text = paste0("function() {", code, "}")))
                    referenced <- codetools::findGlobals(fn)
                    intersect(referenced, ws_names())
                }, error = function(e) character())
                ws_put(nm, val, origin = origin, deps = deps)
            }
        }
    }

    # The outer tool handler applies the same universal cap, but it does not
    # know which private evaluator produced this result. Admit it here first
    # so any overflow handle belongs to this R scope rather than the process.
    # `r_error` lets a caller (e.g. an audit log) tell a failed evaluation
    # from a successful one; the model-facing text and `isError` are
    # unchanged, since an R error is a normal, non-transport tool result.
    result <- ok(admit_tool_result(text, tool = "run_r", store = handle_store))
    result$r_error <- FALSE
    result
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
        # callr::rscript runs Rscript portably (Linux, macOS, Windows).
        # stderr = "2>&1" merges stderr into stdout so the LLM sees both
        # streams as a single text blob in res$stdout.
        #
        # stdout = NULL (not "|"): in CRAN callr <= 3.7.6 on Windows,
        # stdout = "|" combined with stderr = "2>&1" hangs indefinitely
        # when the child errors via stop() — internal timeout never
        # fires (r-lib/callr#313, fixed upstream in e93efd1). Passing
        # stdout = NULL skips the hanging cat(file = "|") call in
        # setup_callbacks() while still populating res$stdout via the
        # 2>&1 redirect. Can be reverted to "|" once corteza depends
        # on a fixed callr release.
        res <- callr::rscript(tmp, show = FALSE, fail_on_status = FALSE,
                              timeout = timeout, stdout = NULL,
                              stderr = "2>&1")
        if (isTRUE(res$timeout)) {
            paste0("Error: timed out after ", timeout, "s")
        } else {
            res$stdout
        }
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
    if (.Platform$OS.type != "windows") {
        return("bash")
    }
    rtools_home <- Sys.getenv("RTOOLS45_HOME", Sys.getenv("RTOOLS44_HOME", ""))
    candidates <- c(
        if (nzchar(rtools_home)) file.path(rtools_home, "usr", "bin",
            "bash.exe"),
                    "C:/rtools45/usr/bin/bash.exe",
                    "C:/rtools44/usr/bin/bash.exe",
                    "C:/Program Files/Git/bin/bash.exe",
                    "C:/Program Files (x86)/Git/bin/bash.exe"
    )
    for (p in candidates) {
        if (file.exists(p)) {
            return(p)
        }
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

    exe_args <- switch(shell_name, bash = c("-c", cmd), cmd = c("/c", cmd))

    if (background) {
        proc <- processx::process$new(shell_exe, exe_args, stdout = "|",
                                      stderr = "|", cleanup_tree = TRUE)
        id <- bg_register(cmd, proc)
        return(ok(sprintf(
                          "Started background process [%s] (pid %d)\nCheck with: bg_status tool",
                          id, proc$get_pid()
                )))
    }

    # Foreground uses the same args as background now: processx passes
    # each arg literally (no shell word-splitting), so the command goes
    # through as a single unquoted `-c` arg. (system2 needed shQuote
    # because it pastes args; processx does not.)
    tryCatch({
        # processx::run() instead of system2(stdout = TRUE): the latter
        # is a blocking C call R can't interrupt, so Ctrl+C during a
        # foreground tool (e.g. `bash sleep 30`) halted the whole REPL.
        # processx polls and is interruptible -- the interrupt propagates
        # to the turn's handler in run_repl_loop, which returns to the
        # prompt. cleanup_tree kills the child's descendants; status is
        # surfaced structurally via err()/ok() rather than a warning.
        res <- processx::run(shell_exe, exe_args,
                             error_on_status = FALSE,
                             stderr_to_stdout = TRUE,
                             timeout = timeout,
                             cleanup_tree = TRUE)
        status <- res$status %||% 0L
        text <- res$stdout %||% ""
        if (!is.null(status) && status != 0L) {
            err(sprintf("[exit status %d]\n%s", status, text))
        } else {
            ok(text)
        }
    }, system_command_timeout_error = function(e) {
        partial <- tryCatch(e$stdout %||% "", error = function(e2) "")
        err(sprintf("[timed out after %ss]\n%s", timeout, partial))
    }, error = function(e) {
        err(paste("Error:", e$message))
    })
}

# Background process registry ----

.bg_processes <- new.env(parent = emptyenv())

bg_register <- function(cmd, proc) {
    id <- sprintf("bg_%d", length(ls(.bg_processes)) + 1L)
    .bg_processes[[id]] <- list(id = id, command = substr(cmd, 1, 80),
                                process = proc, started = Sys.time())
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
        ok(sprintf("Process [%s] already exited with status %d", id,
                   entry$process$get_exit_status()))
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
        body <- list(api_key = api_key, query = query,
                     max_results = max_results, include_answer = TRUE)

        h <- curl::new_handle()
        curl::handle_setopt(h,
                            customrequest = "POST",
                            postfields = jsonlite::toJSON(body, auto_unbox = TRUE),
                            connecttimeout = 10L,
                            timeout = 30L
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
        # Bound the request with curl's own connect/total timeout rather than
        # an outer setTimeLimit, which can't reliably abort a blocking libcurl
        # transfer. fetch_url is therefore on .self_bounded_tools.
        curl::handle_setopt(h, followlocation = TRUE, connecttimeout = 10L,
                            timeout = 30L)
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
    repo <- git_tool_repo(path)
    if (!repo$ok) {
        return(err(repo$message))
    }
    repo_path <- repo$path

    # A submodule's commit is compared, its working tree is not: that
    # takes a second git inside the submodule, under its configuration.
    result <- git_run(c("status", "--short", "--branch",
                        "--ignore-submodules=dirty", repo$scope),
                      path = repo_path)
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
    repo <- git_tool_repo(path)
    if (!repo$ok) {
        return(err(repo$message))
    }
    repo_path <- repo$path

    ref <- trimws(ref %||% "HEAD")
    problem <- git_ref_problem(ref, repo_path)
    if (!is.null(problem)) {
        return(err(problem))
    }
    file_path <- trimws(file_path %||% "")
    staged <- isTRUE(staged)
    context_lines <- as.integer(context_lines %||% 3L)
    if (is.na(context_lines) || context_lines < 0L) {
        context_lines <- 3L
    }

    # --no-textconv with --no-ext-diff: neither a configured external
    # diff nor a textconv filter runs a program on behalf of a read.
    cmd <- c("diff", "--no-ext-diff", "--no-textconv", "--find-renames",
             "--ignore-submodules=dirty",
             sprintf("--unified=%d", context_lines))
    if (staged) {
        cmd <- c(cmd, "--cached")
    }
    if (nchar(ref) > 0) {
        cmd <- c(cmd, ref)
    }
    # "--" always: what precedes it is a revision, never a path.
    if (nchar(file_path) > 0) {
        # The filter is a path like any other: it has to resolve inside
        # what the session may read. git_run() passes it to git as a
        # literal name, so git reads it the way this check does and
        # ":(top)b/x.R" cannot reach out of the directory.
        target <- if (grepl("^(/|~|[A-Za-z]:)", file_path)) {
            file_path
        } else {
            file.path(repo_path, file_path)
        }
        checked <- tool_check_path(target, operation = "read")
        if (!checked$ok) {
            return(err(checked$message))
        }
        cmd <- c(cmd, "--", file_path)
    } else {
        cmd <- c(cmd, repo$scope %||% "--")
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
    repo <- git_tool_repo(path)
    if (!repo$ok) {
        return(err(repo$message))
    }
    repo_path <- repo$path

    n <- as.integer(n %||% 10L)
    if (is.na(n) || n < 1L) {
        n <- 10L
    }
    ref <- trimws(ref %||% "HEAD")
    problem <- git_ref_problem(ref, repo_path)
    if (!is.null(problem)) {
        return(err(problem))
    }

    cmd <- c("log", "--oneline", "--decorate", sprintf("-n%d", n))
    if (nchar(ref) > 0) {
        cmd <- c(cmd, ref)
    }
    cmd <- c(cmd, repo$scope %||% "--")

    result <- git_run(cmd, path = repo_path)
    if (result$status != 0L) {
        return(err(result$text))
    }

    ok(result$text)
}

# Subagent tools ----

#' Spawn a specialized subagent for a task.
#'
#' Use for parallel work or tasks requiring focused attention. Parent
#' session is read from `ctx$session`, which the skill handler injects
#' from the invoking context; not from LLM-provided args.
#'
#' @param task (character) Task description for the subagent.
#' @param model (character) Optional model override.
#' @param tools (character vector) Optional explicit tool filter.
#' @param preset (character) Preset name: "investigate" (default, read-only),
#'   "work" (read + write + bash), or "minimal" (read_file + grep_files only).
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_spawn_subagent <- function(task, model = NULL, tools = NULL,
                                preset = NULL, ctx = list()) {
    tryCatch({
        id <- subagent_spawn(task = task, model = model, tools = tools,
                             preset = preset, parent_session = ctx$session)
        ok(sprintf("Spawned subagent %s for: %s", id, task))
    }, error = function(e) {
        err(paste("Spawn failed:", e$message))
    })
}

#' Preserve an optional machine-readable subagent handoff on a tool result.
#' @noRd
.subagent_tool_result <- function(result) {
    report <- attr(result, "corteza_report", exact = TRUE)
    out <- ok(as.character(result))
    if (!is.null(report)) {
        # MCP clients that understand structuredContent can consume this
        # directly; existing clients continue to read the same text block.
        out$structuredContent <- list(report = report)
    }
    out
}

#' Send a prompt to a running subagent.
#'
#' Fires the prompt and returns at once; collect the reply later with
#' collect_subagent, so several subagents can run in parallel. wait =
#' TRUE for a quick answer.
#'
#' @param id (character) Subagent ID.
#' @param prompt (character) Prompt to send.
#' @param wait (logical) If FALSE (default), fire the prompt and return
#'   immediately; collect the reply with `collect_subagent`. If TRUE,
#'   block up to `timeout` seconds for the reply; on timeout the query
#'   stays pending and `collect_subagent` fetches it later.
#' @param return_name (string) Optional name or `.h_NNN` handle for a
#'   value the subagent should hand back. Tell the subagent to leave
#'   its result bound under this name (it needs `run_r`); the value is
#'   returned as a handle you can reference in a later `run_r`, instead
#'   of being inlined into the reply text.
#' @param timeout (numeric) Maximum seconds to block when `wait = TRUE`.
#'   Default 60.
#' @param report (logical) Request a structured completion handoff containing
#'   findings, concerns, deviations, open questions, and feedback. Default
#'   FALSE preserves the plain-reply behavior.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_query_subagent <- function(id, prompt, wait = FALSE, return_name = NULL,
                                timeout = 60, report = FALSE) {
    tryCatch({
        result <- subagent_query(id, prompt, wait = wait, timeout = timeout,
                                 return_name = return_name, report = report)
        if (!isTRUE(wait)) {
            ok(sprintf("Queued for subagent %s; collect with collect_subagent.",
                       result))
        } else if (is.null(result)) {
            ok(sprintf(
                       "Subagent %s still working after %s s; collect with collect_subagent.",
                       id, format(timeout)
                ))
        } else {
            .subagent_tool_result(result)
        }
    }, error = function(e) {
        err(paste("Query failed:", e$message))
    })
}

#' Collect the result of a previously-fired async subagent query.
#'
#' @param id (character) Subagent ID.
#' @param wait (logical) If TRUE (default), block up to `timeout`
#'   seconds. If FALSE, poll once and return immediately.
#' @param timeout (numeric) Maximum seconds to block when `wait =
#'   TRUE`. Default 60.
#' @return An MCP tool-result list. On timeout returns a note that
#'   the query is still running.
#' @keywords internal
#' @export
tool_collect_subagent <- function(id, wait = TRUE, timeout = 60) {
    tryCatch({
        result <- subagent_collect(id, wait = wait, timeout = timeout)
        if (is.null(result)) {
            ok(sprintf("Subagent %s still working; try collect_subagent again.",
                       id))
        } else {
            .subagent_tool_result(result)
        }
    }, error = function(e) {
        err(paste("Collect failed:", e$message))
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
    register_skill(skill_spec(
                              "skill_instructions",
                              paste(
                                    "Read one instruction document from this session's immutable",
                                    "catalog, or a snapshotted relative supporting resource."
            ),
                              params = list(
                id = list(
                          type = "string",
                          description = "Exact instruction id shown in the catalog.",
                          required = TRUE
                ),
                resource = list(
                                type = "string",
                                description = paste(
                        "Optional relative supporting-file path.",
                        "Omit to read SKILL.md."
                    ),
                                required = FALSE
                )
            ),
                              handler = function(args, ctx) {
        tool_skill_instructions(args$id, args$resource %||% NULL, ctx = ctx)
    }
        ))
    register_skill_from_fn("write_file", tool_write_file)
    register_skill_from_fn("replace_in_file", tool_replace_in_file)
    register_skill_from_fn("list_files", tool_list_files)

    # Search
    register_skill_from_fn("grep_files", tool_grep_files)

    # Code execution
    # `envir` remains a host-owned capability boundary on tool_run_r(). The
    # model may request a timeout, but the session dispatcher validates it
    # against a host-owned maximum and only supervised worker mode can enforce
    # it safely.
    register_skill(skill_spec(
                              "run_r",
                              paste("Execute R code in the persistent host session.",
                                    "Returns what the code prints (cat, print, messages,",
                                    "warnings) followed by the value of the last expression.",
                                    "Assignments survive subsequent run_r calls; large results",
                                    "are returned as reusable handles."),
                              params = list(code = list(
                    type = "string",
                    description = "R code to execute.",
                    required = TRUE
                ), timeout = list(
                                  type = "number",
                                  description = paste(
                        "Optional wall-clock seconds for supervised execution.",
                        "The host may impose a lower maximum."
                    ),
                                  required = FALSE
                )),
                              handler = function(args, ctx) {
        .tool_run_r_session(args$code, args$timeout %||% NULL, ctx)
    }
        ))
    register_skill(skill_spec(
                              "read_handle",
                              paste("Inspect a large value previously returned as a handle.",
                                    "The handle remains in the same persistent R workspace."),
                              params = list(
                handle = list(type = "string", description = "Handle id, e.g. .h_001.",
                              required = TRUE),
                op = list(type = "string",
                          description = "Inspection: str, head, summary, or print.",
                          enum = c("str", "head", "summary", "print"),
                          required = FALSE)
            ),
                              handler = function(args, ctx) {
        .tool_read_handle_session(args$handle, args$op %||% "str", ctx)
    }
        ))
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

    # Web. web_search needs a Tavily API key; hide it from the LLM
    # payload when the key isn't set so the model doesn't try calling
    # a tool that can't work.
    .have_tavily <- function() nzchar(Sys.getenv("TAVILY_API_KEY"))
    register_skill_from_fn("web_search", tool_web_search,
                           available = .have_tavily)
    register_skill_from_fn("fetch_url", tool_fetch_url)

    # Git tools only make sense inside a working tree. Check both the
    # cheap `.git` directory case and the more general `git rev-parse`
    # form so worktrees and submodules still count.
    .in_git_repo <- function() {
        if (dir.exists(".git")) {
            return(TRUE)
        }
        status <- tryCatch(
                           suppressWarnings(system2("git",
                    c("rev-parse", "--is-inside-work-tree"),
                    stdout = TRUE, stderr = FALSE)),
                           error = function(e) character()
        )
        isTRUE(identical(trimws(status[1]), "true"))
    }
    register_skill_from_fn("git_status", tool_git_status,
                           available = .in_git_repo)
    register_skill_from_fn("git_diff", tool_git_diff, available = .in_git_repo)
    register_skill_from_fn("git_log", tool_git_log, available = .in_git_repo)

    # Continual harness: one-line lesson capture, approval-gated via
    # the per-tool permissions default ("ask") in load_config().
    register_skill_from_fn("harness_note", tool_harness_note)

    # Subagent tools
    register_skill_from_fn("spawn_subagent", tool_spawn_subagent)
    register_skill_from_fn("query_subagent", tool_query_subagent)
    register_skill_from_fn("collect_subagent", tool_collect_subagent)
    register_skill_from_fn("list_subagents", tool_list_subagents)
    register_skill_from_fn("kill_subagent", tool_kill_subagent)

    # Talker job tools: exposed only to talker-mode sessions
    # (see .talker_filter_tools).
    register_skill_from_fn("delegate", tool_delegate)
    register_skill_from_fn("job_status", tool_job_status)
    register_skill_from_fn("job_cancel", tool_job_cancel)

    # Plan mode: exit_plan_mode is registered always but exposed in the
    # tool list only when session$plan_mode is TRUE
    # (see .plan_mode_filter_tools).
    register_skill_from_fn("exit_plan_mode", tool_exit_plan_mode)

    # Task tracker: schema only -- the real handlers live in
    # task_tool_intercept() called from .make_tool_handler(), so they
    # mutate the live session env directly (a registered stub handler
    # only ever sees ctx = list(), not the session).
    register_skill_from_fn("task_create", tool_task_create)
    register_skill_from_fn("task_update", tool_task_update)

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
