.prompt_input_state <- new.env(parent = emptyenv())
.prompt_input_state$stdin_con <- NULL

# Deny-key label for the R-console approval prompt. Plain "Deny": the
# interrupt-key hint is omitted for now because the actual key differs
# across RStudio, the terminal R console, and the corteza CLI, and the
# inconsistency misled more than it helped. Reinstate a surface-aware
# hint once the TUI lands.
.console_deny_label <- function() {
    "Deny"
}

.read_prompt_via_bash <- function(prompt_str = "> ") {
    # Hand the prompt to bash's `read -e -p` instead of cat()ing it
    # ourselves. Readline now owns the cursor position, so a Backspace
    # past the start of the user's input stops at the prompt instead
    # of erasing it. ANSI color escapes confuse readline's column math
    # unless wrapped in \001 ... \002 (RL_PROMPT_START_IGNORE /
    # RL_PROMPT_END_IGNORE); markup_prompt does that wrap.
    utils::flush.console()
    bash_prompt <- .markup_prompt_for_readline(prompt_str)

    script <- paste('out="$1"', 'prompt="$2"',
                    # Turn off readline's bracketed-paste mode. With it on
                    # (the bash 5.2 default), readline swallows a multi-line
                    # paste as one unit and `read -e` returns only the first
                    # line -- the rest are discarded before the drain below
                    # can see them. Off, a paste arrives as a raw newline
                    # burst the drain collects. The "line editing not enabled"
                    # warning here is benign (the setting still applies once
                    # read -e turns editing on), so silence it.
                    "bind 'set enable-bracketed-paste off' 2>/dev/null",
                    'IFS= read -r -e -p "$prompt" line || exit 1',
                    'printf "%s\\n" "$line" > "$out"',
                    # Drain the remaining lines of a pasted burst (already
                    # buffered behind line 1). 50ms tolerates paste/SSH
                    # latency between lines; a typed line just waits this out
                    # once before the prompt returns.
                    'while IFS= read -r -t 0.05 next; do',
                    '  printf "%s\\n" "$next" >> "$out"', 'done', sep = "\n")

    path <- tempfile("corteza-prompt-")
    on.exit(unlink(path), add = TRUE)
    status <- suppressWarnings(
                               system2(
                                       "bash",
                                       c("-c", shQuote(script), "bash",
                shQuote(path), shQuote(bash_prompt)),
                                       stdout = "",
                                       stderr = ""
        )
    )
    .handle_bash_prompt_status(status, path)
}

# Post-system2 dispatch for .read_prompt_via_bash. Extracted so the
# SIGINT path can be tested without spawning a real bash subprocess.
#
# - status 130 (bash killed by SIGINT, the Ctrl+C path) re-raises as
#   an R interrupt condition. The surrounding tryCatch(interrupt = ...)
#   in inst/bin/corteza and R/chat.R catches it the same way it would
#   a real terminal Ctrl+C, so the turn aborts instead of returning
#   empty to a caller that would default to "1" (Approve).
# - Any other non-zero status (read failure) returns empty.
# - status 0 with a missing tempfile returns empty.
# - status 0 with a present tempfile returns its lines.
.handle_bash_prompt_status <- function(status, path) {
    if (!is.null(status) && status == 130L) {
        stop(structure(
                       class = c("interrupt", "condition"),
                       list(message = "Interrupted via Ctrl+C at input prompt",
                            call = sys.call())
            ))
    }
    if (!is.null(status) && status != 0L) {
        return(character())
    }
    if (!file.exists(path)) {
        return(character())
    }
    readLines(path, warn = FALSE)
}

# Wrap each ANSI color escape sequence in \001 ... \002 so bash's
# readline counts only the printable characters when positioning the
# cursor. Otherwise a colored prompt like "\033[32m> \033[0m" makes
# readline think the prompt is 12 chars wide instead of 2, and
# Backspace / line wrap go to the wrong place.
.markup_prompt_for_readline <- function(prompt_str) {
    if (!is.character(prompt_str) || length(prompt_str) != 1L) {
        return(prompt_str)
    }
    if (!grepl("\033", prompt_str, fixed = TRUE)) {
        return(prompt_str)
    }
    gsub("(\033\\[[0-9;]*m)", "\001\\1\002", prompt_str, perl = TRUE)
}

# Read a `/paste`-style multi-line block: print a header, accept lines
# at a continuation prompt, terminate on `/end` or EOF. Returns the
# joined buffer (single character scalar) or NULL when the buffer ends
# up empty. Used by both the explicit `/paste` slash command and the
# implicit trailing-backslash continuation in the chat() and CLI REPL
# loops, so the UX is identical regardless of how the user got here.
#
# `seed` seeds the buffer with the partial line the user already had
# (the text after `/paste`, or the line-with-trailing-backslash
# stripped). `continuation_prompt` lets surfaces colorize differently;
# the rest of the messaging is uniform.
read_paste_block <- function(seed = NULL, continuation_prompt = "... ",
                             header = "",
                             empty_message = "Empty paste, nothing sent.",
                             heredoc = FALSE) {
    buffer <- character()
    if (!is.null(seed) && nzchar(trimws(seed))) {
        buffer <- c(buffer, seed)
    }
    if (nzchar(header)) {
        cat(header, "\n", sep = "")
    }
    # State machine over sub-lines: bash's `.read_prompt_via_bash`
    # returns a single string containing pasted multi-line content
    # joined with `\n`. Splitting per call lets us see each pasted
    # line individually so a `/end` sentinel buried in a paste fires
    # immediately.
    #
    # Two modes diverge only on what counts as "the last line":
    #
    #   heredoc = FALSE (the /paste contract): collect every sub-line
    #     verbatim. Only `/end` or EOF terminates. Backslashes are
    #     literal. Use this when the user wants to paste arbitrary
    #     text (logs, code, etc.) and shouldn't have to escape `\` at
    #     end of line.
    #
    #   heredoc = TRUE (trailing-backslash continuation entry):
    #     bash-heredoc-with-continuation semantics. A sub-line ending
    #     in an unescaped `\` continues (with the `\` stripped). The
    #     first sub-line without trailing `\` is the final line and
    #     gets included. `/end` and EOF still terminate explicitly.
    done <- FALSE
    repeat {
        if (done) {
            break
        }
        ln <- read_prompt_input(continuation_prompt)
        if (length(ln) == 0L) {
            # EOF (Ctrl+D)
            break
        }
        chunks <- unlist(strsplit(ln, "\n", fixed = TRUE), use.names = FALSE)
        if (length(chunks) == 0L) {
            chunks <- ""
        }
        for (chunk in chunks) {
            if (identical(trimws(chunk), "/end")) {
                # Explicit sentinel — terminate without including the
                # /end sub-line.
                done <- TRUE
                break
            }
            if (isTRUE(heredoc)) {
                cont_seed <- backslash_continuation_seed(chunk)
                if (is.null(cont_seed)) {
                    # No trailing unescaped `\` — final sub-line.
                    buffer <- c(buffer, chunk)
                    done <- TRUE
                    break
                }
                buffer <- c(buffer, cont_seed)
            } else {
                # /paste mode: keep every line verbatim.
                buffer <- c(buffer, chunk)
            }
        }
    }
    if (length(buffer) == 0L) {
        cat(empty_message, "\n", sep = "")
        return(NULL)
    }
    paste(buffer, collapse = "\n")
}

# Detect "trailing unescaped backslash" — odd number of trailing
# backslashes means the last one is a continuation marker; even
# means they're all paired escapes and stand for literal backslash
# characters. Returns the trimmed line (with the trailing `\`
# dropped) when continuation should fire, or NULL otherwise.
backslash_continuation_seed <- function(line) {
    if (!is.character(line) || length(line) != 1L) {
        return(NULL)
    }
    m <- regexpr("\\\\+$", line, perl = TRUE)
    if (m < 0L) {
        return(NULL)
    }
    n <- attr(m, "match.length")
    if (n %% 2L != 1L) {
        return(NULL)
    }
    substr(line, 1L, nchar(line) - 1L)
}

read_prompt_input <- function(prompt_str = "> ", use_readline = TRUE) {
    if (.Platform$OS.type == "windows") {
        if (isTRUE(use_readline)) {
            return(readline(prompt_str))
        }
        cat(prompt_str)
        if (is.null(.prompt_input_state$stdin_con)) {
            .prompt_input_state$stdin_con <- file("stdin", open = "r")
        }
        line <- tryCatch(
                         readLines(.prompt_input_state$stdin_con, n = 1L, warn = FALSE),
                         error = function(e) character()
        )
        if (length(line) == 0L) {
            return(character())
        }
        return(line[1])
    }

    if (isTRUE(tryCatch(isatty(stdin()), error = function(e) FALSE))) {
        out <- tryCatch(.read_prompt_via_bash(prompt_str),
                        error = function(e) NULL)
        if (!is.null(out)) {
            if (length(out) == 0L) {
                return(character())
            }
            return(paste(out, collapse = "\n"))
        }
    }

    if (isTRUE(use_readline)) {
        return(readline(prompt_str))
    }

    cat(prompt_str)
    if (is.null(.prompt_input_state$stdin_con)) {
        .prompt_input_state$stdin_con <- file("stdin", open = "r")
    }
    line <- tryCatch(
                     readLines(.prompt_input_state$stdin_con, n = 1L, warn = FALSE),
                     error = function(e) character()
    )
    if (length(line) == 0L) {
        return(character())
    }
    line[1]
}

.cli_args_list <- function(args) {
    if (is.null(args)) {
        return(list())
    }
    if (is.list(args)) {
        return(args)
    }
    as.list(args)
}

.cli_truncate <- function(text, width = 72L) {
    if (!length(text) || is.null(text) || anyNA(text) ||
        !nzchar(text) || nchar(text) <= width) {
        return(text %||% "")
    }
    paste0(substr(text, 1L, max(1L, width - 3L)), "...")
}

.cli_wrap_lines <- function(lines, width = 88L) {
    if (!length(lines)) {
        return(character())
    }
    out <- character()
    for (line in lines) {
        if (!nzchar(line)) {
            out <- c(out, "")
            next
        }
        out <- c(out, strwrap(line, width = width))
    }
    out
}

cli_tool_label <- function(tool_name, long = FALSE) {
    # Model-controlled (built from the LLM tool-call name); sanitize so an
    # exotic name can't forge a line in the rendered approval title. Known
    # tool names are unaffected, so their switch mappings still match.
    tool_name <- .sanitize_inline(tool_name %||% "", max_chars = 60L)
    label <- switch(
                    tool_name,
                    bash = "Bash",
                    cmd = "Command",
                    run_r = "Run R",
                    run_r_script = "Run R Script",
                    read_file = "Read File",
                    "base::readLines" = "Read File",
                    write_file = "Write",
                    "base::writeLines" = "Write",
                    replace_in_file = "Update",
                    list_files = "List Files",
                    "base::list.files" = "List Files",
                    grep_files = "Grep Files",
                    web_search = "Web Search",
                    fetch_url = "Fetch URL",
                    git_status = "Git Status",
                    git_diff = "Git Diff",
                    git_log = "Git Log",
                    r_help = "R Help",
                    installed_packages = "Installed Packages",
                    exit_plan_mode = "Exit Plan Mode",
                    tools::toTitleCase(gsub("_", " ", gsub("::", " ", tool_name)))
    )

    if (!isTRUE(long)) {
        return(label)
    }

    switch(tool_name, bash = "Bash command", cmd = "System command",
           run_r = "Run R code", run_r_script = "Run R script",
           read_file = "Read file", "base::readLines" = "Read file",
           write_file = "Write file", "base::writeLines" = "Write file",
           replace_in_file = "Update file", list_files = "List files",
           "base::list.files" = "List files", grep_files = "Search files",
           web_search = "Web search", fetch_url = "Fetch URL",
           git_status = "Git status", git_diff = "Git diff",
           git_log = "Git log", r_help = "R help",
           installed_packages = "List installed packages",
           exit_plan_mode = "Submit plan and exit plan mode", label)
}

cli_tool_preview <- function(tool_name, args = list(), width = 72L) {
    args <- .cli_args_list(args)

    preview <- if (tool_name %in% c("bash", "cmd")) {
        cmd <- args$command %||% args$cmd %||% ""
        strsplit(cmd, "\n", fixed = TRUE)[[1]][1] %||% ""
    } else if (tool_name == "run_r") {
        code <- args$code %||% ""
        strsplit(code, "\n", fixed = TRUE)[[1]][1] %||% ""
    } else {
        sub("^\\s+", "", tool_hint(tool_name, args))
    }

    # Model-controlled (tool_hint renders raw args); sanitize so a crafted
    # value can't forge a line via the progress display or the approval
    # detail-line fallback that consumes this preview.
    .cli_truncate(.sanitize_inline(preview %||% ""), width = width)
}

cli_tool_detail_lines <- function(tool_name, args = list(), cwd = NULL,
                                  width = 88L) {
    args <- .cli_args_list(args)
    lines <- character()

    if (tool_name %in% c("bash", "cmd")) {
        cmd <- args$command %||% args$cmd %||% ""
        if (nzchar(cmd)) {
            lines <- c(
                       lines,
                       .cli_wrap_lines(strsplit(cmd, "\n", fixed = TRUE)[[1]], width)
            )
        }
        if (!is.null(cwd) && nzchar(cwd)) {
            lines <- c(lines, sprintf("Working directory: %s", cwd))
        }
        return(lines)
    }

    if (tool_name == "run_r") {
        code <- args$code %||% ""
        if (nzchar(code)) {
            lines <- c(
                       lines,
                       .cli_wrap_lines(strsplit(code, "\n", fixed = TRUE)[[1]], width)
            )
        }
        if (!is.null(cwd) && nzchar(cwd)) {
            lines <- c(lines, sprintf("Project: %s", cwd))
        }
        return(lines)
    }

    if (tool_name == "run_r_script" && !is.null(cwd) && nzchar(cwd)) {
        lines <- c(lines, sprintf("Project: %s", cwd))
    }

    if (tool_name == "exit_plan_mode") {
        plan <- args$plan %||% ""
        if (nzchar(plan)) {
            lines <- c(
                       lines,
                       .cli_wrap_lines(strsplit(plan, "\n", fixed = TRUE)[[1]], width)
            )
        }
        return(lines)
    }

    call <- list(tool = tool_name, args = args)
    paths <- unique(resolve_paths(call))
    urls <- unique(resolve_urls(call))

    # Every interpolated field below is model-controlled, so sanitize each
    # one (strip ANSI/control chars incl. newlines) before it becomes a
    # labeled line -- a crafted path/pattern/query must not forge its own.
    if (length(paths) > 0L) {
        lines <- c(lines, sprintf("Path: %s", .sanitize_inline(paths)))
    }
    if (length(urls) > 0L) {
        lines <- c(lines, sprintf("URL: %s", .sanitize_inline(urls)))
    }

    if (tool_name == "grep_files" && nzchar(args$pattern %||% "")) {
        lines <- c(lines, sprintf("Pattern: %s", .sanitize_inline(args$pattern)))
    }
    if (tool_name == "web_search" && nzchar(args$query %||% "")) {
        lines <- c(lines, sprintf("Query: %s", .sanitize_inline(args$query)))
    }
    if (tool_name == "r_help" && nzchar(args$topic %||% "")) {
        lines <- c(lines, sprintf("Topic: %s", .sanitize_inline(args$topic)))
    }

    if (!length(lines)) {
        preview <- cli_tool_preview(tool_name, args, width = width)
        if (nzchar(preview)) {
            lines <- .cli_wrap_lines(preview, width = width)
        }
    }

    lines
}

cli_call_access_lines <- function(call, cwd = NULL) {
    call$paths <- call$paths %||% resolve_paths(call)
    call$urls <- call$urls %||% resolve_urls(call)

    # Model-controlled tool name; sanitize so the "Tool: %s" fallback (and the
    # verb phrases below) can't forge a line.
    tool <- .sanitize_inline(call$tool %||% "")
    op <- classify_op(tool)
    paths <- unique(call$paths %||% character())
    urls <- unique(call$urls %||% character())
    if (length(paths) > 0L) {
        path_str <- .sanitize_inline(paths[[1L]])
    } else {
        path_str <- ""
    }
    if (length(urls) > 0L) {
        url_str <- .sanitize_inline(urls[[1L]])
    } else {
        url_str <- ""
    }

    line <- if (tool %in% c("run_r", "run_r_script")) {
        "Run R code"
    } else if (tool %in% c("bash", "cmd")) {
        if (nzchar(cwd)) {
            sprintf("Run command in %s", cwd)
        } else {
            "Run command"
        }
    } else if (nzchar(url_str)) {
        sprintf("%s %s",
                switch(op, read = "Fetch", write = "Send to", "Access"),
                url_str)
    } else if (nzchar(path_str)) {
        verb <- switch(op,
                       read = "Read from",
                       write = "Write to",
                       sprintf("%s on", tools::toTitleCase(op)))
        sprintf("%s %s", verb, path_str)
    } else {
        # Fallback when neither a path nor a URL resolved. Better than
        # showing nothing at all but indicates the tool is non-standard.
        sprintf("Tool: %s", tool)
    }

    line
}

#' Approval-prompt warnings, both the boilerplate and the noteworthy
#' kinds. Tests still call this directly; the user-facing approval
#' prompt now filters via \code{cli_call_noteworthy_warnings()} so the
#' usual "bash can invoke scripts" reminders no longer clutter every
#' single prompt.
#' @noRd
cli_call_warning_lines <- function(call, cwd = NULL, decision = NULL) {
    call$paths <- call$paths %||% resolve_paths(call)
    warnings <- character()

    if ((call$tool %||% "") %in% c("bash", "cmd")) {
        warnings <- c(
                      warnings,
                      "Shell commands can invoke scripts, hooks, and other executables from the working directory."
        )
    }
    if ((call$tool %||% "") %in% c("run_r", "run_r_script")) {
        warnings <- c(
                      warnings,
                      "R code runs locally with access to your current session, packages, and project files."
        )
    }
    if (!is.null(cwd) && nzchar(cwd) && length(call$paths) > 0L) {
        outside <- vapply(call$paths,
                          function(path) !is_path_under(path, cwd), logical(1))
        if (any(outside)) {
            warnings <- c(
                          warnings,
                          "Some referenced paths are outside the current project directory."
            )
        }
    }
    if (!is.null(decision$reason) &&
        grepl("credential path", decision$reason, fixed = TRUE)) {
        warnings <- c(warnings, "This request touches a credential path.")
    }

    warnings
}

#' Noteworthy warnings only — the ones a user genuinely needs to see
#' on each approval prompt. The generic "bash can invoke scripts" /
#' "R code runs locally" reminders are skipped: they're true for every
#' bash and run_r call respectively, so they add noise rather than
#' signal. Credential-touching calls, outside-project paths, and other
#' policy-flagged conditions stay.
#' @noRd
cli_call_noteworthy_warnings <- function(call, cwd = NULL, decision = NULL) {
    all <- cli_call_warning_lines(call, cwd = cwd, decision = decision)
    boilerplate <- c(
                     "Shell commands can invoke scripts, hooks, and other executables from the working directory.",
                     "R code runs locally with access to your current session, packages, and project files."
    )
    all[!all %in% boilerplate]
}

#' One-line explanation of what a tool call intends to do, for the
#' approval prompt -- distinct from the policy reason. Path/URL tools
#' (read/write/fetch/grep/search) template deterministically from their
#' args; opaque exec tools (bash/run_r) instead surface the model's own
#' narration from the per-call context, the only thing that conveys
#' intent there. NULL when nothing useful is available.
#' @noRd
cli_tool_explanation <- function(call) {
    tool <- call$tool %||% ""
    args <- .cli_args_list(call$args %||% list())
    # Every templated field is model-controlled, so sanitize each one (strip
    # ANSI/control chars incl. newlines, bound length) before interpolating --
    # otherwise a crafted path/query could forge extra approval-prompt lines.
    fld <- function(x, fallback) {
        s <- .sanitize_inline(x %||% "", max_chars = 120L)
        if (length(s) == 1L && nzchar(s)) s else fallback
    }
    templated <- switch(tool,
                        read_file       = sprintf("Read %s.", fld(args$path, "the file")),
                        list_files      = sprintf("List %s.", fld(args$path, ".")),
                        grep_files      = sprintf("Search files for /%s/.", fld(args$pattern, "")),
                        write_file      = sprintf("Write %s.", fld(args$path, "the file")),
                        replace_in_file = sprintf("Edit %s.", fld(args$path, "the file")),
                        fetch_url       = sprintf("Fetch %s.", fld(args$url, "the URL")),
                        web_search      = sprintf("Search the web for \"%s\".", fld(args$query, "")),
                        NULL)
    if (!is.null(templated)) {
        return(templated)
    }
    # bash / cmd / run_r / run_r_script and other opaque tools: the command
    # or code is shown but not the intent, so lean on the model's narration.
    .bounded_rationale(call$model_context$assistant_text)
}

#' Reduce a model-controlled string to one safe inline fragment for the
#' approval prompt: strip complete ANSI/VT escape sequences first, then any
#' remaining control characters (including newlines, so it can't forge extra
#' lines), collapse whitespace, and cap the length. "" when empty.
#' @noRd
.sanitize_inline <- function(x, max_chars = 200L) {
    x <- as.character(x)
    if (length(x) == 0L) {
        return(character(0))
    }
    x[is.na(x)] <- ""
    # Strip whole escape sequences (params included) so the control-char pass
    # below can't leave a visible "[31m" or OSC payload behind: OSC strings
    # (terminated by BEL or ST), then CSI sequences, then any stray ESC.
    x <- gsub("\033\\][^\007\033]*(\007|\033\\\\)?", "", x)
    x <- gsub("\033\\[[0-9;?]*[ -/]*[@-~]", "", x)
    x <- gsub("\033", "", x)
    x <- gsub("[[:cntrl:]]", " ", x)
    x <- trimws(gsub("[[:space:]]+", " ", x))
    long <- nchar(x) > max_chars
    x[long] <- paste0(substr(x[long], 1L, max_chars - 3L), "...")
    x
}

#' Flatten the model's turn narration to one sanitized, bounded line for
#' the approval prompt. NULL when empty.
#' @noRd
.bounded_rationale <- function(text, max_chars = 200L) {
    if (is.null(text)) {
        return(NULL)
    }
    one_line <- .sanitize_inline(as.character(text)[1L], max_chars = max_chars)
    if (nzchar(one_line)) one_line else NULL
}

cli_approval_lines <- function(call, decision = NULL, gate_reason = NULL,
                               cwd = NULL, persistent_label = "Allow always",
                               deny_label = "Deny", width = 88L) {
    call$paths <- call$paths %||% resolve_paths(call)
    call$urls <- call$urls %||% resolve_urls(call)

    title <- cli_tool_label(call$tool %||% "", long = TRUE)
    details <- cli_tool_detail_lines(call$tool %||% "", call$args %||% list(),
                                     cwd = cwd, width = width - 6L)
    access <- cli_call_access_lines(call, cwd = cwd)
    warnings <- cli_call_noteworthy_warnings(call, cwd = cwd,
        decision = decision)

    # The Access line already names the path/URL. Skip detail lines
    # that duplicate that name so we don't show the path three times
    # (title preview, detail, Access).
    if (length(details) > 0L) {
        path_in_access <- regmatches(
                                     access,
                                     regexpr("(?:Read from|Write to|Fetch|Send to|.* on)\\s+(.+)$",
                access, perl = TRUE)
        )
        if (length(path_in_access) > 0L) {
            access_path <- sub("^[A-Za-z ]+\\s+", "", path_in_access)
            details <- details[!grepl(sprintf("^Path:\\s*%s$",
                        regex_escape(access_path)),
                                      details)]
        }
    }

    lines <- c("", strrep("-", width), sprintf(" %s", title), "")

    explanation <- cli_tool_explanation(call)
    if (!is.null(explanation) && nzchar(explanation)) {
        lines <- c(lines, paste0("   ", .cli_wrap_lines(explanation, width - 6L)), "")
    }

    if (length(details) > 0L) {
        lines <- c(lines, paste0("   ", details), "")
    }

    lines <- c(lines, " Access", paste0("   ", access), "")

    if (length(warnings) > 0L) {
        lines <- c(lines, " Warning", paste0("   ", warnings), "")
    }

    c(
        lines,
        " Do you want to proceed?",
        "   1. Allow once (Enter)",
        sprintf("   2. %s", persistent_label),
        sprintf("   3. %s", deny_label)
    )
}

# Tiny helper: escape regex special chars in a literal string so we can
# embed it in a pattern. Used to test whether a detail line repeats the
# path name already shown on the Access line.
# @noRd
regex_escape <- function(x) {
    gsub("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1", x,
         perl = TRUE)
}

#' Build the action phrase shown in the `User replied:` summary after
#' an approval prompt resolves. Used by both the CLI and chat()
#' surfaces so the wording is consistent.
#'
#' Examples:
#' \itemize{
#'   \item replace_in_file CLAUDE.md, choice 1 → "Allow writing to CLAUDE.md once"
#'   \item bash `git status`, choice 2 → "Always allow running `git status` for this project"
#'   \item run_r, choice 3 → "Deny running R code"
#' }
#'
#' @param call The call list (tool + args + resolved paths/urls).
#' @param choice "1", "2", or "3".
#' @param persistent_label The "always allow" label text (varies by
#'   surface: "Allow always for this project" vs ".. for this session").
#' @param cwd Working directory; passed through for command-style
#'   paraphrases.
#' @return Character scalar.
#' @noRd
cli_user_replied_line <- function(call, choice,
                                  persistent_label = "Allow always for this project",
                                  cwd = NULL) {
    call$paths <- call$paths %||% resolve_paths(call)
    call$urls <- call$urls %||% resolve_urls(call)
    tool <- .sanitize_inline(call$tool %||% "")
    op <- classify_op(tool)
    paths <- unique(call$paths %||% character())
    urls <- unique(call$urls %||% character())

    # target is model-controlled (command / path / url) and this summary is
    # written into the history the model reads next turn, so sanitize it -- a
    # forged newline must not inject a line there.
    target <- if (tool %in% c("bash", "cmd")) {
        cmd <- .sanitize_inline(call$args$command %||% "", max_chars = 40L)
        if (nzchar(cmd)) {
            sprintf("`%s`", cmd)
        } else {
            "shell command"
        }
    } else if (tool %in% c("run_r", "run_r_script")) {
        "R code"
    } else if (length(paths) > 0L) {
        .sanitize_inline(paths[[1L]])
    } else if (length(urls) > 0L) {
        .sanitize_inline(urls[[1L]])
    } else {
        tool
    }

    verb <- if (tool %in% c("run_r", "run_r_script")) {
        "running"
    } else if (tool %in% c("bash", "cmd")) {
        "running"
    } else {
        switch(op, read = "reading", write = "writing to", exec = "running",
               sprintf("using %s on", tool))
    }

    action <- sprintf("%s %s", verb, target)

    # Translate the persistent label down to the short "for this
    # <scope>" phrasing used in the summary. Catches both the CLI's
    # "Allow always for this project" and chat()'s "... for this
    # session".
    scope <- sub("^Allow always(\\s+for\\s+)", "\\1",
                 persistent_label, perl = TRUE)
    if (identical(scope, persistent_label)) {
        scope <- "for this project"
    }

    switch(choice,
           "1" = sprintf("Allow %s once", action),
           "2" = sprintf("Always allow %s %s", action, scope),
           "3" = sprintf("Deny %s", action),
           sprintf("Choice %s on %s", choice, tool))
}

cli_event_summary <- function(event, width = 88L) {
    tool <- event$tool %||% (event$call$tool %||% "")
    args <- .cli_args_list(event$args %||% (event$call$args %||% list()))
    preview <- cli_tool_preview(tool, args, width = width - 20L)
    title <- cli_tool_label(tool)
    if (nzchar(preview)) {
        title <- sprintf("%s(%s)", title, preview)
    }

    if (identical(event$event, "tool_call") ||
        identical(event$outcome, "start")) {
        return(list(
                    kind = "start",
                    title = title,
                    detail_lines = cli_tool_detail_lines(tool, args,
                    width = width - 6L)
            ))
    }

    if (identical(event$event, "tool_result") ||
        identical(event$outcome, "ran")) {
        success <- isTRUE(event$success)
        lines <- event$result_lines
        if (is.null(lines)) {
            result <- event$result %||% ""
            lines <- if (nzchar(result)) {
                length(strsplit(result, "\n", fixed = TRUE)[[1]])
            } else {
                0L
            }
        }
        elapsed <- round(event$elapsed_ms %||% 0)
        detail <- sprintf(
                          "%d line%s in %dms",
                          lines,
            if (identical(lines, 1L)) "" else "s",
                          elapsed
        )
        return(list(
                    kind = if (success) "ok" else "error",
                    title = cli_tool_label(tool),
                    detail_lines = detail
            ))
    }

    if (!is.null(event$level) && event$level %in% c("warn", "error")) {
        return(list(
                    kind = event$level,
                    title = event$level,
                    detail_lines = event$message %||% (event$event %||% "")
            ))
    }

    list(
         kind = event$outcome %||% (event$event %||% "other"),
         title = title,
         detail_lines = character()
    )
}

