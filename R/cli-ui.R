.prompt_input_state <- new.env(parent = emptyenv())
.prompt_input_state$stdin_con <- NULL

.read_prompt_via_bash <- function(prompt_str = "> ") {
    cat(prompt_str)
    utils::flush.console()

    script <- paste(
        'out="$1"',
        'IFS= read -r -e line || exit 1',
        'printf "%s\\n" "$line" > "$out"',
        'while IFS= read -r -t 0.01 next; do',
        '  printf "%s\\n" "$next" >> "$out"',
        'done',
        sep = "\n"
    )

    path <- tempfile("corteza-prompt-")
    on.exit(unlink(path), add = TRUE)
    status <- suppressWarnings(
        system2(
            "bash",
            c("-c", shQuote(script), "bash", shQuote(path)),
            stdout = "",
            stderr = ""
        )
    )
    if (!is.null(status) && status != 0L) {
        return(character())
    }
    if (!file.exists(path)) {
        return(character())
    }
    readLines(path, warn = FALSE)
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
        out <- tryCatch(
            .read_prompt_via_bash(prompt_str),
            error = function(e) NULL
        )
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
    label <- switch(
        tool_name,
        bash = "Bash",
        cmd = "Command",
        run_r = "Run R",
        run_r_script = "Run R Script",
        read_file = "Read File",
        "base::readLines" = "Read File",
        write_file = "Write File",
        "base::writeLines" = "Write File",
        replace_in_file = "Replace in File",
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
        tools::toTitleCase(gsub("_", " ", gsub("::", " ", tool_name)))
    )

    if (!isTRUE(long)) {
        return(label)
    }

    switch(
        tool_name,
        bash = "Bash command",
        cmd = "System command",
        run_r = "Run R code",
        run_r_script = "Run R script",
        read_file = "Read file",
        "base::readLines" = "Read file",
        write_file = "Write file",
        "base::writeLines" = "Write file",
        replace_in_file = "Replace in file",
        list_files = "List files",
        "base::list.files" = "List files",
        grep_files = "Search files",
        web_search = "Web search",
        fetch_url = "Fetch URL",
        git_status = "Git status",
        git_diff = "Git diff",
        git_log = "Git log",
        r_help = "R help",
        installed_packages = "List installed packages",
        label
    )
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

    .cli_truncate(preview %||% "", width = width)
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

    call <- list(tool = tool_name, args = args)
    paths <- unique(resolve_paths(call))
    urls <- unique(resolve_urls(call))

    if (length(paths) > 0L) {
        lines <- c(lines, sprintf("Path: %s", paths))
    }
    if (length(urls) > 0L) {
        lines <- c(lines, sprintf("URL: %s", urls))
    }

    if (tool_name == "grep_files" && nzchar(args$pattern %||% "")) {
        lines <- c(lines, sprintf("Pattern: %s", args$pattern))
    }
    if (tool_name == "web_search" && nzchar(args$query %||% "")) {
        lines <- c(lines, sprintf("Query: %s", args$query))
    }
    if (tool_name == "r_help" && nzchar(args$topic %||% "")) {
        lines <- c(lines, sprintf("Topic: %s", args$topic))
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

    op <- classify_op(call$tool %||% "")
    lines <- switch(
        op,
        read = "Read access to files or external content",
        write = "Write access to local files",
        exec = if ((call$tool %||% "") %in% c("run_r", "run_r_script")) {
            "Executes local R code"
        } else {
            "Executes local shell commands"
        },
        "Tool access"
    )

    if ((call$tool %||% "") %in% c("bash", "cmd") &&
        !is.null(cwd) && nzchar(cwd)) {
        lines <- c(lines, sprintf("Working directory: %s", cwd))
    }
    if (length(call$paths) > 0L) {
        lines <- c(lines, sprintf("Path: %s", unique(call$paths)))
    }
    if (length(call$urls) > 0L) {
        lines <- c(lines, sprintf("URL: %s", unique(call$urls)))
    }

    lines
}

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
        outside <- vapply(
            call$paths,
            function(path) !is_path_under(path, cwd),
            logical(1)
        )
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

cli_approval_lines <- function(call, decision = NULL, gate_reason = NULL,
                               cwd = NULL,
                               persistent_label = "Allow always",
                               width = 88L) {
    call$paths <- call$paths %||% resolve_paths(call)
    call$urls <- call$urls %||% resolve_urls(call)

    title <- cli_tool_label(call$tool %||% "", long = TRUE)
    details <- cli_tool_detail_lines(call$tool %||% "", call$args %||% list(),
                                     cwd = cwd, width = width - 6L)
    access <- cli_call_access_lines(call, cwd = cwd)
    reasons <- character()
    warnings <- cli_call_warning_lines(call, cwd = cwd, decision = decision)

    if (!is.null(gate_reason) && nzchar(gate_reason)) {
        reasons <- c(reasons, gate_reason)
    }
    if (!is.null(decision$reason) && nzchar(decision$reason)) {
        reasons <- c(reasons, sprintf("Policy: %s", decision$reason))
    }
    if (!is.null(decision$model) && nzchar(decision$model)) {
        reasons <- c(reasons, sprintf("Model route: %s", decision$model))
    }

    lines <- c(
        "",
        strrep("-", width),
        sprintf(" %s", title),
        ""
    )

    if (length(details) > 0L) {
        lines <- c(lines, paste0("   ", details), "")
    }

    lines <- c(lines, " Access", paste0("   ", access), "")

    if (length(reasons) > 0L) {
        lines <- c(lines, " Reason", paste0("   ", reasons), "")
    }

    if (length(warnings) > 0L) {
        lines <- c(lines, " Warning", paste0("   ", warnings), "")
    }

    c(
        lines,
        " Do you want to proceed?",
        "   1. Allow once",
        sprintf("   2. %s", persistent_label),
        "   3. Deny"
    )
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
            detail_lines = cli_tool_detail_lines(tool, args, width = width - 6L)
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
