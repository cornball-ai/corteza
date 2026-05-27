# Slash-command helpers shared between corteza::chat() and the
# inst/bin/corteza CLI surface. The chat() loop has historically only
# handled /quit and /clear; this file ports the rest of the high-value
# CLI commands so chat() users don't have to drop down to the binary
# to spawn / query / kill subagents.

#' Output cap for staged local-eval results (`!` and `/r`). Above this
#' size the staged version replaces the raw output with a truncation
#' note so the LLM doesn't get flooded by a single `find /` or `cat
#' big.csv`. The on-screen output stays full -- only the staged
#' version is truncated.
#' @noRd
.LOCAL_EVAL_STAGE_CAP <- 4000L

#' Run a `! <cmd>` shell line locally. Uses bash on unix and
#' `cmd /c` on Windows -- the same split the corteza `bash` tool
#' uses for cross-OS consistency. stderr is folded into stdout so
#' the user sees error output too. Runs in the session cwd so it
#' tracks what the LLM's tools see.
#'
#' This is a *user-local shortcut*, not an LLM tool. It runs in
#' the main R process, bypassing `policy()` / `/permissions` /
#' `/dryrun` -- the same way `/r <expr>` does. The premise: the
#' user typing `! cmd` is itself the authorization, and a deny /
#' approval prompt for output they explicitly asked to see would
#' be noise. The staged output is still sent to the LLM normally
#' on the next turn.
#'
#' Returns a list with:
#'   $text   the on-screen string (full output)
#'   $staged the version queued for the next LLM message (capped
#'           at `.LOCAL_EVAL_STAGE_CAP`)
#' @noRd
run_bang_shell <- function(cmd, cwd = getwd()) {
    is_unix <- .Platform$OS.type == "unix"
    # processx passes each arg literally (no shell word-splitting), so
    # the command goes through as a single `-c` arg, unquoted. Unlike
    # system2(stdout = TRUE) -- a blocking C call R can't interrupt --
    # processx::run() polls and is interruptible: Ctrl+C terminates the
    # child (cleanup_tree kills descendants) and raises an interrupt the
    # caller's handler returns to the prompt on.
    args <- if (is_unix) {
        c("-c", cmd)
    } else {
        c("/c", cmd)
    }
    shell_bin <- if (is_unix) {
        "bash"
    } else {
        Sys.getenv("COMSPEC", "cmd.exe")
    }
    raw <- tryCatch(
                    processx::run(shell_bin, args, wd = cwd, error_on_status = FALSE,
                                  stderr_to_stdout = TRUE, cleanup_tree = TRUE)$stdout,
                    error = function(e) paste("Error:", conditionMessage(e))
    )
    # processx returns stdout as one string with a trailing newline;
    # strip trailing newlines to match the old line-vector join.
    text <- sub("\n+$", "", as.character(raw))
    if (!nzchar(text)) {
        text <- ""
    }
    staged <- if (nchar(text) > .LOCAL_EVAL_STAGE_CAP) {
        sprintf("(%d chars of output truncated; showing first %d)\n%s",
                nchar(text), .LOCAL_EVAL_STAGE_CAP,
                substr(text, 1L, .LOCAL_EVAL_STAGE_CAP))
    } else {
        text
    }
    list(text = text, staged = staged)
}

#' setwd() inside a function and restore on exit. Tiny base-R
#' equivalent of withr::with_dir to avoid a Suggests entry just for
#' `! <cmd>`.
#' @noRd
withr_local_dir <- function(dir, expr) {
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE)
    force(expr)
}

#' Is `code` a syntactically complete R expression?
#'
#' Returns TRUE when `parse(text = code)` succeeds, or when it
#' fails with a real syntax error (not worth waiting for more
#' input). Returns FALSE only when the parser ran out of tokens
#' mid-expression -- the caller can then read another line and
#' retry.
#'
#' Two distinct "incomplete" parser signals are honored: the
#' usual "unexpected end of input" (open paren / brace / op
#' waiting for a right-hand side) and "unexpected INCOMPLETE_STRING"
#' (a quoted string with no closing quote).
#'
#' Used by `corteza::chat()`'s `/r` handler to mimic R's normal
#' "+" continuation prompt: an RStudio addin Ctrl+Enter on a
#' multi-line `lm(y ~ x,\n   data = df)` arrives as two separate
#' readline cycles; the first is incomplete so we wait for the
#' second before evaluating.
#' @noRd
.r_expr_complete <- function(code) {
    err <- tryCatch(parse(text = code), error = identity)
    if (!inherits(err, "error")) {
        return(TRUE)
    }
    msg <- conditionMessage(err)
    !(grepl("end of input", msg, fixed = TRUE) ||
        grepl("INCOMPLETE_STRING", msg, fixed = TRUE))
}

#' Eval an R expression locally for `/r <expr>`. Mirrors
#' `run_bang_shell`'s shape so both local-eval flows share a
#' return type. The staged version for the next LLM message swaps
#' an oversized printed result for `str()` of the same value, since
#' a printed data frame or vector can easily be tens of thousands
#' of tokens.
#'
#' Like `! <cmd>`, this is a user-local shortcut and bypasses
#' `policy()` / `/permissions` / `/dryrun`. The user typing `/r`
#' is the authorization.
#'
#' Returns a list with:
#'   $text   the on-screen string
#'   $staged the version queued for the next LLM message
#' @noRd
run_r_eval <- function(code) {
    r_env <- new.env(parent = emptyenv())
    result_lines <- tryCatch(
                             utils::capture.output({
        r_env$r <- withVisible(eval(parse(text = code), envir = .GlobalEnv))
        if (r_env$r$visible) {
            print(r_env$r$value)
        }
    }),
                             error = function(e) {
        r_env$r <- NULL
        paste("Error:", conditionMessage(e))
    }
    )
    text <- paste(result_lines, collapse = "\n")
    staged <- if (nchar(text) > .LOCAL_EVAL_STAGE_CAP && !is.null(r_env$r)) {
        str_lines <- tryCatch(
                              utils::capture.output(utils::str(r_env$r$value)),
                              error = function(e) paste("Error:", conditionMessage(e))
        )
        sprintf("(%d chars of output truncated; showing str())\n%s",
                nchar(text), paste(str_lines, collapse = "\n"))
    } else {
        text
    }
    list(text = text, staged = staged)
}

#' Pull `--flag <value>` pairs out of a /spawn argument string.
#'
#' Mirrors the parser used by the inst/bin/corteza CLI's `/spawn`
#' branch. Order-independent. `--tools` is comma-split.
#' @param text Argument tail after the `/spawn` token.
#' @return List with `task`, `model`, `preset`, `tools` fields. `tools`
#'   is a character vector or NULL.
#' @noRd
parse_spawn_flags <- function(text) {
    extract <- function(text, flag) {
        pat <- paste0("\\s*", flag, "\\s+(\\S+)")
        loc <- regexpr(pat, text)
        if (loc == -1L) {
            return(list(text = text, value = NULL))
        }
        matched <- regmatches(text, loc)
        value <- sub(paste0("^\\s*", flag, "\\s+"), "", matched)
        list(text = trimws(sub(pat, "", text)), value = value)
    }
    p <- extract(text, "--model")
    text <- p$text
    model <- p$value

    p <- extract(text, "--preset")
    text <- p$text
    preset <- p$value

    p <- extract(text, "--tools")
    text <- p$text
    if (!is.null(p$value)) {
        tools <- strsplit(p$value, ",")[[1]]
    } else {
        tools <- NULL
    }

    list(task = trimws(text), model = model, preset = preset, tools = tools)
}

#' Run a manual memory flush as one in-process agent turn.
#'
#' Shared by the `/flush` slash command in `run_repl_loop()` for both
#' the chat() and CLI surfaces. The flush is just another agent turn:
#' same provider, model, system prompt, tools, and approval gate, but
#' pointed at the configured `memory_flush_prompt` with the current
#' conversation history as context. Routing through `turn()` (not raw
#' `agent()`) keeps the policy + approval path consistent, so a flush
#' that decides to call `write_file` still respects `config$permissions`.
#'
#' Tools execute IN-PROCESS via `turn()`'s default `call_skill`
#' dispatcher (no `tool_executor`), so any subagents the flush spawns
#' land in the one shared `.subagent_registry`.
#'
#' @param ctx The REPL context env (see `run_repl_loop()`). Reads
#'   `ctx$session` (live turn session, for system prompt / provider /
#'   model / tools_filter / history), `ctx$config`, and `ctx$cwd`.
#' @return A list with `content` (the flush reply text) and `history`
#'   (the flush session's history), or NULL when the flush was denied
#'   or errored. Errors and denials are reported via `message()`.
#' @noRd
run_memory_flush <- function(ctx) {
    session <- ctx$session
    config <- ctx$config
    flush_prompt <- config$memory_flush_prompt
    flush_history <- session$history %||% list()

    flush_session <- new_session(
                                 channel = session$channel %||% "cli",
                                 provider = session$provider %||% ctx$provider,
                                 model_map = list(
            cloud = resolve_provider_model(session$provider %||% ctx$provider,
                session$model_map$cloud %||% ctx$model),
            local = default_local_model()
        ),
                                 system = session$system,
                                 history = flush_history,
                                 approval_cb = session$approval_cb,
                                 tools_filter = session$tools_filter,
                                 max_turns = 20L
    )
    flush_session$config <- config
    flush_session$cwd <- ctx$cwd
    flush_session$dry_run <- isTRUE(session$dry_run) ||
    isTRUE(config$dry_run)

    tryCatch({
        r <- turn(prompt = flush_prompt, session = flush_session)
        list(content = r$reply, history = flush_session$history)
    }, corteza_user_deny = function(c) {
        message("Memory flush denied -- skipping.")
        NULL
    }, error = function(e) {
        message(sprintf("Flush failed: %s", e$message))
        NULL
    })
}

#' Slash-command help text for `chat()`.
#'
#' Mirrors the inst/bin/corteza CLI surface. A handful of CLI commands
#' that depend on terminal-only state (tool_buffer, color formatting,
#' opts) aren't yet shared; those are flagged as CLI-only.
#' @noRd
chat_help_text <- function() {
    paste(
          "",
          "Commands:",
          "  /quit, /exit, /q              Exit chat",
          "  /clear, /reset, /new          Clear conversation, keep transcript",
          "  /help                         Show this help",
          "  /tools                        List active tools",
          "  /model <name>                 Switch model",
          "  /provider <name>              Switch provider (anthropic, openai, moonshot, ollama)",
          "  /context, /status             Session + context meter (model, dir, tokens by component)",
          "  /spent, /cost                 Approximate USD spent this run (main-agent turns)",
          "  /doctor                       Diagnostics: provider/git/context health",
          "  /config                       Active runtime configuration",
          "  /diff [ref]                   Colored git diff against HEAD or a ref",
          "  /review [ref]                 Review local changes with the current model",
          "  /last [N]                     Show tool output (1=most recent)",
          "  /outputs                      List recent tool outputs",
          "  /sessions                     List sessions for this directory",
          "  /trace [N]                    Show last N tool executions (default 20)",
          "  /permissions                  Show tool approval and sandbox settings",
          "  /dryrun                       Toggle dry-run mode (preview tools)",
          "  /plan [task]                  Toggle plan mode (reads only, LLM proposes plan)",
          "  /compact                      Summarize conversation to free context",
          "  /flush                        Write durable memories from the conversation",
          "  /paste [text]                 Multi-line input. Collects every line verbatim until `/end` (or Ctrl+D).",
          "  /copy                         Copy the last assistant response to the system clipboard.",
          "  /tasks [clear]                Show (or clear) the current task list.",
          "  /r <expr>                     Eval R expression locally (skips policy/dryrun); output staged for next prompt",
          "  ! <cmd>                       Run a shell command locally (skips policy/dryrun); output staged for next prompt",
          "",
          "Subagents:",
          "  /spawn <task>                 Spawn a subagent",
          "  /spawn <task> --model <name>  Spawn with specific model",
          "  /spawn <task> --preset <name> investigate (default), work, minimal",
          "  /spawn <task> --tools <a,b,c> Explicit tool filter",
          "  /agents                       List active subagents",
          "  /ask <id> <prompt>            Query a subagent (blocks for reply)",
          "  /queue <id> <prompt>          Fire a query and return; collect later",
          "  /collect <id>                 Collect a pending reply (NULL if still running)",
          "  /kill <id>                    Terminate a subagent",
          "",
          "Skills:",
          "  /skill list                   List installed skills",
          "  /skill install <path|url>     Install a skill (--force to reinstall)",
          "  /skill remove <name>          Remove a skill",
          "  /skill test <path>            Run skill tests",
          "",
          "Keys:",
          "  Esc                           Interrupt the current turn and return to the prompt.",
          "                                (RStudio's console intercepts Ctrl+C for copy. In the",
          "                                terminal ~/bin/corteza CLI the split is reversed:",
          "                                Ctrl+C interrupts, Esc does nothing.)",
          "",
          sep = "\n"
    )
}

#' Detect the runtime context the user is driving corteza from. Used by
#' `/copy` to choose a context-appropriate clipboard-fallback message.
#' Returns one of `"rstudio_server"`, `"rstudio_desktop"`, `"ssh"`, or
#' `"other"`.
#' @noRd
chat_clipboard_context <- function() {
    if (identical(Sys.getenv("RSTUDIO_PROGRAM_MODE"), "server")) {
        return("rstudio_server")
    }
    if (identical(Sys.getenv("RSTUDIO"), "1")) {
        return("rstudio_desktop")
    }
    if (nzchar(Sys.getenv("SSH_CONNECTION"))) {
        return("ssh")
    }
    "other"
}

#' Try to write `text` to the system clipboard via clipr. Returns TRUE on
#' success, FALSE if clipr is missing, the clipboard isn't reachable, or
#' the write itself fails. Warnings from clipr's xclip/xsel probing are
#' suppressed so they don't bleed into the chat output.
#' @noRd
chat_clipboard_write <- function(text) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
        return(FALSE)
    }
    if (!suppressWarnings(clipr::clipr_available())) {
        return(FALSE)
    }
    tryCatch({
        suppressWarnings(clipr::write_clip(text))
        TRUE
    },
             error = function(e) FALSE
    )
}

#' Emit an OSC 52 clipboard escape sequence so the user's *local*
#' terminal emulator writes `text` into their *local* system clipboard.
#' Works over SSH, screen, and tmux (when tmux passthrough is enabled).
#' Cannot detect whether the terminal actually honored the escape, so
#' callers should treat success as best-effort and pair with the file
#' fallback.
#'
#' Returns TRUE if the escape was emitted, FALSE if the environment is
#' clearly unsuitable (no /dev/tty, TERM is "dumb", text is too large,
#' or non-Unix).
#' @noRd
chat_osc52_write <- function(text) {
    if (.Platform$OS.type != "unix") {
        return(FALSE)
    }
    term <- Sys.getenv("TERM")
    if (!nzchar(term) || term == "dumb") {
        return(FALSE)
    }
    raw <- charToRaw(enc2utf8(text))
    # xterm and most terminals cap OSC 52 around 100k base64 chars; stay
    # well under to avoid silent truncation.
    if (length(raw) > 74000L) {
        return(FALSE)
    }
    b64 <- jsonlite::base64_enc(raw)
    esc <- paste0("\033]52;c;", b64, "\007")

    # tmux only forwards OSC 52 when (a) wrapped in DCS-passthrough
    # *and* (b) `set -g allow-passthrough on` is configured. (b) we
    # cannot detect; emit the wrapped form anyway and let tmux drop
    # it silently if disabled.
    if (nzchar(Sys.getenv("TMUX"))) {
        inner <- gsub("\033", "\033\033", esc, fixed = TRUE)
        esc <- paste0("\033Ptmux;", inner, "\033\\")
    }
    tryCatch({
        tty <- suppressWarnings(file("/dev/tty", "w"))
        on.exit(close(tty), add = TRUE)
        cat(esc, file = tty)
        TRUE
    },
             error = function(e) FALSE,
             warning = function(w) FALSE
    )
}

#' Resolve the on-disk file path used by `/copy`. Lives under
#' `tools::R_user_dir("corteza", "cache")` so the path is stable
#' across sessions (the user can scp / rsync from another device) and
#' CRAN-clean (no writes under the user's home filespace by package
#' default).
#' @noRd
chat_copy_fallback_path <- function() {
    file.path(corteza_cache_dir(), "last-response.md")
}

#' Handle the `/copy` slash command. Always writes the response to a
#' file so the user has a recoverable copy; additionally attempts the
#' system clipboard (clipr) and, when that's unreachable, an OSC 52
#' terminal escape. Prints one terse status line.
#' @noRd
chat_handle_copy <- function(text) {
    if (!nzchar(text)) {
        cat("Nothing to copy.\n")
        return(invisible())
    }
    n <- nchar(text)
    path <- chat_copy_fallback_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, path)

    # On RStudio Server console, neither clipr (xclip) nor OSC 52 can
    # reach the browser-side clipboard, and clipr's xclip probing
    # leaks warnings through suppressWarnings() under RStudio Server.
    # Skip both transports there and go straight to the file.
    ctx <- chat_clipboard_context()
    clipped <- if (ctx == "rstudio_server") {
        FALSE
    } else {
        chat_clipboard_write(text) || chat_osc52_write(text)
    }

    if (clipped) {
        cat(sprintf("Copied (%d chars) to clipboard | Saved to %s\n", n, path))
    } else {
        cat(sprintf("Saved (%d chars) to %s\n", n, path))
    }
    invisible()
}

#' Format the active tool list for /tools.
#' @noRd
chat_format_tools_list <- function(turn_session) {
    api_tools <- tryCatch(skills_as_api_tools(turn_session$tools_filter),
                          error = function(e) list())
    if (length(api_tools) == 0L) {
        return("No tools active.\n")
    }
    lines <- "Active tools:"
    for (tool in api_tools) {
        nm <- tool$name %||% tool[["function"]]$name %||% "?"
        desc <- tool$description %||% tool[["function"]]$description %||% ""
        lines <- c(lines, sprintf("  %s - %s", nm, desc))
    }
    paste(c(lines, ""), collapse = "\n")
}

