# Slash-command helpers shared between corteza::chat() and the
# inst/bin/corteza CLI surface. The chat() loop has historically only
# handled /quit and /clear; this file ports the rest of the high-value
# CLI commands so chat() users don't have to drop down to the binary
# to spawn / query / kill subagents.

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
          "  /paste [text]                 Multi-line input. Collects every line verbatim until `/end` (or Ctrl+D).",
          "  /copy                         Copy the last assistant response to the system clipboard.",
          "  /r <expr>                     Eval R expression locally; output staged for next prompt",
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

#' Context-aware "clipboard not reachable" hint, used by `/copy` when the
#' system clipboard isn't writable from the current runtime.
#' @noRd
chat_clipboard_unavailable_hint <- function(ctx = chat_clipboard_context()) {
    switch(ctx,
           rstudio_server = paste(
                                  "RStudio Server can't reach your browser's clipboard from R.",
                                  "Select the response in the console and Ctrl+C to copy manually."),
           ssh = paste(
                       "Headless SSH session; no clipboard reachable from this side.",
                       "Select the response in your terminal to copy."),
           rstudio_desktop = paste(
                                   "Clipboard unavailable on this RStudio Desktop session.",
                                   "Install 'xclip' (apt install xclip) or 'wl-clipboard'."),
           # default / "other"
           paste(
                 "Clipboard not available.",
                 "Install the 'clipr' package, plus 'xclip' or 'wl-clipboard' on Linux."))
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

#' Resolve the on-disk fallback path used by `/copy` when no clipboard
#' transport works. On Unix, /tmp gives the user a stable, well-known
#' location they can rsync / scp from another device; on Windows fall
#' back to tempdir().
#' @noRd
chat_copy_fallback_path <- function() {
    if (.Platform$OS.type == "unix") {
        "/tmp/corteza_last_response.md"
    } else {
        file.path(tempdir(), "corteza_last_response.md")
    }
}

#' Handle the `/copy` slash command. Tries the system clipboard first
#' (clipr), then OSC 52 for terminals over SSH/tmux, then falls back to
#' a `/tmp/corteza_last_response.md` file with a context-aware hint.
#' @noRd
chat_handle_copy <- function(text) {
    if (!nzchar(text)) {
        cat("Nothing to copy yet.\n")
        return(invisible())
    }
    n <- nchar(text)

    if (chat_clipboard_write(text)) {
        cat(sprintf("Copied last response (%d chars).\n", n))
        return(invisible())
    }

    ctx <- chat_clipboard_context()
    # OSC 52 can't reach the browser-side clipboard of an RStudio
    # Server *console* session, so don't bother trying there.
    if (ctx != "rstudio_server" && chat_osc52_write(text)) {
        path <- chat_copy_fallback_path()
        writeLines(text, path)
        cat(sprintf(
                    "Sent OSC 52 clipboard escape (%d chars). If your terminal didn't capture it, the response is also at %s.\n",
                    n, path))
        return(invisible())
    }

    path <- chat_copy_fallback_path()
    writeLines(text, path)
    cat(chat_clipboard_unavailable_hint(ctx), "\n",
        sprintf("Wrote response to %s (%d chars).\n", path, n), sep = "")
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

