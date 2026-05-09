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
    tools <- if (!is.null(p$value)) strsplit(p$value, ",")[[1]] else NULL

    list(task = trimws(text), model = model, preset = preset, tools = tools)
}

#' Slash-command help text for `chat()`.
#'
#' chat() doesn't ship the full CLI command set. List what's available
#' so users don't waste time guessing.
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
        "  /r <expr>                     Eval R expression locally; output staged for next prompt",
        "",
        "Subagents:",
        "  /spawn <task>                 Spawn a subagent",
        "  /spawn <task> --model <name>  Spawn with specific model",
        "  /spawn <task> --preset <name> investigate (default), work, minimal",
        "  /spawn <task> --tools <a,b,c> Explicit tool filter",
        "  /agents                       List active subagents",
        "  /ask <id> <prompt>            Query a subagent (id, prefix, or seq number)",
        "  /kill <id>                    Terminate a subagent",
        "",
        sep = "\n"
    )
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
