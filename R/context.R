# Context loading for llamaR
# Loads project context for the system prompt. Standard files (memory,
# SOUL.md, USER.md, CLAUDE.md, AGENTS.md) and project briefings are
# delegated to saber. Custom user-specified files and skill docs are
# layered on top.

#' Load context for the system prompt
#'
#' Assembles a system prompt for the LLM by combining:
#' \enumerate{
#'   \item llamaR's preamble
#'   \item \code{saber::briefing()} project metadata (if available)
#'   \item \code{saber::agent_context()} runtime context files (memory,
#'         SOUL.md, USER.md, CLAUDE.md, AGENTS.md)
#'   \item Optional daily memory logs (legacy, opt-in)
#'   \item Any custom \code{context_files} from \code{.llamar/config.json}
#'   \item Loaded skill docs and package tool docs
#' }
#'
#' @param cwd Working directory.
#' @return Character string with assembled context, or NULL if empty.
#' @noRd
load_context <- function(cwd = getwd()) {
    config <- load_config(cwd)

    parts <- c(
               "You are an AI assistant with access to tools for working with R and the file system.",
               "Use the bash tool to run shell commands. Below is context about the current project",
               "and available skills.",
               ""
    )

    # Project briefing (DESCRIPTION, downstream deps, git log)
    brief <- load_saber_briefing(cwd)
    if (!is.null(brief) && nchar(brief) > 0L) {
        parts <- c(parts, brief, "")
    }

    # Standard agent context files via saber (memory, SOUL, USER, CLAUDE, AGENTS)
    agent_ctx <- load_saber_agent_context(cwd, config)
    if (!is.null(agent_ctx) && nchar(agent_ctx) > 0L) {
        parts <- c(parts, agent_ctx, "")
    }

    # Legacy: daily memory logs (opt-in)
    if (isTRUE(config$context_include_memory_logs)) {
        memory_logs <- memory_log_load_all()
        if (!is.null(memory_logs)) {
            parts <- c(parts, "## Daily Memory Logs", "", memory_logs, "")
        }
    }

    # Custom user-specified context files (default: empty)
    custom_files <- config$context_files %||% character(0)
    for (name in custom_files) {
        path <- file.path(cwd, name)
        if (file.exists(path)) {
            content <- paste(readLines(path, warn = FALSE), collapse = "\n")
            if (nchar(content) > 0L) {
                parts <- c(parts, sprintf("## %s", basename(path)), "",
                           content, "")
            }
        }
    }

    # Skill docs
    skill_docs_text <- format_skill_docs()
    if (nchar(skill_docs_text) > 0) {
        parts <- c(parts,
                   "# Available Skills",
                   "",
                   "The following skills describe how to accomplish common tasks using shell commands.",
                   "Use the bash tool to execute the commands shown.",
                   "",
                   skill_docs_text
        )
    }

    # Package tool documentation
    pkg_docs <- format_pkg_skill_docs(config)
    if (!is.null(pkg_docs) && nchar(pkg_docs) > 0) {
        parts <- c(parts,
                   "# Package Tools",
                   "",
                   "Documentation for R package functions available as tools.",
                   "",
                   pkg_docs
        )
    }

    # If only the preamble remains, return NULL
    if (length(parts) <= 4) {
        return(NULL)
    }

    paste(parts, collapse = "\n")
}

#' Call saber::briefing() for project metadata
#'
#' Returns the briefing text or NULL on failure / when saber is unavailable.
#' @noRd
load_saber_briefing <- function(cwd) {
    if (!requireNamespace("saber", quietly = TRUE)) {
        return(NULL)
    }
    project <- basename(cwd)
    scan_dir <- dirname(cwd)
    tryCatch({
        # Suppress saber's cat() to stdout - we want the return value only
        utils::capture.output(
                              text <- saber::briefing(project = project, scan_dir = scan_dir)
        )
        if (is.null(text) || nchar(trimws(text)) == 0L) {
            NULL
        } else {
            text
        }
    }, error = function(e) NULL)
}

#' Call saber::agent_context() for runtime context files
#'
#' Returns the assembled context or NULL on failure / when saber is unavailable.
#' @noRd
load_saber_agent_context <- function(cwd, config) {
    if (!requireNamespace("saber", quietly = TRUE)) {
        return(NULL)
    }
    workspace_dir <- get_workspace_dir()
    tryCatch({
        text <- saber::agent_context(
                                     agent = "llamar",
                                     project_dir = cwd,
                                     workspace_dir = workspace_dir,
                                     include_soul = config$context_include_soul,
                                     include_global = config$context_include_user
        )
        if (is.null(text) || nchar(trimws(text)) == 0L) {
            NULL
        } else {
            text
        }
    }, error = function(e) NULL)
}

#' List custom context files that would be loaded
#'
#' Returns the configured \code{context_files} that exist in the project
#' directory. Standard files (memory, SOUL.md, USER.md, CLAUDE.md,
#' AGENTS.md) are loaded via saber and not included here.
#'
#' @param cwd Working directory.
#' @return Character vector of existing custom context file paths.
#' @noRd
list_context_files <- function(cwd = getwd()) {
    config <- load_config(cwd)
    file_names <- config$context_files %||% character(0)
    if (length(file_names) == 0L) {
        return(character(0))
    }
    paths <- file.path(cwd, file_names)
    paths[file.exists(paths)]
}

