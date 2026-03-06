# Context loading for llamaR
# Loads project context files to inject into the system prompt

#' Load context files from project directory
#'
#' Looks for context files in the current directory and assembles
#' them into a system prompt for the LLM. Also includes loaded skill docs.
#'
#' @param cwd Working directory to search
#' @return Character string with assembled context, or NULL if no files found
#' @noRd
load_context <- function(cwd = getwd()) {
    # Get file list from config (or defaults)
    file_names <- get_context_files(cwd)

    # Build full paths
    context_files <- file.path(cwd, file_names)

    # Assemble into system prompt
    parts <- c(
               "You are an AI assistant with access to tools for working with R and the file system.",
               "Use the bash tool to run shell commands. Below is context about the current project",
               "and available skills.",
               ""
    )

    # Load global context files from ~/.llamar/workspace/ (SOUL.md, USER.md, MEMORY.md)
    workspace_dir <- get_workspace_dir()
    global_files <- global_context_files()
    global_labels <- c(
                       "SOUL.md" = "Agent Identity (Global)",
                       "USER.md" = "User Preferences (Global)",
                       "MEMORY.md" = "User Memory (Global)"
    )
    for (gf in global_files) {
        global_path <- file.path(workspace_dir, gf)
        if (file.exists(global_path)) {
            content <- paste(readLines(global_path, warn = FALSE),
                             collapse = "\n")
            if (nchar(trimws(content)) > 0) {
                label <- global_labels[[gf]] %||% gf
                parts <- c(parts, sprintf("## %s", label), "", content, "")
            }
        }
    }

    # Load daily memory logs from ~/.llamar/workspace/memory/*.md
    memory_logs <- memory_log_load_all()
    if (!is.null(memory_logs)) {
        parts <- c(parts, "## Daily Memory Logs", "", memory_logs, "")
    }

    # Read existing project files
    contents <- list()
    for (f in context_files) {
        if (file.exists(f)) {
            content <- paste(readLines(f, warn = FALSE), collapse = "\n")
            if (nchar(content) > 0) {
                name <- basename(f)
                contents[[name]] <- content
            }
        }
    }

    for (name in names(contents)) {
        parts <- c(parts, sprintf("## %s", name), "", contents[[name]], "")
    }

    # Add skill docs if any are loaded
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

    # Add package tool documentation
    config <- load_config(cwd)
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

    if (length(parts) <= 4 && length(contents) == 0) {
        # Only preamble, no actual content
        return(NULL)
    }

    paste(parts, collapse = "\n")
}

#' List context files that would be loaded
#'
#' @param cwd Working directory to search
#' @return Character vector of existing context file paths
#' @noRd
list_context_files <- function(cwd = getwd()) {
    # Get file list from config (or defaults)
    file_names <- get_context_files(cwd)

    # Build full paths
    context_files <- file.path(cwd, file_names)

    # Return only existing files
    context_files[file.exists(context_files)]
}

