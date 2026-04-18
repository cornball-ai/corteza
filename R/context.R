# Context loading for corteza
# Loads project context for the system prompt. Project briefings are
# delegated to saber::briefing(). Custom user-specified files and
# skill docs are layered on top.

#' Load context for the system prompt
#'
#' Assembles a system prompt for the LLM by combining:
#' \enumerate{
#'   \item corteza's preamble
#'   \item \code{saber::briefing()} project metadata (if available)
#'   \item Any custom \code{context_files} from \code{.corteza/config.json}
#'   \item Loaded skill docs and package tool docs
#' }
#'
#' @param cwd Working directory.
#' @return Character string with assembled context, or NULL if empty.
#' @noRd
load_context <- function(cwd = getwd()) {
    config <- load_config(cwd)

    b <- new_context_builder()
    b <- add_context(b, paste(
                              "You are an AI assistant with access to tools for working with R and the file system.",
                              "Use the bash tool to run shell commands. Below is context about the current project",
                              "and available skills.",
                              sep = "\n"
        ))

    # Project briefing (DESCRIPTION, downstream deps, git log)
    b <- add_context(b, load_saber_briefing(cwd))

    # Agent context files. We call saber's agent_context() when the
    # installed version exports it (saber >= 0.4.0) AND our inlined
    # copy. Duplicate output gets deduped at the builder level, so
    # double-calling is benign whether the two produce identical or
    # divergent text.
    b <- add_context(b, load_saber_agent_context(cwd, config))
    b <- add_context(b, load_local_agent_context(cwd, config))

    # Custom user-specified context files (default: empty)
    custom_files <- config$context_files %||% character(0)
    for (name in custom_files) {
        path <- file.path(cwd, name)
        if (file.exists(path)) {
            content <- paste(readLines(path, warn = FALSE), collapse = "\n")
            if (nchar(content) > 0L) {
                b <- add_context(b, paste(
                        sprintf("## %s", basename(path)), "", content,
                        sep = "\n"
                    ))
            }
        }
    }

    # Skill docs
    skill_docs_text <- format_skill_docs()
    if (nchar(skill_docs_text) > 0) {
        b <- add_context(b, paste(
                                  "# Available Skills", "",
                                  "The following skills describe how to accomplish common tasks using shell commands.",
                                  "Use the bash tool to execute the commands shown.", "",
                                  skill_docs_text,
                                  sep = "\n"
            ))
    }

    # Package tool documentation
    pkg_docs <- format_pkg_skill_docs(config)
    if (nchar(pkg_docs %||% "") > 0) {
        b <- add_context(b, paste(
                                  "# Package Tools", "",
                                  "Documentation for R package functions available as tools.", "",
                                  pkg_docs,
                                  sep = "\n"
            ))
    }

    # If only the preamble made it in, there's nothing project-specific
    # to ship back.
    if (length(b$parts) <= 1L) {
        return(NULL)
    }
    paste(b$parts, collapse = "\n\n")
}

# ---- Context builder (dedupes exact-match blocks) ----
#
# Every producer of context (saber briefing, agent_context variants,
# custom files, skill docs, package docs) pushes into this builder.
# If two producers emit identical blocks, only the first is kept.
# Deliberately conservative: no fuzzy matching, no normalization
# beyond what the source produces. That's enough to handle the case
# where saber::agent_context() and our inlined fallback both run and
# produce the same text, or where two sources happen to load the
# same file.
#
# @noRd
new_context_builder <- function() {
    list(parts = character())
}

#' @noRd
add_context <- function(builder, text) {
    if (is.null(text)) {
        return(builder)
    }
    if (length(text) != 1L) {
        text <- paste(text, collapse = "\n")
    }
    text <- trimws(text, which = "right")
    if (!nzchar(text)) {
        return(builder)
    }
    if (text %in% builder$parts) {
        return(builder)
    }
    builder$parts <- c(builder$parts, text)
    builder
}

#' Load agent context via our inlined fallback copy
#'
#' Mirrors \code{load_saber_agent_context()} but always calls the
#' local \code{agent_context()} defined in \code{R/agent_context.R}.
#' Used alongside the saber call in \code{load_context()}; the
#' context builder dedupes identical output. Delete this function
#' and the corresponding call in \code{load_context()} once saber
#' >= 0.4.0 is required in DESCRIPTION.
#' @noRd
load_local_agent_context <- function(cwd, config) {
    workspace_dir <- get_workspace_dir()
    tryCatch({
        text <- agent_context(
                              agent = "corteza",
                              project_dir = cwd,
                              workspace_dir = workspace_dir,
                              include_soul = config$context_include_soul,
                              include_global = config$context_include_user
        )
        if (is.null(text) || nchar(trimws(text)) == 0L) NULL else text
    }, error = function(e) NULL)
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
    # Call saber::agent_context() via dynamic lookup so R CMD check's
    # static ::-scan doesn't flag a symbol that isn't in saber's CRAN
    # namespace yet. When saber doesn't export it (0.3.0 and earlier),
    # this returns NULL and load_local_agent_context() covers the feature
    # via the inlined copy in R/agent_context.R.
    if (!requireNamespace("saber", quietly = TRUE)) {
        return(NULL)
    }
    saber_fn <- tryCatch(
                         getExportedValue("saber", "agent_context"),
                         error = function(e) NULL
    )
    if (is.null(saber_fn)) {
        return(NULL)
    }

    workspace_dir <- get_workspace_dir()
    tryCatch({
        text <- saber_fn(
                         agent = "corteza",
                         project_dir = cwd,
                         workspace_dir = workspace_dir,
                         include_soul = config$context_include_soul,
                         include_global = config$context_include_user
        )
        if (is.null(text) || nchar(trimws(text)) == 0L) NULL else text
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

