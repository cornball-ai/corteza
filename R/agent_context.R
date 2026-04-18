# Fallback copy of saber::agent_context() for use when the installed
# saber predates 0.4.0 (where agent_context first appears). Kept in
# sync with saber's implementation; delete this file once saber >=
# 0.4.0 is required in DESCRIPTION.
#
# Not exported. The corresponding gate in load_saber_agent_context()
# uses getExportedValue("saber", "agent_context") first and falls
# through to this copy only when saber's own export is absent.
#
# Ported verbatim from saber/R/agent_context.R (saber 0.4.0).

agent_context <- function(agent = NULL, project_dir = getwd(),
                          workspace_dir = NULL,
                          memory_base = file.path(path.expand("~"), ".claude", "projects"),
                          claude_global_path = file.path(path.expand("~"), ".claude", "CLAUDE.md"),
                          include_memory = NULL, include_project = NULL,
                          include_global = NULL, include_soul = NULL,
                          max_memory_lines = 100L) {
    if (is.null(agent)) {
        agent_key <- NA_character_
    } else {
        agent_key <- as.character(agent)[1L]
    }

    defaults <- agent_context_defaults(agent_key)
    incl_mem <- include_memory %||% defaults$memory
    incl_proj <- include_project %||% defaults$project
    incl_glob <- include_global %||% defaults$global
    incl_soul <- include_soul %||% defaults$soul

    parts <- character(0L)

    if (isTRUE(incl_mem)) {
        mem <- agent_context_memory(project_dir, memory_base, max_memory_lines)
        if (length(mem) > 0L) {
            parts <- c(parts, mem, "")
        }
    }
    if (isTRUE(incl_proj)) {
        proj <- agent_context_project(project_dir, agent_key,
                                      forced = !is.null(include_project))
        if (length(proj) > 0L) {
            parts <- c(parts, proj, "")
        }
    }
    if (isTRUE(incl_glob)) {
        glob <- agent_context_global(workspace_dir, agent_key,
                                     claude_global_path,
                                     forced = !is.null(include_global))
        if (length(glob) > 0L) {
            parts <- c(parts, glob, "")
        }
    }
    if (isTRUE(incl_soul) && !is.null(workspace_dir)) {
        soul <- agent_context_soul(workspace_dir)
        if (length(soul) > 0L) {
            parts <- c(parts, soul, "")
        }
    }

    paste(parts, collapse = "\n")
}

agent_context_defaults <- function(agent) {
    if (is.na(agent)) {
        return(list(memory = TRUE, project = TRUE, global = TRUE, soul = TRUE))
    }
    switch(agent,
           claude = list(memory = FALSE, project = TRUE,
                         global = TRUE, soul = TRUE),
           codex = list(memory = TRUE, project = TRUE, global = TRUE, soul = TRUE),
           corteza = list(memory = TRUE, project = TRUE,
                          global = TRUE, soul = TRUE),
           list(memory = TRUE, project = TRUE, global = TRUE, soul = TRUE)
    )
}

agent_context_memory <- function(project_dir, memory_base, max_lines) {
    if (is.null(memory_base) || !dir.exists(memory_base)) {
        return(character(0L))
    }

    project <- basename(normalizePath(project_dir, mustWork = FALSE))
    mem_file <- NULL
    mem_dirs <- list.dirs(memory_base, recursive = FALSE, full.names = TRUE)
    for (md in mem_dirs) {
        proj_encoded <- basename(md)
        proj_name <- sub("^.*-home-[^-]+-", "", proj_encoded)
        if (proj_name == project) {
            candidate <- file.path(md, "memory", "MEMORY.md")
            if (file.exists(candidate)) {
                mem_file <- candidate
                break
            }
        }
    }
    if (is.null(mem_file)) {
        return(character(0L))
    }

    mem_lines <- readLines(mem_file, warn = FALSE)
    lines <- "## Memory"
    if (length(mem_lines) > max_lines) {
        lines <- c(lines, mem_lines[seq_len(max_lines)],
                   sprintf("_... truncated (%d more lines)_",
                           length(mem_lines) - max_lines))
    } else {
        lines <- c(lines, mem_lines)
    }
    lines
}

agent_context_project <- function(project_dir, agent, forced = FALSE) {
    claude_path <- file.path(project_dir, "CLAUDE.md")
    agents_path <- file.path(project_dir, "AGENTS.md")
    claude_exists <- file.exists(claude_path)
    agents_exists <- file.exists(agents_path)

    if (!claude_exists && !agents_exists) {
        return(character(0L))
    }

    file_to_load <- NULL
    if (forced || is.na(agent)) {
        if (claude_exists) {
            file_to_load <- claude_path
        } else {
            file_to_load <- agents_path
        }
    } else if (identical(agent, "claude")) {
        if (agents_exists && !same_file(claude_path, agents_path)) {
            file_to_load <- agents_path
        }
    } else if (identical(agent, "codex")) {
        if (claude_exists && !same_file(claude_path, agents_path)) {
            file_to_load <- claude_path
        }
    } else {
        if (claude_exists) {
            file_to_load <- claude_path
        } else {
            file_to_load <- agents_path
        }
    }
    if (is.null(file_to_load)) {
        return(character(0L))
    }

    content <- tryCatch(readLines(file_to_load, warn = FALSE),
                        error = function(e) character(0L))
    if (length(content) == 0L) {
        return(character(0L))
    }
    c(sprintf("## %s", basename(file_to_load)), "", content)
}

agent_context_global <- function(workspace_dir, agent, claude_global,
                                 forced = FALSE) {
    user_path <- if (!is.null(workspace_dir)) {
        file.path(workspace_dir, "USER.md")
    } else {
        NULL
    }
    claude_exists <- file.exists(claude_global)
    user_exists <- !is.null(user_path) && file.exists(user_path)
    if (!claude_exists && !user_exists) {
        return(character(0L))
    }

    file_to_load <- NULL
    if (forced || is.na(agent)) {
        if (claude_exists) {
            file_to_load <- claude_global
        } else {
            file_to_load <- user_path
        }
    } else if (identical(agent, "claude")) {
        if (user_exists && !same_file(claude_global, user_path)) {
            file_to_load <- user_path
        }
    } else {
        if (claude_exists) {
            file_to_load <- claude_global
        } else {
            file_to_load <- user_path
        }
    }
    if (is.null(file_to_load)) {
        return(character(0L))
    }

    content <- tryCatch(readLines(file_to_load, warn = FALSE),
                        error = function(e) character(0L))
    if (length(content) == 0L) {
        return(character(0L))
    }

    label <- if (identical(file_to_load, claude_global)) {
        "Global Instructions (~/.claude/CLAUDE.md)"
    } else {
        "User Preferences (USER.md)"
    }
    c(sprintf("## %s", label), "", content)
}

agent_context_soul <- function(workspace_dir) {
    soul_path <- file.path(workspace_dir, "SOUL.md")
    if (!file.exists(soul_path)) {
        return(character(0L))
    }
    content <- tryCatch(readLines(soul_path, warn = FALSE),
                        error = function(e) character(0L))
    if (length(content) == 0L) {
        return(character(0L))
    }
    c("## Agent Identity (SOUL.md)", "", content)
}

same_file <- function(a, b) {
    if (is.null(a) || is.null(b)) {
        return(FALSE)
    }
    if (!file.exists(a) || !file.exists(b)) {
        return(FALSE)
    }
    norm_a <- tryCatch(normalizePath(a, mustWork = FALSE),
                       error = function(e) a)
    norm_b <- tryCatch(normalizePath(b, mustWork = FALSE),
                       error = function(e) b)
    identical(norm_a, norm_b)
}

