# MCP Tool Definitions
# Schema definitions for all tools exposed by the MCP server

# Built-in tool categories for filtering
.builtin_categories <- list(
                            file = c("read_file", "write_file", "replace_in_file", "list_files"),
                            code = c("run_r", "run_r_script", "bash"),
                            search = c("grep_files"),
                            web = c("web_search", "fetch_url"),
                            git = c("git_status", "git_diff", "git_log"),
                            r = c("r_help", "installed_packages"),
                            subagent = c("spawn_subagent", "query_subagent",
        "list_subagents", "kill_subagent")
)

#' Tools hidden by config
#'
#' @param cwd Working directory
#' @return Character vector of hidden tool names
#' @noRd
hidden_tools <- function(cwd = getwd()) {
    character()
}

#' Get tool categories (built-in + package-derived)
#'
#' Built-in tools keep named categories. Package tools are auto-categorized
#' by package name (e.g., base::readLines -> "base" category).
#'
#' @return Named list of category -> tool name vectors
#' @noRd
get_tool_categories <- function() {
    cats <- .builtin_categories
    all_names <- list_skills()
    pkg_tools <- all_names[grepl("::", all_names, fixed = TRUE)]
    for (tool_name in pkg_tools) {
        pkg <- sub("::.*", "", tool_name)
        cats[[pkg]] <- c(cats[[pkg]], tool_name)
    }
    cats
}

#' Get list of available MCP tools
#'
#' Returns tool definitions from the skill registry if skills are registered,
#' otherwise returns empty list. Use ensure_skills() first to register built-in skills.
#'
#' @param filter Character vector of tool names or categories to include.
#'   Categories: file, code, r, search, web, git, subagent.
#'   Use "core" for file+code+git, "all" for everything.
#' @return List of tool definitions with names, descriptions, and schemas
#' @noRd
get_tools <- function(filter = NULL) {
    # Get tools from skill registry
    all_tools <- skills_as_tools()

    # If no skills registered, register built-ins and try again
    if (length(all_tools) == 0) {
        register_builtin_skills()
        all_tools <- skills_as_tools()
    }

    hidden <- hidden_tools()
    if (length(hidden) > 0) {
        all_tools <- Filter(function(t) !t$name %in% hidden, all_tools)
    }

    # No filter = all tools
    if (is.null(filter)) {
        return(all_tools)
    }

    # Expand category shortcuts
    if ("all" %in% filter) {
        return(all_tools)
    }
    if ("core" %in% filter) {
        filter <- c(filter[filter != "core"], "file", "git", "code", "search")
    }

    # Expand categories to tool names
    cats <- get_tool_categories()
    tool_names <- character()
    for (f in filter) {
        if (f %in% names(cats)) {
            tool_names <- c(tool_names, cats[[f]])
        } else {
            tool_names <- c(tool_names, f)
        }
    }
    tool_names <- unique(tool_names)

    # Filter tools
    Filter(function(t) t$name %in% tool_names, all_tools)
}

#' Ensure skills are registered
#'
#' Registers built-in skills if not already registered.
#'
#' @return Invisible character vector of skill names
#' @noRd
ensure_skills <- function() {
    if (length(list_skills()) == 0) {
        register_builtin_skills()
    }
    invisible(list_skills())
}

#' Convert skills to llm.api tool format
#'
#' Converts MCP-format tool specs (inputSchema) to the format
#' expected by llm.api::agent() (input_schema).
#'
#' @param filter Character vector of tool names or categories to include.
#' @return List of tool definitions for llm.api::agent()
#' @noRd
skills_as_api_tools <- function(filter = NULL) {
    mcp_tools <- get_tools(filter)
    lapply(mcp_tools, function(t) {
        list(
             name = sanitize_tool_name(t$name),
             description = t$description,
             input_schema = t$inputSchema
        )
    })
}

#' Sanitize tool name for LLM API compatibility
#'
#' Anthropic/OpenAI require tool names to match [a-zA-Z0-9_-].
#' Converts "::" to "__" for the API.
#'
#' @param name Internal tool name (e.g. "base::readLines")
#' @return API-safe name (e.g. "base__readLines")
#' @noRd
sanitize_tool_name <- function(name) {
    name <- gsub("::", "__", name, fixed = TRUE)
    gsub(".", "-", name, fixed = TRUE)
}

#' Restore internal tool name from API-safe name
#'
#' Reverses sanitize_tool_name. Dot becomes "-", "::" becomes "__".
#'
#' @param name API-safe name (e.g. "base__list-files")
#' @return Internal name (e.g. "base::list.files")
#' @noRd
unsanitize_tool_name <- function(name) {
    name <- gsub("__", "::", name, fixed = TRUE)
    gsub("-", ".", name, fixed = TRUE)
}

