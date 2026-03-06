# MCP Tool Definitions
# Schema definitions for all tools exposed by the MCP server

# Tool categories for filtering
tool_categories <- list(
                        file = c("read_file", "write_file", "list_files", "grep_files"),
                        code = c("run_r", "run_r_script", "bash"),
                        r = c("r_help", "installed_packages"),
                        data = c("read_csv"),
                        web = c("web_search", "fetch_url"),
                        git = c("git_status", "git_diff", "git_log"),
                        chat = c("chat", "chat_models"),
                        memory = c("memory_store", "memory_recall", "memory_get")
)

#' Get list of available MCP tools
#'
#' Returns tool definitions from the skill registry if skills are registered,
#' otherwise returns empty list. Use ensure_skills() first to register built-in skills.
#'
#' @param filter Character vector of tool names or categories to include.
#'   Categories: file, code, r, data, web, git, chat, memory.
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

    # No filter = all tools
    if (is.null(filter)) {
        return(all_tools)
    }

    # Expand category shortcuts
    if ("all" %in% filter) {
        return(all_tools)
    }
    if ("core" %in% filter) {
        filter <- c(filter[filter != "core"], "file", "code", "git")
    }

    # Expand categories to tool names
    tool_names <- character()
    for (f in filter) {
        if (f %in% names(tool_categories)) {
            tool_names <- c(tool_names, tool_categories[[f]])
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
             name = t$name,
             description = t$description,
             input_schema = t$inputSchema
        )
    })
}

