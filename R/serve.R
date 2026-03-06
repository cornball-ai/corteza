#' Start MCP Server
#'
#' Start the llamaR MCP server. This exposes R tools to MCP clients like
#' Claude Desktop, VS Code, or the llamar CLI.
#'
#' @param port Port number for socket transport. If NULL, uses stdio transport.
#' @param cwd Working directory for the server. Defaults to current directory.
#' @param tools Character vector of tools or categories to enable. Categories:
#'   file, code, r, data, web, git, chat. Use "core" for file+code+git,
#'   "all" for everything (default).
#'
#' @details
#' The server supports two transport modes:
#'
#' - **stdio** (default): For Claude Desktop and other MCP clients.
#'   Communication happens via stdin/stdout.
#'
#' - **socket**: For the llamar CLI and R clients. Listens on a TCP port.
#'
#' ## Tools Provided
#'
#' - `read_file`, `write_file`, `list_files`, `grep_files` - File operations
#' - `run_r` - Execute R code in the server session
#' - `bash` - Run shell commands
#' - `r_help` - Query package docs via fyi (exports, internals, options)
#' - `installed_packages` - List installed packages
#' - `read_csv` - Read and summarize CSV files
#' - `web_search` - Search the web via Tavily (requires TAVILY_API_KEY)
#' - `fetch_url` - Fetch web content
#' - `git_status`, `git_diff`, `git_log` - Git operations
#' - `chat`, `chat_models` - LLM chat (requires llm.api)
#'
#' @return NULL (runs until interrupted or client disconnects)
#' @export
#'
#' @examples
#' \dontrun{
#' # For Claude Desktop (stdio)
#' serve()
#'
#' # For llamar CLI (socket) with all tools
#' serve(port = 7850)
#'
#' # Minimal tools for small context models
#' serve(port = 7850, tools = "core")
#'
#' # Specific categories
#' serve(port = 7850, tools = c("file", "git"))
#' }
serve <- function(port = NULL, cwd = NULL, tools = NULL) {
    # Set working directory if specified
    if (!is.null(cwd) && dir.exists(cwd)) {
        setwd(cwd)
    }

    # Register built-in R skills
    ensure_skills()

    # Load user R skills (.R files)
    load_skills(path.expand("~/.llamar/skills"))
    load_skills(file.path(getwd(), ".llamar", "skills"))

    # Load skill docs (SKILL.md files) for context injection
    load_skill_docs(path.expand("~/.llamar/skills"))
    load_skill_docs(file.path(getwd(), ".llamar", "skills"))

    # Load skill packages from config
    config <- load_config(getwd())
    load_skill_packages(config)

    # Set tool filter option
    options(llamar.tools = tools)

    # Run appropriate transport
    if (!is.null(port)) {
        run_socket(port)
    } else {
        run_stdio()
    }
}

