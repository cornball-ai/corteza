#' Start MCP Server
#'
#' Start the corteza MCP server. This exposes R tools to MCP clients like
#' Claude Desktop, VS Code, or the corteza CLI.
#'
#' @param port Port number for socket transport. If NULL, uses stdio transport.
#' @param cwd Working directory for the server. Defaults to current directory.
#' @param tools Character vector of tools or categories to enable. Categories:
#'   file, code, r, data, web, git, chat. Use "core" for file+code+git,
#'   "all" for everything (default).
#' @param expose_subagents Whether MCP clients may call the subagent
#'   tools (`spawn_subagent`, `query_subagent`, `collect_subagent`,
#'   `list_subagents`, `kill_subagent`). `NULL` (default) defers to the
#'   `subagents$expose_over_mcp` config flag (itself FALSE by default);
#'   `TRUE`/`FALSE` overrides it. Off by default because a spawned
#'   subagent runs its own agent loop and spends autonomously on the
#'   host's LLM credentials -- an unattended MCP client could otherwise
#'   trigger unbounded cost the client never sees. When on, cumulative
#'   subagent spend is capped by `subagents$mcp_spend_cap_usd`
#'   (default $5.00).
#'
#' @details
#' The server supports two transport modes:
#'
#' - **stdio** (default): For Claude Desktop and other MCP clients.
#'   Communication happens via stdin/stdout.
#'
#' - **socket**: For the corteza CLI and R clients. Listens on a TCP port.
#'
#' ## Tools Provided
#'
#' - `read_file`, `write_file`, `replace_in_file`, `list_files`, `grep_files` - File operations
#' - `run_r` - Execute R code in the server session
#' - `bash` - Run shell commands
#' - `r_help` - Query package docs via saber (exports, function help)
#' - `installed_packages` - List installed packages
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
#' # For corteza CLI (socket) with all tools
#' serve(port = 7850)
#'
#' # Minimal tools for small context models
#' serve(port = 7850, tools = "core")
#'
#' # Specific categories
#' serve(port = 7850, tools = c("file", "git"))
#' }
serve <- function(port = NULL, cwd = NULL, tools = NULL,
                  expose_subagents = NULL) {
    # Set working directory if specified, restoring on exit so we don't
    # leave the caller's session pointed somewhere unexpected.
    if (!is.null(cwd) && dir.exists(cwd)) {
        oldwd <- getwd()
        on.exit(setwd(oldwd), add = TRUE)
        setwd(cwd)
    }

    # Register built-in R skills
    ensure_skills()

    # Load user R skills (.R files)
    load_skills(corteza_data_path("skills"))
    load_skills(file.path(getwd(), ".corteza", "skills"))

    # Load skill docs (SKILL.md files) for context injection
    load_skill_docs(corteza_data_path("skills"))
    load_skill_docs(file.path(getwd(), ".corteza", "skills"))

    # Load skill packages from config
    config <- load_config(getwd())
    load_skill_packages(config)

    # Set tool filter option
    options(corteza.tools = tools)

    # Resolve subagent-over-MCP exposure: explicit arg wins, else the
    # config flag (default FALSE). The MCP handler reads these options to
    # gate tools/list and tools/call (mcp_subagent_guard()).
    expose <- if (is.null(expose_subagents)) {
        isTRUE(config$subagents$expose_over_mcp)
    } else {
        isTRUE(expose_subagents)
    }
    options(corteza.mcp_expose_subagents = expose)
    options(corteza.mcp_subagent_cap_usd = config$subagents$mcp_spend_cap_usd)
    options(corteza.mcp_subagent_cap_tokens = config$subagents$mcp_spend_cap_tokens)
    if (expose) {
        cap <- config$subagents$mcp_spend_cap_usd %||% 5
        log_event("mcp_subagents_exposed", cap_usd = cap, level = "warn")
    }

    # Run appropriate transport
    if (!is.null(port)) {
        run_socket(port)
    } else {
        run_stdio()
    }
}

