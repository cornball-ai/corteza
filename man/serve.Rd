% tinyrox says don't edit this manually, but it can't stop you!
\name{serve}
\alias{serve}
\title{Start MCP Server}
\usage{
serve(port = NULL, cwd = NULL, tools = NULL, expose_subagents = NULL)
}
\arguments{
\item{port}{Port number for socket transport. If NULL, uses stdio transport.}

\item{cwd}{Working directory for the server. Defaults to current directory.}

\item{tools}{Character vector of tools or categories to enable. Categories:
file, code, r, data, web, git, chat. Use "core" for file+code+git,
"all" for everything (default).}

\item{expose_subagents}{Whether MCP clients may call the subagent
tools (`spawn_subagent`, `query_subagent`, `collect_subagent`,
`list_subagents`, `kill_subagent`). `NULL` (default) defers to the
`subagents$expose_over_mcp` config flag (itself FALSE by default);
`TRUE`/`FALSE` overrides it. Off by default because a spawned
subagent runs its own agent loop and spends autonomously on the
host's LLM credentials -- an unattended MCP client could otherwise
trigger unbounded cost the client never sees. When on, cumulative
subagent spend is capped by `subagents$mcp_spend_cap_usd`
(default $5.00).}
}
\value{
NULL (runs until interrupted or client disconnects)
}
\description{
Start the corteza MCP server. This exposes R tools to MCP clients like
Claude Desktop, VS Code, or the corteza CLI.
}
\details{
The server supports two transport modes:

- **stdio** (default): For Claude Desktop and other MCP clients.
  Communication happens via stdin/stdout.

- **socket**: For the corteza CLI and R clients. Listens on a TCP port.

## Tools Provided

- `read_file`, `write_file`, `replace_in_file`, `list_files`, `grep_files` - File operations
- `run_r` - Execute R code in the server session
- `bash` - Run shell commands
- `r_help` - Query package docs via saber (exports, function help)
- `installed_packages` - List installed packages
- `web_search` - Search the web via Tavily (requires TAVILY_API_KEY)
- `fetch_url` - Fetch web content
- `git_status`, `git_diff`, `git_log` - Git operations
- `chat`, `chat_models` - LLM chat (requires llm.api)
}
\examples{
\dontrun{
# For Claude Desktop (stdio)
serve()

# For corteza CLI (socket) with all tools
serve(port = 7850)

# Minimal tools for small context models
serve(port = 7850, tools = "core")

# Specific categories
serve(port = 7850, tools = c("file", "git"))
}
}
