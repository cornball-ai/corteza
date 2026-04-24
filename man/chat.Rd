% tinyrox says don't edit this manually, but it can't stop you!
\name{chat}
\alias{chat}
\title{Start Interactive Chat}
\usage{
chat(provider = NULL, model = NULL, tools = NULL, session = NULL,
     max_turns = NULL)
}
\arguments{
\item{provider}{LLM provider: "anthropic", "openai", "moonshot", or
"ollama".
Defaults to config value or "anthropic".}

\item{model}{Model name. Defaults to config value or provider default.}

\item{tools}{Character vector of tool names or categories to enable.
Categories: file, code, r, data, web, git, chat, memory.
Use "core" for file+code+git, "all" for everything (default).}

\item{session}{Session resume control. NULL (default) starts fresh,
TRUE resumes the latest session, or a character session key to
resume a specific session.}

\item{max_turns}{Integer or NULL. Maximum LLM turns per user prompt
before the loop stops with \code{[Max turns reached]}. NULL (default)
reads \code{getOption("corteza.max_turns")}, then falls back to the
\code{\link{session_setup}} default (50).}
}
\value{
The session object (invisibly).
}
\description{
Run a conversational agent inside your R session. Tools execute as direct
function calls, no MCP server needed.
}
\examples{
\dontrun{
# Start chatting with defaults from config
chat()

# Use a specific provider/model
chat(provider = "ollama", model = "llama3.2")

# Minimal tools for focused work
chat(tools = "core")
}
}
