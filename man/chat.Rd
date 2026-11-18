% tinyrox says don't edit this manually, but it can't stop you!
\name{chat}
\alias{chat}
\title{Start Interactive Chat}
\usage{
chat(
  provider = NULL,
  model = NULL,
  tools = NULL,
  session = NULL,
  max_turns = NULL,
  web_search = NULL
)
}
\arguments{
\item{provider}{LLM provider: "anthropic", "openai", "moonshot", or
"ollama".
Defaults to the saved provider when resuming, otherwise the config value
or "anthropic".}

\item{model}{Model name. Defaults to the saved model when resuming with the
same provider, otherwise the matching config value or provider default.}

\item{tools}{Character vector of tool names or categories to enable.
Categories: file, code, r, data, web, git, chat, memory.
Use "core" for file+code+git, "all" for everything (default).}

\item{session}{Session resume control. NULL (default) starts fresh,
TRUE resumes the latest session started in the current directory,
or a character session key to resume a specific session from any
directory.}

\item{max_turns}{Integer or NULL. Maximum LLM turns per user prompt
before the loop stops with \code{[Max turns reached]}. NULL (default)
reads \code{getOption("corteza.max_turns")}, then falls back to the
\code{\link{session_setup}} default (50).}

\item{web_search}{Logical or NULL. Enable provider-native
(server-side) web search for the conversation. NULL uses the config
default (on for supported providers); the model searches inside its
own turn with no Tavily key required.}
}
\value{
The session object (invisibly).
}
\description{
Run a conversational agent inside your R session. Tools execute as direct
function calls, no MCP server needed.
}
\details{
A project's own files (its \code{.corteza/} folder, \code{AGENTS.md} or
\code{CLAUDE.md}, and the briefing built from it) load only from a
directory you have trusted. Where the working directory is not trusted
and holds such files, \code{chat()} lists them and asks once before it
starts; \code{/trust} shows the same later. See "Trusted projects" in
\code{vignette("configuration")}.

}
\examples{
if (interactive()) {
    # Start chatting with defaults from config
    chat()

    # Use a specific provider/model
    chat(provider = "ollama", model = "llama3.2")

    # Minimal tools for focused work
    chat(tools = "core")
}
}
