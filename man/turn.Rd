% tinyrox says don't edit this manually, but it can't stop you!
\name{turn}
\alias{turn}
\title{Run one agent turn}
\usage{
turn(prompt, session, tool_executor = NULL, tools = NULL)
}
\arguments{
\item{prompt}{Character. User prompt.}

\item{session}{A session environment created by \code{\link{new_session}}.}

\item{tool_executor}{Function or NULL. Dispatcher with signature
\code{function(name, args) -> list}. NULL uses the in-process
\code{call_skill} path.}

\item{tools}{List or NULL. Tool schemas to pass the LLM. NULL uses
the in-process skill registry (filtered by \code{session$tools_filter}).
Pass explicit schemas when running against a remote skill source.}
}
\value{
A list with \code{reply} (character) and \code{session} (the
  updated session environment; also mutated in place).
}
\description{
Sends \code{prompt} to the configured LLM with tool use enabled. Every
tool call the LLM makes is routed through \code{\link{policy}} before
being dispatched.
}
\details{
Tool dispatch is pluggable via \code{tool_executor}, but the CLI and
\code{chat()} both leave it NULL: tools run in-process through the
default \code{call_skill} dispatcher against the local skill registry.
\code{serve()} is a separate MCP server for external clients only; it
is not part of the CLI's tool path. Pass an explicit
\code{function(name, args) -> list} executor only when dispatching
tools somewhere other than the in-process registry.

}
\examples{
\dontrun{
# Requires ANTHROPIC_API_KEY (or the configured provider's key) and
# a network connection to the LLM.
s <- new_session(provider = "anthropic")
out <- turn("Say hello", s)
out$reply
}
}
