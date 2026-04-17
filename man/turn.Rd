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
Tool dispatch is pluggable via \code{tool_executor}. The default is an
in-process dispatcher that calls the local skill registry — suitable
for \code{chat()} and matrix adapters running in the same R process as
their skills. Pass \code{\link{mcp_tool_executor}} (or any
\code{function(name, args) -> MCP-format result}) to run tools in a
separate process, which is how the CLI talks to \code{serve()}.
}
