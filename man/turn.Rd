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

When the provider refuses the request with a limit error (HTTP 429,
503, or 529, or a body naming a rate, usage, or quota limit) and the
session has a fallback chain, the turn is retried on the next
\code{"model provider"} entry in that chain. The chain comes from
\code{session$fallback} (the Matrix config's \code{fallback} key, see
\code{\link{bot_configure}}) or the cwd config's \code{fallback}
key. A provider that hit a limit is skipped process-wide for
\code{fallback_cooldown_minutes} (default 30), after which the
primary is tried again. A limit hit after tool calls have already
run is not retried: the error surfaces and the next turn takes the
fallback, so no tool call runs twice. Errors that are not limits
are rethrown untouched.

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
