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
A list with \code{reply} (character), \code{session} (the
  updated session environment; also mutated in place), \code{route}
  (the actual model/provider and fallback credential mode), and
  \code{termination} (status \code{done}, \code{incomplete}, or
  \code{cancelled}, with a machine-readable reason), and \code{audit}
  (the stream path and canonical turn ID, or NULL when recording is disabled).
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
\code{fallback_cooldown_minutes} (default 30); a configured
\code{fallback_primary_retry_at} can hold the primary until a weekly
reset boundary. Provider-native histories are bridged when the wire
changes, and a partially completed tool run resumes from its captured
results so no completed tool call runs twice. Fallback replies identify
their actual model/provider; API-key fallbacks carry a prominent billable
usage warning. Errors that are not limits are rethrown untouched.

A program that runs turns for someone else can let them stop one. It
sets \code{session$stop_requested} to a function of no arguments that
returns \code{TRUE} once a stop is asked for. The turn asks it before
each request to the model, before each tool call, again just before the
call runs (a call can wait for an approval or a review in between) and,
when the turn streams (\code{session$on_delta}), before each piece of
the reply goes out. A stop ends the turn with status
\code{cancelled}: a tool call that got no result is given a
placeholder, and the history ends on a note that the turn was stopped,
so the next turn can go on from it. A stop cannot cut short a request
already waiting for its first word, nor the reply of a provider that
does not stream.

A turn that ends on an error leaves a history the next request can
use. Before the error leaves \code{turn()}, each tool call the model
made that has no result gets a placeholder result, and a note says
the turn stopped on that error. A turn also closes, before its first
request, any call that an earlier turn or an earlier process left
without a result.

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
