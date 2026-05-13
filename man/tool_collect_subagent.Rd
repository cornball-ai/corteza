% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_collect_subagent}
\alias{tool_collect_subagent}
\title{Collect the result of a previously-fired async subagent query.}
\usage{
tool_collect_subagent(id, wait = TRUE, timeout = 60)
}
\arguments{
\item{id}{(character) Subagent ID.}

\item{wait}{(logical) If TRUE (default), block up to `timeout`
seconds. If FALSE, poll once and return immediately.}

\item{timeout}{(numeric) Maximum seconds to block when `wait =
TRUE`. Default 60.}
}
\value{
An MCP tool-result list. On timeout returns a note that
  the query is still running.
}
\description{
Collect the result of a previously-fired async subagent query.
}
\keyword{internal}
