% tinyrox says don't edit this manually, but it can't stop you!
\name{worker_dispatch}
\alias{worker_dispatch}
\title{Worker-side tool dispatch.}
\usage{
worker_dispatch(name, args, ctx = list(), timeout = 30L, dry_run = FALSE)
}
\arguments{
\item{name}{Tool name.}

\item{args}{Named list of arguments.}

\item{ctx}{Optional context (cwd, session metadata).}

\item{timeout}{Timeout in seconds.}

\item{dry_run}{If TRUE, preview only.}
}
\value{
MCP-shaped tool result list (content, isError).
}
\description{
Called from the CLI over `callr::r_session$run()`. Looks up the skill
in the registry, runs it, and normalizes any dispatch-level failures
as a `corteza_tool_error` condition. Tool-body failures that are
already caught by `skill_run()` remain as `err()` envelopes.
Exported (with `@keywords internal`) because it runs inside a
`callr::r_session` child process, where `corteza:::` would otherwise
trip the R CMD check "calls to the package's namespace" NOTE.
}
\keyword{internal}
