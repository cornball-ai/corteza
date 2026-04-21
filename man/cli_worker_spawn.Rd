% tinyrox says don't edit this manually, but it can't stop you!
\name{cli_worker_spawn}
\alias{cli_worker_spawn}
\title{Spawn and initialize a CLI worker session.}
\usage{
cli_worker_spawn(cwd = getwd(), tools_filter = NULL)
}
\arguments{
\item{cwd}{Working directory for the worker.}

\item{tools_filter}{Optional filter for `worker_tool_list()`.}
}
\value{
A list with `session` (the `callr::r_session` instance),
  `tools` (tool list ready for LLM API conversion), and `cwd`.
}
\description{
Starts a fresh `callr::r_session`, loads corteza inside it, runs
`worker_init()` so skills are registered, and returns a handle the
CLI treats opaquely. The CLI (and any other caller that drives tools
through a private subprocess) should use this rather than invoking
`callr::r_session$new()` directly.
}
\keyword{internal}
