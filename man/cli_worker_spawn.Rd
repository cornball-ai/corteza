% tinyrox says don't edit this manually, but it can't stop you!
\name{cli_worker_spawn}
\alias{cli_worker_spawn}
\title{Spawn and initialize a CLI worker session.}
\usage{
cli_worker_spawn(cwd = getwd())
}
\arguments{
\item{cwd}{Working directory for the worker.}
}
\value{
A list with `session` (the `callr::r_session` instance) and
  `cwd`, with class `corteza_cli_worker`.
}
\description{
Starts a fresh `callr::r_session`, loads corteza inside it, and runs
`worker_init()` so skills are registered in the session. Returns an
opaque handle the CLI uses for tool dispatch.
Schema production is CLI-side. The worker registers skills only so
it can execute them; the CLI builds the LLM `tools` payload from its
own registry via `schema_from_registry()`. Nothing about schema
shape travels over the worker pipe.
}
\keyword{internal}
