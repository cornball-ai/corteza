% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_run_r}
\alias{tool_run_r}
\title{Execute R code in a persistent environment.}
\usage{
tool_run_r(code, envir = globalenv())
}
\arguments{
\item{code}{(character) R code to execute.}

\item{envir}{(environment) Persistent environment in which assignments
should land. Defaults to \code{globalenv()} for backwards compatibility.}
}
\value{
An MCP tool-result list.
}
\description{
By default, code runs in the host's global environment, preserving the
original \code{run_r} contract. Pass a private persistent environment to
give an agent or capability its own R scope. Scoped evaluation retains
assignments and handles but does not copy bindings into the host workspace
cache.
}
\keyword{internal}
