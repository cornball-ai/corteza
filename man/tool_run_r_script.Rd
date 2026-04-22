% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_run_r_script}
\alias{tool_run_r_script}
\title{Execute R code in a clean subprocess via littler.}
\usage{
tool_run_r_script(code, timeout = 30L)
}
\arguments{
\item{code}{(character) R code to execute.}

\item{timeout}{(integer) Timeout in seconds.}
}
\value{
An MCP tool-result list.
}
\description{
Use for scripts that modify packages, run tests, or need isolation
from the server.
}
\keyword{internal}
