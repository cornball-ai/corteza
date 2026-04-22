% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_run_r}
\alias{tool_run_r}
\title{Execute R code in the session's global environment.}
\usage{
tool_run_r(code)
}
\arguments{
\item{code}{(character) R code to execute.}
}
\value{
An MCP tool-result list.
}
\description{
New bindings are auto-captured into the workspace cache.
}
\keyword{internal}
