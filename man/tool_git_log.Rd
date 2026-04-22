% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_git_log}
\alias{tool_git_log}
\title{Show recent git commits.}
\usage{
tool_git_log(n = 10L, ref = "HEAD", path = ".")
}
\arguments{
\item{n}{(integer) Number of commits to return.}

\item{ref}{(character) Optional ref to log from.}

\item{path}{(character) Repository path.}
}
\value{
An MCP tool-result list.
}
\description{
Show recent git commits.
}
\keyword{internal}
