% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_bash}
\alias{tool_bash}
\title{Run a bash shell command.}
\usage{
tool_bash(command, timeout = 30L, background = FALSE)
}
\arguments{
\item{command}{(character) Shell command to execute.}

\item{timeout}{(integer) Timeout in seconds.}

\item{background}{(logical) Run in background and return immediately.}
}
\value{
An MCP tool-result list.
}
\description{
Use background=true for long-running servers or processes.
}
\keyword{internal}
