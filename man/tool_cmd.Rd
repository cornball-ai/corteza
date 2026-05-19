% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_cmd}
\alias{tool_cmd}
\title{Run a Windows cmd.exe command.}
\usage{
tool_cmd(command, timeout = 30L, background = FALSE)
}
\arguments{
\item{command}{(character) cmd.exe command to execute.}

\item{timeout}{(integer) Timeout in seconds.}

\item{background}{(logical) Run in background and return immediately.}
}
\value{
An MCP tool-result list.
}
\description{
Use background=true for long-running processes.
}
\keyword{internal}
