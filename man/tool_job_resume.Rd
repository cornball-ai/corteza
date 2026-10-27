% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_job_resume}
\alias{tool_job_resume}
\title{Resume an incomplete delegated job after an explicit user request.}
\usage{
tool_job_resume(id, ctx = list())
}
\arguments{
\item{id}{(character) Incomplete job id.}

\item{ctx}{Server-side context; supplies requester authorization.}
}
\value{
An MCP tool-result list.
}
\description{
Resume an incomplete delegated job after an explicit user request.
}
\keyword{internal}
