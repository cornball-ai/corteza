% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_delegate}
\alias{tool_delegate}
\title{Hand a task to this session's doer.}
\usage{
tool_delegate(task, review = NULL, ctx = list())
}
\arguments{
\item{task}{(character) Complete description of the work: files,
goal, constraints, and how to check the result.}

\item{review}{(logical) Have a read-only reviewer check the work when
the doer finishes. The checkout stays locked until the review ends,
and the review arrives as a second message. Omit to use the
configured default.}

\item{ctx}{Server-side context; supplies the session.}
}
\value{
An MCP tool-result list.
}
\description{
Queues a job for the session's persistent worker and returns at once.
The result arrives later as its own message. The doer does not see
this conversation, so the task must be self-contained.
}
\keyword{internal}
