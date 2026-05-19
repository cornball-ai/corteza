% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_task_update}
\alias{tool_task_update}
\title{Update one task's status in the current session task list.}
\usage{
tool_task_update(index, status)
}
\arguments{
\item{index}{(integer) 1-based position of the task to update.}

\item{status}{(character) New status. One of `pending`,
`in_progress`, `completed`, `cancelled`. Promoting a task to
`in_progress` automatically demotes any other `in_progress`
task to `pending`, so there is at most one active task at a
time.}
}
\value{
Confirmation string.
}
\description{
Update one task's status in the current session task list.
}
\keyword{internal}
