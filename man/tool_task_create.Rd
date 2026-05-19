% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_task_create}
\alias{tool_task_create}
\title{Create a list of tasks to track for the current request.}
\usage{
tool_task_create(tasks)
}
\arguments{
\item{tasks}{(character vector) One task description per element.
Order matters; task_update references tasks by 1-based index.}
}
\value{
Confirmation string.
}
\description{
Call this at the start of any multi-step user request (3+ steps)
to commit to a visible plan. The list persists across turns and
replaces any prior list. After creating, update each task's
status with `task_update` as you work: mark a task `in_progress`
when you start it, `completed` when done, `cancelled` if the
user redirects.
}
\keyword{internal}
