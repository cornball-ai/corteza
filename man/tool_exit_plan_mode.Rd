% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_exit_plan_mode}
\alias{tool_exit_plan_mode}
\title{Submit a plan and exit plan mode.}
\usage{
tool_exit_plan_mode(plan)
}
\arguments{
\item{plan}{(character) Markdown-formatted implementation plan. State
what will change, in which files, and why. Be concrete.}
}
\value{
An MCP tool-result list.
}
\description{
Called by the LLM after it has finished research in plan mode. The
plan text is shown to the user; on approval the session leaves plan
mode and the LLM proceeds with the work. On decline the session
stays in plan mode and the LLM iterates.
}
\details{
Every session that can be in plan mode is offered this tool, so its
tool list stays the same when plan mode starts or ends. Outside plan
mode the call is refused.

}
\keyword{internal}
