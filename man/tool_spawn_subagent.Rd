% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_spawn_subagent}
\alias{tool_spawn_subagent}
\title{Spawn a specialized subagent for a task.}
\usage{
tool_spawn_subagent(task, model = NULL, tools = NULL, parent_session = NULL)
}
\arguments{
\item{task}{(character) Task description for the subagent.}

\item{model}{(character) Optional model override.}

\item{tools}{(character vector) Optional tool filter (list of tool names).}

\item{parent_session}{(list) Parent session object (typically NULL when
invoked as a skill).}
}
\value{
An MCP tool-result list.
}
\description{
Use for parallel work or tasks requiring focused attention. Note that
parent_session is not wired through the skill handler and defaults to
NULL; the legacy registration forwarded ctx$session here.
}
\keyword{internal}
