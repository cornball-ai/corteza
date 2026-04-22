% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_spawn_subagent}
\alias{tool_spawn_subagent}
\title{Spawn a specialized subagent for a task.}
\usage{
tool_spawn_subagent(task, model = NULL, tools = NULL, ctx = list())
}
\arguments{
\item{task}{(character) Task description for the subagent.}

\item{model}{(character) Optional model override.}

\item{tools}{(character vector) Optional tool filter (list of tool names).}
}
\value{
An MCP tool-result list.
}
\description{
Use for parallel work or tasks requiring focused attention. Parent
session is read from `ctx$session`, which the skill handler injects
from the invoking context; not from LLM-provided args.
}
\keyword{internal}
