% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_skill_instructions}
\alias{tool_skill_instructions}
\title{Read a session-scoped instruction or supporting resource.}
\usage{
tool_skill_instructions(id, resource = NULL, ctx = list())
}
\arguments{
\item{id}{(character) Exact instruction identifier shown in the session
catalog.}

\item{resource}{(character) Optional relative supporting-resource path.
Omit to read SKILL.md.}

\item{ctx}{(list) Host-owned tool execution context containing the session.}
}
\value{
An MCP tool-result list.
}
\description{
Resolves an exact instruction identifier against the immutable catalog
captured for the current agent session. Supporting resources are confined to
the same snapshotted instruction directory and checked for drift.
}
\keyword{internal}
