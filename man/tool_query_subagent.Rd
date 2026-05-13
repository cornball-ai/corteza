% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_query_subagent}
\alias{tool_query_subagent}
\title{Send a prompt to a running subagent and get the response.}
\usage{
tool_query_subagent(id, prompt, wait = TRUE)
}
\arguments{
\item{id}{(character) Subagent ID.}

\item{prompt}{(character) Prompt to send.}

\item{wait}{(logical) If TRUE (default), block until the child
replies and return the reply. If FALSE, fire the prompt and
return immediately; caller collects via `collect_subagent`.}
}
\value{
An MCP tool-result list.
}
\description{
Send a prompt to a running subagent and get the response.
}
\keyword{internal}
