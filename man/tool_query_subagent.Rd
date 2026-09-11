% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_query_subagent}
\alias{tool_query_subagent}
\title{Send a prompt to a running subagent.}
\usage{
tool_query_subagent(id, prompt, wait = FALSE, return_name = NULL, timeout = 60)
}
\arguments{
\item{id}{(character) Subagent ID.}

\item{prompt}{(character) Prompt to send.}

\item{wait}{(logical) If FALSE (default), fire the prompt and return
immediately; collect the reply with `collect_subagent`. If TRUE,
block up to `timeout` seconds for the reply; on timeout the query
stays pending and `collect_subagent` fetches it later.}

\item{return_name}{(string) Optional name or `.h_NNN` handle for a
value the subagent should hand back. Tell the subagent to leave
its result bound under this name (it needs `run_r`); the value is
returned as a handle you can reference in a later `run_r`, instead
of being inlined into the reply text.}

\item{timeout}{(numeric) Maximum seconds to block when `wait = TRUE`.
Default 60.}
}
\value{
An MCP tool-result list.
}
\description{
Fires the prompt and returns at once; collect the reply later with
collect_subagent, so several subagents can run in parallel. wait =
TRUE for a quick answer.
}
\keyword{internal}
