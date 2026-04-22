% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_query}
\alias{subagent_query}
\title{Query a subagent.}
\usage{
subagent_query(id, prompt, timeout = 60L)
}
\arguments{
\item{id}{Subagent ID.}

\item{prompt}{Prompt to send.}

\item{timeout}{Timeout in seconds (currently advisory; callr-level
hard timeouts are future work).}
}
\value{
Reply text (character).
}
\description{
Sends a prompt to a running subagent. Inside the child it runs
through [turn()] with the child's persistent turn session: the LLM
replies, any tool calls it makes resolve against the child's
in-process skill registry, and history accumulates across queries.
}
