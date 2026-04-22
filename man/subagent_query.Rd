% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_query}
\alias{subagent_query}
\title{Query a subagent.}
\usage{
subagent_query(id, prompt, timeout = 60L)
}
\arguments{
\item{id}{Subagent ID.}

\item{prompt}{Prompt / code to send.}

\item{timeout}{Timeout in seconds.}
}
\value{
Response text.
}
\description{
Sends a prompt to a running subagent. The current implementation
routes the prompt through `worker_dispatch("run_r", ...)` — i.e. the
subagent evaluates the prompt as R code. True agent-loop query
(prompt goes through `turn()` inside the child, LLM replies,
optional tool calls) is a TODO.
}
