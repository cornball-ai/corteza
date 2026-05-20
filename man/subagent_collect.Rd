% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_collect}
\alias{subagent_collect}
\title{Collect the result of a previously-fired async subagent query.}
\usage{
subagent_collect(id, wait = TRUE, timeout = 60L)
}
\arguments{
\item{id}{Subagent identifier (UUID, prefix, or sequence number).}

\item{wait}{If TRUE (default), block up to `timeout` seconds waiting
for the child to finish. If FALSE, poll once and return
immediately.}

\item{timeout}{Maximum seconds to block when `wait = TRUE`. On
timeout the child is left running; caller may collect again later
or kill explicitly.}
}
\value{
Reply text (character) when ready; NULL when still running.
}
\description{
Pairs with `subagent_query(..., wait = FALSE)`. Returns the reply
text once the child finishes its turn, or NULL while the query is
still running. Result is read exactly once: after a successful
collect the pending slot is cleared, so the next async query can
fire.
}
\examples{
\dontrun{
id <- subagent_spawn("background research")
subagent_query(id, "what's in DESCRIPTION?", wait = FALSE)
# ... do other work ...
subagent_collect(id, wait = TRUE, timeout = 30)
subagent_kill(id)
}
}
