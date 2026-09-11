% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_query}
\alias{subagent_query}
\title{Query a subagent.}
\usage{
subagent_query(id, prompt, wait = TRUE, timeout = 60L, return_name = NULL)
}
\arguments{
\item{id}{Subagent identifier. Accepts the canonical UUID, a unique
UUID prefix, or the per-session sequence number printed by
`subagent_list()` / `/agents`.}

\item{prompt}{Prompt to send.}

\item{wait}{If TRUE (default), block up to `timeout` seconds for the
child's reply and return the reply text. If FALSE, fire the prompt
and return the canonical id invisibly; caller must collect via
[subagent_collect()].}

\item{timeout}{Maximum seconds to block when `wait = TRUE`; `Inf`
waits until the child replies. On timeout the query stays pending:
collect it later with [subagent_collect()] or end it with
[subagent_kill()].}

\item{return_name}{Optional single name or `.h_NNN` handle for a
value the child should hand back. When set, the child must have
left the result bound under that name (e.g. via `run_r`); the
resolved value is stashed in the parent handle store and the reply
gains a `[stored as .h_NNN]` block referencing it. Requires a
subagent with `run_r` (the `work` preset). For `wait = FALSE` the
name is captured now and applied when collected.}
}
\value{
Reply text (character) when `wait = TRUE` and the child
  replied within `timeout`, with a handle block appended when
  `return_name` resolved; NULL (invisibly) when the timeout elapsed
  and the query is still pending. Canonical id (character,
  invisibly) when `wait = FALSE`.
}
\description{
Sends a prompt to a running subagent. Inside the child it runs
through [turn()] with the child's persistent turn session: the LLM
replies, any tool calls it makes resolve against the child's
in-process skill registry, and history accumulates across queries.
}
\details{
Both paths fire the prompt the same way, as a one-shot call on the
child's `callr::r_session`. With `wait = FALSE` the call returns
immediately and the parent collects the reply later with
[subagent_collect()]. With `wait = TRUE` the call collects on the
parent's behalf, but only up to `timeout` seconds: a child that has
not replied by then is left running with the query pending, and the
call returns NULL, so the wait is bounded by default (`Inf` opts out).
A subagent can only carry one in-flight query at a time: firing a
second one while the first is pending raises an error.

}
\examples{
\dontrun{
# Requires LLM credentials in the child's environment.
id <- subagent_spawn("read R/skill.R and summarize", preset = "minimal")
subagent_query(id, "what does this file do?", wait = TRUE)
subagent_kill(id)
}
}
