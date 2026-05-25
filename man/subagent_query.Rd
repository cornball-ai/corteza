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

\item{wait}{If TRUE (default), block until the child replies and
return the reply text. If FALSE, fire the prompt and return the
canonical id invisibly; caller must collect via
[subagent_collect()].}

\item{timeout}{Timeout in seconds (currently advisory; callr-level
hard timeouts are future work).}

\item{return_name}{Optional single name or `.h_NNN` handle for a
value the child should hand back. When set, the child must have
left the result bound under that name (e.g. via `run_r`); the
resolved value is stashed in the parent handle store and the reply
gains a `[stored as .h_NNN]` block referencing it. Requires a
subagent with `run_r` (the `work` preset). For `wait = FALSE` the
name is captured now and applied when collected.}
}
\value{
Reply text (character) when `wait = TRUE`, with a handle
  block appended when `return_name` resolved. Canonical id
  (character, invisibly) when `wait = FALSE`.
}
\description{
Sends a prompt to a running subagent. Inside the child it runs
through [turn()] with the child's persistent turn session: the LLM
replies, any tool calls it makes resolve against the child's
in-process skill registry, and history accumulates across queries.
}
\details{
With `wait = FALSE` the call returns immediately after firing the
prompt; the parent collects the reply later with [subagent_collect()].
A subagent can only carry one in-flight async query at a time:
firing a second one while the first is pending raises an error.

}
\examples{
\dontrun{
# Requires LLM credentials in the child's environment.
id <- subagent_spawn("read R/skill.R and summarize", preset = "minimal")
subagent_query(id, "what does this file do?", wait = TRUE)
subagent_kill(id)
}
}
