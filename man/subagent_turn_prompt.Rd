% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_turn_prompt}
\alias{subagent_turn_prompt}
\title{Forward a prompt to the child-side turn session.}
\usage{
subagent_turn_prompt(prompt)
}
\arguments{
\item{prompt}{User prompt (character).}
}
\value{
A list with `$reply` (character, the LLM reply text) and
  `$usage` (list with `input_tokens`, `output_tokens`, `total_tokens`,
  and optionally `cost` -- provider-dependent). Callers extract the
  reply and accumulate usage into the parent-side registry.
}
\description{
Captures the pre-turn history length so that, if archival is enabled
and this query qualifies, the child can recursively archive its own
turn into a sub-subagent (capped by depth_cap).
}
\keyword{internal}
