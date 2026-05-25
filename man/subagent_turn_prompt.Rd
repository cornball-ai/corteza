% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_turn_prompt}
\alias{subagent_turn_prompt}
\title{Forward a prompt to the child-side turn session.}
\usage{
subagent_turn_prompt(prompt, return_name = NULL)
}
\arguments{
\item{prompt}{User prompt (character).}

\item{return_name}{Optional single name or `.h_NNN` handle. When
set, after the turn the child resolves it (handle store, then
globalenv) and ships the value back as `$final` so the parent can
stash it by handle. A bad or unresolved name yields `$final_note`.}
}
\value{
A list with `$reply` (character, the LLM reply text),
  `$usage` (list with `input_tokens`, `output_tokens`, `total_tokens`,
  and optionally `cost` -- provider-dependent), and, when
  `return_name` is set, `$final` (the resolved value) or
  `$final_note` (why nothing was returned). Callers extract the
  reply and accumulate usage into the parent-side registry.
}
\description{
Captures the pre-turn history length so that, if archival is enabled
and this query qualifies, the child can recursively archive its own
turn into a sub-subagent (capped by depth_cap).
}
\keyword{internal}
