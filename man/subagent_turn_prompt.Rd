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
Reply text (character).
}
\description{
Captures the pre-turn history length so that, if archival is enabled
and this query qualifies, the child can recursively archive its own
turn into a sub-subagent (capped by depth_cap).
}
\keyword{internal}
