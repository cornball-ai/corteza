% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_turn_set_id}
\alias{subagent_turn_set_id}
\title{Set this child's subagent id post-spawn.}
\usage{
subagent_turn_set_id(id)
}
\arguments{
\item{id}{Subagent id assigned by the parent.}
}
\value{
Invisible TRUE.
}
\description{
Called from [subagent_spawn()] right after [subagent_turn_init()] so
the child knows its own id when archival inside the child needs to
pass `parent_session_id`.
}
\keyword{internal}
