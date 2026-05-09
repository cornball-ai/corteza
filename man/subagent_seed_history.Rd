% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_seed_history}
\alias{subagent_seed_history}
\title{Seed the child's turn-session history with an externally-built slice.}
\usage{
subagent_seed_history(history)
}
\arguments{
\item{history}{List of message entries.}
}
\value{
Invisible TRUE.
}
\description{
Used by the archival runtime: the parent spawns a holder subagent,
then ships the just-finished turn's history into the holder via this
function so the holder owns the full transcript while the parent
keeps only `\{summary, subagent_id\}`.
}
\keyword{internal}
