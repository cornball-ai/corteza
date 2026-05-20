% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_kill}
\alias{subagent_kill}
\title{Kill a subagent.}
\usage{
subagent_kill(id)
}
\arguments{
\item{id}{Subagent identifier (UUID, prefix, or sequence number).}
}
\value{
Invisible TRUE if killed, FALSE if not found.
}
\description{
Kill a subagent.
}
\examples{
# Unknown id is a silent no-op (returns FALSE), so this is safe to
# run during R CMD check without a live subagent.
subagent_kill("no-such-id")
}
