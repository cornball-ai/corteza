% tinyrox says don't edit this manually, but it can't stop you!
\name{user_deny_marker}
\alias{user_deny_marker}
\title{History marker written when a turn is aborted by a user deny.}
\usage{
user_deny_marker(tool = "?")
}
\arguments{
\item{tool}{Character. Name of the denied tool.}
}
\value{
Character scalar.
}
\description{
Format chosen so the LLM, reading the marker on the next turn, knows
to stop and ask the user how to proceed instead of retrying the same
tool or planning a workaround.
}
\keyword{internal}
