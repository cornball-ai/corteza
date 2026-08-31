% tinyrox says don't edit this manually, but it can't stop you!
\name{auto_escalate_marker}
\alias{auto_escalate_marker}
\title{History marker written when an unattended run escalates to a human.}
\usage{
auto_escalate_marker(reason = "")
}
\arguments{
\item{reason}{Character. Why a human is needed.}
}
\value{
Character scalar.
}
\description{
Carries the same "stop and ask" directive as its siblings, for the
same reason: whatever the worker does next, it must not be another
attempt at the thing that just got escalated.
}
\keyword{internal}
