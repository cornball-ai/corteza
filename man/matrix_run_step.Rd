% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_run_step}
\alias{matrix_run_step}
\title{One Matrix long-poll iteration}
\usage{
matrix_run_step(state, timeout = 30000L)
}
\arguments{
\item{state}{A state object from \code{\link{matrix_run_init}}.}

\item{timeout}{Integer. Long-poll timeout in milliseconds.}
}
\value{
Invisibly, the integer count of messages replied to this poll.
}
\description{
Polls \code{/sync} once (blocking up to \code{timeout} ms, returning
early when a message arrives), runs the agent against any new messages
and posts the replies, then services a pending archive-flush signal.
Mutates the session registry and crypto context held in \code{state}
in place, so successive calls accumulate conversation history.
}
\examples{
\dontrun{
state <- matrix_run_init()
matrix_run_step(state, timeout = 5000L)
}
}
\seealso{
\code{\link{matrix_run_init}}, \code{\link{matrix_run}}
}
