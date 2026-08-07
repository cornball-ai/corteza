% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_run_step}
\alias{matrix_run_step}
\title{Deprecated: use bot_run_step()}
\usage{
matrix_run_step(state, timeout = 30000L)
}
\arguments{
\item{state}{A state object from \code{\link{bot_run_init}}.}

\item{timeout}{Integer. Long-poll timeout in milliseconds.}
}
\value{
See \code{\link{bot_run_step}}.
}
\description{
Deprecated: use bot_run_step()
}
\examples{
\dontrun{
matrix_run_step(state)  # use bot_run_step()
}
}
\seealso{
\code{\link{bot_run_step}}
}
