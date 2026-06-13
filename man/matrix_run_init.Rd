% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_run_init}
\alias{matrix_run_init}
\title{Initialize the Matrix long-poll state}
\usage{
matrix_run_init(system = NULL, model = NULL, provider = NULL,
                tools_filter = NULL)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}
}
\value{
A list holding the session registry, startup session handle,
  crypto context (or NULL), archive-flush signal path, and the saved
  poll options. Pass it to \code{\link{matrix_run_step}}.
}
\description{
Performs everything \code{\link{matrix_run}} does before its loop:
builds the per-room session registry, catches up on invites that
predate the saved sync token, backfills recent room history into the
registry, and (when the config sets \code{e2ee}) builds the E2EE
crypto context. Returns an opaque state object to drive with
\code{\link{matrix_run_step}}.
}
\details{
Use this with \code{matrix_run_step()} when an external loop owns the
main process and needs to interleave the Matrix poll with other work
(a scheduler, a multiplexer, an embedding host). For a standalone bot,
call \code{\link{matrix_run}}, which wraps both.

}
\examples{
\dontrun{
# Drive the loop yourself instead of calling matrix_run():
state <- matrix_run_init()
repeat matrix_run_step(state, timeout = 30000L)
}
}
\seealso{
\code{\link{matrix_run_step}}, \code{\link{matrix_run}}
}
