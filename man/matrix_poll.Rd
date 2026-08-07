% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_poll}
\alias{matrix_poll}
\title{Deprecated: use bot_poll()}
\usage{
matrix_poll(
  system = NULL,
  model = NULL,
  provider = NULL,
  tools_filter = NULL,
  timeout = 0L,
  sessions = NULL
)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}

\item{timeout}{Integer. Long-poll timeout in milliseconds. 0 returns
immediately.}

\item{sessions}{Environment from \code{bot_new_session_registry()}
keyed by room_id, or NULL to build fresh sessions each call.}
}
\value{
See \code{\link{bot_poll}}.
}
\description{
Deprecated: use bot_poll()
}
\examples{
\dontrun{
matrix_poll()  # use bot_poll()
}
}
\seealso{
\code{\link{bot_poll}}
}
