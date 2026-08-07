% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_run}
\alias{matrix_run}
\title{Deprecated: use bot_run()}
\usage{
matrix_run(
  timeout = 30000L,
  system = NULL,
  model = NULL,
  provider = NULL,
  tools_filter = NULL
)
}
\arguments{
\item{timeout}{Integer. Long-poll timeout in milliseconds.}

\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}
}
\value{
See \code{\link{bot_run}}.
}
\description{
Deprecated: use bot_run()
}
\examples{
\dontrun{
matrix_run()  # use bot_run()
}
}
\seealso{
\code{\link{bot_run}}
}
