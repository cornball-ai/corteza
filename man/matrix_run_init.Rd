% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_run_init}
\alias{matrix_run_init}
\title{Deprecated: use bot_run_init()}
\usage{
matrix_run_init(
  system = NULL,
  model = NULL,
  provider = NULL,
  tools_filter = NULL
)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}
}
\value{
See \code{\link{bot_run_init}}.
}
\description{
Deprecated: use bot_run_init()
}
\examples{
\dontrun{
matrix_run_init()  # use bot_run_init()
}
}
\seealso{
\code{\link{bot_run_init}}
}
