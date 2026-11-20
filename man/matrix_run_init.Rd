% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_run_init}
\alias{matrix_run_init}
\title{Deprecated: use bot_run_init()}
\usage{
matrix_run_init(
  system = NULL,
  model = NULL,
  provider = NULL,
  tools_filter = NULL,
  hooks = NULL,
  cwd = NULL,
  project_roots = NULL
)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}

\item{hooks}{Named list of functions or NULL. Policy the loop asks
the host about; see Hooks. NULL, the default, means a bare bot:
\code{/clear} resets and nothing is filed, a thread reply starts
from the thread's own history, and every message is the bot's to
answer.}

\item{cwd}{Character or NULL. The directory a room works in when no
operator has bound it to one with \code{/cd}, for this run. It must
exist: corteza does not create it, and the run stops at its start
when this directory, or the one saved by \code{\link{bot_configure}},
is missing. NULL (default) uses the saved one. With neither, such a
room works in a scratch folder under the session's temporary
directory, which is deleted when the process ends.}

\item{project_roots}{Character vector or NULL. The directories inside
which an operator may bind a room with \code{/cd} and list with
\code{/ls}, for this run. Each must exist: corteza does not create
them. NULL (default) uses the ones saved by
\code{\link{bot_configure}}; with none, \code{/cd <dir>} is refused.}
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
