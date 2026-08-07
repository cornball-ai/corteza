% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_archive_all}
\alias{matrix_archive_all}
\title{Deprecated: use bot_archive_all()}
\usage{
matrix_archive_all(sessions, chat = NULL)
}
\arguments{
\item{sessions}{A registry environment built by
\code{bot_run}/\code{bot_poll}. Keys are room IDs, values
are session environments carrying \code{$transcript}, the
Matrix-visible event ledger.}

\item{chat}{Optional chat.api client for room-name lookups. When
NULL, the room ID is used as the source identifier.}
}
\value{
See \code{\link{bot_archive_all}}.
}
\description{
Deprecated: use bot_archive_all()
}
\examples{
\dontrun{
matrix_archive_all(sessions)  # use bot_archive_all()
}
}
\seealso{
\code{\link{bot_archive_all}}
}
