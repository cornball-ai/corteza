% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_archive_all}
\alias{matrix_archive_all}
\title{Flush all in-memory matrix sessions to the pensar vault}
\usage{
matrix_archive_all(sessions, mx_sess = NULL)
}
\arguments{
\item{sessions}{A registry environment built by
\code{matrix_run}/\code{matrix_poll}. Keys are room IDs, values
are session environments carrying \code{$transcript}, the
Matrix-visible event ledger.}

\item{mx_sess}{Optional Matrix session for room-name lookups. When
NULL, the room ID is used as the source identifier.}
}
\value{
Integer count of rooms ingested, invisibly.
}
\description{
Walks the per-room session registry and archives each room's
unarchived Matrix events via the pensar archive ingest. Archived
events are consumed from the session ledger, and their Matrix event
ids are persisted per room under \code{CORTEZA_STATE_DIR} so a
restart's backfill is recognized rather than archived again. Silent
no-op when \code{pensar} is not installed.
}
\examples{
\dontrun{
# Requires a running Matrix session registry and the optional
# pensar package for the actual archive step.
reg <- new.env(parent = emptyenv())
matrix_archive_all(reg)
}
}
