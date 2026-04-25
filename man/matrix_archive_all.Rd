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
are session lists carrying \code{$history}.}

\item{mx_sess}{Optional Matrix session for room-name lookups. When
NULL, the room ID is used as the source identifier.}
}
\value{
Integer count of rooms ingested, invisibly.
}
\description{
Walks the per-room session registry and archives any turns that
haven't been ingested yet via \code{pensar::ingest(type = "matrix")}.
Each session tracks an \code{ingested_through} watermark so repeated
calls only write new turns. Silent no-op when \code{pensar} is not
installed.
}
