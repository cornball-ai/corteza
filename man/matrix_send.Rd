% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_send}
\alias{matrix_send}
\title{Send a message to a Matrix room}
\usage{
matrix_send(text, room_id = NULL, msgtype = "m.text")
}
\arguments{
\item{text}{Character. Plain text body.}

\item{room_id}{Character. Matrix room id. Defaults to \code{cfg$room_id}
from the saved Matrix config (see \code{\link{matrix_configure}}).}

\item{msgtype}{Character. Matrix msgtype, default "m.text".}
}
\value{
The event ID of the sent message.
}
\description{
Send a message to a Matrix room
}
\examples{
\dontrun{
# Requires matrix_configure() to have run.
matrix_send("hello from corteza")
}
}
