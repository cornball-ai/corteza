% tinyrox says don't edit this manually, but it can't stop you!
\name{bot_send}
\alias{bot_send}
\title{Send a message to a Matrix room}
\usage{
bot_send(text, room_id = NULL, msgtype = "m.text", markdown = FALSE)
}
\arguments{
\item{text}{Character. Plain text body.}

\item{room_id}{Character. Matrix room id. Defaults to \code{cfg$room_id}
from the saved Matrix config (see \code{\link{bot_configure}}).}

\item{msgtype}{Character. Matrix msgtype, default "m.text".}

\item{markdown}{Logical. If TRUE, also send Matrix custom HTML derived
from a conservative markdown subset.}
}
\value{
The event ID of the sent message.
}
\description{
Send a message to a Matrix room
}
\examples{
\dontrun{
# Requires bot_configure() to have run.
bot_send("hello from corteza")
}
}
