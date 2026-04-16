% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_send}
\alias{matrix_send}
\title{Send a message to the configured Matrix room}
\usage{
matrix_send(text, msgtype = "m.text")
}
\arguments{
\item{text}{Character. Plain text body.}

\item{msgtype}{Character. Matrix msgtype, default "m.text".}
}
\value{
The event ID of the sent message.
}
\description{
Send a message to the configured Matrix room
}
