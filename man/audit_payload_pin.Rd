% tinyrox says don't edit this manually, but it can't stop you!
\name{audit_payload_pin}
\alias{audit_payload_pin}
\title{Pin or unpin an audit payload}
\usage{
audit_payload_pin(ref, pin = TRUE)
}
\arguments{
\item{ref}{Payload reference from an audit event.}

\item{pin}{TRUE to pin, FALSE to remove the pin.}
}
\value{
The payload reference, invisibly.
}
\description{
Pins keep incident/evaluation evidence through explicit expiry. Pinned bytes
still count toward the store quota; capture reports quota omissions when full.
}
\examples{
\dontrun{
audit_payload_pin(ref)
}
}
