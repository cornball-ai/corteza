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
# A payload as an audit event records it: bytes named by their SHA-256
# in a writer's store, under a folder called "payloads".
store <- file.path(tempfile("audit"), "payloads", "example")
dir.create(store, recursive = TRUE)
text <- "the full output of a tool call"
hash <- digest::digest(charToRaw(text), algo = "sha256", serialize = FALSE)
writeBin(charToRaw(text), file.path(store, hash))
ref <- list(store = store, hash = hash, format = "text", status = "stored")
audit_payload_read(ref)
audit_payload_pin(ref)
audit_payload_pin(ref, pin = FALSE)
# A dry run lists what is older than 30 days and unpinned: nothing here.
audit_payload_expire(store)
unlink(dirname(dirname(store)), recursive = TRUE)
}
