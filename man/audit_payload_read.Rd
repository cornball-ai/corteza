% tinyrox says don't edit this manually, but it can't stop you!
\name{audit_payload_read}
\alias{audit_payload_read}
\title{Read an immutable audit payload}
\usage{
audit_payload_read(ref, redact = TRUE, secrets = character())
}
\arguments{
\item{ref}{Payload reference from an audit event or request manifest.}

\item{redact}{Apply display redaction. Defaults to TRUE.}

\item{secrets}{Additional known credential values to hide in the view.}
}
\value{
The decoded JSON object or UTF-8 text, as recorded.
}
\description{
Reads only a payload whose bytes still match its recorded SHA-256. Redaction
affects the returned view, never the stored record. An omitted, expired,
missing or damaged payload is an error, not an empty successful read.
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
