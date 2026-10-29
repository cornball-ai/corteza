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
\dontrun{
audit_payload_read(ref)
}
}
