% tinyrox says don't edit this manually, but it can't stop you!
\name{audit_read}
\alias{audit_read}
\title{Read an audit event stream}
\usage{
audit_read(path, redact = TRUE, secrets = character())
}
\arguments{
\item{path}{Path to a writer's JSONL stream.}

\item{redact}{Apply display redaction, default TRUE.}

\item{secrets}{Additional known credential values to hide.}
}
\value{
List with records, auto-run start/end compatibility fields and
  malformed-record diagnostics. Turn endings remain in records.
}
\description{
Uses the auto-log decoder, retaining malformed-line locations and separating
a torn final line from interior damage. A missing terminal turn record means
unresolved completion. Redaction changes this returned view only; callers
can supply additional known credential values. It cannot detect all secrets.
}
\examples{
audit_read(tempfile())
}
