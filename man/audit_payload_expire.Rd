% tinyrox says don't edit this manually, but it can't stop you!
\name{audit_payload_expire}
\alias{audit_payload_expire}
\title{Expire unpinned audit payloads explicitly}
\usage{
audit_payload_expire(store, days = 30, now = Sys.time(), dry_run = TRUE)
}
\arguments{
\item{store}{A writer's payload store path from a payload reference.}

\item{days}{Retain this many days, default 30.}

\item{now}{Comparison time, default Sys.time().}

\item{dry_run}{Report eligible hashes without deleting, default TRUE.}
}
\value{
Character vector of eligible (or deleted) hashes.
}
\description{
Automatic expiry requires a positive \code{retention.audit_payload_days}
in the user's configuration. Only files in the named writer's payload store are
eligible; event metadata and other writers' stores are left intact. Expiry
decisions append to a separate maintenance stream before deletion. Event
retention is a planning value until an explicit event-retention tool is used.
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
