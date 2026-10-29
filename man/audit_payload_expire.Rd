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
Never runs automatically. Only files in the named writer's payload store are
eligible; event metadata and other writers' stores are left intact. Expiry
decisions append to a separate maintenance stream before deletion. Event
retention is a planning value until an explicit event-retention tool is used.
}
\examples{
\dontrun{
audit_payload_expire(ref$store)
}
}
