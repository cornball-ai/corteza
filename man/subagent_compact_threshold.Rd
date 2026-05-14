% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_compact_threshold}
\alias{subagent_compact_threshold}
\title{Resolve the effective compaction threshold for a subagent.}
\usage{
subagent_compact_threshold(config)
}
\arguments{
\item{config}{Full corteza config (post-defaults).}
}
\value{
Numeric percent in (0, 100], or NULL.
}
\description{
Returns a numeric percent. NULL means "compaction off for this
child" — caller skips entirely.
}
\keyword{internal}
