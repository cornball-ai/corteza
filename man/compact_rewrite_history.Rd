% tinyrox says don't edit this manually, but it can't stop you!
\name{compact_rewrite_history}
\alias{compact_rewrite_history}
\title{Replace the compacted prefix of a session's history with a
single assistant summary message.}
\usage{
compact_rewrite_history(history, cut, summary)
}
\description{
Pure function: returns the new history list, doesn't mutate
anything. The summary is prefixed with a `[compacted history]`
tag (followed by a blank line) so it's visually distinct in the
transcript.
}
\keyword{internal}
