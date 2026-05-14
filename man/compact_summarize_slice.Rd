% tinyrox says don't edit this manually, but it can't stop you!
\name{compact_summarize_slice}
\alias{compact_summarize_slice}
\title{Summarize the prefix of a history slice via the LLM.}
\usage{
compact_summarize_slice(slice, provider = "anthropic", model = NULL,
                        timeout_seconds = 60L)
}
\arguments{
\item{slice}{List of history entries to summarize (the part being
compacted; the recent tail is excluded).}

\item{provider}{Provider name.}

\item{model}{Model name.}

\item{timeout_seconds}{Hard wall on the summarizer call.}
}
\description{
Returns the summary text on success or NULL on any error
(including timeout). Caller leaves history intact on NULL.
}
\keyword{internal}
