% tinyrox says don't edit this manually, but it can't stop you!
\name{compact_summarize_slice}
\alias{compact_summarize_slice}
\title{Summarize the prefix of a history slice via the LLM.}
\usage{
compact_summarize_slice(
  slice,
  provider = "anthropic",
  model = NULL,
  timeout_seconds = 60L,
  summary_prompt = NULL,
  error_callback = NULL,
  session = NULL,
  max_total_chars = .compact_max_total_chars,
  max_request_bytes = NA_real_
)
}
\arguments{
\item{slice}{List of history entries to summarize (the part being
compacted; the recent tail is excluded).}

\item{provider}{Provider name.}

\item{model}{Model name.}

\item{timeout_seconds}{Hard wall on the summarizer call.}

\item{summary_prompt}{Optional caller-supplied summarization instructions.
NULL uses corteza's general coding-agent brief.}

\item{error_callback}{Optional function called with a caught summarizer
error before NULL is returned.}

\item{session}{Optional session supplying audit settings, identity and the
parent turn reference.}

\item{max_total_chars}{How many characters of the rendered slice the
request may hold. Past it the oldest entries are left out and the
summarizer is told how many. The compactor passes a budget that
follows the model's window.}

\item{max_request_bytes}{The most bytes the serialized request may
take, its framing included, or NA for no limit. Past it the oldest
entries are left out as well. A request that cannot fit is not sent,
nor one with room for no entry of the slice.}
}
\description{
Returns the summary text on success or NULL on any error
(including timeout). Caller leaves history intact on NULL.
}
\keyword{internal}
