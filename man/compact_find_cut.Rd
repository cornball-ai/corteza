% tinyrox says don't edit this manually, but it can't stop you!
\name{compact_find_cut}
\alias{compact_find_cut}
\title{Find the largest cut point in `history` that doesn't split a
tool_use / tool_result pair.}
\usage{
compact_find_cut(history, keep_recent_turns = 1L)
}
\arguments{
\item{history}{Live in-memory history list.}

\item{keep_recent_turns}{Number of recent user→assistant turns to
keep verbatim (a turn starts at a user message).}
}
\description{
Returns the number of entries that can safely be summarized
(entries `1..cut`). Entries `cut+1..end` are preserved verbatim.
Returns 0 when no safe cut is available.
}
\details{
Strategy: start from the maximum cut that leaves `keep_recent_turns`
user-prompt boundaries intact, then walk back as needed so the cut
doesn't land between a tool_use and the tool_result that satisfies
it.
}
\keyword{internal}
