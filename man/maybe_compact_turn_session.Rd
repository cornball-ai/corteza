% tinyrox says don't edit this manually, but it can't stop you!
\name{maybe_compact_turn_session}
\alias{maybe_compact_turn_session}
\title{Maybe compact a turn_session's in-memory history.}
\usage{
maybe_compact_turn_session(session, config, kind = NULL)
}
\arguments{
\item{session}{A turn_session (`new_session()`).}

\item{config}{Full corteza config (post-defaults).}

\item{kind}{Optional marker. "archive_holder" skips compaction
entirely so seeded transcript history is preserved.}
}
\description{
Decision points:
- Compaction mode off → return invisibly without checking.
- History shorter than `min_messages` → skip (nothing to gain).
- Live token usage below threshold → skip.
- No safe cut available (e.g. open tool_use) → skip.
- Summarizer fails → log and leave history intact.
}
\details{
On success, mutates `session$history` in place. Returns invisibly
TRUE if compaction ran successfully, FALSE otherwise.

}
\keyword{internal}
