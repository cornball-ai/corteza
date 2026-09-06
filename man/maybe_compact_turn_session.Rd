% tinyrox says don't edit this manually, but it can't stop you!
\name{maybe_compact_turn_session}
\alias{maybe_compact_turn_session}
\title{Maybe compact a turn_session's in-memory history.}
\usage{
maybe_compact_turn_session(
  session,
  config,
  kind = NULL,
  tools = NULL,
  system = NULL,
  threshold,
  min_messages,
  reason = "threshold"
)
}
\arguments{
\item{session}{A turn_session (`new_session()`).}

\item{config}{Full corteza config (post-defaults).}

\item{kind}{Optional marker. "archive_holder" skips compaction
entirely so seeded transcript history is preserved.}

\item{tools}{Optional exact model-facing tool schema. Direct `turn()`
callers with custom tools should pass it; otherwise tools are resolved
from `session$tools_filter` as before.}

\item{system}{Optional exact model-facing system prompt.}

\item{threshold}{Optional explicit percentage. When omitted, parent/direct
sessions use `config$context_compact_pct` and subagents use their stricter
inherited policy. Explicit NULL disables compaction.}

\item{min_messages}{Optional minimum history entries. The ordinary policy
uses its configured value; forced overflow recovery may lower this because
a few individual entries can themselves exceed the context window.}

\item{reason}{Short lifecycle marker recorded in the compaction event.}
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
