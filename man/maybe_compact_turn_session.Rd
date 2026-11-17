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
  byte_limit,
  min_messages,
  reason = "threshold",
  now = Sys.time()
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

\item{byte_limit}{Optional serialized-request threshold. When omitted,
`config$context_compact_bytes` applies to `openai_codex`; other providers
have no byte threshold. Explicit `Inf` disables the byte guard.}

\item{min_messages}{Optional minimum history entries. The ordinary policy
uses its configured value; forced overflow recovery may lower this because
a few individual entries can themselves exceed the context window.}

\item{reason}{Short lifecycle marker recorded in the compaction event.
`"preflight"`, the check ahead of a turn's first request, is also
where a session back from an idle gap compacts; the event then
carries the reason `"idle"`.}

\item{now}{The time the idle gap is measured to. A test passes its
own.}
}
\description{
Decision points:
- Compaction mode off → return invisibly without checking.
- History shorter than `min_messages` → skip (nothing to gain).
- Live token usage and serialized request size below thresholds → skip.
The token threshold is the smaller of `context_compact_pct` of the
window and `context_compact_tokens`. Ahead of a turn's first
request, a session idle for `context_idle_minutes` with
`context_idle_tokens` of context compacts below it
(R/compact-trigger.R).
- No safe cut available (e.g. open tool_use) → skip.
- Summarizer fails → log and leave history intact. The session's
next summary then reads the smaller budget of 120,000 characters,
until one succeeds. So does a summary made after the provider
refused a request for its size (R/compact-trigger.R).
}
\details{
On success, mutates `session$history` in place. Returns invisibly
TRUE if compaction ran successfully, FALSE otherwise.

}
\keyword{internal}
