<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Audit records}
-->
---
title: Audit records
---

# Audit records

Corteza records model turns, tool decisions, provider requests and room
deliveries as linked events. Each writer owns its JSONL stream. A warm worker
keeps its stream across jobs, with the job ID recorded on each applicable
event. Child processes write separate streams and carry parent turn/call
references. There is no global sequence and no shared-file append protocol.

The history journal remains the provider-native conversation record. Audit
streams live in its `audit/` directory, or in the package data `audit/` directory
when no journal is configured. `journal_enabled: false` does not disable
accounting. Set `audit.enabled: false` explicitly to disable it.

## Reading and accounting

`turn()` returns an `audit` reference containing an absolute stream path and
turn ID. `audit_read(path)` reads the stream through the auto-log decoder and
returns records plus malformed-line counts/locations, `torn_tail`,
`malformed_tail`, and `interior_damage`. Its default view is redacted; use
`redact = FALSE` only when an exact restricted view is needed. Additional known
values can be supplied in `secrets`. Pattern-based filtering cannot identify
every credential.

| Event | Meaning |
|---|---|
| `turn_start`, `turn_end` | Canonical model work, including error/interruption endings |
| `tool_call`, `policy`, `tool_event` | A call ID exists before allow/ask/deny; decisions and execution join by it |
| `request_start`, `request_end`, `request_error` | Logical provider request and individual transport attempts |
| `tool_payload`, `tool_capture_child` | Full capture and admitted preview, including supervised evaluator output |
| `delivery_start`, `delivery_end` | Destination, every acknowledged event ID, posted-text hash or send error |
| `auxiliary_dispatch`, `auxiliary_exit`, `auxiliary_turn` | Links and exits for summaries, reviews and other auxiliary calls |
| `gap`, `payload_omitted`, `payload_expiry` | Explicit gaps, bounded capture and maintenance decisions |

Sum canonical `turn_end` usage once per turn ID. Job outcomes, monitor verdicts
and transcripts preserve their compatibility usage fields and mark them
`usage_source: derived`, with originating turn references. Provider request
events are a `breakdown`, not another charge to add. Auxiliary summaries and
consultations own their own canonical turns, linked to their parent.

With llm.api 0.1.9.15, usage comes from observed requests, including final
tool-less replies and partial requests before a fallback. Request records keep
raw usage separately from normalized counts. Requested and reported models are
distinct. A failed attempt without provider usage makes coverage partial;
missing usage is not invented as zero. Costs are estimates tied to the llm.api
version and its bundled table/calculation fingerprints. Earlier llm.api remains
supported with per-request coverage `unavailable`. An instrumented client that
emits no requests is marked `unobserved`, as happens with some test adapters.

A missing terminal record means unresolved completion. It does not establish
that no tool effect or model charge occurred. A killed worker's ordinary job
outcome can report indeterminate effects while its child stream ends at start.

## Immutable requests and output

Request starts point to a manifest containing the final serialized adapter
body, system/instructions, tool schema, ordered message objects and effective
settings. This includes provider-added tools and cache annotations. Authentication
headers are excluded. SHA-256 references remain valid after in-place history
growth, context edits, compaction and restart. Journal sequence/length is only
a navigation aid because the live snapshot can change within a sequence.

`audit_payload_read(ref)` checks the content hash before returning a view.
Missing, expired, omitted or damaged payloads are explicit errors. Native history
and stored payloads retain their exact contents; redaction never rewrites them.
The full captured text of a tool result is recorded before the output cap, with
the admitted preview separately identified. Supervised R evaluators write their
own linked stream before their local cap; R object summaries also preserve the
full printed output. This records captured output, not arbitrary hidden process
state or an execution sandbox.

Streams, payloads, transcripts and traces are private on creation. They can
contain sensitive data. Room displays keep their existing redaction and
summary/detail policy; audit data is not automatically sent to a room. Any
same-user code with filesystem access can still tamper with records. Hashes
detect payload damage but do not establish tamper-proof provenance.

## Bounds, pins and failures

Capture defaults are 16 MiB per full tool result, 256 MiB per turn, and 2 GiB per
writer's payload store. Set `audit.limits.result_bytes`, `turn_bytes` and
`quota_bytes` to change them. Each writer deduplicates identical bytes by hash;
quotas are not a machine-wide bound across restarts. Supervised captures charge
their bytes to the parent turn's remaining allowance. Omission reasons are
`per_result`, `per_turn`, `quota` or `capture_failure`.

`audit_payload_pin(ref)` exempts a payload from expiry. Pins still count toward
quota, so a full pinned store causes future capture omissions. Explicit
`audit_payload_expire(ref$store, days = 30)` previews expiry using last-capture
time. Passing `dry_run = FALSE` logs expiry intents/results and removes eligible
unpinned bytes. Missing end records leave maintenance unresolved too. No expiry
runs automatically, and event streams are not deleted by this function. A
365-day event-retention horizon is planning guidance until an explicit
event-retention policy is selected.

New audit writes are observational: they emit one process notice on failure,
count missed events, and append a `gap` on recovery. A recording failure cannot
retry a provider request or resend a room message. If the process dies before
recovery, the lost count is unknown. Existing journal rewrites, job intent and
approval commits keep their required-write behavior.

Records support later investigation and evaluation. Replay, scoring and derived
signals such as user corrections remain separate analysis work.
