## Submission summary

Update of 'corteza' (0.7.1 -> 0.8.0), an agent runtime that lets Large
Language Models (LLMs) drive an R session through a policy-gated
tool-use loop. Three entry points: an interactive console
read-eval-print-loop (`chat()`), a shell command-line interface
(`corteza`), and a Model Context Protocol (MCP) server (`serve()`)
for external clients like Claude Code or Codex.

Minor release over the on-CRAN 0.7.1, batching the 0.7.1.1-0.7.1.94 dev
cycles. No exported function was removed. Seventeen were added, and the
four `matrix_*` functions remain as deprecated wrappers around their
`bot_*` replacements, so existing calls are unaffected.

Highlights:

* Chat rooms run on the 'chat.api' transport contract; the room
  functions are renamed `bot_*`, with the `matrix_*` names deprecated.
* Delegated jobs in rooms: supervised worker processes with a reviewer,
  per-job approval, and resumable attempts.
* Remote control of a command-line session from a private chat room.
* Audit records of turns, tool decisions, and request payloads, read
  with `audit_read()` and the `audit_payload_*()` functions.
* A session journal of the provider-native history and opt-in context
  editing.
* `voice_serve()`, a gRPC service for one-to-one voice.
* Provider fallback, bounded autonomous runs, and per-tool output caps.

## Dependency note

Every dependency is on CRAN; the package no longer declares
`Additional_repositories`.

* `Imports: llm.api (>= 0.2.0)` and `saber (>= 0.8.0)`, both published.
* `Suggests: chat.api (>= 0.2.0)` and `mx.client (>= 0.3.0)`. This
  submission follows those two releases, by the same maintainer.

## R CMD check results

- 0 errors
- 0 warnings
- 1 note

`R CMD check --as-cran` (Status: 1 NOTE) on:

* Ubuntu 24.04 LTS, R 4.6.1

The NOTE:

```
Found the following assignments to the global environment:
File 'corteza/R/job-resume-snapshot.R':
  assign(name, values[[name]], globalenv())
File 'corteza/R/job-worker.R':
  assign(name, values[[name]], envir = globalenv())
```

Both run only inside a worker R process that the package itself starts
for a delegated job, never in the user's session. They restore that
worker's own workspace checkpoint when a job resumes, so the code the
job runs next finds the objects its earlier attempt created.

## Notes

* Every example that would contact a live LLM endpoint, start a
  subprocess, or write outside `tempdir()` is wrapped in `\dontrun{}`.
  The rest run, and write only to `tempdir()`.
* The package reads API keys from environment variables only. No
  credentials are bundled, and nothing is written outside `tempdir()`
  during checks: the test runner redirects R's user cache, data, and
  config directories into `tempdir()`.
* Optional integrations (chat rooms via 'chat.api' and 'mx.client',
  voice via 'rgrpc' and 'RProtoBuf', knowledge-vault archiving via
  'pensar', clipboard via 'clipr') are in Suggests and guarded at every
  call site, so the package is fully functional without them.
* There are no reverse dependencies on CRAN.
