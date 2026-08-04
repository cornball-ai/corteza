## Submission summary

Update of 'corteza' (0.7.0 -> 0.8.0), an agent runtime that lets Large
Language Models (LLMs) drive an R session through a policy-gated
tool-use loop. Three entry points: an interactive console
read-eval-print-loop (`chat()`), a shell command-line interface
(`corteza`), and a Model Context Protocol (MCP) server (`serve()`)
for external clients like Claude Code or Codex.

Minor bump from the on-CRAN 0.7.0, batching the 0.7.0.1-0.7.0.9 dev
cycles. It is a minor rather than a patch because it adds features: a
new provider, provider-native web search, and four new configuration
keys. No exported functions were added or removed. Four gained optional
arguments with defaults preserving the previous behaviour (`chat()`,
`matrix_configure()`, `new_session()`, `session_setup()`), so existing
calls are unaffected.

Highlights:

* New `openai_compatible` provider, reaching any OpenAI-compatible
  gateway (OpenRouter, DeepSeek, a local proxy, a corporate gateway)
  through a `base_url` config key or `--base-url` flag.
* Provider-native web search in the agent loop, on by default for the
  hosted providers that support it, with no third-party search key
  required.
* Matrix access control: the new `operators` key restricts one-on-one
  conversations and invite acceptance to named accounts. Leaving it
  unset preserves the previous behaviour.
* Matrix reply gating counts humans rather than members, with a
  declared `bots` list and a cached, refreshed membership view.
* Archiving is keyed on Matrix event ids through a persisted per-room
  ledger, fixing repeated re-archiving of the backfill tail on restart.

## Dependency note

`Imports: llm.api (>= 0.1.9)`. The `openai_compatible` provider applies
its endpoint through `llm.api::llm_base()` scoped to each agent call,
which needs that version. llm.api 0.1.9 is submitted alongside this
release; please process it first.

## R CMD check results

- 0 errors
- 0 warnings
- 0 notes

`R CMD check --as-cran` is clean (Status: OK) on:

* Ubuntu 24.04 LTS, R 4.5.3
* Windows 10, R 4.6.0
* Windows 10, R-devel (2026-07-21 r90286 ucrt)

## Notes

* Every example that would contact a live LLM endpoint, start a
  subprocess, or write outside `tempdir()` is wrapped in `\dontrun{}`.
  The rest run, and write only to `tempdir()`.
* The package reads API keys from environment variables only. No
  credentials are bundled, and nothing is written outside `tempdir()`
  during checks.
* Optional integrations (Matrix chat via 'mx.api'/'mx.client'/
  'mx.crypto', knowledge-vault archiving via 'pensar', clipboard via
  'clipr') are in Suggests and guarded at every call site, so the
  package is fully functional without them.
