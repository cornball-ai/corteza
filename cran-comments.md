## Submission summary

Update of 'corteza' (v0.7.0), an agent runtime that lets Large
Language Models (LLMs) drive an R session through a policy-gated
tool-use loop. Three entry points: an interactive console
read-eval-print-loop (`chat()`), a shell command-line interface
(`corteza`), and a Model Context Protocol (MCP) server (`serve()`)
for external clients like Claude Code or Codex.

This release is a minor bump from the on-CRAN 0.6.9, batching the
0.6.9.1-0.6.9.7 dev cycles plus a runtime-hardening pass. The dev
markers are preserved in NEWS.md so reviewers can trace each
substantive change to its PR.

## R CMD check results

- 0 errors
- 0 warnings
- 0 notes

`R CMD check --as-cran` is clean on Ubuntu 24.04 LTS, R 4.6.0, against
llm.api 0.1.8 (this release's required Imports floor).

## Note on the llm.api (>= 0.1.8) dependency

corteza 0.7.0 requires llm.api (>= 0.1.8), which was published to CRAN
earlier today. The macOS and Windows binaries for llm.api 0.1.8 are still
being built, so the Ubuntu check above used llm.api 0.1.8 installed from
source. A win-builder run can't complete the dependency install until those
binaries are available; I'm glad to provide a win-builder report once they
land if that would help.

## Changes since v0.6.9

Highlights, with the full per-PR detail in NEWS.md:

- **Matrix loop split** into `matrix_run_init()` / `matrix_run_step()`,
  letting an external scheduler own the main process; the channel
  delegates session and transport plumbing to `mx.client`.
- **`anthropic_claude` provider** drives Claude on a Claude subscription
  via OAuth (no API key), through `llm.api` (>= 0.1.8).
- **Tool-timeout hardening.** Self-bounding tools (bash/cmd/run_r/
  run_r_script) and the network tools (`fetch_url`/`web_search`, now
  bounded by curl's own connect/total timeout) are no longer wrapped in
  an R-level `setTimeLimit`; the residual flush moved to `skill_run`
  entry so a validation or dry-run early return can't leak a queued
  transient interrupt.
- **Narration guard.** Every tool outcome (executed, denied, declined,
  dry-run, task) routes through one nudge finalizer, and the streak
  resets at each agent-run boundary.
- **Approval prompt.** Model-controlled fields rendered into the approval
  prompt and history (paths, tool names, args, policy reasons) are sanitized
  so a crafted value cannot forge a prompt line; a one-line tool explanation
  is shown at approval time.
- **`/compact` survives provider-native history** (Anthropic, OpenAI chat,
  and role-less OpenAI Codex Responses entries), so a wedged session can be
  recovered regardless of provider.

## Reviewer-facing notes (carried over from v0.6.3)

The CRAN-policy carve-outs from the v0.6.3 submission still apply
unchanged; they're documented in detail below in case reviewers want
to revisit them.

### Console output policy

Three audiences for console output, each handled separately:

1. **User-facing chat / REPL prompts** in `R/chat.R`, `R/turn.R`,
   `R/cli-ui.R`. These are interactive functions per the cookbook
   exception for "print, summary, interactive functions". Every
   non-essential `cat()` is gated behind
   `getOption("corteza.verbose", interactive())` so a non-verbose
   user (or test harness) gets quiet behavior.
2. **Diagnostic loggers** in `R/log.R`, `R/utils.R`, `R/dispatch.R`,
   `R/session.R`. All logger calls route through a single
   `log_msg()` helper that respects the same `corteza.verbose`
   option and writes to `stderr()`, so users can redirect or
   suppress with standard shell mechanics.
3. **MCP protocol traffic** in `R/mcp-transport.R`. The two
   `cat(json, ..., file = stdout())` and `writeLines(json, client)`
   calls are *protocol writes* (a JSON-RPC server speaking to a
   client over stdio or a socket); they are not user-readable
   information messages and cannot be suppressed without breaking
   the MCP transport.

### Subagent session cwd

`worker_init()` runs in a private `callr::r_session` subprocess
(separate R session, separate process) when a subagent child session
starts. Its job is to set that child's `cwd` for the lifetime of the
subprocess so every tool the subagent runs inherits it. Wrapping that
`setwd()` in `on.exit(setwd(oldwd))` would *immediately* undo what the
function exists to do -- the subagent would then run every tool from
the wrong directory. The user's main R session is never touched.

### Tool evaluation in `.GlobalEnv`

The `tool_run_r()` tool (and the agent more broadly) is the
package's reason for existing: it lets the LLM read and modify the
user's live R session by request. That necessarily means evaluating
in `.GlobalEnv`. We treat this analogously to the `shiny` exception
the cookbook mentions for the same rule:

- The package itself never writes to `.GlobalEnv` from its own
  functions outside the explicit tool surface.
- All package-internal `<<-` usages target closure parents
  (`R/workspace.R`, `R/tool-impl.R`), never `.GlobalEnv`.
- Reads of `.GlobalEnv` (`ls(globalenv())`, `get(... envir =
  globalenv())`) appear only inside the tool surface to capture
  what the user's instructions just produced.
- The agent runs under a policy gate (`R/policy.R`) and an
  approval callback so destructive calls require user consent.

### `installed.packages()` usage

Used in two sites under `R/tool-impl.R`:

- `tool_installed_packages()`: the user-callable LLM tool whose
  contract is "list every installed package, optionally filtered".
  The cookbook's suggested replacements (`find.package`,
  `system.file`, `requireNamespace`) all answer "is this one
  package available?" and cannot enumerate. The tool is also
  rate-limited at the LLM-call level (called interactively, not in
  a hot loop) so the cookbook's slowness concern doesn't apply.
- `tool_r_help()`: validates that a help-topic exists in an
  installed package before attempting to render. The check could
  be rewritten using `find.package(quiet = TRUE)`; happy to do
  that on request, but the current single call per `?topic` lookup
  is bounded.

### Filesystem policy

The package never writes to `~/`, the package directory, the
working directory, or the clipboard. User-scoped state goes through
`tools::R_user_dir("corteza", "config" | "data" | "cache")`.
Directories are created only when the user explicitly invokes a
function that persists state (`chat()`, `install_cli()`,
`skill_install()`, `matrix_configure()`, the `/copy` slash command,
etc.) -- never during load, install, examples, or tests. `R/paths.R`
centralizes the path helpers. Tests write only to `tempfile()` /
`tempdir()` locations.

The Matrix config has a read-only fallback to the legacy
`~/.corteza/matrix.json` path so users upgrading from
pre-v0.6.3 setups don't see their configuration vanish. New writes
always land in the `R_user_dir` location; the legacy path is never
written to.

### Optional companion package (pensar)

`matrix_archive_session()` calls into the companion package
**`pensar`** (now on CRAN). corteza keeps it an optional integration
rather than a hard `Suggests`, so a base install doesn't pull it: the
function uses
`tryCatch(getExportedValue("pensar", "ingest"), error = function(e) NULL)`
for dynamic lookup, and when pensar isn't installed
`matrix_archive_session()` is a silent no-op.

### Examples

`chat()` is wrapped in `if (interactive())`. The `subagent_*`,
`matrix_*`, `turn()`, `session_setup()`, `mcp_tool_executor()`,
`install_cli()`, `uninstall_cli()`, and `serve()` examples use
`\dontrun{}` because they require LLM credentials, a Matrix server,
a running MCP socket, or write user-installation state -- none of
which are appropriate to exercise under `R CMD check`. Pure helpers
(`policy()`, `add_observer()`, `observer_progress()`,
`skill_list_installed()`, `skill_test()`, `skill_install()`,
`skill_remove()`, `subagent_list()`, `subagent_kill()`,
`matrix_request_flush()`) have runnable examples that operate on
`tempdir()` or a temporary `CORTEZA_STATE_DIR`.

### Platform support

- Tested on Linux (Ubuntu 24.04) with R 4.6 and Windows 10 with
  R 4.5.3 + Rtools45 + Git for Windows.
- The shell tool registers as `bash` when a real bash is available
  (always on POSIX; Rtools/Git for Windows on Windows). On
  minimal-install Windows it falls back to a `cmd` tool.
- The CLI helper `install_cli()` handles both platforms, writing an
  Rscript shebang file on Unix and a `.cmd` wrapper on Windows.
- Path validation uses `normalizePath(winslash = "/")` consistently
  so prefix checks match identically on both platforms.

### Non-interactive guard

No package code runs during `library(corteza)` that could surprise a
user. There are no `.onLoad` or `.onAttach` hooks; no file-system
writes at load time; no network activity at load time.

### Imports

All Imports are on CRAN: `callr`, `codetools`, `curl`, `jsonlite`,
`llm.api`, `printify`, `processx`, `saber`.

### Suggests

All Suggests are on CRAN: `clipr` (clipboard for `/copy`),
`fortunes` (Easter-egg quotes), `mx.api` (Matrix Client-Server
API), `mx.client` (stateful Matrix client + E2EE orchestration),
`mx.crypto` (Olm/Megolm E2EE), `rstudioapi` (RStudio addin glue),
`simplermarkdown` (vignettes), `tinytest` (tests). Users who don't
use a given feature don't need that Suggests installed. Every
Suggests-backed call site uses `requireNamespace(..., quietly =
TRUE)` before calling into the dependency.

### SystemRequirements

On Windows, Rtools45 (R 4.5.x) or Rtools44 (R 4.4.x) is recommended
so the `bash` shell tool is available; minimal installs fall back
to `cmd`. `git` is required for `git_status`, `git_diff`, and
`git_log` tools (install Git for Windows, or `pacman -Sy git` from
an Rtools shell).

### Architecture note for reviewers

The shell CLI (`corteza`) and `chat()` share one read-eval-print loop
(`run_repl_loop()`) and execute tools in-process, in a single R
session -- no CLI worker subprocess and no internal MCP transport.
The MCP surface in `serve()` is independent and continues to exist for
external clients (Claude Desktop, VS Code, mcptools, etc.). The only
child R process is the per-subagent `callr::r_session`.
