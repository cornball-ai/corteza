## Submission summary

Resubmission of 'corteza' (v0.6.7), an agent runtime that lets Large
Language Models (LLMs) drive an R session through a policy-gated
tool-use loop. Three entry points: an interactive console
read-eval-print-loop (`chat()`), a shell command-line interface
(`corteza`), and a Model Context Protocol (MCP) server (`serve()`)
for external clients like Claude Code or Codex.

This release is a patch bump from the on-CRAN 0.6.3 series, batching
twenty post-release dev cycles (0.6.6.1 -> 0.6.6.20) plus one
out-of-band change (deny aborts) that merged without a dev marker.
The dev markers are preserved in NEWS.md so reviewers can trace each
substantive change to its PR.

## R CMD check results

- 0 errors
- 0 warnings
- 0 notes

`R CMD check --as-cran` is clean on Ubuntu 24.04 LTS, R 4.6.0.
`devtools::check_win_devel()` / `tinypkgr::check_win_devel()` was
run for win-devel and win-release.

## Changes since v0.6.3

Highlights, with the full per-PR detail in NEWS.md:

- **CLI / worker split.** The shell CLI now drives a private
  `callr::r_session` for tool dispatch (no internal MCP). `serve()`
  remains a spec-compliant MCP server for external clients. Same
  tool registry, shared via `R/registry.R`.
- **Derived tool schemas.** Tool definitions are derived at runtime
  from `formals()` and the package's `.Rd` files. Replaced 20+
  hand-written `skill_spec()` blocks with one-line registrations.
  A test asserts every formal maps to a `@param` entry and vice
  versa.
- **Subagents.** `subagent_spawn()` runs a child via
  `callr::r_session`; the parent talks to it through
  `subagent_query()` / `subagent_collect()` (sync or async).
  `subagent_list()` / `subagent_kill()` round out the surface.
- **Retroactive-extraction runtime (opt-in).** Off by default;
  enabling it collapses finished turns into holder subagents that
  keep the full transcript on disk while the parent keeps only a
  summary. See `vignette("retroactive-extraction")`.
- **Handle-based large results.** `tool_run_r()` wraps oversized
  values with `with_handle()` and returns an opaque `.h_NNN` handle
  the LLM can dereference in later `tool_read_handle()` calls.
- **`/copy` slash command, inline diffs, markdown rendering,
  per-turn timing footer, /context meter, /tasks, /paste,
  interrupt handling, Matrix bot adapter,** and other UX work.
  Each PR documented in NEWS.md.

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

### Worker cwd

`worker_init()` runs in a private `callr::r_session` subprocess
(separate R session, separate process), called once when the
worker starts. Its job is to set the worker's `cwd` for the
lifetime of the subprocess so every subsequent `worker_dispatch()`
inherits it. Wrapping that `setwd()` in `on.exit(setwd(oldwd))`
would *immediately* undo what the function exists to do -- the
worker would then dispatch every tool from the wrong directory.
The user's main R session is never touched.

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

`matrix_archive_session()` calls into a companion package
**`pensar`** that is not on CRAN (lives at
<https://github.com/cornball-ai/pensar>). Because CRAN does not
accept `Suggests:` entries that aren't on CRAN/Bioconductor, the
function uses
`tryCatch(getExportedValue("pensar", "ingest"), error = function(e) NULL)`
for dynamic lookup: when pensar isn't installed,
`matrix_archive_session()` is a silent no-op. Users who want
session archiving install pensar manually from GitHub.

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
API), `rstudioapi` (RStudio addin glue), `simplermarkdown`
(vignettes), `tinytest` (tests). Users who don't use a given
feature don't need that Suggests installed. Every Suggests-backed
call site uses `requireNamespace(..., quietly = TRUE)` before
calling into the dependency.

### SystemRequirements

On Windows, Rtools45 (R 4.5.x) or Rtools44 (R 4.4.x) is recommended
so the `bash` shell tool is available; minimal installs fall back
to `cmd`. `git` is required for `git_status`, `git_diff`, and
`git_log` tools (install Git for Windows, or `pacman -Sy git` from
an Rtools shell).

### Architecture note for reviewers

The shell CLI (`corteza`) spawns a private `callr::r_session` for
tool execution, rather than an MCP subprocess. CLI users pay a
one-time ~250ms callr worker warm-up but then each tool call is a
direct R-native dispatch (~11ms) through `worker_dispatch()`. The
MCP surface in `serve()` is independent and continues to exist for
external clients (Claude Desktop, VS Code, mcptools, etc.).
