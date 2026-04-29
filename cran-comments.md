## Submission summary

Resubmission of 'corteza' (v0.6.3), an agent runtime that lets Large
Language Models (LLMs) drive an R session through a policy-gated
tool-use loop. Three entry points: an interactive console
read-eval-print-loop (`chat()`), a shell command-line interface
(`corteza`), and a Model Context Protocol (MCP) server (`serve()`)
for external clients like Claude Code or Codex.

## Changes since v0.6.0 in response to reviewer feedback

- **Title**: dropped redundant "for R".
- **Description**: removed single quotes from non-package names (MCP,
  the package's own names); added angle-bracketed URLs for each named
  external service (Anthropic, OpenAI, Moonshot, Ollama); expanded
  the REPL acronym to "read-eval-print-loop".
- **Console output**: gated user-facing `cat()`/`print()` calls
  behind a `corteza.verbose` option (default `interactive()`). See
  "Console output policy" below.
- **Examples**: `chat()`'s `\dontrun{}` switched to
  `if (interactive())` per the cookbook for interactive functions;
  added toy examples to several previously-undocumented exports.
- **`setwd()`**: `serve()` now restores the caller's working
  directory via `on.exit()`. `worker_init()`'s `setwd()` is unchanged
  on purpose — see "Worker cwd" below.
- **`.GlobalEnv`**: scoped to the agent's tool-evaluation surface
  only — see "Tool evaluation in `.GlobalEnv`" below.
- **`installed.packages()`**: kept for `tool_installed_packages()`
  because that tool's contract is "list every installed package",
  which `find.package()` and `system.file()` cannot satisfy. See
  "`installed.packages()` usage" below.
- **Filesystem**: Matrix configuration moved from
  `~/.corteza/matrix.json` to
  `tools::R_user_dir("corteza", "config")`. Existing files at the
  legacy path are read transparently; on next save, configuration
  writes to the new location.
- Reworded a few error messages so they no longer literally include
  `install.packages('...')` as a string — those triggered an
  automated "installing packages in functions" flag even though the
  text is pure user hint, never executed.

## R CMD check results

- 0 errors
- 0 warnings
- 1 NOTE ("New submission") — expected.

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

## Notes for reviewers

### Console output policy

Three audiences for console output, each handled separately:

1. **User-facing chat / REPL prompts** in `R/chat.R`, `R/turn.R`,
   `R/cli-ui.R`. These are interactive functions per the cookbook
   exception for "print, summary, interactive functions". Every
   non-essential `cat()` is now gated behind
   `getOption("corteza.verbose", interactive())` so a non-verbose
   user (or test harness) gets quiet behavior.
2. **Diagnostic loggers** in `R/log.R`, `R/utils.R`, `R/dispatch.R`,
   `R/session.R`, `R/transport.R`. All logger calls are routed
   through a single `log_msg()` helper that respects the same
   `corteza.verbose` option and writes to `stderr()`, so users can
   redirect or suppress with standard shell mechanics.
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
would *immediately* undo what the function exists to do — the
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
  (`R/transport-signal.R`, `R/workspace.R`, `R/tool-impl.R`), never
  `.GlobalEnv`.
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
`tools::R_user_dir("corteza", "config" | "data")`. Directories are
created only when the user explicitly invokes a function that
persists state (`chat()`, `install_cli()`, `skill_install()`,
`matrix_configure()`, etc.) — never during load, install,
examples, or tests. `R/paths.R` centralizes the path helpers.
Tests write only to `tempfile()` / `tempdir()` locations.

### Examples

`chat()` is wrapped in `if (interactive())`; remaining
`\dontrun{}` blocks (`install_cli()`, `uninstall_cli()`, `serve()`)
genuinely cannot run in `R CMD check`:

- `install_cli()` / `uninstall_cli()` modify files under
  `tools::R_user_dir("corteza", "data")/bin`. Running these during
  check would leave artefacts on reviewer / builder machines.
- `serve()` starts a long-running MCP server on a local socket and
  blocks until interrupted.

### Platform support

- Tested on Linux (Ubuntu 24.04) with R 4.5, and Windows 10 with
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

All Imports are already on CRAN: `callr`, `codetools`, `curl`,
`jsonlite`, `llm.api`, `printify`, `processx`, `saber`.

### Suggests

`mx.api` (Matrix Client-Server API, on CRAN) is in Suggests — it
powers the Matrix chat adapter. Users who don't use Matrix don't
need it installed. All Matrix-dependent code paths use
`requireNamespace("mx.api", quietly = TRUE)` before calling mx.api
functions.

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
external clients.
