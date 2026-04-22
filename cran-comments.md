## Submission summary

This is the first CRAN submission of 'corteza' (v0.6.0), an agent
runtime that lets Large Language Models (LLMs) drive an R session
through a policy-gated tool-use loop. Three entry points: an
interactive console REPL (`chat()`), a shell CLI (`corteza`), and a
Model Context Protocol ('MCP') server (`serve()`) for external
clients like Claude Code or Codex.

## R CMD check results

- 0 errors
- 0 warnings
- 1 NOTE ("New submission") — expected on a first submission

## Notes for reviewers

### Examples use \dontrun{}

All exported functions with `@examples` blocks (`chat()`,
`install_cli()`, `uninstall_cli()`, `serve()`, `subagent_spawn()`,
`subagent_query()`, `subagent_kill()`) wrap their examples in
`\dontrun{}`. Each genuinely cannot be run during non-interactive
`R CMD check`:

- `chat()` errors with `"chat() requires an interactive R session"`
  when `interactive()` is FALSE, and additionally requires one of
  `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, or `MOONSHOT_API_KEY` in the
  environment (or a reachable Ollama server) to make any progress.
- `install_cli()` and `uninstall_cli()` modify files in
  `tools::R_user_dir("corteza", "data")/bin`. Running these during
  check would leave artefacts on reviewer / CRAN-builder machines.
- `serve()` starts a long-running MCP server on a local socket and
  blocks. Not safe to run unattended in a check environment.
- `subagent_spawn()` creates a `callr::r_session` that loads corteza
  inside it, which is too heavy for a check-time example.

### Filesystem policy

The package never writes to `~/.corteza/` or any hardcoded user-home
path. All user-scoped state goes through `tools::R_user_dir("corteza",
"config" | "data")` and the directories are created only when the
user explicitly invokes a function that persists state (`chat()`,
`install_cli()`, `skill_install()`, etc.) — never during load,
install, examples, or tests. `R/paths.R` centralizes the path
helpers. Tests write only to `tempfile()` / `tempdir()` locations.

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
powers the Matrix chat adapter (`matrix_run()` and friends). Users
who don't use Matrix don't need it installed. All Matrix-dependent
code paths use `requireNamespace("mx.api", quietly = TRUE)` before
calling mx.api functions.

### SystemRequirements

On Windows, Rtools45 (R 4.5.x) or Rtools44 (R 4.4.x) is recommended
so the `bash` shell tool is available; minimal installs fall back to
`cmd`. `git` is required for `git_status`, `git_diff`, and `git_log`
tools (install Git for Windows, or `pacman -Sy git` from an Rtools
shell).

### Architecture note for reviewers

The shell CLI (`corteza`) spawns a private `callr::r_session` for tool
execution, rather than an MCP subprocess. This means CLI users pay a
one-time ~250ms callr worker warm-up but then each tool call is a
direct R-native dispatch (~11ms) through `worker_dispatch()`. The MCP
surface in `serve()` is independent and continues to exist for
external clients.
