## Submission summary

This is the first submission of 'corteza' (v0.5.0), an agent runtime
that lets Large Language Models (LLMs) drive an interactive R session
through a policy-gated tool-use loop.

## R CMD check results

- 0 errors
- 0 warnings
- 1 NOTE ("New submission") — expected on a first submission

## Notes for reviewers

### Examples use \dontrun{}

All four exported functions with `@examples` blocks (`chat()`,
`install_cli()`, `uninstall_cli()`, `serve()`) wrap their examples in
`\dontrun{}`. This is not a style choice — each genuinely cannot be
run during non-interactive `R CMD check`:

- `chat()` errors with `"chat() requires an interactive R session"`
  when `interactive()` is FALSE, and additionally requires one of
  `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, or `MOONSHOT_API_KEY` in the
  environment (or a reachable Ollama server) to make any progress.
- `install_cli()` and `uninstall_cli()` modify files in the user's
  `~/bin` (Unix) or `tools::R_user_dir("corteza", "data")/bin`
  (Windows). Running these during check would leave artefacts on
  reviewer / CRAN-builder machines.
- `serve()` starts a long-running MCP server on a local socket and
  blocks. It is not safe to run unattended in a check environment.

### Filesystem policy

The package never writes to `~/.corteza/` or any hardcoded user-home
path. All user-scoped state goes through `tools::R_user_dir("corteza",
"config" | "data")` and the directories are created only when the
user explicitly invokes a function that persists state
(`chat()`, `install_cli()`, `skill_install()`, etc.) — never during
load, install, examples, or tests.

`R/paths.R` centralizes the path helpers. Tests write only to
`tempfile()` / `tempdir()` locations.

### Platform support

- Tested on Linux (Ubuntu 24.04) with R 4.5.
- The shell-execution tool registers as `bash` on Unix and `cmd` on
  Windows (`tool-impl.R` inspects `.Platform$OS.type`).
- The CLI helper `install_cli()` handles both platforms, writing an
  Rscript shebang file on Unix and a `.cmd` wrapper on Windows.

### Non-interactive guard

No package code runs during `library(corteza)` that could surprise a
user. There are no `.onLoad` or `.onAttach` hooks; no file-system
writes at load time; no network activity at load time.

### Downstream package notes

- `saber`, `llm.api`: already on CRAN.
- `mx.api` (for a Matrix chat adapter) is deliberately not part of
  this submission. The Matrix adapter lives on a separate branch and
  will be added back as a point release after `mx.api` itself lands
  on CRAN.
