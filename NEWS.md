# corteza 0.6.0

First CRAN submission.

## Architecture: CLI / worker split

Eight-phase refactor of the command-line interface so its subprocess
no longer speaks MCP internally.

* The CLI now drives a private `callr::r_session` worker. Tool
  dispatch goes through `corteza::worker_dispatch()` directly; no
  JSON-RPC, no `tools/list` handshake, no per-call envelope on the
  CLI-to-worker path.
* `serve()` remains a spec-compliant MCP server for external clients
  (Claude Desktop, VS Code, `mcptools`). Public MCP behavior is
  unchanged.
* New shared tool registry in `R/registry.R`. `chat()`, `serve()`,
  and the CLI all read from `.skill_registry`. No state duplication.
* `cli_worker_spawn()`, `worker_init()`, `worker_dispatch()`,
  `worker_tool_list()`, `cli_worker_drain_events()` exposed with
  `@keywords internal` so the callr session can reach them as
  `corteza::*`.
* Boundary-normalized errors: `corteza_tool_error` condition class
  carries tool name, args, original class, and message across the
  worker pipe.
* Subagents (`R/subagent.R`) also use `callr::r_session` instead of
  spawning `corteza::serve()` children. Same architecture: one
  persistent worker per subagent, direct tool dispatch, no MCP.

## Derived tool schemas

* New `R/schema.R` with `schema_from_fn()` and `register_skill_from_fn()`.
  Tool definitions are derived at runtime from `formals()` and the
  package's `.Rd` files via `tools::Rd_db()`. Replaces 20+
  hand-written `skill_spec(params = list(...))` blocks with one-line
  registrations.
* Type hints come from an R-style `(type)` parenthetical in `@param`
  docs (`(character)`, `(integer)`, `(logical)`, `(character vector)`,
  `(character; one of: a, b, c)` for enums).
* `schema_from_registry()` produces the Anthropic-API-shaped tools
  payload the CLI sends to the model — in the CLI's own process, not
  round-tripped through the worker.
* `inst/tinytest/test_tool_schemas.R` asserts every formal maps to a
  `@param` entry and vice versa. Drift between doc and signature
  fails the test suite.

## Context-aware tool pruning

* `register_skill_from_fn()` accepts an `available` predicate;
  `schema_from_registry()` filters tools whose predicate returns
  `FALSE`. Git tools gate on `.git`; web search gates on
  `TAVILY_API_KEY`. 18-20% fewer tokens in the system prompt for a
  bare environment.

## Handle-based large results

* `tool_run_r()` wraps non-scalar or over-threshold values with
  `with_handle()`. The LLM gets a `str()` summary plus an opaque
  `.h_NNN` handle instead of a flood of printed output.
* New `tool_read_handle(handle, op)` for subsequent inspection
  (`str`, `head`, `summary`, `print`). Handles are addressable by
  name in later `run_r` calls.

## Observability

* Worker emits structured JSON events (`tool_call`, `tool_result`,
  timings) to stderr. CLI drains them between calls.
* New `--trace` flag (and `options(corteza.trace = TRUE)`)
  pretty-prints the events inline via `printify::print_step()` /
  `printify::print_message()`.
* ANSI color detection: `NO_COLOR` honored, `FORCE_COLOR` overrides,
  classic Windows consoles fall back to plain text.

## Platform support

* Windows tested against R 4.5.3 + Rtools45 + Git for Windows.
* The shell tool resolves `bash` to an absolute path (Rtools first,
  Git Bash fallback) so `C:\Windows\System32\bash.exe` (the WSL
  launcher stub) cannot intercept commands.
* Fallback `cmd` shell tool when no real bash is found.
* Path validation uses `normalizePath(winslash = "/")` consistently.

## Dependencies

* Added: `callr` (for the worker transport), `printify` (for `--trace`
  rendering).
* Kept: `codetools`, `curl`, `jsonlite`, `llm.api`, `processx`,
  `saber`.
* Suggests: `mx.api`, `tinytest`.
