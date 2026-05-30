# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Corteza is an R-native AI agent runtime: a CLI binary
(`inst/bin/corteza`, installed as `~/bin/corteza`) plus an in-R
`corteza::chat()`, both backed by a CRAN-installable package. This
file is what an agent (LLM or human) needs to know to work in this
codebase effectively. `AGENTS.md` is a symlink to this file so both
agent conventions read the same content.

## Tinyverse toolchain

```bash
# Format -> document -> install -> test (full suite)
r -e 'rformat::rformat_dir("R", control_braces = "multi", expand_if = TRUE); tinyrox::document(); tinypkgr::install(); tinytest::test_package("corteza")'

# Run a single test file
r -e 'tinypkgr::install(); tinytest::run_test_file("inst/tinytest/test_policy.R")'
```

- Use `tinypkgr::install()` not `R CMD INSTALL`.
  `tinypkgr::reload()` is the live-dev variant.
- `tinyrox` is the doc generator (not roxygen2). Same `@param`,
  `@return`, `@export` syntax; minimal feature set.
- `tinytest` is the test framework (not testthat). Tests live in
  `inst/tinytest/`.
- Use `r` (littler) for internal commands and `Rscript` for
  user-facing examples that need to run on Windows.

## Architecture: three surfaces, one `turn()`

All three user-facing surfaces share `turn()` as the single entry point
(`R/turn.R`). Each surface constructs a session environment via
`new_session()`, wires up a channel-specific `approval_cb`, then calls
`turn(prompt, session)`.

```
CLI (inst/bin/corteza)
  │  approval_cb = readline prompt
  │  tool_executor = NULL  <- in-process call_skill
  v
run_repl_loop(ctx) -> turn(prompt, session)  <- R/repl.R, R/turn.R (shared)
  │  policy(call, config)  <- R/policy.R
  │  tool_handler dispatches in-process
  v
call_skill()  <- R/skill.R

chat()  (corteza::chat(), R/chat.R)
  │  approval_cb = readline prompt
  │  tool_executor = NULL  <- in-process call_skill
  v
run_repl_loop(ctx) -> turn(prompt, session)  <- same shared loop

matrix adapter  (R/matrix.R)
  │  mx.api long-poll, tool_executor = NULL (in-process)
  v
turn(prompt, session)
```

**CLI and chat() share one in-process loop**: both build a `ctx` and call
`run_repl_loop()` (`R/repl.R`) in a single R process; tools run in-process
via `call_skill`. There is no CLI worker subprocess and no internal MCP
transport. `serve()` (`R/serve.R`) is a separate, spec-compliant MCP
server for external clients only. Subagents are the one place a child R
process is still used (`callr::r_session`, initialized via `worker_init()`).

**Tool registry** (`R/registry.R`): `.skill_registry` is the single
source of truth. `ensure_skills()` / `register_builtin_skills()`
populate it on first use. `skills_as_api_tools()` converts to the
`llm.api::agent()` format; `sanitize_tool_name()` maps `::` to `__`
for API compatibility.

**System prompt assembly** (`R/context.R`): layers (1) a preamble,
(2) `saber::briefing()` (package metadata, recent commits),
(3) `saber::agent_context()` (CLAUDE.md/AGENTS.md, SOUL.md, USER.md),
(4) custom `config$context_files`, (5) skill docs, (6) package tool
docs, (7) a live-subagents block if archival is active. Duplicate
blocks are deduped by exact match.

## Policy engine (`R/policy.R`)

Every tool call passes through `policy(call, config)` before dispatch.
Returns `list(model, approval, reason)` where `approval` is
`"allow"`, `"ask"`, or `"deny"`.

**Precedence (most specific wins):**
1. Hard safety rules — credential paths (`~/.ssh`, `~/.aws`, etc.)
   are always `ask` on local model; cannot be overridden.
2. Plan-mode gate — write/exec tools denied in plan mode.
3. User policy function via `options(corteza.policy = function(call) ...)`.
4. Default tensor on `(data_class, op, channel)`:
   - `data_class`: `"personal"`, `"code"`, or `"random"` (sticky
     within a turn — personal contaminates all later calls).
   - `op`: `"read"`, `"write"`, `"exec"`.
   - `channel`: `"cli"`, `"console"`, `"matrix"`.
5. Config overlay from `.corteza/config.json` (`approval_mode`,
   `dangerous_tools`, per-tool `permissions`).

## Configuration

Config merges global (`tools::R_user_dir("corteza", "config")/config.json`)
with project (`.corteza/config.json`); project overrides global.
Key fields: `provider`, `port` (default 7850), `context_warn_pct` /
`context_high_pct` / `context_crit_pct` / `context_compact_pct`,
`context_files`, `approval_mode`, `dangerous_tools`, `permissions`.

## Tool execution model: `run_r` vs `run_r_script`

`run_r` is **stateful**. `tool_run_r` calls
`eval(parse(text = code), envir = handle_eval_env(parent = globalenv()))`,
so `<-` and `=` assignments land in `globalenv()` and persist across
calls. The workspace auto-capture explicitly depends on this. Use
`run_r` for incremental exploration where intermediate results need to
carry forward.

`run_r_script` is **stateless**: each call runs in a fresh R
subprocess via `callr::rscript()` (see `R/tool-impl.R`). Variables do
**not** persist across calls. Use `run_r_script` for clean-slate runs:
reproducible test execution, isolation from a polluted `globalenv`,
or commands you don't want leaking state into the live session.

```r
# run_r — stateful
run_r("x <- 1")
run_r("x")           # 1

# run_r_script — stateless
run_r_script("x <- 1")
run_r_script("x")    # Error: object 'x' not found
```

Implications:

- Large outputs from either tool are captured as handles (e.g.
  `.h_001`) for the agent to reference later; those are read-only
  snapshots, not workspace state.
- Same isolation rationale as `run_r_script` applies to subagents:
  each `subagent_spawn()` opens a private `callr::r_session` so child
  work can't leak into the parent's R session.

## Subagent registry is in-memory only

`.subagent_registry` is a package-level environment. Spawned subagents
die with the parent R process. Don't rely on subagents persisting
across `chat()` exits.

When the **archival** runtime is on (`config$archival$enabled = TRUE`)
finished turns collapse into holder subagents that hold the full
transcript on disk under `agents/subagent-<id>/sessions/`. The
parent's in-memory history keeps only a summary plus the holder's id;
the LLM sees the holder in its system prompt and picks
`query_subagent` vs `spawn_subagent` via normal tool selection.
Default off — see `vignette("retroactive-extraction")`.

## Subagent ids

Subagents have three valid identifiers:

- **UUID** (canonical): `c39a6889-4bb0-4425-896c-65a5b59c2b41`
- **8-char prefix**: `c39a6889` (or any unambiguous prefix)
- **Sequence number**: `1`, `2`, ... (per-process monotonic counter,
  surfaced in `subagent_list()` as `seq`)

`subagent_query` and `subagent_kill` accept any of the three.

## Saber for introspection (mandatory before exported-API changes)

```r
saber::pkg_exports("corteza")
saber::pkg_help("subagent_spawn", "corteza")
saber::blast_radius("subagent_spawn", project = ".")
```

`blast_radius` is required before renaming, moving, or changing the
signature of any exported function. Skipping it breaks downstream
packages silently.

## Test conventions and gotchas

- `at_home()` gates tests that need network, API keys, or writable
  state. They run locally; `R CMD check` skips them.
- **Don't use `on.exit()` at the top level of a tinytest file** — it
  fires immediately, not at script end. Use explicit cleanup at the
  bottom of the file instead.
- `corteza:::env[[key]] <- value` doesn't parse the way you'd expect
  for a package-private environment. Use
  `assign(key, value, envir = corteza:::env)`.
- `llm.api::agent` calls hit the network. Gate any test that calls it
  with both `at_home()` and `nzchar(Sys.getenv("ANTHROPIC_API_KEY"))`.

## CLI vs `chat()` slash-command parity

Both surfaces share one slash-command implementation. `inst/bin/corteza`
(shell binary) and `corteza::chat()` (in-R) each build a `ctx` and hand
off to `run_repl_loop()` (`R/repl.R`), which dispatches every slash
command, so the command set is identical across both. The
`/status /doctor /config /diff /review /last /outputs` commands (once
CLI-only) now work in `chat()` too.

`/remember` and `/recall` no longer exist. `/flush` is the live memory
command (manual memory flush), sharing one `run_memory_flush()`
implementation (`R/chat-slash.R`) across both surfaces via
`run_repl_loop()`. The legacy `memory_store` / `memory_search` tools are
off by default behind `config$legacy_memory_tools_enabled`.

## Project context loading

Project context comes from `saber::briefing()` and
`saber::agent_context()` (recent commits, package summary, AGENTS.md
or CLAUDE.md, etc.) plus any files explicitly listed in
`config$context_files`. The `/context` slash command shows live token
usage broken into system / tools / history.

## Things to avoid

- **No `Co-Authored-By` trailers** in commits.
- **No drive-by cosmetic edits** — don't bundle whitespace or comment
  reformats into substantive PRs.
- **Don't hide real dependencies behind `requireNamespace()`**. If a
  package is needed for core functionality, declare it in `Imports`.
- **Don't force-push.** Use incremental commits during PR review;
  squash at merge time via `gh pr merge --squash`.

## Repo layout (orientation)

```
R/
├── turn.R             # Shared agent turn (entry point for all surfaces)
├── policy.R           # Tool-call policy engine (allow/ask/deny)
├── context.R          # System prompt assembly
├── registry.R         # Shared .skill_registry environment
├── skill.R            # skill_spec, skill_run, SKILL.md loading
├── tool-impl.R        # Built-in tool implementations
├── tools.R            # Tool category groupings + get_tools()
├── chat.R             # corteza::chat() in-R loop
├── chat-slash.R       # Slash-command helpers shared with chat()
├── serve.R            # MCP server (stdio + socket transports)
├── subagent.R         # callr-based subagents + registry
├── matrix.R           # Matrix channel adapter (mx.api)
├── config.R           # Config file loading + defaults
├── session.R          # Session persistence (transcripts, metadata)
├── archival.R         # Retroactive-extraction runtime (opt-in)
├── log.R              # Structured JSON logging
├── tasks.R            # Task-tracker intercept in turn()
├── permissions.R      # /permissions surface helpers
├── plan-mode.R        # Plan-mode gate helpers
├── workspace.R        # Workspace dir management
└── (many more helpers: chunk.R, handles.R, retrieval.R, etc.)

inst/
├── bin/corteza        # CLI binary (Rscript shebang)
└── tinytest/          # Tests (1300+ asserts)

vignettes/
├── configuration.md
├── retroactive-extraction.md
└── skills.md
```
