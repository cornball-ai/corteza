# Working in corteza

Corteza is an R-native AI agent runtime: a CLI binary
(`inst/bin/corteza`, installed as `~/bin/corteza`) plus an in-R
`corteza::chat()`, both backed by a CRAN-installable package. This
file is what an agent (LLM or human) needs to know to work in this
codebase effectively. `AGENTS.md` is a symlink to this file so both
agent conventions read the same content.

## Tool isolation: `run_r` is stateless

`run_r` runs every call in a fresh R process via `callr::rscript()`
(see `R/tool-impl.R`). Variables do **not** persist across calls.

```r
# Call 1
x <- 1
# Call 2
x  # Error: object 'x' not found
```

Implications:

- Build the full computation in one `run_r` block, or stash
  intermediate results to disk yourself.
- Large outputs are captured as handles (e.g. `.h_001`) for the agent
  to reference later; those are read-only snapshots, not workspace
  state.
- Same isolation rationale applies to subagents: each
  `subagent_spawn()` opens a private `callr::r_session` so child work
  can't leak into the parent's R session.

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

## Tinyverse toolchain

```bash
# Format -> document -> install -> test
r -e 'rformat::rformat_dir("R", control_braces = "multi", expand_if = TRUE); tinyrox::document(); tinypkgr::install(); tinytest::test_package("corteza")'
```

- Use `tinypkgr::install()` not `R CMD INSTALL`.
  `tinypkgr::reload()` is the live-dev variant.
- `tinyrox` is the doc generator (not roxygen2). Same `@param`,
  `@return`, `@export` syntax; minimal feature set.
- `tinytest` is the test framework (not testthat). Tests live in
  `inst/tinytest/`.
- Use `r` (littler) for internal commands and `Rscript` for
  user-facing examples that need to run on Windows.

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

`inst/bin/corteza` (shell binary) has the full command set:
`/quit /clear /tools /spawn /agents /ask /kill /help /sessions /trace
/permissions /context /dryrun /compact /skill /model /provider /diff
/review /status /doctor /config /last /outputs /r`.

`corteza::chat()` (in-R) covers the subagent, skill, info, and basic
introspection surface. It does **not** yet support
`/status /doctor /config /diff /review /last /outputs` — those depend
on display helpers that still live inside the CLI script. Filed as a
follow-up.

The CLI's `/remember /recall /flush` are dead branches: they call
`memory_store` / `memory_search` / `strip_tags` / `parse_tags` which
don't exist in the package. Don't use them.

## Project context loading

Project context comes from `saber::briefing()` and
`saber::agent_context()` (recent commits, package summary, AGENTS.md
or CLAUDE.md, etc.) plus any files explicitly listed in
`config$context_files`. The `/context` slash command shows live token
usage broken into system / tools / history.

## Things to avoid

- **No `Co-Authored-By` trailers** in commits.
- **Version bumps follow `~/.claude/CLAUDE.md` "Version Bumping":**
  fourth-component dev markers ride with every non-test code PR
  (so `main` is always visibly ahead of CRAN); third/minor/major
  *release* bumps go in the last PR of a batched release, never the
  first. Test-only, CI-only, and docs-fix PRs don't bump.
- **No drive-by cosmetic edits** — don't bundle whitespace or comment
  reformats into substantive PRs.
- **Don't hide real dependencies behind `requireNamespace()`**. If a
  package is needed for core functionality, declare it in `Imports`.
- **Don't force-push.** Use incremental commits during PR review;
  squash at merge time via `gh pr merge --squash`.

## Repo layout (orientation)

```
R/
├── archival.R         # Retroactive-extraction runtime (opt-in)
├── chat.R             # corteza::chat() in-R loop
├── chat-slash.R       # Slash-command helpers shared with chat()
├── config.R           # Config file loading + defaults
├── context.R          # System prompt assembly (saber + tools + skills)
├── log.R              # Structured JSON logging
├── session.R          # Session persistence (transcripts, metadata)
├── skill.R            # Skill system (SKILL.md parsing, registry)
├── subagent.R         # callr-based subagents + registry
├── tool-impl.R        # Tool implementations + built-in skill registration
├── tools.R            # Tool category groupings
└── turn.R             # Single agent turn / observer hook

inst/
├── bin/corteza        # CLI binary (Rscript shebang)
└── tinytest/          # Tests (1300+ asserts)

vignettes/
├── configuration.Rmd
├── retroactive-extraction.Rmd
└── skills.Rmd
```
