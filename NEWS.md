# corteza 0.6.7

## Deny aborts the whole turn

* Picking "3. Deny" at the tool-approval prompt now aborts the
  entire turn instead of declining a single tool call. Previously
  the deny was returned as a `[user declined: ...]` tool result that
  the LLM saw and treated as feedback, planning the next call --
  which forced users to mash "3" through cascades of dependent tool
  calls. The next turn now starts with a history marker that names
  the denied tool and tells the LLM to stop and ask the user what to
  do instead, rather than retrying or planning a workaround.

# corteza 0.6.6.9

## tool_run_r_script dodges callr Windows hang

* `tool_run_r_script()` switches its `callr::rscript()` call from
  `stdout = "|"` to `stdout = NULL`, with `stderr = "2>&1"`
  unchanged. The old combination hangs indefinitely on Windows with
  CRAN callr 3.7.6 when the child script errors via `stop()` — the
  internal `timeout` never fires (r-lib/callr#313). `res$stdout` is
  still populated by the `2>&1` redirect, so the return value and
  LLM-facing text are identical. Can be reverted once we depend on a
  fixed callr (>= the post-`e93efd1` release).
* `test_subagent_callr.R` gate rationale corrected: that gate is
  about per-test budget, not the callr bug; `r_session` uses a
  separate code path and was empirically verified not affected.

# corteza 0.6.6.8

## Tool-count parity between chat() and CLI

* `skills_as_api_tools()` (the chat() path) now applies the same
  `available()` predicate filter that `schema_from_registry()` (the
  CLI path) has always used. Conditional tools — `git_*` when not
  inside a repo, platform-specific shell tools — no longer show up
  in chat() while being hidden in the CLI. Counts agree.

## Per-turn timing footer spans the terminal

* `turn_footer_line()` defaults to the detected terminal width
  (`COLUMNS` env, `options("width")`) instead of a fixed 60-char
  line, so the `─ Worked for 3m 18s ────` separator reaches the
  right edge.

## /context and /status are one command

* `/status` is now an alias of `/context`. Both render the same
  block: a Codex-style header (corteza version, model+provider,
  cwd, session id) followed by the context meter.
* The meter now segments by component — `system` in bright blue,
  `tools` in bright magenta, `history` in cyan — so the bar maps
  visually to the breakdown rows below.
* Empty cells take the threshold color (yellow / orange / red) once
  usage crosses the warn line so saturation reads at a glance
  before any single segment dominates.

# corteza 0.6.6.7

## /context shows a real meter

* New compact horizontal `/context` display answers the two questions
  a user actually has: how full is context, and what's using it. Same
  in both `corteza::chat()` and the CLI:

  ```
  Context  24.7K / 128.0K  19%  compact 90%
  [██████████..................................│.....]
    system    22.0K  89%
    tools      2.7K  11%
    history      56
  ```

  - Filled cells color-grade through normal / warn / high / crit
    thresholds (defaults 75/90/95) as usage climbs.
  - The auto-compact threshold shows as a subtle `│` tick at its
    cell position in the empty part of the bar.
  - Breakdown rows are right-aligned percentages of the *used* total;
    rows under 1% drop the percent to avoid "0%" noise.
* Dropped the "Project context still comes from saber::briefing() and
  saber::agent_context()..." paragraph — the bar already shows where
  the system budget is going, and the prose was longer than the data.

# corteza 0.6.6.6

## Per-turn timing footer

* After each `corteza::chat()` or `~/bin/corteza` turn, a dim
  footer line shows wall-clock duration: `─ Worked for 3m 18s ────`.
* Fires on success, interrupt (Esc / Ctrl+C), and error. Useful
  data point in the exact moments you wanted to know how long
  something ran before you bailed.
* Static-at-end, not a live ticking counter. A live counter while
  R is blocked on the LLM HTTP call would need async polling in
  llm.api or a background process; both are real concurrency
  surfaces worth their own design pass.

# corteza 0.6.6.5

## Slash-command parity between chat() and the CLI

* Display/info helpers used to live in `inst/bin/corteza` only, so
  `corteza::chat()` couldn't render the same `/status`, `/doctor`,
  `/config`, `/diff`, or `/review` output. They now live in
  `R/cli-helpers.R` as internal package functions; both surfaces call
  them. chat() gets all five commands plus `/last` and `/outputs`.
* `/compact` in chat() now uses the same `do_compact()` the CLI does,
  instead of a separate inline implementation. Both routes share one
  prompt and one chat-call shape.
* Tool output buffer (`/last`, `/outputs`) is now session-scoped via
  `session$sessionId` rather than a CLI-process global. Subagents and
  parents have isolated buffers; `/clear` drops the outgoing
  session's buffer.

## Dead commands removed

* `/remember` and `/recall` in the CLI are gone. They called
  `memory_store` / `memory_search` / `strip_tags` / `parse_tags` —
  none of which exist in the package — and would error on use.
  `/flush` is alive (rebuilt earlier this cycle as a real memory
  flush via `.run_memory_flush()`) and stays.

# corteza 0.6.6.4

## Input handling

* Backspace past the start of your typing no longer eats the `> `
  prompt character. The bash hack used for line editing now passes
  the prompt to `read -e -p` so readline owns the cursor instead of
  the prompt being a `cat()` that readline can't see. ANSI color
  escapes in the prompt are wrapped in `\001 \002` so readline's
  column math stays correct.
* The approval prompt's `Choice [1]: ` dropped the `[1]` shell
  convention; the `(Enter)` hint already lives on choice 1 itself,
  so the bracket was redundant.
* Multi-line input in both `corteza::chat()` and the CLI. Two
  entries with two contracts:
  - `/paste [optional text]` — explicit "paste anything" mode.
    Collects every line verbatim (logs, code, paths with literal
    trailing `\`, etc.) until `/end` on its own line or Ctrl+D.
  - Any non-slash line ending with an unescaped `\` — drops into
    bash-heredoc-with-continuation mode mid-line, seeded with what
    you already typed. Keep ending lines with `\` to continue;
    the first line without a trailing `\` is final and gets
    included. `\\` at end of a line stays literal. `/end` and EOF
    also terminate.

  Paste content that happens to start with `/` is not reinterpreted
  as a corteza command. No "Paste mode..." banner — IYKYK.

# corteza 0.6.6.3

## Approval prompt

* The tool-approval prompt is much tighter. The `Reason` section
  (gate text + `Policy:` + `Model route:`) is gone; `Access` collapses
  to a single line that names the path or command (e.g. `Write to
  CLAUDE.md`, `Run command in /home/troy/corteza`); the redundant
  `Path:` detail line above `Access` is suppressed when it would just
  repeat the same path. Choices 1 and 3 carry key hints: `(Enter)` and
  `(Esc)`.
* Boilerplate warnings ("Shell commands can invoke scripts...",
  "R code runs locally...") no longer appear on every bash / run_r
  prompt. Noteworthy warnings — credential paths, paths outside the
  project — still surface.
* After the user answers, both surfaces print a single-line
  `● User replied:` summary paraphrasing the chosen action (e.g.
  "Allow writing to CLAUDE.md once").
* In the terminal CLI, the approval block is erased and replaced by
  the `User replied:` summary via ANSI cursor-up + clear-down. In
  `corteza::chat()` running under RStudio (whose console doesn't
  honor cursor-position escapes) the block stays in scrollback with
  the summary appended below.

# corteza 0.6.6.2

## Inline diffs on file edits

* `replace_in_file` and `write_file` now attach a unified-diff payload
  to their MCP result. The CLI and `corteza::chat()` render it inline
  in the tool-call output as `⎿ Added N, removed M` followed by one
  row per kept line (`NNNN +|-| content` with red/green color) instead
  of the prior `N lines in Xms` summary. The LLM-facing result text is
  unchanged — the diff is for the human reading the terminal.
* Tool labels renamed for clarity: `replace_in_file` → "Update",
  `write_file` → "Write" (matches the inline-diff phrasing).
* Diff generation shells out to the system `diff -u`. If `diff` isn't
  on `PATH` the tool degrades to a one-line size summary rather than
  failing. Diff payload is capped at 200 lines / 20000 chars with a
  `[diff truncated: N more lines]` marker so big writes don't dump
  thousands of lines into chat scrollback.
* The `/diff` slash command's output is also ANSI-colored.

## Console color policy is shared

* `ansi_supported()` / `ansi_colors()` in the package are now the
  single source of truth for both `corteza::chat()` and the
  `~/bin/corteza` CLI. RStudio's R console (which is not a tty) is now
  correctly detected as ANSI-capable, and `NO_COLOR` / `FORCE_COLOR`
  overrides work in both surfaces.

# corteza 0.6.6.1

## Interrupt key

* Pressing the interrupt key during an in-flight agent turn now aborts
  the turn cleanly and returns control to the prompt instead of
  escaping the REPL entirely. Both `corteza::chat()` and the
  `~/bin/corteza` CLI catch the R-level interrupt.
* In the CLI, if the interrupt arrives while a tool call is running
  inside the `callr` worker subprocess, the worker is sent SIGINT so
  the in-flight tool (e.g. a long `bash` or `run_r` call) actually
  stops. The worker is recycled only if it doesn't return to idle.
* The aborted exchange is recorded in history with an
  `[Interrupted by user before completing.]` marker so the next turn's
  model sees that the prior turn ended early.
* Interrupt keys differ by environment: in the RStudio console
  `corteza::chat()` is interrupted by **Esc** (RStudio's console
  intercepts Ctrl+C for copy). In the terminal `~/bin/corteza` CLI it's
  **Ctrl+C** — terminals send raw `^[` for Esc, which is not a signal.

## Other

* `load_saber_briefing()` now wraps `saber::briefing()` in
  `suppressMessages()` so the briefing text no longer leaks to the
  user's terminal every time a subagent calls `session_setup()`.

# corteza 0.6.6

## Async subagent queries

* `subagent_query(id, prompt, wait = FALSE)` fires a prompt and
  returns the canonical id immediately; the parent collects the
  reply later with `subagent_collect(id)`. A subagent can carry
  only one in-flight async query at a time — both wait paths
  refuse to stack on top of a pending call.
* CLI gains `/queue <id> <prompt>` (fire) and `/collect <id>`
  (drain). `/agents` distinguishes idle vs busy.
* New MCP tool `collect_subagent` mirrors the CLI surface.

## Durable subagent transcripts

* Each working subagent now writes an append-only JSONL transcript
  at `agents/subagent-<id>/sessions/<id>.jsonl`, matching the
  shape archival holders already use. Disk space is cheap; context
  is expensive. Compaction (below) can rewrite the in-memory
  history without losing anything on disk.

## Context-budget helpers

* Token-counting helpers moved out of `inst/bin/corteza` into
  package code so chat, the CLI loop, and subagents share the
  same budget math: `context_limit_for_model()`, `format_tokens()`,
  `estimate_text_tokens()`, `estimate_history_tokens()`,
  `estimate_tool_tokens()`, `estimate_live_context_tokens()`,
  `context_usage_pct()`. Also `default_provider_model()` for
  resolving the model identity a subagent will actually run with.

## Subagent context compaction

* New `subagents.context_compaction` config block. Defaults to
  `mode: inherit_strict` with `compact_pct: 75`. Working subagents
  compact their own in-memory history after each turn when usage
  passes the threshold; the on-disk transcript stays intact.
  Archive holders are skipped via a kind marker stamped by
  `subagent_seed_history()`.

## Token visibility in /agents

* `/agents` now shows model, age, live context (tokens / limit),
  cumulative input/output tokens, and cumulative cost per
  subagent. Live tokens are computed via a child-side
  `r_session$run()` call per `/agents` invocation; busy children
  show `ctx ?`. Cost is captured when the provider returns it;
  shown as `?` otherwise (most non-Anthropic providers).

# corteza 0.6.5.1 (development)

## Plan mode

* New session-scoped `plan_mode` flag. When on, the LLM is told to
  research and propose rather than act: the policy engine denies
  write/exec tool calls (`write_file`, `replace_in_file`, `bash`,
  `run_r`, `run_r_script`), and an `exit_plan_mode` tool is injected
  into the tool list. A successful `exit_plan_mode` call flips the
  flag back off so the LLM proceeds with the work.
* `/plan` slash command in `chat()` and the `corteza` CLI: bare
  toggles, `/plan <task>` enables and submits the task as the next
  prompt.
* Subagents inherit `plan_mode` from `parent_session` so spawning a
  child can't launder a write through plan mode.

## Retroactive-extraction runtime (opt-in)

* New `archival` config block. Default off — CRAN users see no behavior
  change. When enabled, finished turns collapse into a fresh subagent
  that holds the full transcript, while the parent's history keeps a
  compact `{summary, subagent_id}` block. The LLM sees live subagents
  in its system prompt and picks `query_subagent` vs `spawn_subagent`
  as a normal tool decision.
* `[Max turns reached]` is no longer a dead-end string: with archival
  on, the full transcript persists in a subagent for follow-up via
  `query_subagent`.
* Recursion supported: subagents finishing their own queries
  re-evaluate triggers and archive into sub-subagents. Capped at depth
  3 by default (`archival.trigger.depth_cap`).
* Subagent transcripts persist to disk via the existing
  `transcript_append` infra under
  `agents/subagent-<id>/sessions/<id>.jsonl`.
* New internal helpers: `subagent_seed_history`, `subagent_turn_set_id`.
* Startup validation: `archival.enabled` requires `subagents.enabled`.
  No silent overrides.
* See `vignette("retroactive-extraction")` for the full opt-in
  surface, design notes, and known limitations.

## CLI

* `/spawn` now parses `--model`, `--preset`, and `--tools` in any
  order. Matches the MCP `tool_spawn_subagent` surface.

## Subagents

* Configurable subagent presets (`investigate`, `work`, `minimal`).
  Default is `investigate` (read/search only).
* `subagent_spawn(tools = character(0))` is now a documented
  configuration: spawns a holder with no active tools. Used by the
  archival runtime to create transcript-only subagents.
* `resolve_subagent_tools()` honors `config$subagents$default_tools`
  when neither preset nor tools is supplied (was silently bypassed
  before).

## MCP

* Fix MCP stdio transport compatibility with Claude Code: read from
  `file("stdin")` rather than `stdin()` (which reads from the script
  source under `Rscript -e`), echo the client's `protocolVersion` in
  the `initialize` response instead of hardcoding it, and serialize
  empty `capabilities.tools` as a JSON object (`{}`) rather than an
  array (`[]`). Thanks to Grant McDermott (@grantmcdermott, #62).

## Documentation

* New `configuration` vignette covering config files and precedence,
  CLI flags, the full JSON config-key surface (core, context, safety,
  skills, subagents, channels, etc.), slash commands, MCP server setup
  (stdio and socket transports), session tuning, systemd service, and
  environment variables. Thanks to Bob Rudis (@hrbrmstr, #54).

# corteza 0.6.2

## CLI

* The `Live context` indicator now reflects the actual size of the next
  prompt (system + tools + message history) rather than cumulative
  billed API tokens. Old behavior counted up forever — `/clear` and
  `/compact` had no visible effect on the indicator. Status line label
  updated from `Usage` to `Live context`.
* `/context` now prints live usage and the auto-compact threshold
  alongside the loaded context files.
* Auto-compact threshold raised from 80% to 90%. Pairs with the
  estimate above to avoid over-eager compaction now that the metric is
  more accurate.
* Provider/model defaults centralized in `resolve_provider_model()`.
  Legacy `kimi-k2` now resolves to `kimi-k2.6` for moonshot.
  Moonshot's chat temperature is forced to 1 (their API rejects other
  values on kimi).
* Session field renamed: `session$compactions` ->
  `session$compactionCount`, matching `memoryFlushCompactionCount`.
  Existing on-disk sessions show 0 compactions until the next compact;
  display-only.

## CLI prompt input

* `_read_prompt_via_bash` now prints the prompt from R and captures
  input through a tempfile. Previously relied on `bash -p` plus
  `system2(stdout = TRUE)`, which was fragile on terminals that mixed
  the prompt into stdout.

# corteza 0.6.1

* Relicensed from MIT to Apache License (>= 2) for explicit patent
  grant. Aligns with the rest of the cerebro toolchain (saber, pensar,
  hacer, cerebro). The LICENSE stub file is removed; Apache 2.0
  R-package convention points to the system-installed template.

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
