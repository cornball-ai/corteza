# corteza 0.6.9

## Fix: scope all three `tools::R_user_dir()` roots to tempdir under R CMD check

PR #96 already redirected `R_USER_CACHE_DIR` for the test run (so
saber's transitive briefs writes landed in tempdir), but corteza
itself writes to all three `tools::R_user_dir()` roots — `cache`
(saber briefs, `/last` response), `data` (session transcripts via
`R/paths.R`, matrix state), and `config` (matrix.json). Session tests
were still leaking files under
`~/.local/share/R/corteza/agents/…/sessions/*.jsonl`, which would
have tripped CRAN's "checking for new files in some other
directories" NOTE on BDR's `donttest` box even when local
`tinypkgr::check()` reports `Status: OK`. The suite-level redirect in
`tests/tinytest.R` now sets all three (`R_USER_CACHE_DIR`,
`R_USER_DATA_DIR`, `R_USER_CONFIG_DIR`) to tempfiles for the check
run; end-user behavior is unchanged.

# corteza 0.6.8

Patch release batching the 0.6.7.1 through 0.6.7.12 dev cycles. Per-PR
detail is preserved in the dev-marker sections below.

# corteza 0.6.7.12

## Fix: spawned subagents inherit the parent session's provider/model

`chat(provider = "ollama")` followed by an LLM-triggered
`spawn_subagent` was silently producing children on Anthropic: the
default `tool_executor` in `.make_tool_handler()` called `call_skill()`
with no `ctx`, so `tool_spawn_subagent()` saw `ctx$session = NULL` and
`subagent_spawn()` fell through to `getOption("corteza.provider")`. The
executor now passes `ctx = list(session = session)`, so provider, model,
`cwd`, `plan_mode`, and `archival_depth` all flow through to the child.

# corteza 0.6.7.11

## Fix: cap tool output before it reaches model context

A single unbounded tool result (e.g. a `bash` command that printed 57k
lines) was flattened straight to the model and mirrored into history,
blowing the next call past the provider token limit; `/compact` then
couldn't recover the session. Tool results now pass through a universal
guard (`admit_tool_result()`) that caps oversized output to a marker plus
a recoverable handle, and `/compact` elides giant message bodies so it can
rescue an already-wedged session.

# corteza 0.6.7.10

## Fix: `chat()` crash on a model-less session

A `chat()` started without an explicit `model` no longer crashes after
the first turn. The per-turn context meter called
`context_limit_for_model()` with a NULL model and errored on the table
subscript; it now falls back to the default limit (and resolves a
model-less session to the provider default for display).

# corteza 0.6.7.9

## CRAN pre-submission housekeeping

Packaging-only pass ahead of the 0.6.8 release: applied a deferred
rformat reflow, generated the `user_interrupt_marker` man page,
refreshed `cran-comments.md`, and turned the skills vignette's clone
example into a `<your-skills-repo-url>` placeholder so it carries no
dead link. No code behavior changes.

# corteza 0.6.7.8

## Interrupt marker matches the deny marker's directive

A turn interrupted with Ctrl+C / Esc now leaves the same "stop and ask
the user what to do instead" instruction in history as an explicit
deny, so the LLM checks in on the next turn rather than quietly
continuing. Previously the interrupt marker was terser than the deny
marker even though the approval prompt's Esc/Ctrl+C hint routes through
the interrupt path.

# corteza 0.6.7.7

## Provider default models come from llm.api

`default_provider_model()` now delegates to
`llm.api::provider_default_model()` instead of corteza's own
provider-to-model table, which had drifted. corteza tracks llm.api's
picks going forward (e.g. `openai` and `moonshot` defaults shift to
llm.api's current choices). No change when you set a model explicitly.

# corteza 0.6.7.6

## MCP subagent exposure is opt-in, with a spend cap

`serve()` no longer hands the subagent tools (`spawn_subagent`,
`query_subagent`, `collect_subagent`, `list_subagents`, `kill_subagent`)
to MCP clients by default. A spawned subagent runs its own agent loop
and spends autonomously on the host's LLM credentials, so an unattended
MCP client could otherwise trigger unbounded cost it never sees.

Enable with `subagents.expose_over_mcp` (config, default FALSE) or
`serve(expose_subagents = TRUE)`. When enabled, cumulative subagent
spend over MCP is capped: `spawn_subagent`/`query_subagent` are refused
once spend crosses `subagents.mcp_spend_cap_usd` (default $5.00; `<= 0`
disables) or an optional `subagents.mcp_spend_cap_tokens` for providers
that don't report cost. The cap reads the same meter as `/spent`; the
in-process `chat()`/CLI loop is unaffected.

# corteza 0.6.7.5

## /spent: approximate session cost

`/spent` (alias `/cost`) reports the approximate USD spent this process
run, in both `chat()` and the CLI. Each turn accumulates the `cost`
scalar that llm.api 0.1.4 returns, plus token counts. When a model is
absent from llm.api's price snapshot its cost is unknown, so totals are
shown as a floor rather than a precise figure.

Spend is process-lifetime: `/clear` no longer zeroes the tally, it
closes the current conversation and opens a new one, so `/spent`
itemizes each conversation between clears with a grand total. `/clear`
now also kills any live subagents (a fresh conversation leaves none
behind); their spend is retired into the run total, shown as a separate
process-level subagents line that keeps counting an agent after it is
killed. Resumed prior-run spend is not loaded from disk.

# corteza 0.6.7.4

## CLI and chat() share one in-process REPL loop

The CLI no longer runs tools in a separate `callr` worker. `chat()` and
the CLI now drive one shared loop (`run_repl_loop`) in a single R
process, executing tools in-process via `corteza::turn()`.

* **One subagent registry.** Model-driven and slash-driven subagents
  previously lived in separate registries (worker vs main process) and
  were mutually invisible in CLI mode. They now share one
  `.subagent_registry` per session: a model-spawned subagent shows up in
  `/agents`, and a `/spawn`-ed one is reachable by `query_subagent`.
* **One command set.** Both surfaces run the same slash commands; `/flush`
  moved into the shared loop. Per-surface differences (rendering, help
  text, input reader) ride through injected hooks, not forked logic.
* `chat()` gains the per-turn context meter and auto-compaction the CLI
  already had.
* The local-eval ops (`/r`, `!`) and the foreground `bash` tool moved off
  blocking `system2()` onto interruptible `processx::run()`, so an
  interrupt returns to the prompt instead of killing the session.
* Removed the CLI worker transport and its four exports
  (`worker_dispatch`, `worker_tool_list`, `cli_worker_spawn`,
  `cli_worker_drain_events`). `worker_init()` stays exported: subagents
  call it across the `callr::r_session` boundary to set cwd and register
  skills.

# corteza 0.6.7.3

## Interrupt / denial preserves context across the exit

Hitting Ctrl+C (CLI) or Esc (RStudio chat) after a multi-tool turn
no longer loses the tool calls that completed before the interrupt.
Previously the next turn / next CLI invocation saw the original
user prompt, an interrupt marker, and nothing else.

* `Imports: llm.api (>= 0.1.4)` so `agent()` exposes the
  `history_callback` parameter. `R/turn.R` uses it to mirror the
  in-progress provider-native history into the live session env as
  each assistant message and each tool result lands. With the
  callback wired, `turn_session$history` reflects the latest snapshot
  even when an interrupt unwinds the turn mid-flight.
* New helper `repair_interrupted_tool_history()` (with the
  surrounding `apply_exit_marker()`) synthesizes provider-correct
  tool_result blocks for any tool_use issued during the current
  turn that never got matching results. Wired into chat()'s
  `interrupt` and `corteza_user_deny` condition handlers. Anthropic's
  "every tool_use needs a tool_result" requirement no longer 400s
  the next chat() API call after an interrupted multi-tool turn.
* New helper `dump_completed_tools_summary()` walks the per-turn
  history slice and appends a text summary of completed tool calls
  to the persistent flat-text `session$messages` before the
  interrupt / denial marker. Wired into both CLI exit handlers
  (`inst/bin/corteza`). The next CLI turn's `api_history` rebuild
  now carries the work the LLM accomplished before the interrupt
  ("[ran tool_name(...) -> result]" entries instead of nothing).
* Two-layer `on.exit` guard around the archival ownership transfer.
  An inner guard inside `archival_archive_turn()` kills the holder
  subagent if anything between spawn and the explicit
  `transferred <- TRUE` unwinds; an outer guard in
  `maybe_archive_turn()` covers the parent-history-collapse window.
  Either failure cleans the orphan instead of leaving it sitting in
  `.subagent_registry` with no parent reference.

# corteza 0.6.7.2

## Subagents can return a value by handle

* `subagent_query()` / the `query_subagent` tool gain a `return_name`
  argument. When set to a name (or `.h_NNN` handle) the child left its
  result bound under, the child resolves it after the turn and ships
  the value back; the parent stashes it in the handle store and
  appends a `[stored as .h_NNN]` block to the reply. A large
  structured result is then referenced by name in a later `run_r`
  instead of being inlined into the parent's context, mirroring how
  `run_r` already returns large values. The async (`wait = FALSE`)
  path captures the name at query time and applies it on collect.
* The name is validated as a single syntactic name or handle and
  resolved (handle store, then globalenv) without evaluating
  expressions; an unresolved or malformed name yields a clear note
  rather than silently dropping the value.

# corteza 0.6.7.1

## Base-R cleanups from a redundancy audit

* `skill_install()` clones GitHub repos via `processx::run()`
  (already an import) instead of `system2()`, with a nonzero git
  exit surfaced as a clear error. The clone is extracted into an
  internal `git_clone()` helper so its status handling is
  unit-testable against a local repository, no network required.
* `find_break_point()` (text chunking) replaces two backward
  character-scan loops with a two-tier regex: last newline, else
  last whitespace (tabs included). Behavior preserved; added
  regression tests for newline-beats-later-space and tab handling.

# corteza 0.6.7

Patch release batching the 0.6.6.1 through 0.6.6.20 dev cycles plus
the out-of-band "Deny aborts the whole turn" change. Per-PR detail
is preserved in the dev-marker sections below.

## RStudio addin: multi-line statements

* `corteza_execute_in_chat()` / `_retain()` now expand the current
  line to the full top-level R statement before sending, matching
  RStudio's built-in Ctrl+Enter behavior. Before, hitting Ctrl+Enter
  on the first line of `lm(y ~ x,\n   data = df)` sent only the
  unclosed first line -- annoying outside `chat()`, and broken
  inside `chat()` where `/r lm(y ~ x,` failed to parse. Buffer is
  parsed with `keep.source = TRUE`; falls back to single-line when
  the buffer has a syntax error elsewhere or the cursor sits on a
  blank / comment line outside any expression.
* `chat()` and the CLI `/r` handlers now read continuation lines
  with a `+` prompt until the expression parses, mirroring R's
  built-in REPL continuation. Capped at 100 continuation lines to
  keep a stuck parse from blocking the REPL.

# corteza 0.6.6.20

## Codex review for #110 + #111

* **Handle staleness (medium).** `handle_eval_env()` used to skip
  reassignment when the `.h_NNN` symbol already existed in
  globalenv, so rebinding a handle id in the store left the old
  globalenv copy in place (codex repro'd: `tool_run_r('.h_001')`
  returned the previous snapshot after the store was rewritten).
  Now `handle_eval_env()` assigns unconditionally and removes
  globalenv bindings the package previously created that are no
  longer in the store, via a new `.handle_managed` registry.
  `clear_handles()` also sweeps the managed bindings out of
  globalenv.
* **Session-name collisions across agents (medium/low).**
  `session_id()` ignored its caller's agent and only collision-
  checked the default agent's transcript dir. With the
  docker-style ~6000-name pool, a non-main agent could mint a
  name that already existed in its own store and silently reuse
  the transcript. `session_id()` now takes `agent_id`, threads
  it to `.session_name_exists()`, and the collision check now
  consults both the transcript file *and* the in-store metadata
  for that agent.
* **Untitled buffers in the RStudio addin (low).** Unsaved
  source-editor buffers return empty `ctx$path`, so
  `tools::file_ext()` returned `""` and the addin routed the
  line as "other" -- inside chat() that meant the LLM saw raw
  R code instead of a `/r ...` invocation. Untitled buffers are
  now treated as R, matching RStudio's built-in assumption.

# corteza 0.6.6.19

## RStudio addin: route Ctrl+Enter to /r or ! when chat() is active

* New addin `corteza_execute_in_chat()` (plus
  `corteza_execute_in_chat_retain()` for Alt+Enter) reads the
  current line / selection from RStudio's source editor and sends
  it to the console, auto-prefixing `/r ` for `.R` files and
  `! ` for `.sh` / `.bash` files when `corteza::chat()` is the
  active REPL. When `chat()` is not running, no prefix is added --
  the addin is a superset of RStudio's default execute-line
  behavior, so binding it to Ctrl+Enter doesn't break normal R
  scripting outside chat.
* `chat()` now sets `options(corteza.chat_active = TRUE)` on entry
  and clears it on exit; the addin reads this flag to decide
  whether to prefix.
* `rstudioapi` added to Suggests. Addin file lives at
  `inst/rstudio/addins.dcf` so RStudio picks it up after install.

**Setup:** in RStudio, open Tools -> Modify Keyboard Shortcuts,
pick "Addins" in the dropdown, and bind Ctrl+Enter (and optionally
Alt+Enter) to the two corteza addins. Override the built-in
"Run current line/selection" mappings.

# corteza 0.6.6.18

## Fix: tool_run_r now actually persists `<-` assignments

* `tool_run_r()` was advertised as "Execute R code in the session's
  global environment", but PR #36 (Phase 5 of CLI/worker split,
  2026-04-21) silently changed it to evaluate in a fresh child env
  of globalenv. Ordinary `<-` and `=` assignments landed in the
  child env and disappeared when the call returned; only `<<-`
  walked up the scope chain and persisted. Reported 2026-05-20.
* `handle_eval_env()` now copies handles INTO globalenv (under
  their hidden `.h_NNN` names that `ls()` filters out by default)
  and returns globalenv itself. `tool_run_r()` evals in globalenv,
  so `<-` writes the the right place and survives between calls.
* Regression tests in `test_tool_impl.R` and `test_handles.R`
  exercise multi-call persistence with both `<-` and `=`.

# corteza 0.6.6.17

## Brain-corn brick banner + docker-style session names

* Banner kernels are now actual yellow-square emoji (`U+1F7E8`)
  laid out in a brick pattern: adjacent rows offset by an odd
  number of cells so the kernels stagger like masonry instead of
  stacking in straight columns. No ANSI escapes -- the emoji is
  its own colour, supported across every modern terminal.
* Model name is now right-padded to 9 chars, provider
  left-padded to 8 chars, so row-5 width stays constant
  regardless of name length. Without this, swapping
  `kimi-k2.6` for `gpt-4o` shifted the right edge and broke the
  brick offset.
* Version display drops the 4th-component dev marker:
  `0.6.6.17` -> `v0.6.6`.
* Tool count moved out of the banner -- the brain stays cleaner
  with just the model/provider labels.
* Session names are now docker-style `adjective_surname`
  (e.g. `boring_wozniak`) instead of UUIDs. Wordlists are the
  Docker moby/moby adjectives plus ~60 scientist surnames.
  Collisions retry up to 10 times before falling back to a hex
  suffix.

## CLI startup layout

* "Starting worker..." and "Connected. N tools available." now
  print on one combined line instead of two stacked lines.
* The blank line that previously sat between the worker line and
  the banner is gone; the banner appears one line below the
  startup line, with a single blank between them.
* `corteza::chat()` now prints a `session adjective_surname`
  line below the banner, matching the CLI's layout.

# corteza 0.6.6.16

## Brain-corn startup banner

* `chat()` and the `~/bin/corteza` CLI now open with a gold
  brain-corn silhouette rendered in 256-color ANSI, with the
  corteza version, active model, provider, tool count, and
  `/help` / `/quit` hints embedded directly inside the kernels.
  Replaces the prior single-line `corteza chat | model @ provider`
  header. Uses 256-color index 220 (gold) for compatibility with
  terminals that don't advertise true-color via `COLORTERM`.
* New internal `corteza_startup_banner()` in `R/banner.R` --
  template-driven, so future tweaks to the silhouette only touch
  one constant.

# corteza 0.6.6.15

## `! <cmd>` shell-line in chat() and CLI

* New `! <cmd>` prompt prefix runs a shell command locally (bash on
  unix, cmd.exe on Windows), prints output, and stages it for the
  next LLM message. Matches the Claude Code / codex `!` convention.
  The space after `!` is required so prompts that legitimately
  start with `!` aren't captured. Output is capped at 4000
  characters in the staged version (with a truncation note); the
  on-screen output is full.
* `/r <expr>` is now available in the CLI too -- previously chat()
  only. Same staging behavior: visible result prints inline, gets
  queued for the next LLM message, with an oversized printed
  result swapped for `str()` of the value.
* Shared helpers `run_bang_shell()` and `run_r_eval()` live in
  `R/chat-slash.R` so both surfaces use one source of truth for
  the local-eval + cap logic.

# corteza 0.6.6.14

## Persistent task list the LLM maintains across turns

* New internal `task_create(tasks)` / `task_update(index, status)`
  LLM tools (Claude-Code-style TaskCreate pattern). The list lives
  on the session, is injected into each turn's system prompt as
  numbered `[ ] / [>] / [x] / [-]` lines, and is shown to the user
  as a compact summary whenever it changes during a turn. Status
  is `pending`, `in_progress`, `completed`, or `cancelled`.
  Promoting a task to `in_progress` auto-demotes any other
  `in_progress` task to `pending` so the "one active at a time"
  invariant holds without rejecting valid edits.
* Tools dispatched via an in-process intercept in
  `.make_tool_handler()` (`R/turn.R`) rather than the normal skill
  executor, so the CLI's callr-worker dispatch can't strand
  mutations in the wrong process. The intercept fires before
  dry-run, policy, and approval -- task-list updates never prompt
  the user.
* `/tasks` (and `/tasks clear`) slash commands in both `chat()`
  and the terminal CLI. `/clear` wipes the task list along with
  the conversation, matching the "fresh start" intent.
* Persistence: task list is saved as part of the session record
  (`session_new`, `session_save`, `session_load`) so it survives
  CLI restarts and `chat(session = ...)` resumes.
* Distinct from `/plan` mode, which is a one-shot
  research-and-propose flow; the task list tracks ongoing
  progress for multi-step work without changing the policy gate.

# corteza 0.6.6.13

## chat() banner resolves provider default model

* When `model` is NULL, `corteza::chat()`'s startup banner and
  per-turn "Thinking with ..." line route through
  `resolve_provider_model(provider, model)` instead of falling
  straight to the literal "(provider default)" placeholder. With
  `config$provider = "moonshot"` and no explicit `model`, the
  banner now reads `kimi-k2.6 @ moonshot`, matching what the
  terminal CLI already showed. Unknown providers still degrade to
  the "(provider default)" string.

# corteza 0.6.6.12

## Approval-prompt deny advertises the surface-appropriate
## interrupt key

* `cli_approval_lines()` takes a new `deny_label` argument (default
  `"Deny"`). `corteza::chat()` passes `"Deny (Esc)"`; the terminal
  CLI passes `"Deny (Ctrl+C)"`. The key doesn't literally cancel
  the readline prompt itself, but it cancels the in-flight turn,
  which is the user-facing escape hatch users want to know about.
  Deny still raises the `corteza_user_deny` marker; Esc/Ctrl+C
  during a turn raises an interrupt marker. The LLM treats both as
  "stop and check with the user" -- the wording of the interrupt
  marker is less directive than the deny marker, worth tightening
  in a follow-up.

# corteza 0.6.6.11

## Markdown rendering for chat() responses

* New `R/render-md-ansi.R` adds `render_md_ansi(text)` (internal):
  strip markdown syntax markers and apply ANSI styling for terminal
  display, leaving the raw markdown source intact when the output
  isn't a TTY (NO_COLOR, piped to file) or when the user opts out
  via `options(corteza.markdown = FALSE)`. Both `corteza::chat()`
  and `inst/bin/corteza` now route assistant responses through it,
  so the two surfaces stay in sync. Replaces the smaller bespoke
  renderer that previously lived in `inst/bin/corteza`. Handles
  H1/H2/H3 headings, `**bold**`, `*italic*` / `_italic_` (modern
  terminals interpret as italic), `` `inline code` ``, fenced code
  blocks (dim, 2-space indent), `- ` / `* ` bullets, `> blockquote`
  lines, and `[text](url)` links.

## Deny aborts the whole turn

* Picking "3. Deny" at the tool-approval prompt now aborts the
  entire turn instead of declining a single tool call. Previously
  the deny was returned as a `[user declined: ...]` tool result that
  the LLM saw and treated as feedback, planning the next call --
  which forced users to mash "3" through cascades of dependent tool
  calls. The next turn now starts with a history marker that names
  the denied tool and tells the LLM to stop and ask the user what to
  do instead, rather than retrying or planning a workaround.

# corteza 0.6.6.10

## /copy command in chat()

* New `/copy` slash command copies the most recent assistant
  response to the system clipboard via the optional `clipr`
  package. Silent no-op until the first assistant reply lands;
  prints a hint if `clipr` is not installed or the clipboard
  isn't available (e.g., headless Linux without xclip /
  wl-clipboard).

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
