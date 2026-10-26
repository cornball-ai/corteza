<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Configuration}
-->
---
title: Configuration
---

<img src="../man/figures/corteza.png" alt="corteza logo" align="right" width="160" />

# Configuration

In a shared Matrix room, say `Let's take this offline` or use `/sidebar [N]`.
The selected conversation enters the private context as one user-role quotation,
with original speaker roles and source-event links retained. It is background
for the continuation, not new instructions or the private assistant's own reply.
The bot copies the last N human exchanges (default 5, maximum 20) into your
existing private room with source-event links, and redirects active jobs'
progress, approvals and results there. Their worker and workspace stay the same.
The source gets one continuation notice. Original messages remain in the source
room; the extracted copy is durable and enters the private room's normal archive.
Only configured operators can do this. `private_rooms` maps operator IDs to
destination IDs; `sidebar_room` and then the bot's default `room_id` are fallbacks.
The destination must be invite-only with just the operator and bot joined.
Delivery is withheld if those checks later fail. A room controlling a remote CLI
cannot receive a sidebar until that CLI disconnects. A failed send can be retried;
as with other Matrix sends, a crash after acceptance but before recording its ID
can duplicate a post.

In a Matrix room, `/approve-all [job-id]` authorizes eligible remaining approval
requests for one active job. `/approve-all status [job-id]` shows the grant;
`/approve-all off [job-id]`
revokes it. With multiple active jobs an ID is required. Only a configured
operator who is still in the room can authorize requests. The authorization
includes requests normally escalated by the supervisor, is recorded against
that operator, and expires when the job ends. It does not override a policy
denial, authorize other jobs, or cover credential-path requests: those still
require individual human approval. Credential-bearing calls require local review.
Only the explicit slash command grants approval; ordinary words and `/auto`
do not. CLI `/auto`, including through remote control, is unchanged.

Display output masks known credentials from configuration and environment, plus
quoted credential-shaped literals. The registry belongs to one session; source
expressions and unquoted words are never learned as values. Tool inputs/results,
handles, native history, transcripts and journals remain unchanged. These private
records can contain secrets. Worker checkpoints and approval request files use
owner-only permissions. A Matrix `sync_token` is a cursor, not a credential.

Room activity defaults to counts only. Set `activity_detail: true` in the bot
configuration to include commands, paths, result previews and model narration.
Masking cannot identify arbitrary encodings or unknown unlabelled secrets; it
does not replace the summary setting. Approval displays are never masked or
truncated: calls containing credentials or too large to display in full require
review in a local CLI. Voice deltas are delivered immediately except for a partial
match to a known credential, which is held until it can be masked safely.

The CLI shows five-line result previews and separates commentary from tool logs.
After 60 seconds without commentary, the next completed tool emits a factual
progress update. A blocking request or tool must return before that update can
run. Reaching the tool-turn limit reports the last update and how to continue.

How to configure corteza for your workflow.

## Config files

corteza reads two JSON files and merges them. Project config overrides global config.

| Layer | Path |
|-------|------|
| Global | `tools::R_user_dir("corteza", "config")/config.json` |
| Project | `.corteza/config.json` |

**Merge semantics:** project keys replace global keys at the top level; no deep merge.

**Precedence:** CLI flags > project config > global config > built-in defaults.

Example project config:

```json
{
  "provider": "ollama",
  "model": "llama3.2",
  "context_files": ["README.md", "PLAN.md"],
  "approval_mode": "ask"
}
```

Create the directory first:

```r
dir.create(".corteza", showWarnings = FALSE)
```

## CLI flags

```bash
corteza [options]
```

| Flag | Description | Default |
|------|-------------|---------|
| `--provider <p>` | LLM provider: `anthropic`, `openai`, `moonshot`, `openai_codex`, `ollama`, `openai_compatible` | `anthropic` |
| `--model <name>` | Model name | provider default |
| `--base-url <url>` | Endpoint for `--provider openai_compatible` | config / env |
| `--tools <filter>` | Tool filter: `core`, `file`, `code`, `git`, `r`, `data`, `web`, `chat` | all |
| `--session <key>` | Session key; resumes if exists | none |
| `--resume` | Resume most recent session | `false` |
| `--list` | List sessions and exit | `false` |
| `--dry-run` | Preview tool calls without executing | `false` |
| `--context-edit` | Enable conversation context editing for this session | `false` |
| `--trace` | Print structured tool-call events to stderr | `false` |
| `--help` | Show help and exit | - |

Flags override config values for the current run.

Resuming restores the saved provider and model together, even if configuration
has changed. Explicit `--provider` and `--model` flags still override that pair.
Changing only the provider uses its matching configured model or its default;
it does not carry the old provider's model across. The same rules apply to
`chat(session = ...)` and its `provider` and `model` arguments.

## Unattended work

`/auto` continues the current task using the conversation and its approved
plan. `/auto <goal>` supplies a new goal; an empty session needs one. Runs
continue until completion, a blocker, or a resource limit. `--loops N` sets
an iteration limit. `/auto status` reports the last stop, and `/auto resume`
restarts that goal in the same conversation.

The startup display names the working-directory scope and whether shell/R
execution is enabled. `/auto --exec` explicitly permits the monitor to assess
shell/R calls; a project restriction can still veto it. Without that grant,
those tools are omitted. Unclassified package tools are also omitted rather
than offered to the model and then stopping the run when selected. Path,
credential, policy, and monitor checks still apply to every dispatched call.

When a run stops, auto mode is off. Ordinary messages do not restart it.
`/permissions` controls ordinary tool approvals separately from `/auto`.

## Matrix remote control

`/remote-control [--bot @bot:server] [room]` hands the current CLI or `chat()`
session to a Matrix room. The model, history, working directory, and approval
policy stay in the CLI process. Keep that process running. The existing bot
loop carries messages and replies; the CLI does not log in to Matrix or share
the bot's sync cursor or encryption store.

`/rc` is an alias for `/remote-control`, with the same arguments. In the
attached room, `/rc status` reports the connection and `/rc off` disconnects.

Both processes must run a version with remote control, on the same machine
under the same user. The bot must have an explicit `operators` list. With one
available bot it is selected automatically; otherwise specify `--bot`. The
default room name is `~/<working-directory-name>`. Set `remote_control.bot_id`
and `remote_control.room` in Corteza config to choose other defaults. The room
must already exist and be accessible to the bot. Rooms with delegated work in
flight or another attached CLI are refused.

In the room, send text tasks, `/auto [--exec] [goal]`, `/auto status`,
`/auto resume`, or `/max-tokens [N|default]`. The bot relays model replies,
tool previews, and errors. Approval prompts require `/approve <request-id>`
or `/deny <request-id>` from a configured operator. Each answer applies only
to that pending request; ordinary conversation cannot approve a tool.
Attachments and other slash commands are currently unsupported.

`/stop` interrupts at the next tool or completed-batch boundary. It does not
kill a running shell command or an in-flight model request. Send
`/remote-control off` to stop at that boundary and return control to the
terminal. Ctrl+C at the terminal also disconnects; a stopped turn retains the
completed tool history. Only the room's main timeline is attached; threads
keep their own bot sessions. Other senders cannot control the CLI, but room
members can see its public output.

Remote operators act with the CLI's channel policy. A Matrix session's
supervisor gate is not added to the CLI; `/auto` still uses its own monitor.
Approval prompts include full tool arguments, including file bodies. Treat
the configured operators as people authorized to control this CLI session.
Room prompts and replies also enter the bot's normal conversation archive.

The local mailbox lives under Corteza's user data directory, in
`remote-control`. `CORTEZA_REMOTE_DIR` can relocate it, with the same setting
in both processes. Records remain available for diagnosing failed delivery;
the bot retries unsent output without resubmitting model work. An ambiguous
send failure or crash before recording its event ID can duplicate a post.
If the courier disappears, the CLI returns to its local prompt when it next
waits for input or an approval.

## Response output limits

`max_tokens` is the per-response output budget. `/max-tokens` shows the active
budget; `/max-tokens 32768` changes it for the current CLI or `chat()` session.
`/max-tokens default` clears that session override and any learned budget.

When Anthropic's unset default of 4,096 tokens cuts a response short, Corteza
retries the unfinished response with 8,192, 16,384, then 32,768 tokens. Completed
tool calls stay in history and are not replayed. Partial calls are not executed.
A successful larger budget is reused for that provider/model in the session.

`output_retries` defaults to 3 (0 disables recovery; maximum 10).
`output_retry_max_tokens` sets the ceiling. An explicit `max_tokens` is also
the ceiling unless `output_retry_max_tokens` explicitly permits a larger one.
For providers with no known default, recovery requires an explicit starting
budget and ceiling. Codex does not support this output-budget setting.
Streaming consumers do not retry text they may already have delivered.

Exhausting recovery prints a clear stop and the budget command. An unattended
run stops rather than treating the incomplete response as progress; change
the budget and use `/auto resume`. Output cutoffs are separate from context
window exhaustion and do not trigger summary compaction.
If recovery ends with a partial reply, the rendered text is kept in conversation
history and the journal. A later turn can refer to what the user actually saw;
unfinished tool calls are still discarded.

## Conversation context editing

Context editing is experimental and off by default. Enable it in the global
or project `config.json`:

```json
"context_edit": {
  "enabled": true,
  "cadence": "pressure"
}
```

Project fields override global fields within this block. `cadence` is
`"pressure"` (allow edits after the `context_warn_pct` notice, 75% by default)
or `"always"` (allow edits after each complete tool batch). Existing threshold
compaction remains the fallback at `context_compact_pct`, 90% by default.

`/context-edit on|off|status` controls the current session in the CLI, `chat()`,
and Matrix rooms. The CLI also accepts `--context-edit`. R callers can pass
`context_edit = TRUE`, `FALSE`, or `list(enabled = TRUE, cadence = "always")`
to `new_session()`. A NULL override inherits configuration. Spawned subagents
inherit the parent's effective setting. `/status` reports the effective mode,
including when an enabled setting is unavailable on an unsupported provider
or because the journal is disabled.

On Anthropic and OpenAI chat-completions history shapes, the model receives
an `edit_context` tool and visible IDs on completed tool outputs (`[rN]`) and
earlier assistant text (`[mN]`). The tool supports:

- `drop`: remove the selected entries, keeping each tool call and result together.
- `replace`: summarize the selected entries in the first selected entry and
  remove the others. A retained tool result keeps its original call paired.
- `note`: replace a working note containing task facts, decisions, and next
  actions, placed before the latest human prompt so it cannot merge into the
  current assistant message or split a tool call from its result. On the first
  turn, a short context-workspace header keeps a user message at the start.

Visible IDs survive session resume and provider changes. Label-only updates
and rejected edits do not suppress archival; applied edits and compaction
still skip archival for that turn because its live message positions changed.
Invalid `context_edit` configuration emits a warning and disables the feature.

For an opt-in trial without automatic summaries, add `"strategy": "clm"`
and use `"cadence": "always"`. This disables threshold compaction, forced
summary recovery on context overflow, and holder archival. The model must
manage the window before it fills; exhaustion reports an error and retains
the journal. In a room, later messages can also overflow the same full window.
Use `/clear` to start a fresh working window; the journal retains the previous
record. Unsupported providers are skipped in a pure CLM fallback chain.
The package default remains the existing threshold-compaction behavior.

Doers can select this policy independently of their conversation session:

```json
"jobs": {
  "doer": {
    "context_edit": {"enabled": true, "cadence": "always", "strategy": "clm"}
  }
}
```

A doer inherits the conversation's effective editing settings, with
`jobs.doer.context_edit` taking precedence. Reviewers and approval monitors
retain their existing configuration. Doers preserve their complete record
in the session journal; they do not file context into a vault.

For a Matrix talker, configure the policy and an explicit vault in the
Matrix account's `talker` block:

```json
"talker": {
  "enabled": true,
  "context_edit": {"enabled": true, "cadence": "always", "strategy": "clm"},
  "context_vault": "/path/to/this-bots/vault"
}
```

For an unsplit Matrix bot, put `context_edit` and `context_vault` at the
account config's top level instead. Its existing model and tools stay in
place; the combined conversation/work session owns filing. In split mode,
the `talker` block overrides those account settings. Workers inherit only
the editing policy, never the vault or conversation-owner role. Invalid
conversation policies or vault configuration warn and disable context editing
for that session. Vaults must have `schema.md` and must not be adopted read-only.

Set `"retain_turns": 10` inside the conversation owner's `context_edit` block
to protect the previous 10 human-led turns plus the current turn. Each protected
turn includes its human message, assistant replies, and tool traffic. Only older
entries receive editable IDs; edits are checked again at the checkpoint, so
previously issued IDs cannot bypass the retention setting. Tool-result messages
do not count as human turns. The default is 0 (the existing eligibility rules).
This setting applies to split talkers and unsplit room conversations. Doers,
reviewers, and monitors do not acquire a retention floor through inheritance.
It limits CLM edits, not threshold compaction in the `hybrid` strategy.

Only a conversation owner with a configured vault may retire older human turns, labeled
`[uN]`. It can drop them, but cannot replace them with model-written words.
The latest human turn, instructions, and current tool batch stay protected.
On `drop` or `replace`, the checkpoint first files the original entries and
the edit's optional `text` note into immutable Markdown and provider-native
JSON under `raw/chats/`. Paired tool messages are included as evidence.
Replacing a working note also preserves the previous note. Filing uses no
extra model call and does not commit or push the vault. It updates the vault
index through pensar; it does not generate a wiki synthesis or post a recap.

The journal commits the context change only after filing succeeds. A failed
vault write rejects the edit with a tool receipt and continues the turn with
the original entries intact; a failed journal commit can
leave an unused archive, which a retry reuses. The conversation's edited history,
consumed Matrix event IDs, and transcript ledger restart together from the
journal, so backfill skips already-consumed events. Rooms and threads retain
separate journals. `/clear` explicitly clears that saved working window.

Topic retirement is a choice in the conversation owner's normal turn. It can retain open
topics and commitments in its working note while filing completed topics,
then use the archive path in the tool receipt to read an earlier topic when
needed. This does not create or move Matrix rooms.

Outside the vault-backed conversation policy, all human messages are protected.
Messages mixing human text and tool results remain protected in every mode.
Instructions and tool schemas are outside the edit
surface. The current assistant message and its results cannot be edited until
a later batch. Older assistant messages containing non-text blocks are also
excluded from message-level editing. Threshold compaction keeps its existing
behavior and can still summarize human messages.

Requests queue during tool execution, then apply atomically at the checkpoint
after the batch completes and after the compaction check. Stale or overlapping
requests are rejected as a batch. Edits require journaling: a failed journal
write prevents the live-history replacement. Edit segments store operations,
removed/replaced entries, and references to shared immutable entries under
`<id>.journal/entries/`; `journal_load()` and `journal_history()` reconstruct
the same full record as ordinary segments. CLI and room replies report the
edit and estimated history tokens before and after it; structured
`context_edit` events include operations, IDs, cadence, and application status.

This feature does not support the OpenAI Responses/Codex history shape.
It does not change instructions, publish data, or run an additional model call.

## OpenAI-compatible endpoints

The `openai_compatible` provider targets any endpoint that speaks the
OpenAI chat-completions wire format: OpenRouter, DeepSeek, DeepInfra,
a local proxy, or a corporate gateway. It separates three things the
named providers hardcode:

- **provider** — `openai_compatible` (the wire format, not an official endpoint)
- **endpoint** — `base_url`, from config or the `OPENAI_COMPATIBLE_BASE_URL` environment variable
- **model** — passed through untouched; required, since there is no default

`base_url` is used as given with `/chat/completions` appended, so include
whatever `/v1` prefix the gateway expects.

```json
{
  "provider": "openai_compatible",
  "base_url": "https://openrouter.ai/api/v1",
  "model": "meta-llama/llama-3-70b-instruct"
}
```

The API key is read from `OPENAI_COMPATIBLE_API_KEY`, falling back to
`OPENAI_API_KEY`. A keyless gateway (an internal proxy that needs no
`Authorization` header) works with neither set. Equivalent one-liners:

```bash
corteza --provider openai_compatible \
  --base-url https://openrouter.ai/api/v1 \
  --model meta-llama/llama-3-70b-instruct
```

```r
# base_url comes from config or OPENAI_COMPATIBLE_BASE_URL
corteza::chat(provider = "openai_compatible",
              model = "meta-llama/llama-3-70b-instruct")
```

A missing endpoint or model is reported at startup, not at the first
request. Cost is usually `NA`, since gateway model ids rarely match
the bundled price table. Requires `llm.api >= 0.1.9`.

## JSON config keys

All keys shown with type and default, current as of corteza 0.6.3. Most defaults live in `R/config.R::load_config()`; a few (memory keys and the CLI port fallback) live in `inst/bin/corteza`.

### Core

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `provider` | string | `"anthropic"` | LLM provider: `anthropic`, `openai`, `moonshot`, `openai_codex`, `ollama`, `openai_compatible` |
| `model` | string or null | null | Model name (null = provider default; required for `openai_compatible`) |
| `base_url` | string or null | null | Endpoint for `openai_compatible` (see below) |
| `port` | integer | `7850` | MCP server port |

### Context

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `context_files` | string[] | `[]` | Extra files to load into the system prompt |
| `context_warn_pct` | integer | `75` | Token-usage % to start showing warnings |
| `context_high_pct` | integer | `90` | Token-usage % for orange indicator |
| `context_crit_pct` | integer | `95` | Token-usage % for red indicator + hint to `/clear` |
| `context_compact_pct` | integer | `90` | Auto-compaction threshold |
| `context_compact_bytes` | integer | `900000` | Serialized-history compaction guard for the Codex transport |
| `context_request_buffer_retries` | integer | `3` | Forced compact-and-continue attempts after a Codex request-buffer error |
| `journal_enabled` | boolean | `true` | Keep the provider-native history journal (`<id>.journal/`: a live snapshot plus a segment per compaction) beside the session transcript |
| `context_edit` | object | `{"enabled": false, "cadence": "pressure"}` | Model-requested conversation edits; cadence is `pressure` or `always`, and journaling is required |
| `tool_output_caps` | object | `{}` | Per-tool result caps, e.g. `{"game_action": {"max_chars": 400000, "max_lines": 20000}}`; other tools keep the 50-line / 5000-char cap |
| `context_include_soul` | boolean or null | null | Include `SOUL.md` (null = saber default) |
| `context_include_user` | boolean or null | null | Include `USER.md` (null = saber default) |
| `instruction_roots` | object | `{}` | Additional named roots for lazily loaded `SKILL.md` instructions |
| `instruction_disabled` | string[] | `[]` | Exact stable instruction ids to omit |

### Safety

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `approval_mode` | string | `"ask"` | `"ask"`, `"allow"`, or `"deny"` |
| `dangerous_tools` | string[] | `["bash", "run_r", "run_r_script", "write_file", "replace_in_file", "base::writeLines"]` | Tools requiring approval |
| `permissions` | object | `{}` | Per-tool overrides, e.g. `{"bash": "deny"}` |
| `denied_paths` | string[] | `["~/.ssh", "~/.gnupg", "~/.aws", "~/.config/gcloud", "~/.kube", "~/.docker"]` | Blocked paths |
| `allowed_paths` | string[] or null | null | If set, only these paths allowed |

### Supervisor

A Matrix room's tool calls, and those of the jobs it delegates, run with
nobody at a prompt. Each call goes through three steps:

1. **Rules in code.** They read the call, shell commands and R code
   included. Credentials, writes outside the project, elevated
   privileges, other machines, publishing, and git commands that throw
   work away are put to a person. No model is asked about these.
2. **A monitor**, where the bot config sets `auto_approve_asks: true`.
   It is a model with read-only tools in its own process, started with
   the shared and project instructions. It rules on what the rules left
   open: it approves the call, refuses it with a reason the working
   agent reads, or passes it to a person.
3. **A person**, by reaction in the room, for what steps 1 and 2 sent
   on. Without `auto_approve_asks` a person is asked in place of step 2.

No setting approves a call unseen. The rules read text and are not a
sandbox: a command can be written so that they miss it, which is what
the monitor and the tool policy above are also there for.

These keys are read from the user's config file only. A project's
`.corteza/config.json` cannot set them.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `supervisor.provider` | string | the working agent's | Provider for the monitor |
| `supervisor.model` | string | the working agent's | Model for the monitor. A small, fast model keeps each ruling to a few seconds |
| `supervisor.thinking` | string | provider default | Anthropic `thinking.type` for the monitor |
| `supervisor.reasoning_effort` | string | provider default | Reasoning effort for the monitor |
| `supervisor.timeout_sec` | number | `120` | A monitor that has not ruled by then is stopped and the call goes to a person |
| `supervisor.max_turns` | integer | `6` | Model turns the monitor may take on one call |
| `supervisor.write_roots` | string[] | `[]` | Directories outside the project that may be written without asking a person, such as a notes directory |

### Skills

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `skill_paths` | string[] | `[]` | Legacy executable/file-skill configuration |
| `skill_packages` | object[] | `[{"package":"base", ...}, {"package":"utils", ...}]` | R packages registered as tools |
| `skill_timeout` | number | `30` | Default skill execution timeout (seconds) |
| `skill_timeout_max` | number | `1800` | Host ceiling for a model-requested supervised `run_r` timeout |
| `run_r_mode` | string | `"in_process"` | `"in_process"` preserves the caller's R workspace; `"worker"` uses a private persistent `callr::r_session` with enforceable deadlines |
| `run_r_worker_options` | object | `{}` | Named arguments for `callr::r_session_options()` when `run_r_mode` is `"worker"`: `env`, `libpath`, `cmdargs`, `arch`, ... |

`skill_timeout` is the default for ordinary skills and for self-bounded tools
such as `bash`, `cmd`, and `run_r_script`. An explicit tool argument still wins.
Those self-bounded tools enforce the deadline in processx/callr rather than via
R's process-global `setTimeLimit()`.

The default `run_r_mode = "in_process"` preserves the historical embedding
contract: `tool_run_r(code, envir = globalenv())` and existing CLI/chat sessions
continue to use the host process. That is the fastest path and shares the
caller's live `.GlobalEnv`, which is what an interactive session usually wants.

Any session, interactive or unattended, can opt into `"worker"` instead. Choose
it when you want a real wall-clock kill for hang-prone or untrusted R, where
`setTimeLimit()` in the host process is not safe, or crash isolation so a
segfault or a runaway allocation takes down a disposable child rather than your
session. The cost is per-call serialization and a workspace kept separate from
the host's `.GlobalEnv`, so it is an opt-in for safety, not the interactive
default.

Each worker session owns one persistent `callr::r_session`: R objects, attached
packages, the working directory, helpers, and handles survive between `run_r`
calls, but do not enter the host's `.GlobalEnv`. The worker starts with R's
normal default packages (`utils`, `stats`, `methods`, and the rest), so helpers
like `str()`, `tail()`, and `setNames()` work exactly as in a standard session;
set `R_DEFAULT_PACKAGES` in `run_r_worker_options$env` to override. A model may
request a different timeout on an individual call; `skill_timeout_max` is the
host-owned ceiling. A host may narrow it further for an expiring external lease.

`run_r_worker_options` shapes that worker process without touching the host:
a private environment or library path, extra command-line arguments, or
`arch`, which callr resolves to `R.home("bin")/<arch>/R`. A strict host can
install a wrapper there that starts R inside a sandbox (bubblewrap, say), so
only the model's R runs confined. `env` replaces callr's default
(`TERM=dumb`) rather than extending it.

Every tool result is capped at 50 lines or 5000 characters before it reaches
the model (content reads such as `read_file` get a larger budget), with the
full text kept behind a handle. `tool_output_caps` raises the cap for named
tools whose results the model must see whole: a host-supplied game board, a
table, a structured report. Past the raised cap the result still stashes to
a handle.

For a changing deadline, an embedded host may set
`session$run_r_timeout_cap <- function() seconds_remaining` before calling
`turn()`. The callback takes no arguments and returns a non-negative number of
seconds. The standard dispatcher evaluates it before every supervised R call;
zero refuses execution. A numeric value also works. This host-owned cap can
only shorten the configured or requested timeout, and needs no custom executor.

On a clean interrupt, assignments completed before the deadline remain in the
worker for inspection. If the worker cannot stop cleanly, corteza terminates it
and reports that its in-memory state was lost. In-process mode rejects the new
model-facing timeout argument because it cannot enforce it without risking a
late interrupt in unrelated host code.

### Diagnostics

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `dry_run` | boolean | `false` | Preview tools without executing |
| `trace` | boolean | `false` | Emit structured trace events |

### Rate limiting

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `rate_limits` | object | `{}` | Per-provider limits |

### Subagents

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `subagents.enabled` | boolean | `true` | Enable subagent commands |
| `subagents.max_concurrent` | integer | `3` | Max parallel subagents |
| `subagents.timeout_minutes` | integer | `30` | Subagent kill timeout |
| `subagents.allow_nested` | boolean | `false` | Allow nested subagents |
| `subagents.default_tools` | string[] | `["read_file", "grep_files", "r_help", "web_search", "fetch_url"]` | Tools available to subagents when no preset is given |
| `subagents.base_port` | integer | `7851` | Starting port for subagent MCP servers |

**Presets.** `/spawn --preset <name>` picks a fixed tool list at spawn time:

- `investigate` (default): `read_file`, `grep_files`, `r_help`, `web_search`, `fetch_url` (read/search/help, plus network reads).
- `minimal`: `read_file`, `grep_files`.
- `work`: investigate + `bash`, `write_file`, `replace_in_file`, `list_files`, `git_status`, `git_diff`, `git_log`, `run_r`.

`/spawn --tools <comma-list>` overrides the preset with an explicit tool list.

**Permission model.** Subagents have no interactive approval channel back to the parent or user. A child's tool list is fixed at spawn time — via the `--preset` or `--tools` flags on `/spawn`, or the `preset` / `tools` arguments to `subagent_spawn()`. There is no mid-run path to escalate to a riskier capability: the child's approval callback denies by default, and the parent cannot grant new tools after the fact. If a task may need shell, write, or network capability, choose a preset or explicit tool list that includes it at spawn time; otherwise the child should report that it is blocked.

### Workspace

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `workspace.enabled` | boolean | `true` | Enable managed workspace |
| `workspace.budget_chars` | integer | `32000` | Context budget in characters |
| `workspace.capture_results` | boolean | `true` | Promote large results to handles |
| `workspace.max_result_size` | integer | `50000` | Max result size before handle promotion |
| `workspace.scan_globalenv` | boolean | `true` | Scan `.GlobalEnv` on startup |
| `workspace.scan_max_bytes` | integer | `52428800` | Max bytes to scan from `.GlobalEnv` (50 MB) |
| `workspace.max_object_summary_chars` | integer | `2000` | Max summary length per object |

### Channels

#### Chat channel

The chat channel requires `chat.api` (Suggests), which carries the transport adapters. Configured via a separate helper, not the main JSON config.

```r
corteza::bot_configure(
  server = "https://matrix.example.com",
  user = "corteza_bot",
  password = "verysecure",
  room = "#general:example.com",
  model = "llama3.2",
  provider = "ollama"
)
```

#### Voice in a call

A bot can take part in a call as a participant: it hears each person's
audio from the SFU, answers, and speaks. The call runs in a worker
process beside the bot (`R/call-worker.R`); the bot process keeps the
Matrix side. It needs `livekitr` (Suggests) and the two speech routes
below. These keys live in the bot config (`bot_configure()`'s file),
under `voice`:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `voice.stt.wire` | string | `"openai"` | The route's shape: `"openai"` (the OpenAI audio API) or `"gpu-host"` (a gpu.ctl host's `POST /infer`, spoken directly) |
| `voice.stt.url` | string | required | Base URL: of an OpenAI-shaped transcription route (`/v1/audio/transcriptions` is appended), or of the gpu.ctl host |
| `voice.stt.model` | string | required | Model name sent with each utterance; on the gpu-host wire, the catalog entry (`whisper-small`) |
| `voice.stt.language` | string | unset | Language hint sent with each utterance (OpenAI wire) |
| `voice.stt.key` / `voice.stt.key_env` / `voice.stt.key_file` | string | unset | Bearer credential: literally, as the name of an environment variable, or as the path of a file of raw bytes sent base64-encoded (the gpu-host token; read at each request) |
| `voice.tts.wire` | string | `"openai"` | As for `stt` |
| `voice.tts.url` | string | required | Base URL of an OpenAI-shaped synthesis route (`/v1/audio/speech`), or of the gpu.ctl host |
| `voice.tts.model` | string | required | Model name; on the gpu-host wire, the catalog entry (`chatterbox-turbo`) |
| `voice.tts.voice` | string | unset | Voice name; on the gpu-host wire, required, the path of a reference WAV the voice is cloned from |
| `voice.tts.key` / `voice.tts.key_env` / `voice.tts.key_file` | string | unset | As for `stt` |
| `voice.call.auto_join` | logical | `true` | Join a room's call when someone else is in it (a call button posts a membership event; that is the bot's signal). `/call` still works when this is off. |
| `voice.call.rooms` | array | unset | Rooms whose calls the bot joins on its own; absent, any room it is in |
| `voice.call.connect_delay_s` | number | 0 | Seconds to wait after posting the call membership before joining the media room; for a client that only notices participants who connect after it does |
| `voice.call.greeting` | string | unset | Said once on joining a call, before anyone has spoken; not posted to the room |
| `voice.call.livekit_log` | string | unset | livekitr's log level in the call worker (`warn`, `info`, `debug`, `trace`), forwarded to the bot's log |
| `voice.call.ice_transport` | string | unset | Passed to `livekitr::lk_connect()`: `"relay"` forces media through TURN, `"all"` (the default) allows every candidate |
| `voice.call.answer` | string | `"addressed"` | With several people in the call, answer only when addressed by name or as a follow-up (`"addressed"`), or answer everyone (`"always"`). With one person the bot always answers. |
| `voice.call.vad` | object | unset | Endpointing settings passed to the energy detector: `start_ms` (200), `end_ms` (700), `margin_db` (10), `min_ms` (300), `max_ms` (30000) |

The transcription route takes one file per utterance, so the bot
decides where an utterance ends itself, from signal energy. Replies are
synthesized a sentence at a time; someone talking over the bot stops it
within about a fifth of a second, and the reply posted to the room is
trimmed to what was heard.

The bot joins a room's MatrixRTC call (what Element Call and FluffyChat
2.10 start with their call button) when someone else is in it, in the
rooms `voice.call.rooms` allows, and hangs up when everyone else has
left. `/call` in a room joins by hand and `/hangup` leaves.
Joining needs chat.api 0.1.0.1 and a bot configured with `e2ee: true`,
since the call's media keys travel over Olm. In an encrypted room the
trim of an interrupted reply is not applied: chat.api does not edit in
encrypted rooms, so the full reply stands.

### Legacy memory

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `legacy_memory_tools_enabled` | boolean | `false` | Show memory slash commands |
| `memory_flush_enabled` | boolean | `true` | Auto-flush before compaction |
| `context_include_memory_logs` | boolean | `false` | Include daily logs in context |

## Slash commands

In-chat commands prefixed with `/`.

| Command | Description |
|---------|-------------|
| `/quit`, `/exit`, `/q` | Exit corteza |
| `/status` | Runtime and session status |
| `/doctor` | Check provider, git, MCP, context health |
| `/tools` | List available tools |
| `/diff [ref]` | Git diff against HEAD or ref |
| `/review [ref]` | Review local changes with LLM |
| `/config` | Active runtime configuration |
| `/permissions` | Tool approval and sandbox settings |
| `/clear` | Clear conversation (keeps session) |
| `/compact` | Summarize conversation to free context |
| `/sessions` | List sessions |
| `/context` | Live context usage and loaded files |
| `/model` | List current/configured models and the active provider's catalog |
| `/model <name> [provider]` or `/model <number>` | Switch model, optionally provider; numbers select from the last displayed list |
| `/effort [level \| default]` | Show or change reasoning effort for this conversation; `default` restores the configured/provider default |
| `/provider <p>` | Switch provider |
| `/dryrun` | Toggle dry-run mode |
| `/trace [N]` | Last N tool executions |
| `/skill list` | List installed skills |
| `/skill install <path\|url>` | Install a skill |
| `/skill remove <name>` | Remove a skill |
| `/skill test <path>` | Run skill tests |
| `/spawn <task>` | Spawn a subagent |
| `/agents` | List active subagents |
| `/ask <id> <prompt>` | Query a subagent |
| `/kill <id>` | Terminate a subagent |
| `/remember <fact> #tags` | Remember with tags |
| `/remember --global <fact>` | Remember globally |
| `/recall <query>` | Search memories |
| `/recall --tags` | List memory tags |
| `/flush` | Flush memories to daily log |
| `/last [N]` | Show Nth most recent tool output |
| `/outputs` | List recent tool outputs |
| `/help` | Show help |

The model menu is shared by the CLI, `chat()`, and Matrix rooms. With a
version of llm.api that exports `list_models()`, it queries the active
provider's catalog (OpenAI, Anthropic API, Moonshot, Ollama, or a compatible
gateway). Codex subscription entries come from its local `models_cache.json`
and are identified as cached. Catalog membership is not a tool-support check.
If discovery fails or is unavailable, the current and configured `models`
entries remain selectable. Refresh the list with `/model`; numeric selection
uses the last displayed list.

`/effort` reports the current reasoning setting and `/effort high` changes it
for the current conversation. `/effort default` removes the session override
and uses configuration, or the provider default when none is configured.
The provider validates which levels its model accepts. In a split bot this
changes the talker, leaving the doer's effort unchanged. These overrides are
not global configuration changes; use `reasoning_effort` (or
`talker.reasoning_effort` for a split bot) for a startup default.

GPT-6 tool calls (including Astra and Sol) require OpenAI's Responses API.
The corresponding llm.api routing update selects Responses with web search
off as well as on. Older OpenAI models keep their Chat Completions route
when search is off, and `openai_codex` always uses Responses. Corteza's
context-editing tool currently excludes Responses histories, so CLM edits
remain unavailable on that wire. With llm.api's `agent_uses_responses()`
export, Corteza checks the selected model and search setting before the first
request, including fallback candidates. Pure CLM rejects unsupported routes
before generation; hybrid mode keeps automatic compaction and omits the
editing tool. With older llm.api versions, the preflight follows their
search-only OpenAI routing rule.

## Instruction skills

Instruction skills are exact `SKILL.md` files discovered recursively at
session startup. Built-in roots are:

| Scope | Path |
|-------|------|
| Global | `tools::R_user_dir("corteza", "data")/skills/` |
| Project | `.corteza/skills/` |

Only compact metadata enters the system prompt. The agent calls the read-only
`skill_instructions` tool with the displayed stable id when it needs the
body, or supplies a relative `resource` path for a supporting file. The
catalog and hashes are fixed for the session; restart or an archival context
refresh creates a new snapshot.

Additional roots must be explicit and named:

```json
{
  "instruction_roots": {
    "shared": "../../skills",
    "team": "/srv/team/instructions"
  },
  "instruction_disabled": [
    "shared:deploy/production"
  ]
}
```

An entry at `<root>/analysis/logs/SKILL.md` has id
`<root-id>:analysis/logs`. A `SKILL.md` directly in the root has the
root id. Project roots override global roots with the same id; disabled ids
from both configs are combined and cannot be re-enabled by the project.
Relative paths in global config resolve from the global config directory;
project paths resolve from the project working directory. The built-in ids
`corteza-data` and `project` are reserved. corteza does not implicitly
scan `~/skills`.

A symlink directly under a root is an alias: `<root>/saber ->
~/saber/inst/skills/saber` yields id `<root-id>:saber`, and the target is
walked as its own bounded tree. When two roots reach the same `SKILL.md`,
the more specific root owns the entry. Symlinks below the top level must
stay inside their root.

Instruction documents are not executable tools. They can teach the model to
use tools it already has, but do not add capabilities to the executable tool
registry.

**Package skills:** register R packages as tools via `skill_packages` in config.

## MCP server

Expose corteza tools to external MCP clients. `serve()` supports two transports:

```r
# stdio transport (Claude Desktop, Claude Code, other MCP clients)
corteza::serve()

# socket transport (corteza CLI, R-to-R clients)
corteza::serve(port = 7850)

# restrict the toolset
corteza::serve(port = 7850, tools = "core")
```

`port = NULL` (the default) selects stdio. `cwd = NULL` (the default) leaves the working directory alone; pass an explicit path to anchor the server elsewhere. Tools execute in a persistent R session, so objects and loaded packages survive across calls.

Claude Desktop config (`~/.config/claude/claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "corteza": {
      "command": "Rscript",
      "args": ["-e", "corteza::serve()"]
    }
  }
}
```

## Session tuning

`new_session()` is the programmatic entry point for embedding corteza in your own R code: a Shiny app, a batch script, a custom approval flow, or tests. The CLI and Matrix channels build on this same primitive.

```r
library(corteza)

s <- new_session(
  channel = "cli",
  provider = "anthropic",
  max_turns = 20L,
  verbose = FALSE
)

result <- turn("What packages are loaded?", session = s)
```

The session is an environment that carries history, the active provider/model, and the approval callback. `turn()` mutates it in place and also returns the updated session, so you can chain calls or persist `s$history` between runs.

### Session parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `channel` | string | `"cli"` | `"cli"`, `"console"`, `"matrix"` |
| `history` | list or null | null | Prior messages; resumes mid-conversation |
| `model_map` | list or null | null | `list(cloud = ..., local = ...)`; falls back to `corteza.model_map` option |
| `provider` | string | `"anthropic"` | LLM provider |
| `tools_filter` | string[] or null | null | Restrict available tools |
| `system` | string or null | null | System prompt override |
| `approval_cb` | function or null | null | Called when policy returns `"ask"`. Default denies. |
| `max_turns` | integer | `10` | Max tool-use turns per call |
| `verbose` | boolean | `FALSE` | Print tool-call progress |

## Room progress and images

Room turns and delegated jobs show tool counts and failed or refused outcomes.
With `activity_detail: true`, they also show commentary, command or path previews,
and short result excerpts in expandable tool groups. The final answer is a separate message. Progress
stays in the originating room or thread and uses notices so bots do not answer
the activity trail.

Updates to the notice are coalesced by `activity_interval` in the bot config
(15 seconds by default), with a final flush when the turn or job ends. Transports
that cannot edit messages receive the completed trail once. Long trails show a
bounded tail; full tool traffic remains in the session journal. Plain-text
clients receive the same selected detail level without the disclosure widgets.

Image attachments are checked against the configured size ceiling after download.
Their PNG, JPEG, GIF, or WebP signatures determine the format sent to the model,
even when the uploader supplies the wrong MIME type or filename extension.

## systemd service

`bot_run()` is designed for a systemd user unit.

`~/.config/systemd/user/corteza-matrix.service`:

```ini
[Unit]
Description=corteza chat bot
After=network.target

[Service]
Type=simple
ExecStart=Rscript -e 'corteza::bot_run()'
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

Enable and start:

```bash
systemctl --user daemon-reload
systemctl --user enable corteza-matrix.service
systemctl --user start corteza-matrix.service
```

View logs:

```bash
journalctl --user -u corteza-matrix.service -f
```

## Environment variables

| Variable | Required for | Description |
|----------|-------------|-------------|
| `ANTHROPIC_API_KEY` | Anthropic | API key |
| `OPENAI_API_KEY` | OpenAI | API key |
| `MOONSHOT_API_KEY` | Moonshot | API key |
| `TAVILY_API_KEY` | `web_search` tool | Optional |
| `NO_COLOR` | CLI | Disable ANSI colors |
| `FORCE_COLOR` | CLI | Force ANSI colors |
| `CORTEZA_STATE_DIR` | Matrix bot | Out-of-band signal directory |

Set API keys in `~/.Renviron`:

```
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
```

## R options

| Option | Default | Description |
|--------|---------|-------------|
| `corteza.model` | null | Default model (overrides provider default) |
| `corteza.local_models` | `c("gpt-oss:120b", "gpt-oss:20b")` | Candidates for `default_local_model()` |
| `corteza.max_turns` | null | Default `max_turns` for `chat()` (null falls back to 50) |
| `corteza.trace` | `FALSE` | Enable structured trace events |

Set in `~/.Rprofile` or per-session via `options()`.

---

*Guide version: 0.6.3*
