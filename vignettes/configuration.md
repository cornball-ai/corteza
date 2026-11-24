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

corteza reads two JSON files and merges them. Project config overrides global
config. The project file is read from a trusted directory only (see
[Trusted projects](#trusted-projects)).

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

## Trusted projects

A project's own files arrive with its repository, and a cloned repository is
somebody else's text. A session therefore loads them only from a directory
you have trusted. In any other directory it loads nothing of the project's,
says so, and runs on your global config and the package's own guidance.

What trust covers:

| From the project | Effect when trusted |
|------------------|---------------------|
| `.corteza/config.json` | Merged over the global config: provider, model, base URL, approval settings and the rest |
| `.corteza/skills/` | R files are sourced at start; `SKILL.md` files join the instruction catalog |
| `AGENTS.md` or `CLAUDE.md` | Added to the system prompt |
| `.corteza/harness.json` | Its lessons are added to the system prompt, and `/refine` and the lesson tool may write it |
| `DESCRIPTION` and commit subjects | Summarized in the briefing at the top of the prompt |
| Files your global `context_files` names by a relative path, or by a path inside the project | Added to the system prompt |

A directory is trusted when it is, or lies inside, a directory on your list.
Paths are compared after links are resolved, so a link into a trusted
directory is judged by where it points. The list lives in your config
directory, where no project can write:

- `trusted_projects` in the global `config.json`, which you edit:

  ```json
  {
    "trusted_projects": ["~/work", "/srv/projects/reports"]
  }
  ```

- `trusted-projects.json` beside it, which `/trust yes` writes. Remove a line
  there to undo it.

Trusting a directory trusts every directory inside it, so trusting your home
trusts everything you clone into it.

How you are asked:

- The `corteza` command and `chat()` ask once at start, when the directory is
  not trusted and holds something a session would load. Enter means no.
- `/trust` shows where the directory stands and what it holds. `/trust yes`
  trusts it.
- A new answer applies from the next conversation: `/clear` starts one, with
  the config, the tools and the prompt taken again from the directory. A
  session's prompt and tools are fixed while a conversation lasts. What a
  flag or a command set in this process (`--dry-run`, `/permissions`,
  `/dryrun`, `/context-edit`) carries over.
- `/status` shows `trusted:` and lists what an untrusted session left alone.

In a Matrix room, `/trust` acts on the room's directory and only an operator
of the bot may use it. The answer applies from the room's next conversation
(send `/clear` to start one). A room whose directory is not trusted says so
once, ahead of its first reply. A bot's directory has to be on the list
before the bot starts, or the bot runs without its project config and
instructions. A room with no directory works in a scratch folder, which is
never trusted.

In an untrusted directory the terminal's "Allow always" lasts for the
session and writes no file. The lessons tool refuses the project's store and
keeps a lesson only under scope `global`, and `/refine` lists the global
store and runs no review pass.

R can read the working directory's `.Rprofile` before the terminal or
`chat()` can ask about trust. The worker startup settings are described
under **Skills** below.

Trust decides what corteza loads by itself. It does not confine the agent:
a tool can still read a file in an untrusted directory when asked, and what
it reads comes back as a tool result under the approval settings below.
For an unattended session (a Matrix room or one of its jobs), a write to a
`.corteza` folder by a file tool, a shell command or R code goes to a person,
and so does R code that names the list of trusted directories. The lessons
tool still records a note in a trusted project's store once that note is
approved.

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
| `--resume` | Resume the latest session started in this directory | `false` |
| `--list` | List the sessions started in this directory and exit | `false` |
| `--all` | With `--resume` or `--list`: every directory | `false` |
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

A session belongs to the directory it was started in. `--resume`, `--list`,
`/sessions` and `chat(session = TRUE)` look at the current directory's
sessions, and say how many there are elsewhere when it has none. `--all` and
`/sessions all` cover every directory. `--session <key>` and
`chat(session = "<key>")` resume that session from anywhere, and say so when
it was started somewhere else: the paths in its history belong to that
directory.

## Unattended work

`/auto` continues the current task using the conversation and its approved
plan. `/auto <goal>` supplies a new goal; an empty session needs one. Runs
continue until completion, a blocker, or a resource limit. `--loops N` sets
an iteration limit. `/auto status` reports the last stop, and `/auto resume`
restarts that goal in the same conversation.

The startup display names the working-directory scope and whether shell/R
execution is enabled. `/auto --exec` explicitly permits the monitor to assess
shell/R calls; a project restriction can still veto it. Without that grant,
those tools are omitted. Tools with no class, the session's control and
continue tools, and writes that name no path the monitor can check are
also omitted rather than offered to the model and then stopping the run
when selected. Path, credential, policy, and monitor checks still apply to
every dispatched call.

The monitor reads each tool argument in full. If any one argument exceeds
2,000 characters in JSON, `/auto` refuses that call before asking the monitor.
The worker receives the reason and can continue with a smaller call.
This includes the content of a file write, even when your config allows the
tool. JSON quotes and escapes count toward the limit.

When a run stops, auto mode is off. Ordinary messages do not restart it.
`/permissions` controls ordinary tool approvals separately from `/auto`.

The `auto` block of `config.json` sets a run's bounds and its monitor. The
first of these limits to be reached stops the run.

The iteration limit is the exception. `/auto --loops N` stops after N
iterations. `/auto` without `--loops` runs with no iteration limit, whatever
`max_loops` says, and the other limits stop it. `/auto resume` keeps the
count of the run it resumes. One iteration can run many model turns.

| Key | Default | Meaning |
|-----|---------|---------|
| `max_loops` | 10 | Iterations, for a run that sets no count of its own. `/auto` always sets one or runs without, so it does not read this |
| `max_tool_calls` | 300 | Tool calls, the bound that holds whatever a turn does |
| `max_minutes` | 30 | Minutes since the run started |
| `max_cost` | 5 | Dollars spent. Skipped where the provider reports no price, as on a subscription, and `max_tokens` holds instead |
| `max_tokens` | 2000000 | Tokens used |
| `stall_loops` | 2 | Iterations in a row that changed nothing on disk |
| `allow_exec` | `false` | Whether the monitor may rule on shell and R calls. Your config or `--exec` grants it; a project's config can only refuse it |
| `never_broker` | none | Tools that always stop the run for you. Yours and the project's together |
| `monitor_model` | the session's model | The model the monitor runs on. The monitor uses the session's provider, so name a model of that provider |
| `monitor_timeout` | 120 | Seconds the monitor has to rule on a call |

The monitor makes a request for each call it rules on, so a long run makes
many. A smaller model in `monitor_model` costs less for each ruling.

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
default is a private room for the current CLI session, named `Corteza: <session-id>`.
Creating session rooms requires `chat.api >= 0.2.0` on the courier.
The courier invites its configured operators and enables room encryption before
inviting them when its `e2ee` setting is on. Two CLI sessions in the same checkout
get separate rooms. Reconnecting, including after `--session` or `--resume`,
reuses the session's room on that bot. The working directory is room context,
not its identity.

Set `remote_control.bot_id` to select a default bot. An explicit `[room]` or
`remote_control.room` still attaches an existing room accessible to the bot.
Rooms with delegated work in flight or another attached CLI are refused.

`/clear` (also `/reset` and `/new`) starts a fresh CLI conversation. While remote
control is connected, it creates a new session room and posts its link in the
old room. At the local prompt, the new room is created on the next `/rc`.
The previous session and room remain available to resume. Disconnected session
rooms ask you to resume the CLI and run `/rc`; they do not start a separate bot
conversation. Explicitly attached existing rooms retain their normal bot behavior.

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
completed tool history. In a managed session room, threaded messages enter the
same CLI conversation, including approval and control commands; replies appear
in the main timeline. In an explicitly attached existing room, only the main
timeline is attached and threads keep their own bot sessions. Other senders
cannot control the CLI, but room members can see its public output.

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

Session rooms accumulate: each `/clear` creates another room. Rooms, local room
bindings, mailboxes, and delivery/deduplication markers are retained indefinitely.
There is no automatic cleanup or grouping into Matrix spaces yet.

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

## The prompt cache

On an Anthropic provider (`anthropic`, `anthropic_claude`) a session caches
its prompt for 5 minutes unless a setting says otherwise. The provider keeps
the tools, the system text and the conversation that a request sent. A
request that starts within 5 minutes of the start of the one before it reads
them back at a lower price and pays the write price only for what it added.
Each read renews the 5 minutes.

```json
{ "cache": "none" }
```

`cache` is `"5m"`, `"1h"` or `"none"`. `new_session(cache = )` sets it for one
session and wins over the config.

The cache changes only the bill: the model receives the same input either
way. Against an input token sent with no cache, a 5-minute write costs 1.25
times as much, a 1-hour write 2 times, and a read a tenth, or less on the
newest models (Anthropic's prompt caching page,
<https://platform.claude.com/docs/en/build-with-claude/prompt-caching>, read
2026-11-17). So:

- A turn of several requests, which every turn with a tool call is, pays the
  write once and reads after it.
- A request with none before it in the last 5 minutes pays the write on its
  whole input and reads nothing: a quarter more than with no cache. That is
  the first request of a session, a reply after a pause, and the request
  after a tool that ran longer than 5 minutes. A conversation of single
  replies more than 5 minutes apart costs more with the cache than without.
  Set `"none"` there, or `"1h"` where the replies come inside the hour.
- A prompt shorter than the provider's minimum, from 512 tokens on the
  newest models to 4,096 on some older ones, is not cached and costs what it
  did.
- A change to the system text is a change to everything after it. With the
  task list on (`task_list_enabled`), the list is part of that text, so the
  turn after one that changed the list writes the whole prompt again.

`turn()$usage` says what a turn read and wrote (`cache_read_input_tokens`,
`cache_creation_input_tokens`), and its `cost` counts both. Anthropic leaves
cached input out of `input_tokens`. `total_tokens` counts it, and so do
`/spent` and the 2 token caps, `auto.max_tokens` for `/auto` and
`subagents.mcp_spend_cap_tokens` for subagents over MCP: a cached token
counts as 1 there, whatever it cost.

The setting reaches Anthropic providers only. corteza sends it to no other,
whatever the config says.

Anthropic sessions with context editing also default to `"5m"`. Editing an
earlier entry changes the reusable prefix. Choose `"1h"` for longer pauses
or `"none"` to disable caching.

## When a conversation is compacted

A compaction replaces the older turns of a conversation with a summary. Three
conditions start one.

- **A share of the window.** The context reaches `context_compact_pct` of the
  model's window, 90% by default.
- **A size.** The context reaches `context_compact_tokens`, 200,000 by default.
  On a model with a 1M window, 90% is 900,000 tokens, so this number decides
  there. `0` turns it off.
- **A return after a pause.** The model last answered at least
  `context_idle_minutes` ago (60 by default), the pause is longer than the
  prompt cache lasts (`cache`, under "The prompt cache"), and the context
  holds at least `context_idle_tokens` (100,000 by default). The provider
  has dropped its cached copy of the conversation by then, so the next
  request pays the full price for the whole history. A summary costs one
  such request and makes every later request smaller.
  `"context_idle_minutes": 0` turns it off.

The first two are checked before a turn and between the tool rounds of a
turn, and the terminal checks again after the reply. The third is checked
once, before a turn's first request. The summary is then a request of its own
ahead of the reply, and a line in the terminal, or in a bot's log, says what
the wait is. A voice session skips the third, since a caller would hear the
wait as silence.

```json
{
  "context_compact_pct": 90,
  "context_compact_tokens": 200000,
  "context_idle_minutes": 60,
  "context_idle_tokens": 100000
}
```

`/context` shows the trigger in force and the window it is measured against.
With `"memory_flush_enabled": true`, an automatic compaction in the terminal
runs the memory flush first, which is a request of its own. It is off by
default, and `/flush` runs it by hand.

The R objects outlive the turns that made them: a compaction leaves every
object in place, whatever its size. So the summary ends with the names of the
objects `run_r` made that are still there, up to 30 of them, then the open
handles. The agent can use an object instead of rebuilding it. Only names are
listed, since reading an object can run code: a binding made with
`makeActiveBinding()` or `delayedAssign()` is named and left alone. This is
for the terminal, `chat()` and a subagent or doer, whose R process is the
session's own. A bot's rooms share one, so a room's summary has no list, and
neither does a session whose `run_r` runs in a worker process
(`run_r_mode: "worker"`).

None of the three applies to a session that manages its own context
(`context_edit` with `"strategy": "clm"`, under "Conversation context
editing"). Such a session is never compacted, and `/context` says so.

Each size is corteza's estimate, about 1 token per 4 characters. A provider
counts with its own tokenizer and can count more, so a context that corteza
puts at 200,000 tokens can be billed as a larger one.

### What the summary reads

The session's own model writes the summary, in one request, from the turns
the summary replaces. That request holds a budget of text that follows the
model's window: 120,000 characters at a window of 128,000 tokens or less, and
the same share of a larger one, which is 937,500 characters at 1M. With the
default `context_compact_tokens`, a model with a 1M window reads every turn it
summarizes. Where the turns do not fit, the oldest are left out and the
summarizer is told how many. On the Codex transport the budget is also held
to a quarter of `context_compact_bytes`, counted in characters.

The summarizer has 60 seconds to answer
(`subagents.context_compaction.timeout_seconds`). When it fails, the
conversation stays as it was and the turn goes on. The session's next summary
then reads 120,000 characters whatever the window, the smaller request, and a
summary that succeeds brings the larger budget back. A model too slow for the
larger request wants a longer `timeout_seconds`.

A summary made because the provider refused a request for its size reads
120,000 characters too. The larger budget rests on corteza's count of 4
characters to a token, and such a refusal says the count ran low for this
conversation, as it does for text in Chinese or Japanese.

### The model's window

corteza reads a model's window from a table in the package
(`R/context-budget.R`), where each number names the provider's page it was
read from. The table holds the largest input a model takes: OpenAI's API
models have a 1,050,000-token window and take 922,000 tokens of input, and
the table says 922,000. A model with no row is taken to have 128,000 tokens,
and `/context` says so.

`context_window` overrides the table:

```json
{ "context_window": 64000 }
```

It takes a number above 0. Anything else, a quoted `"64000"` included, is
ignored and the table answers.

Set it for a local model. An Ollama server decides the window when it loads a
model (`OLLAMA_CONTEXT_LENGTH`, or 4k, 32k or 256k tokens by the memory of
the GPU), so the table's row for an Ollama model is a guess, and your server
may give the model less.

## What a session sends

A session's system text and its tool list are fixed for the session. Each
request carries the earlier requests' messages unchanged, with the new ones
after them, so the provider's cache holds and so do the model's thinking
blocks. A change to either would make the provider read the whole history
again, and a model that signs its thinking (next section) refuses a replay
after one.

- Plan mode, an `/auto` run and the task list change neither. When one of
  them changes, the next prompt opens with a note in brackets, such as
  `[Plan mode is on. ...]`, and the note stays in the history. After a
  compaction, a context edit or a restore, which may have removed earlier
  notes, the next prompt states the session again.
- A tool the mode rules out stays in the list and is refused at the call:
  `exit_plan_mode` outside plan mode, a write or a command in plan mode, a
  call outside an `/auto` run's scope, and a call outside a job's tools. The
  refusal says why.
- The availability tests (the git tools in a repository, a search key) run
  once, so a `git init` during a session adds no tool.

These change the system text or the tools on purpose, and the log records
each (`request_prefix_reset`): a move to another provider, the terminal's
`/clear`, `/cd` in a room, a job that may not search (which sends the
provider's tools without its search tool, with the thinking removed before
and after it), and a worker given other tools. Context edits can reduce cache
reuse from the first changed entry.

## Thinking blocks after a rewrite

A Claude model that thinks by default signs each thinking block against the
conversation that produced it: the system text, the tools and every earlier
message. Where the provider checks that signature, a request that replays a
block after any of the three changed returns `API error (400)` with "bound to
a different conversation", and so does every retry.

corteza removes the thinking blocks from a history whenever it replaces one: a
compaction, a context edit, an archived turn, a move to another provider, a
conversation restored after a restart, and a job resumed in a new worker. Text
and tool calls stay, and the journal keeps the history as the model wrote it.
Appending removes nothing, so the tool rounds of a turn go back with their
thinking.

A session that sets `thinking_budget_tokens` keeps its blocks when its own
history is replaced. The models that accept a budget do not run the check, and
they expect an open tool round to keep the thinking it started with. A history
passed to `new_session()` comes from another conversation and loses its blocks
whatever the session's settings.

When a provider refuses replayed thinking anyway, the turn removes every
thinking block from the history, records that in the journal, and continues
once. Tools that already ran in the turn do not run again. A second refusal in
the same turn is raised as the provider's error.

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

On Anthropic, Codex Responses, and OpenAI chat-completions routes, the model receives
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
record. A provider or history format that cannot support CLM uses standard
compaction instead, including the API-key OpenAI Responses route. The configured CLM
preference stays intact, so a compatible route can use it again. Model-switch
acknowledgments and fallback replies explain the effective context strategy.
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

For an unsplit Matrix bot, one whose rooms are no talkers (see
[Talker mode](#talker-mode)), put `context_edit` and `context_vault` at the
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
| `cache` | string or null | null | Anthropic prompt cache, `"5m"` by default, including CLM. `"1h"` and `"none"` override it. |
| `task_list_enabled` | boolean | `false` | The task list in the terminal and in `chat()`: the `task_create` and `task_update` tools, and the instructions to ask first and propose a plan that go with them |
| `port` | integer | `7850` | MCP server port |

### Context

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `context_files` | string[] | `[]` | Extra files to load into the system prompt |
| `context_warn_pct` | integer | `75` | Token-usage % to start showing warnings |
| `context_high_pct` | integer | `90` | Token-usage % for orange indicator |
| `context_crit_pct` | integer | `95` | Token-usage % for red indicator + hint to `/clear` |
| `context_compact_pct` | integer | `90` | Auto-compaction threshold, as a share of the model's window |
| `context_compact_tokens` | integer | `200000` | A cap on that threshold in estimated tokens; the smaller of the two applies. `0` turns the cap off |
| `context_idle_minutes` | integer | `60` | Compact before a turn when the model last answered this long ago and the prompt cache has expired. `0` turns it off |
| `context_idle_tokens` | integer | `100000` | The smallest context the idle rule compacts |
| `context_window` | integer or null | null | The model's window in tokens (null = the package's table). Set it for a local model |
| `context_window_cap` | integer or null | null | Upper bound for the working window. For example, `400000` gives a 1M route a 400k budget and keeps a 200k route at 200k. Applies to the meter, compaction, model-managed history pressure and reviewer budgets. Ordinary compaction still starts at the earlier configured threshold (200k tokens by default). |
| `context_compact_bytes` | integer | `900000` | Serialized-history compaction guard for the Codex transport |
| `context_request_buffer_retries` | integer | `3` | Forced compact-and-continue attempts after a Codex request-buffer error |
| `journal_enabled` | boolean | `true` | Keep the provider-native history journal (`<id>.journal/`: a live snapshot plus a segment per compaction) beside the session transcript |
| `audit.enabled` | boolean | `true` | Record turns, tool decisions and room deliveries independently of display verbosity and `journal_enabled` |
| `audit.limits` | object | see below | Bound raw payload capture independently of model context caps |
| `context_edit` | object | `{"enabled": false, "cadence": "pressure"}` | Model-requested conversation edits; cadence is `pressure` or `always`, and journaling is required |
| `tool_output_caps` | object | `{}` | Per-tool result caps, e.g. `{"game_action": {"max_chars": 400000, "max_lines": 20000}}`; other tools keep the 50-line / 5000-char cap |
| `context_include_soul` | boolean or null | null | Include `SOUL.md` (null = saber default) |
| `context_include_user` | boolean or null | null | Include `USER.md` (null = saber default) |
| `instruction_roots` | object | `{}` | Additional named roots for lazily loaded `SKILL.md` instructions |
| `instruction_disabled` | string[] | `[]` | Exact stable instruction ids to omit |

Audit streams are private JSONL files under `<journal_dir>/audit/`, with one
writer per stream. Sessions without a journal use the package data directory's
`audit/` folder. Set `audit.enabled` to `false` to disable recording; embedded
callers may set `session$audit_dir` or `session$audit_enabled` explicitly.

Room sends and edits record a delivery start and ending, every returned event
ID, destination, and hashes of the generated and posted text. They work without
a vault. A missing event ID is unconfirmed; an error retains its condition
class. Recording failure never retries a send. Reply, worker-progress and
remote-control records link their originating turn where it is available;
administrative sends without a turn retain a null reference. The existing
conversation transcript still uses its last-event deduplication convention.

New transcript and trace files are created with mode `0600` under private
directories. Appending narrows an existing file to `0600` before writing. The
trace remains a preview: rows carry `truncated` and separate argument/result
flags with the character limits. Journals retain the original history.
The terminal turn record owns usage. Transcript and job-outcome copies are
marked derived and link to it; do not add these copies to turn totals. Unknown
usage is `null`. Older transcript zero blocks cannot establish measured zero.

These records are observational: a write failure emits one notice, and recovery
writes a gap with the missed-record count and interval. A process that exits
before recovery can leave an unresolved ending. Existing job, approval, and
journal commit requirements still apply. With llm.api 0.1.9.15, each provider
request records the final adapter-built body, raw usage and transport attempts.
Older llm.api versions remain supported and report request coverage unavailable.
Requests break down a turn's usage; do not add them to its canonical total.

Payload limits default to 16 MiB per full tool result (`result_bytes`), 256 MiB
per turn (`turn_bytes`), and 2 GiB per writer's store (`quota_bytes`). Requests,
message components and tool output are stored by SHA-256 under each stream's
payload store. Explicit omission records name the limit reached or a capture
failure. `audit_payload_pin()` protects incident/evaluation evidence from
explicit expiry; pinned bytes still count toward quota. `audit_payload_expire()`
defaults to a dry run with a 30-day horizon. It writes expiry records before
deleting unpinned payloads. A positive `retention.audit_payload_days` enables
automatic expiry. Event metadata stays. See `vignette("audit-records")`.

### Safety

Every tool has a class, which decides who approves a call to it:
`read`, `write`, `exec`, `control` (the session's own subagents, tasks
and jobs) or `continue` (work that goes on in a child, a worker or
another room). Reads, control and continue run; writes and commands
ask. A tool that names no class, such as a skill or a package tool
whose `skill_packages` entry gives none, asks too.

Plan mode refuses writes, commands, continuations and tools with no
class, and nothing below lifts that. The agent ends plan mode with
`exit_plan_mode`, whose prompt shows you its plan; a no keeps plan mode.
That prompt comes each time, as does the one for a path under a
credential store (`~/.ssh`, `~/.aws/credentials` and others): a saved
"always" never answers either, and neither prompt offers one. Personal
paths (`~/Documents`, `~/Downloads`) can be read; a write or a command
touching them in a room is refused.

At the terminal, "Allow always for this project" saves the answer in
`approvals.json` in your config directory, by directory, and later
sessions there skip that prompt. A project's own `.corteza/approvals.json`,
where earlier versions kept these answers, is not read, trusted or not:
the terminal says so when it starts. `/permissions` lists what is saved
for the directory.

The answers cover a talker's delegated jobs as well. A tool you allowed
always in the directory runs in a job without a question, except where
the credential rule asks, and a job's question offers "always" beside
yes and no, kept as the terminal's own prompt keeps it. In `chat()` an
"always" lasts for the session, from either prompt. A room's jobs take
none of these answers: a room's approvers answer there.

The keys below apply to what is left. None of them turns a refusal into
a run.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `approval_mode` | string | `"ask"` | What "ask" means for a tool with no `permissions` entry: `"ask"`, `"allow"`, `"deny"`, or `"review"` (the reviewer first; see Supervisor). A project's config cannot set `"review"` |
| `dangerous_tools` | string[] | `["bash", "run_r", "run_r_script", "write_file", "replace_in_file", "base::writeLines"]` | Tools that ask even where their class would run. Leaving a tool off this list does not stop it asking |
| `permissions` | object | `{"harness_note": "ask"}` | Per-tool verdicts, e.g. `{"bash": "deny"}` or `{"bash": "allow"}`. An entry replaces the verdict for its tool |
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

A room's monitor reads the conversation to judge a call. Each
conversation keeps a record beside the room's journal, a file per
conversation: who sent each message, in the words they sent, and the
bot's own posts. Only the lines of someone who may approve are taken as
instructions: the room's operators, or the one person in a private room.
Anyone else's, another bot's or a peer's request included, are shown as
theirs. A job's calls are judged by the conversation that asked for the
job, read when the call comes, so a "stop" sent since is in it. A job
handed over from another room is judged by that room's. The monitor is
not asked, and a person is, when nobody in the room may approve or the
record cannot be read whole:

- one with an unreadable line or over 100,000 new characters;
- one with a break: a restart whose backfill did not reach back to the
  last message on record, a line that could not be written, or a thread
  that went on from its archive;
- one that does not reach back to the start of the conversation. A
  conversation under way when a bot first runs 0.8.1 is one of these,
  and `/clear` starts one the monitor can read.

At the terminal and in `chat()`, `"approval_mode": "review"` gives a
session the same three steps for each call that would prompt. The
monitor there is called the reviewer. It rules on reads, writes and
commands that the rules leave open. It reads the conversation to judge
them, from the session's transcript: every message you sent, whole,
each after all the agent said since your message before it, the reply
and any job's result that came after. Only your words are taken as
instructions. It is not shown these:

- a compaction's summary, which the agent wrote;
- output you staged with `/r` or `!`;
- an `/auto` run's prompts, which its driver wrote, and the goal
  corteza writes for a bare `/auto`. It sees what you typed to start
  the run, and the run's replies;
- the agent's task list.

An approval runs the call and prints a dim line saying so. A refusal
goes back to the agent with its reason. A call it passes on prompts you
with its reason. These still prompt without it:

- a call the rules flag;
- a tool with no class;
- a credential path;
- leaving plan mode;
- any call while the reviewer cannot read the whole conversation: one
  with over 100,000 new characters, one with no transcript or with a line it
  cannot read, one where a reply to your message is missing (its write
  failed, or corteza stopped mid-turn), or one begun before 0.8.1, whose
  transcript does not mark what you typed. `/clear` starts a
  conversation it can read; in an editor, a new session does.

Room and terminal reviewers send a call to a person when any JSON argument
exceeds 2,000 characters, the worker's explanation exceeds 1,500 characters,
or the delegated task exceeds 4,000 characters. The reviewer reads the
delegated task in full whenever it rules on a job's call. The next eligible
call can use the same verified conversation. In `/auto`, an oversized
argument is refused and the run can continue.

The reviewer uses CLM for its own notes and tool results. With
`journal_enabled: false`, it reads the complete record afresh for each call.
A fresh reviewer
reads up to 100,000 characters. A warm reviewer can take another batch of
that size if its earlier inputs are intact and its model has room for the
request. On Claude and Codex, an unchanged request prefix can use the
provider's measured token total. New material and unmeasured requests use a
conservative byte bound. Both reserve room for the reply. A changed record
starts a fresh review.

A tool you allowed always runs without asking it. Plan mode still
refuses what it refuses. A reviewer that fails, or has not ruled by
`supervisor.timeout_sec`, passes the call to you. Interrupting a turn
while the reviewer works cancels its request, and the next call is
asked afresh. "review" changes only what would prompt: a call that
policy allows runs, as under "ask". In a room the rules read every
call, allowed ones included.

`/permissions review` turns it on for the session and `/permissions ask`
turns it off. Your config can turn it on for every session. A project's
config cannot: who reviews the agent is yours to choose. In a Matrix
room, `auto_approve_asks` is this mode. An `/auto` run keeps its own
monitor, since nobody is at its prompt.

A talker's delegated jobs are covered too. The doer reads the mode
before each call, so `/permissions review` typed while a job runs
reaches its next call. The reviewer judges a job's call by the
conversation that asked for the job and the task the talker wrote.
Your "allow always" answers cover a job's calls as they cover the
session's own, before the reviewer is asked.

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

### Retention

corteza keeps its sessions under `tools::R_user_dir("corteza", "data")`.
Once a day, when a terminal session starts and while a chat bot runs, it
removes the journal and the tool trace of every session that has gone
unused for `retention.session_days`. A session was last used when its
journal, its trace or its transcript last changed. The terminal says
what it removed.

Transcripts, session stores, config and audit event records stay. Old
terminal workspaces and unpinned audit payloads have separate opt-in ages.
A session whose journal is gone still resumes from its transcript, and
says what is gone. A room that restores its context from its journal
starts its next conversation fresh. A room's records of its
conversations go with its journal, and the room's calls then go to a
person until `/clear`.

A session's audit records are kept in its journal folder, under
`audit/`. The event stream stays. You can expire unpinned payloads
with `audit_payload_expire()` or the personal retention setting below.

The same run clears corteza's state folder (`state/` beside the sessions,
or `CORTEZA_STATE_DIR`) and its remote-control folder:

- A delegated job's records go with every job linked to it as a resume,
  a review or a delegation, once all of them have ended and none has
  changed for `session_days`. While one of them is queued or running,
  all stay.
- A job that ended incomplete and was never resumed holds the snapshot a
  resume starts from. Its chain waits `incomplete_days` instead, and
  never fewer than `session_days`. The terminal names each such job when
  it goes.
- A worker process's folder with no checkpoint loses its journal, less
  its audit records.
- A worker's folder with a checkpoint (the R objects and history a
  delegated job's worker left for the next job) goes once it and the
  session it belongs to have both gone unused for `session_days`: at the
  terminal the conversation's session, in a room or a thread the room's
  session, judged by its journal, which holds the record of its
  conversations. A room keeps one such folder across `/clear`, so the
  room's use counts, not one conversation's. Where the session has left
  nothing, the folder goes by its own age. The checkpoint goes first,
  then the rest, less the journal's audit records.
- A remote-control session goes once it was closed, or once its process
  on this machine has ended. One from another machine stays.

A worker's folder stays, whatever its age, while its worker process is
alive (a worker on another machine counts as alive), while its session
is open in the terminal or the bot doing the removal, and while a job is
open in its directory. Each of these, and the last use of the folder and
of its session, is read again just before the folder goes.

A folder there whose name and record are not of the form corteza writes
stays whole. Once it is old enough the terminal counts it, and
`sessions_prune(dry_run = TRUE)` lists it with kind `"unknown"`.

`sessions_prune(dry_run = TRUE)` reports what a run would remove.
`sessions_prune()` runs one now.

These keys are read from the user's config file only. A project's
`.corteza/config.json` cannot set them. A config file that does not
parse turns retention off, and a value that is not one non-negative
number keeps everything that key governs.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `retention.session_days` | number | `90` | Days a session goes unused before its journal and tool trace are removed, and days a job chain, worker folder or remote-control session goes unchanged before it is removed. `0` disables idle-session cleanup. |
| `retention.incomplete_days` | number | `180` | Days a job chain that ended incomplete, and was never resumed, is kept for its resume snapshot. Never fewer than `session_days`. `0` keeps it |
| `retention.audit_payload_days` | number | `0` | Days since an unpinned payload was last captured. Audit event records stay. |
| `retention.journal_segment_days` | number | `0` | Whole days to keep earlier rewrites in an active journal. Positive fractions round up. A running turn keeps the entries it needs. The live snapshot and latest committed rewrite stay. New journals group shared entries by day for expiry. Older shared entries keep their original layout until idle-session cleanup. |
| `retention.workspace_snapshot_days` | number | `0` | Days to keep a legacy R workspace snapshot beside its transcript. |

### Skills

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `skill_packages` | object[] | `[]` | R package functions registered as tools, as in `[{"package": "utils", "functions": ["head"], "class": "read"}]`. An entry's `class` applies to all its functions; an entry without one leaves them asking. Up to 0.8.0 the default registered 8 functions of base and utils |
| `skill_timeout` | number | `30` | Default skill execution timeout (seconds) |
| `skill_timeout_max` | number | `1800` | Host ceiling for a model-requested supervised `run_r` timeout |
| `run_r_mode` | string | `"in_process"` | `"in_process"` preserves the caller's R workspace; `"worker"` uses a private persistent `callr::r_session` with enforceable deadlines |
| `run_r_worker_options` | object | `{}` | Named arguments for `callr::r_session_options()` when `run_r_mode` is `"worker"`: `env`, `libpath`, `cmdargs`, `arch`, ... |
| `job_worker_options` | object | `{}` | Named arguments for `callr::r_session_options()` for doers, reviewers and monitors |

Job workers, supervised `run_r` workers and spawned subagents skip the
working directory's `.Rprofile` by default. This includes any options that
file sets, such as `options(corteza.policy = ...)`. To load the profile for
a job or supervised `run_r` worker, set `user_profile` to `"project"` in
that worker's options. Spawned subagents, archive summaries (foreground
and background) and room consultations always skip the project profile.

A project using `renv` can instead supply `job_worker_options.libpath` or
`run_r_worker_options.libpath`. This list replaces the worker's library
search path. Put the project library first, followed by the libraries
containing corteza and its dependencies. The initial R process and the
approved `run_r_script` tool follow their ordinary startup rules.

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

A turn that is asked to stop (an ACP client's cancel, below) interrupts the
worker, as its deadline does, and the worker keeps its R objects. A worker that
does not come back within 5 seconds of the interrupt, because its code is in C
that never checks for one or catches the interrupt and goes on, is ended: all
of the session's R objects are lost, the call's result says so, and the next
`run_r` starts a new worker. Files the code wrote stay.

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

### Subagents

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `subagents.enabled` | boolean | `true` | Enable subagent commands |
| `subagents.max_concurrent` | integer | `3` | Max parallel subagents |
| `subagents.timeout_minutes` | integer | `30` | A query after this deadline ends the child. `/agents` shows the deadline. |
| `subagents.allow_nested` | boolean | `false` | Allow nested subagents |
| `subagents.default_tools` | string[] | `["read_file", "skill_instructions", "grep_files", "r_help", "web_search", "fetch_url"]` | Tools available to subagents when no preset is given |

**Presets.** `/spawn --preset <name>` picks a fixed tool list at spawn time:

- `investigate` (default): `read_file`, `grep_files`, `r_help`, `web_search`, `fetch_url` (read/search/help, plus network reads).
- `minimal`: `read_file`, `grep_files`.
- `work`: investigate + `bash`, `write_file`, `replace_in_file`, `list_files`, `git_status`, `git_diff`, `git_log`, `run_r`.

`/spawn --tools <comma-list>` overrides the preset with an explicit tool list.

**Permission model.** Subagents have no interactive approval channel back to the parent or user. A child's tool list is fixed at spawn time — via the `--preset` or `--tools` flags on `/spawn`, or the `preset` / `tools` arguments to `subagent_spawn()`. There is no mid-run path to escalate to a riskier capability: the child's approval callback denies by default, and the parent cannot grant new tools after the fact. If a task may need shell, write, or network capability, choose a preset or explicit tool list that includes it at spawn time; otherwise the child should report that it is blocked.

### Recovering old R workspace snapshots

Versions through 0.8.0 saved a list of R objects in `<session>_workspace.rds`
under the agent's sessions directory. Use `readRDS(path)` to recover that list.
New sessions keep live `run_r` bindings in their evaluator. Conversation
history and delegated job checkpoints have their own storage.

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
  provider = "ollama",
  operators = "@you:example.com",
  project_roots = "~/projects"
)
```

#### Who a bot answers to

`operators` names the people a bot answers to, each by a full Matrix id.

- The bot accepts an invite only from an operator.
- In a room with one person in it, the bot answers only when that person is
  an operator.
- Operators answer its approval prompts, and commands such as
  `/approve-all`, `/cd` and `/ls` are theirs.

A bot with no operators accepts no invites. It works in the room it was
configured with and in rooms it has already joined. Where such a room has
one person in it, that person is answered and answers the approval prompts.
Where it has more, the bot answers when it is mentioned and nobody can
approve, so a call that needs approval is declined. Each invite the bot
refuses is reported in its log, with what would change that.

Up to 0.8.0 a bot with no operators accepted every invite, which gave a
session with tools to anyone who could invite it. A bot that should go on
accepting invites needs `operators`: pass it to `bot_configure()`, which
writes the whole config again, or add `"operators": ["@you:example.com"]`
to the bot config.

#### Where a room works

A room works in the first of these that applies:

1. The directory an operator bound it to with `/cd <dir>`. It must exist and
   lie inside one of the bot's project roots, links resolved.
2. The bot's `cwd`. `bot_configure(cwd = )` saves one. `bot_run(cwd = )`,
   `bot_run_init(cwd = )` and `bot_poll(cwd = )` give one for a run, and it
   wins over the saved one.
3. A scratch folder under the session's temporary directory, deleted when the
   process ends. The room is told it has no project directory, and it
   reads, searches and talks only. It writes no files, runs no commands,
   starts no subagent and hands no work to a doer or another room, and
   no room hands work to it. A refusal says how to give the room a
   directory. Room lists show it as having none, and it is named by its
   id or name, never by the scratch folder. A room an operator bound with
   `/cd none` works here, as does one whose bound directory is gone or lies
   outside every root now. So does a room whose state file the bot could
   not read: the file is copied aside, and the room stays here, across
   restarts, until an operator runs `/cd`, since what was lost may have
   been `/cd none`.

The project roots are `project_roots`, existing directories inside which a
room may be bound. `bot_configure(project_roots = )` saves them, and
`bot_run()`, `bot_run_init()` and `bot_poll()` take them for a run. With
none, `/cd <dir>` is refused.

```r
corteza::bot_configure(server, user, password, room,
                       project_roots = c("~/projects", "~/src"))
```

In a room, for an operator of the bot:

- `/cd` says where the room works, the 3 forms and the roots.
- `/cd <dir>` binds the room to `<dir>`. A bare name is looked for under each
  root.
- `/cd none` binds it to no directory, and `/cd default` returns it to the
  bot's `cwd`.
- `/ls` lists the roots, and `/ls <dir>` the directories directly inside one,
  names only, at most 50.

A move ends every conversation the room holds, the main timeline's and each
thread's, as `/clear` does: each goes to the host's archive, and the next
starts in the new directory with a new prompt, tools and doer. A thread first
seen later starts there too, and a restart seeds no conversation with
messages from before the move. A move is refused while any of the room's
conversations has a job open, and the refusal names it. The binding is kept
in the room's state file and outlives a restart. A room the `on_clear` hook
files a conversation into takes the binding of the room it came from.

Up to 0.8.0 a room worked in the directory its topic started with, written
`<path> | <description>`. Anyone with the power level can change a topic, and
that directory became the checkout where the supervisor let writes through
without asking a person. A topic names no directory now. A room whose topic
still starts with a path is told once that the path is not read, and which
`/cd` would bind it.

corteza creates none of these directories. A `cwd` or root that does not
exist is an error where it is given, and a run stops at its start when the
saved `cwd` has been removed. Up to 0.8.0 the last step was `~/<bot name>`,
created on first use. A bot that relied on that directory now needs `cwd`:

```r
corteza::bot_run(cwd = "~/mybot")
```

`voice_serve()` sessions resolve their directory the same way. In a process
of their own they use the saved `cwd`.

### Legacy memory

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `memory_flush_enabled` | boolean | `false` | Run the memory flush ahead of an automatic compaction in the terminal. `/flush` runs it by hand either way |
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
| `/trust [yes]` | Show whether this directory is trusted, or trust it |
| `/clear` | Clear conversation (keeps session) |
| `/compact` | Summarize conversation to free context |
| `/sessions [all]` | List this directory's sessions, or every directory's |
| `/context` | Live context usage and loaded files |
| `/tasks [clear]` | Show or clear the task list. With the list off (`task_list_enabled`), says how to turn it on |
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
The provider validates which levels its model accepts. In a talker session
`/model` and `/effort` change the doer's model and effort, which the next job
runs with ([Talker mode](#talker-mode)). These overrides are not global
configuration changes; use `reasoning_effort` (or `talker.reasoning_effort`
for the talker) for a startup default.

GPT-6 tool calls (including Astra and Sol) require OpenAI's Responses API.
The corresponding llm.api routing update selects Responses with web search
off as well as on. Older OpenAI models keep their Chat Completions route
when search is off. `openai_codex` uses Responses and supports CLM on native
tool batches and text replies. The separate API-key `openai` route uses
standard compaction when it selects Responses.

## Instruction skills

At session startup, corteza looks for files named exactly `SKILL.md` under
each instruction root. It skips subdirectories whose names start with a dot,
such as `.archive/`. An explicitly configured root can itself be hidden.
The built-in roots are listed below.

| Scope | Path |
|-------|------|
| Global | `tools::R_user_dir("corteza", "data")/skills/` |
| Project | `.corteza/skills/` |

The project root, and the roots a project config adds, exist in a
[trusted](#trusted-projects) directory only.

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

## Editors and other ACP clients

`corteza acp` runs corteza as an agent of the Agent Client Protocol (ACP, version 1): an editor, or any other client that speaks it, starts the command and talks to it over standard input and output. It runs on Linux and macOS in this release; on Windows it says so and exits. Each session the client opens is a corteza session in the directory the client names, set up as `chat()` sets one up, with that directory's config, trust, tools and instructions. The directory must be an absolute path that exists; corteza creates none.

```sh
corteza acp
corteza acp --provider openai --tools core,git
```

A client needs the command's full path: the one `install_cli()` reports ("Installed corteza to: ...") and returns. The command reads `~/.Renviron` as the terminal does, so the provider's key need not be in the editor's environment. In Zed, for example, a custom agent goes under `agent_servers` in `settings.json` (Zed's page "External Agents", section "Custom Agents"):

```json
{
  "agent_servers": {
    "corteza": {
      "type": "custom",
      "command": "/home/you/.local/share/R/corteza/bin/corteza",
      "args": ["acp"],
      "env": {}
    }
  }
}
```

What a client gets:

- `session/new`, `session/resume`, `session/list` and `session/close`. ACP sessions are kept with the terminal's (Session records below), so `corteza --resume` takes up a conversation a client started in that directory, and a client can resume a terminal's. A resume sends the model the conversation's history and the client none of it.
- Prompts of text, and links to files, which reach the model as the file's path. A prompt with an image, audio or an embedded resource is refused.
- The reply as it streams, with the secrets corteza knows hidden, and each tool call: announced, run, and finished with its result (cut to 4,000 characters for the client; the model reads all of it) and, for an edit, its diff.
- Modes, which set the session's `approval_mode`. In `ask`, each call that would prompt at the terminal becomes a permission request with the terminal's 3 answers: allow, allow always for this project (kept where the terminal keeps it), deny. In `review`, the reviewer rules first, as under `approval_mode: "review"`, and the client is asked what it passes on. A config whose `approval_mode` is `"allow"` or `"deny"` adds that mode, and its sessions start in it. The credential rule's request offers no "always", and no saved answer covers it.
- Commands: `/help`, `/trust [yes]`, `/status` (or `/context`), `/tools`, `/tasks [clear]`, `/spent` and `/compact`, offered to the client after each session opens. A prompt that begins with one runs it in place of a turn, as the terminal does, and what it prints is the reply. Only `/compact` asks a model, for its summary; a cancel stops it before the request, or leaves the summary it returns uninstalled, and the prompt ends `cancelled`. A `/trust yes` applies to the sessions opened after it. `/clear` and `/cd` answer that a client starts a new session to begin again or to move; any other word after a slash is text for the model.
- `session/cancel`. The turn stops at the next piece of the streamed reply, before the next model request or tool call, during a wait for permission or for the reviewer, or while a long tool runs: `bash` and `run_r_script` end their process and its children, and `run_r` interrupts its worker. A worker that does not come back within 5 seconds is ended, and the session's R objects go with it (`run_r_mode`, under JSON config keys, Skills, above); the call's result says so. Files written stay. Other tools finish first, within their own time limits.

corteza connects to no MCP server a client names. A `session/new` or `session/resume` that lists one is refused, naming each server by its `name` alone; send an empty `mcpServers` list.

One process can hold several sessions, and they share no R objects: each session's `run_r` runs in a worker process of its own, whatever `run_r_mode` says, and a session whose config asks for `"in_process"` is told so when it opens. What corteza's own process holds is still shared: the options it sets and the packages a skill loads in it, which trusted R code can change for every session. One prompt runs at a time. A prompt for another session waits until the running one ends, and a second prompt for the running session is refused. The requests that need no turn (opening, listing, closing, setting a mode) are answered while a prompt runs, each time the turn checks for a cancel. A close of the running session cancels its prompt and is answered after it.

Nothing corteza or a tool prints reaches the protocol: while the command runs, R's standard output goes to standard error.

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
| `history` | list or null | null | Prior messages; resumes mid-conversation. Thinking blocks are removed |
| `model_map` | list or null | null | `list(cloud = ..., local = ...)`; falls back to `corteza.model_map` option |
| `provider` | string | `"anthropic"` | LLM provider |
| `tools_filter` | string[] or null | null | Restrict available tools |
| `system` | string or null | null | System prompt override |
| `approval_cb` | function or null | null | Called when policy returns `"ask"`. Default denies. |
| `max_turns` | integer | `10` | Max tool-use turns per call |
| `verbose` | boolean | `FALSE` | Print tool-call progress |

## Session records

Each agent keeps one index of its conversations, a JSON object keyed by
session key:

```
tools::R_user_dir("corteza", "data")/agents/<agent>/sessions/sessions.json
```

The terminal and `chat()` write under agent `main`, a Matrix bot under agent
`matrix`. Another program may read these files. Fields are added in later
versions and never renamed, and a reader should skip an entry or a field it
does not know. The file is replaced whole on every write, so it is always
complete when read.

A terminal or console session, keyed `corteza:<id>` or by the key given with
`--session`:

| Field | Meaning |
|-------|---------|
| `sessionId` | Names the session's files in the same folder: `<id>.jsonl` (the transcript), `<id>_trace.jsonl`, `<id>.journal/` |
| `cwd` | The directory the session was started in, resolved, with forward slashes |
| `title` | The first prompt on one line, cut to 100 characters |
| `updatedAt` | When the entry was last written, in milliseconds since 1970 |
| `surface` | `cli` or `console` |
| `sessionFile`, `createdAt`, `modelProvider`, `model` | The transcript's path, the creation time, and the provider and model last used |

Entries keyed `agent:<id>:subagent:<uuid>` in the same file belong to
subagents and are not conversations a person started.

Sessions made before 0.8.1 have no `cwd` and no `title` in the index. corteza
records a session's directory, from the first line of its transcript, the
first time its sessions are listed, and its title the first time the session
is shown in a list. `surface` is absent on those. A session that never got a
prompt has no `title`.

A room or a thread a Matrix bot answers in:

| Field | Meaning |
|-------|---------|
| `sessionId` | The entry's key. The session's journal is `<sessionId>.journal/` in the same folder |
| `surface` | `matrix` |
| `bot` | The bot's Matrix user id. 2 bots in 1 room have 1 entry each |
| `room` | The room id |
| `thread` | The thread's root event id, when the conversation is a thread |
| `voice` | `true` for the room's voice session |
| `cwd` | The directory the room works in. Absent for a room with no directory |
| `title` | The room's name |
| `journal` | The journal folder's path |
| `updatedAt` | When the bot last built the session: at its first message after a start or a `/clear` |

A room has no transcript. How recently it was used is the modification time
of `live.json` in its journal folder, a file that is not there before the
session's first turn.

A bot also records which conversation each of a room's sessions holds, in one
file for each room, named by the `sessionId` of the room's own entry:

```
<state>/rooms/<sessionId>.json
```

`<state>` is the folder `CORTEZA_STATE_DIR` names, else `state` in the data
directory. Retention leaves the file alone.

| Field | Meaning |
|-------|---------|
| `room` | The room id |
| `main` | The room's own conversation |
| `threads` | Each thread's conversation, by the thread's root event id |

Each conversation:

| Field | Meaning |
|-------|---------|
| `id` | The conversation's id. A job asked in it carries the same id |
| `since` | The event id of the `/clear` that started it. Absent for a conversation the bot started with the session |
| `at` | When that `/clear` was sent, in seconds since 1970 |
| `ack` | The event ids of the bot's answer to that `/clear` |

A restart seeds each conversation with the room's messages after `since`, and
leaves `ack` out.

## Talker mode

A bot's room answers through a talker: a session with read-only tools that
hands work needing edits, commands or tests to a doer, a worker process of the
conversation's own, and goes on answering while the doer works. The result
arrives as its own message. From 0.8.1 this is the default in a bot's rooms.
The CLI and `chat()` run tools directly by default, in the session's own R
process. To enable talker/doer mode there, set:

```json
"talker": {"enabled": true}
```

To turn it off, put this in the config, the bot's or your own:

```json
"talker": {"enabled": false}
```

A room is no talker by default in 2 cases. Where the bot's tools are
restricted (`tools_filter`, from `bot_configure()` or the run), since a doer
gets the work tools whatever that list says. And where the room has no
directory, since such a room hands no work on. `"enabled": true` explicitly
turns it on in any of these; the doer receives its work tools even when the
talker's own tool list is restricted.

At the terminal and in `chat()`, a turn that hands work to the doer stays
with it: the doer's steps, an approval it asks for and its result print as
they come, and the prompt returns when the job ends. Ctrl+C returns the prompt
and leaves the job running; `/jobs` lists the jobs and stays with an open one
again. A terminal driven from a room with `/remote-control` does not stay, so
the room's next message is read as before.

The talker runs on the bot's own model, as the doer does. Up to 0.8.0 it ran
on a fast model of its provider's (Claude Haiku 4.5 for `anthropic` and
`anthropic_claude`, GPT-6 Luna for `openai` and `openai_codex`) and stopped
with an error on any other provider. To give the talker a faster model, name
it:

```json
"talker": {"model": "claude-haiku-4-5-20251001"}
```

`talker.provider` puts the talker on another provider, on that provider's
default model unless `talker.model` names one. On a local model the talker
and the doer share it, and 2 requests can be in flight at once; name another
model for the talker, or turn talker mode off, where the machine cannot serve
both.

The bot's reasoning settings (`reasoning_effort`, `thinking_budget_tokens`,
`thinking`) go to the doer with its model. The talker uses the provider's
defaults unless the `talker` block names its own (`talker.reasoning_effort`
and the others), since a model it names may refuse settings made for another.

In a talker session `/model` and `/effort` change the work: the doer's model
and effort, which the next job runs with. The doer starts on the model and
provider the config's `jobs` block names, if it names them, with that block's
reasoning settings; the talker stays on the session's model, and the 2
commands change the doer's from there. Within a provider, a separately
configured talker keeps its model. Switching provider moves both roles to
that provider's pair, and returning restores the previous pair. An unseen
provider uses the selected model for both roles unless `model_pairs` names
a pair. `/status` names both models. Up to 0.8.0
both commands changed the talker's alone. Column `talker` of the command
table (`corteza:::corteza_commands()`) says what each command acts on: the
talker's conversation, the doer and its jobs, or both.

Optional provider pairs use model names or objects with `model` and the
role's reasoning settings. For example:

```json
"model_pairs": {
  "anthropic_claude": {
    "talker": "claude-sonnet-4-6",
    "doer": "claude-opus-4-7"
  },
  "openai_codex": {
    "talker": {"model": "gpt-5.6-sol", "reasoning_effort": "low"},
    "doer": {"model": "gpt-5.6-sol", "reasoning_effort": "high"}
  }
}
```

The `/model` menu includes configured fallback routes and pairs. Automatic
quota fallback selects the pair too: a job delegated by a fallback talker
uses that provider's doer. The preferred provider remains unchanged for its
next retry after cooldown. Already running jobs keep their own model.

At the terminal, `/provider` switches the pair. `/max-tokens` is the doer's:
the output budget is the next job's,
while the talker's replies keep theirs. A conversation you resume
(`--resume`) starts its doer as a new conversation does: where the `jobs`
block names a model, the doer runs on it again, whatever `/model` chose
before. `/review` reviews with the doer's model, and `/doctor` and
`/config` name it. `/spent` counts each job with the talker's replies, in
the conversation that asked for it, and a job's result goes into that
conversation's transcript, so `--resume` brings it back. `/r` runs R in
the terminal's own process and sends its output with your next message;
the doer's R objects stay in the doer. An `/auto` run works with the
session's own tools, which in a talker session only read and hand work on,
so there `/auto` says to turn talker mode off and starts nothing.

Without talker mode, `run_r` in a bot runs in the bot's own process, which all
its rooms share, so one room's R objects are the others'. A talker's work runs
in its conversation's doer, a process of its own, and a room's objects stay
its own.

## What a delegated job obeys

A delegated job runs with the model, provider, reasoning settings and limits
its conversation has when the job starts, in a worker that may have run
earlier jobs. While it runs it follows the conversation's `/plan`,
`/permissions` and `/dryrun`: the worker checks them before each call, and
again just before the call runs, so a `/plan` typed while a call waits for
your approval stops that call. A worker cannot end plan mode; you do. After
`/clear`, a job started before it keeps the settings its conversation last
had, and `/cancel` stops it.

A job stopped mid-run keeps what it wrote to files. The R objects and history
its worker made since its last checkpoint are gone. The job's report names
that checkpoint, which the next job starts from.

At the terminal, `/clear` starts a new conversation for the worker too. Its R
objects stay, as they do in the terminal's own session, and its history does
not: the next job starts from an empty conversation, with the instructions and
tools a session started now would have, so a `/trust` before the `/clear`
reaches it. A job still running at `/clear` finishes, and its result is shown
and saved to the conversation that asked for it. Each conversation keeps its
own checkpoint, so `corteza --resume <id>` brings back that conversation's
worker as its last job left it, and a new conversation's objects are a copy
taken at the `/clear`.

In a room or a thread, `/clear` does the same for that conversation's worker:
the R objects stay and the next job starts from an empty conversation. A job
still running finishes and posts its result, marked as asked before the
`/clear`. After a restart the bot seeds each conversation, the room's and each
thread's, with nothing from before its own `/clear`.

A checkpoint saved before 0.8.1 does not say which conversation it belongs to,
so the first job after upgrading keeps the worker's R objects and starts its
history over.

## Incomplete worker jobs

A worker that exhausts its tool-turn or output budget is recorded as
`incomplete`, with its stop reason and usage, rather than `done`. Returned
cancellation remains `cancelled`; exceptions remain `failed`. A model quoting
a stop marker does not change the job's status. Room and CLI notifications
show the same outcome and retain the partial reply.

An incomplete attempt releases its checkout lock and does not queue automatic
review. Its outcome is immutable. It saves a separate private snapshot of its
completed native history and restorable R workspace. Later jobs may overwrite
the warm-worker checkpoint, but cannot replace this per-job resume point.
Failures to save a usable snapshot are reported without advertising resume.

Use `/job resume <id>` in the originating room, worker room, or CLI session,
or explicitly ask a talker to continue that identified job with `job_resume`.
Only the original requester or a configured bot operator may request a resume;
seeing a job alone grants no such right. The worker conversation must be loaded
for a cross-room request. Remote delegation and consultation jobs cannot be
resumed through this local-worker command.
CLI jobs remain owned by their launching process; a new CLI process does not
automatically adopt an exited process's jobs. Bot identities survive restarts.

Resume creates a new linked attempt, acquires the normal checkout lock, and
restores the selected history in a fresh worker. It keeps the original tool
restrictions, per-attempt limits, and review preference; approvals must be
obtained again. The provider and model must match the snapshot. Checkout files
are never restored: the worker is told to inspect newer changes before editing.
Connections, external pointers, and transient tool handles are unavailable;
omitted workspace objects are named in the continuation prompt. Missing or
corrupt snapshots fail before a provider request.

Queuing the requested continuation does not require another default approval.
Explicit per-tool ask/deny settings and user policy still apply to `job_resume`;
the host also enforces requester, operator, conversation and workspace checks.

Each stopped attempt has at most one continuation. Repeating its resume command
returns that continuation's ID, including after restart. If that continuation
also stops incomplete, resume its ID. The ledger retains per-attempt usage and
cumulative usage across the chain. No automatic continuation or new spending
policy is enabled.

## Consulting another room

Set `"room_consultation": true` in a bot's account config to expose
`ask_room(room, question, thread)` when it answers a configured operator.
This asks another conversation owned by the same bot. No shared room, second
bot, Matrix self-mention, or additional listener is needed. The default is off.
Use a room name, ID, or project directory, and optionally a destination thread
root ID. Ambiguous names require an ID. `delegate(room=...)` still hands work
to the destination's doer; `ask_room` asks about its conversation instead.

The destination's current text context, including human messages, assistant
replies, and completed tool text, is captured when the question is submitted.
Its conversational model answers in a separate process while both rooms and
any doer remain available. The snapshot excludes images and hidden reasoning;
it does not retrieve material retired into a vault. Missing context must be
reported as missing. The consultation has no tools, web search, CLM edits, or
ability to start another consultation. Its snapshot and outcome stay in the
durable job ledger. Neither live conversation is replaced with the snapshot.

Questions and answers appear in both rooms and their conversation histories,
including the originating thread. Enabling this setting allows operators to
share information between the bot's joined rooms, whose members may differ.
Non-operators cannot request it. Plan mode and rooms attached to a remote CLI
refuse consultations. `job_status` and `job_cancel` work from either conversation.
Repeated pending questions reuse the same job. Consultations have a 120 second
limit, one active query per destination, and at most 4 active queries per bot.
Turning the setting off cancels pending queries. A restart leaves undispatched
queries queued and marks interrupted queries indeterminate, without retrying
the model call. Ordinary failed-message retries deliver results after an outage.

## Delegation between bots

Conversation sessions can ask another configured bot to run a job using
`delegate_to(bot, task, review)`. This works with split talkers and unsplit bots.
It uses the bot's existing chat client and poll cursor. No extra Matrix login
or listener is started. Configure outgoing destinations in the account config:

```json
"delegates": {
  "helper": {
    "bot": "@helper:example.org",
    "channel": "!project:example.org",
    "transport": "matrix"
  }
}
```

The receiving account must explicitly allow the requester. An entry may narrow
the worker's tools or require a review:

```json
"delegators": [
  {"bot": "@requester:example.org", "tools": ["read_file", "grep_files"], "review": true}
]
```

A string entry allows the worker's usual tools. Both lists default to empty.
The originating conversation must be answering a configured human operator.
The receiving room determines the project directory, doer configuration,
supervision, approvals, and checkout lock. Requests carry a self-contained task
of at most 7000 UTF-8 bytes; tasks are rejected rather than shortened. The
destination and its room members can read the task and reports.

The requester records a remote job, with no local worker or checkout lock.
The worker records an ordinary job, with the same doer/reviewer lifecycle as
local delegation. Duplicate delivery reuses that job. Results are authenticated
against the recorded peer, transport, channel, and job ID; a late result cannot
overwrite a terminal outcome. Results and any later review return to the
originating conversation. Long reports are bounded for the transport; the full
report stays in the worker's ledger. `job_status` and `job_cancel` are available
alongside `delegate_to`, including on an unsplit bot.

Protocol notices start with `[corteza-job v1 ...]` and are consumed before the
conversation dispatcher. They mention their recipient and use threads only
when supported. A bot cannot approve a delegated job's tool calls, including
when it was accidentally listed as an operator. Human approvers can react or
send `approve REQUEST_ID` / `deny REQUEST_ID`; the prompt supplies the ID.
Unverified encrypted senders are rejected. Requests are bounded by hop counts
and active root IDs, and a room attached to a remote CLI refuses new jobs.

Unreceived requests expire after 2 minutes. Dispatched requests without an
acknowledgment are probed after 2 minutes; accepted jobs are probed every
5 minutes. Restart also triggers a status probe. An unanswered probe becomes
`indeterminate` after 2 minutes; the job is never resubmitted as new work.
Cancellation before receipt is remembered so a delayed request cannot run.
A completed job is retained in the ledger, including across bot restarts.
These settings do not enable automatic recursive delegation by worker models.

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

## Room attachments

Incoming attachments retain a quoted receipt with their filename, declared type
and size, sender, source event, and readability status. The transcript and
journal keep that receipt even when the file cannot be read. In an exchange
the bot is answering, it posts an explicit notice for unsupported, oversized,
or unreadable files. PDFs and other non-image files are acknowledged without
downloading or extracting their contents. Successful images still reach the
conversation model. Delegation with `attach_context = true` carries the text
and receipt, and explicitly says that image bytes are absent from that text.

Room conversations offer `send_room_file(path)` to configured bot operators.
It sends an existing file to the current room using the host's chat identity;
the model does not load credentials or select another destination. Tool policy
and supervision still apply: it is a write, so each send asks, and goes to the
monitor where `auto_approve_asks` is on. `"permissions": {"send_room_file":
"allow"}` in your config lets it send unasked. Credential files and runtime
configuration files are refused, and the file limit is 25 MiB. Workers do not
receive this tool.

Encrypted Matrix attachments require chat.api 0.1.0.4, mx.client 0.2.1.4,
and mx.crypto 0.2.2.2. Uploads contain ciphertext; the file metadata travels
inside the encrypted room event. Older adapters report that attachment sending
is unavailable. File sends from threads are refused until the transport's
file contract can preserve their destination.

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
