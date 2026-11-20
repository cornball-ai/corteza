# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Corteza is an R-native AI agent runtime: a CLI binary
(`inst/bin/corteza`, copied into place by `install_cli()`) plus an in-R
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

**Tool registry** (`R/registry.R`): `.skill_registry` holds corteza's
own tools and any a host registers by hand. `ensure_skills()` /
`register_builtin_skills()` populate it on first use.

**A session's tools** (`R/tool-table.R`): a session takes its tools
once, when it starts, into `session$tool_table`
(`session_tool_table()`): the registry's, the user's skills, the
config's package tools and a trusted project's skills. A later source
never takes a name an earlier one holds; it is skipped with a message.
`turn()` offers the table, runs the table's handler, and puts the
entry's `class` and `origin` on each call, which `call_class()` and
`call_named_by_corteza()` read. Every surface builds one:
`session_setup()` (`chat()`, rooms), the terminal, a child's
`subagent_turn_init()` and `serve()`. A session made by `new_session()`
alone reads the registry. So a bot's rooms, which share a process, each
see their own project's tools. `skills_as_api_tools()` converts to the
`llm.api::agent()` format; `sanitize_tool_name()` maps `::` to `__`
for API compatibility.

**System prompt assembly** (`R/context.R`): `load_context_bundle()`
builds source descriptors (preamble, runtime guidance,
`saber::briefing()`, workspace USER.md/SOUL.md, `config$context_files`,
the instruction catalog, harness lessons, live subagents) and hands
them to `saber::context_manifest()`, which also discovers the shared
instructions file and the project AGENTS.md/CLAUDE.md. saber drops a
source that resolves to the same canonical path as an earlier one,
then one with identical content; `saber::context_render()` produces
the prompt. In an untrusted directory (next section) the manifest is
built with `discover = FALSE`: the shared file goes in as an explicit
source (`context_shared_source()`) and nothing of the project's does.

## Project trust (`R/trust.R`)

A project's own files arrive with the repository, so a session loads
them only where `project_trusted(cwd)` holds: `cwd` is, or lies inside,
a directory under `trusted_projects` in the user's config or in
`trusted-projects.json` beside it (which `/trust yes` writes). Paths are
compared resolved, and `cwd` has to exist. The scratch folder of a room
with no directory is never trusted.

What that gates, each through one function in `R/trust.R`:

- `.corteza/config.json`: `project_config()`. `load_config()`,
  `instruction_session_config()`, `auto_envelope_config()` and
  `context_config_origin()` all read through it.
- `.corteza/skills/`: `load_project_skills()` (sourced R files and
  skill docs), and the catalog's `project` root.
- `.corteza/harness.json`: `harness_path("project")` stops for an
  untrusted directory; `harness_scopes()` says which stores exist.
- `AGENTS.md`/`CLAUDE.md`, the briefing, and the user's `context_files`
  that point into the project: `load_context_bundle()` and
  `project_context_files()`.

`.corteza/approvals.json`, where 0.8.0 kept the terminal's "Allow
always for this project", is not read at all, trusted or not. Those
answers are the user's, kept in the config directory by directory
(`R/approvals.R`); `project_approvals_path()` serves only to say the old
file is there.

A new reader of anything under `.corteza/` builds its path with one of
the 4 `project_*_path()` builders and decides with `project_trusted()`
first. `test_trust.R` walks the installed namespace and fails on a
function that names the folder outside those builders and the rules.

Trust is fixed when a session starts (`session$project_trusted`); a
`/trust yes` applies to the next one. The terminal and `chat()` ask at
start (`trust_at_start()`); a room is told once (`bot_trust_notice()`)
and only an operator may answer. Trust is not a sandbox: tools still
read files in an untrusted directory as tool results.

## What a session sends (`R/request-prefix.R`)

A session's system text and tool list are fixed (WP14, decision S1):
`session_request_prefix()` composes them once and caches them on the
session, keyed by a digest of what they are made from
(`.prefix_inputs()`: the system text, the filter, the table, the
channel, the talker, whether the session can be in plan mode, the room
tools it holds). A change to any of those composes it again and logs
`request_prefix_reset` with the parts that changed. The availability
tests run once for a table and a filter (`.prefix_base_tools()`).
`turn()` adds only the context-edit tools (the stated exception) and
the web-search drop per request.

Nothing edits the list by mode. A mode or a task change reaches the
model as a note at the head of the next prompt (`.prefix_notes()`,
compared with `session$.prefix_seen`, which `turn()` sets once the
prompt is mirrored). `history_replace()` clears `.prefix_seen` for any
rewrite not in `.prefix_keeps_notes`, so a compaction restates the
session. Permission is checked at the call: `policy()` for plan mode;
the handler refuses `exit_plan_mode` outside plan mode and what an
`/auto` run may not call (`.auto_tool_allowed()`); a job's limit is
`job_allowed_tools`. `exit_plan_mode` is listed where a session can be
in plan mode (`.prefix_plan_capable()`: the terminal and `chat()` with
no tool filter, or any session once it has been in plan mode), never
for a worker or a monitor. A job that may not
search is a deliberate reset (`.prefix_search_reset()`), before the run
and after it, ahead of the checkpoint.

Anything that shows or sends the session's request outside `turn()`
(`/context`, `/status`, `/tools`, `/doctor`, the meter, a compaction, a
subagent's token count) reads `session_request_parts()`, never
`skills_as_api_tools()`. The terminal's `/clear` builds the next
session again (`.repl_session_rebuild()`): config, table, system text
and trust, keeping the keys a flag or command set (`ctx$config_set`).

## Policy engine (`R/policy.R`)

Every tool call passes through `policy(call, config)` before dispatch.
Returns `list(model, approval, reason)` where `approval` is
`"allow"`, `"ask"`, or `"deny"`. Nothing routes on `model` yet.

Every tool has a class, declared where it is registered: `class =` in
`skill_spec()` and `register_skill_from_fn()`, `class` in a
`skill_packages` entry. `read`, `write`, `exec`, `control` (acts on the
session's own work: subagents it starts, tasks, `exit_plan_mode`) or
`continue` (runs work in a child, a worker or a room whose mode was set
elsewhere: `query_subagent`, `delegate`, `job_resume`). A tool that
names none is `unknown`. A call carries the class of the entry it runs
(`call_class()`); `classify_op()` reads a name's class from the
session's table, or the registry without one; a room host's tools
(`send_room_file` and 3 more) are in `.session_tool_classes`. A new built-in tool needs a class and a row in
`test_tool_classes.R`'s table, or that file fails.

**Order (`policy()`); nothing later lifts an earlier deny:**
1. The mode. Plan mode refuses `write`, `exec`, `continue` and
   `unknown` (`.mode_verdicts`, `check_plan_mode()`). A Matrix room
   with no directory (`session$unbound`, set where `bot_new_session()`
   gives it the scratch folder) refuses all but `read` and the control
   tools `.unbound_control` names (`check_unbound_room()`); a new
   control tool is refused there until it is named.
   `tool_delegate()` and `bot_job_handoff()` refuse such a room as well,
   asking or asked.
2. The credential rule. A path under `~/.ssh`, `~/.aws/credentials`
   and the rest asks, marked `rule = "credential"`. No saved "always"
   answers it: both approval callbacks ask each time and offer none.
3. User policy function via `options(corteza.policy = function(call) ...)`.
   Final when it answers.
4. The table: `read`, `control`, `continue` allow; `write`, `exec`,
   `unknown` ask. Personal data (`"personal"`, sticky within a turn)
   refuses a write or a command on the `matrix` channel. A personal
   read is allowed.
5. The config overlay (`.apply_config_overlay()`): a `permissions`
   entry replaces the verdict for its tool; for a tool without one,
   `dangerous_tools` makes an allow ask and `approval_mode` says what
   "ask" becomes. Nothing allows a tool nobody named.

`resolve_paths()` reads a built-in tool's paths by argument name. For a
tool corteza did not write (a package function, a skill) every string
argument counts as a path, for the credential rule, the data class and
the supervisor's rules. `serve()` calls no `policy()`: an MCP client
asks its own user.

## Supervision of unattended calls (`R/supervisor.R`)

A Matrix room session and the job workers it starts carry a gate on
`session$auto_gate` (`supervisor_gate()`), which `turn()` consults for
every call that survived `policy()`, "allow" verdicts included. A call
`policy()` denied is refused by turn()'s handler before any gate: the
gate is shown it, for its record (an `/auto` run's decision log), and
its answer is not read. Three steps, in order:

1. **Rules in code** (`supervisor_route()`, `R/exec-scan.R`). They read
   path arguments and the text of shell commands and R code. A hit
   (credentials, a write outside the checkout, a write to a `.corteza`
   folder, R code that names the trusted list, sudo, another machine,
   pushing or publishing, destructive git) goes to a person. No model
   decides what a model may decide.
2. **The monitor** (`R/job-monitor.R`), only where the bot config has
   `auto_approve_asks: true`. A third worker process per session beside
   the doer and reviewer: read-only tools, confined to the checkout,
   started with the shared and project instructions. Approves, refuses
   (the worker is told why and continues), or passes the call on.
3. **A person**, by reaction in the room.

A worker's rules run in the worker's process; its request file says who
should answer (`route`), and the bot's process owns the monitor and the
room (`bot_job_approval()`, `bot_monitor_answer()`). Nothing approves a
call unseen: `auto_approve_asks` chooses between steps 2 and 3 for what
step 1 leaves open. Settings live under `supervisor` in the user's
config only (`supervisor_config()`); a project config cannot set them.

The monitor reads the room's conversation (`R/room-record.R`), never
`session$transcript`, which the archive empties at a flush and at
`/clear`. Each conversation keeps `conversation-<id>.jsonl` in the
room's journal folder, written where the transcript queue is fed
(`bot_transcript_add()`; the backfill writes its own through
`bot_record_backfill()`): the sender from the event, the text as sent,
the bot's note (an attachment's receipt) apart, and the kind (`message`,
`request` for a peer's, `post` for the bot's). Its first line says
`conversation` or `partial`, and a break is a `gap` line. A restart
(`bot_run_init()`, or the backfill itself) marks its registry
(`bot_record_restart()`, an attribute, since the archive walks every
name in the registry), and the backfill keeps each room's window on
it. A record from before the restart continues only when its room's
window holds its last event; any other gets a gap line when a session
takes it (`bot_record_restart_check()`): the rooms could not be
listed, the room's history could not be read, the backfill failed
partway through the room (`bot_backfill_room()`), or the window does
not reach back far enough, as for a quiet thread.
`bot_record_conversation()` gives the monitor `Person:` lines for the
room's approvers (`bot_approvers()`, membership read fresh), `Other
(<id>):` for anyone else and `Worker:` for posts, or a reason to ask a
person instead: nobody may approve, a partial start, a gap, an
unreadable line, no person's line, or past 100,000 characters. A job is
judged by `origin$conversation` (a hand-off by `origin$from`), read
when its request comes. Each request is a scope of its own
(`review:<id>`), with `earlier` for what came of the calls before it.

`R/monitor.R` is the older hall monitor for `/auto` runs in the CLI and
`chat()`. Same idea, another failure rule: it ends the run where this
one asks a person. `exec_scan()` is not a sandbox. A path built at run
time is invisible to it.

`approval_mode: "review"` (`R/review.R`) gives the terminal and
`chat()` the same 3 steps for the calls that would prompt:
`review_gate()` on `session$auto_gate`, the room's monitor process as
the reviewer, and the prompt for what it passes on. It rules on read,
write and exec calls only, and never on one the person allowed always
(the callback's `answered` attribute). It reads the conversation from
the transcript (`review_conversation()`), never from
`session$history`: a compaction leaves its summary there as a user
message, which would pass for the person's words. Every prompt goes in
whole, each after the whole reply it answers. Each prompt in the
transcript says how it was sent (`staged`, and `typed` when more went
with it: staged output, or an `/auto` run's driver text around its
goal), and the reviewer reads the file with `transcript_load_strict()`,
which counts a line it cannot read. A conversation it cannot read whole
goes to the person: too long, no transcript, a line or message it
cannot read, or a prompt that does not say how it was sent, as none
before 0.8.1 does. Each request is its own scope, so the reviewer keeps
nothing between calls. A wait cut short (an interrupt, an error) cancels the request
(`review_cancel()`), which a room's monitor never does. `review_apply()`
installs it when a loop starts and after `/permissions`, and leaves
another gate (an `/auto` run's) alone. A project's config cannot set
`"review"` (`load_config()`).

A talker's doer is reviewed too. Its worker holds a review gate
(`.job_worker_child_review_gate()`: the same rules, for what would
prompt only), which `.authority_apply()` switches on while the job's
record says `"review"`, so a `/permissions` typed while a job runs
reaches its next call; a room's worker keeps its room's gate. What the
rules leave open comes to the job pump with `route` "monitor", and
`.repl_job_review()` asks the reviewer, judged by the conversation that
asked for the job (`origin$conversation`) and the task the talker
wrote. Before the reviewer, `.repl_job_answered()` asks the session's
callback's `answered` rule, given the rule the request records
(`rule`, written by `job_approval_request()`): a tool allowed always
answers a job's call as it answers the session's own, never the
credential rule's ask, and a request that names no rule is asked. The
job's prompt offers "always" through the callback's `always` attribute
(`.repl_job_always()`), so the terminal saves it for the directory and
`chat()` holds it for the session. A room's jobs take none of these.

## Talker mode (`R/talker.R`)

A talker session has read-only tools and `delegate`; work goes to a
doer, a worker process of the conversation's own (`R/job-worker.R`).
`talker_config(cfg, default)` reads `talker.enabled`, and `default`
decides without it. `bot_talker_config()` gives a bot's rooms TRUE
unless the bot's tools are restricted (`tools_filter`: a doer gets
`SUBAGENT_PRESETS$work` whatever the list says) or the room has no
directory. The terminal's default is TRUE as well (`.repl_talker_default()`:
the CLI sets `ctx$talker_default`, and restricted tools leave it off) and
`chat()`'s FALSE. `talker_enable()`
moves the session's model, provider and reasoning settings to the doer
(`doer_*`) and runs the talker on `talker.model`, else the session's
own model; `talker_follows` says it has no model or provider of its
own. `/model` and `/effort` act on the doer (`.session_work_model()`,
`.set_work_model()`, `.effort_command()` in `R/model-controls.R`), the
talker following `/model` where `talker_follows`. Column `talker` of
`corteza_commands()` says what each room command acts on, and
`test_talker_commands.R` holds the room's to it.

At the terminal and in `chat()`, `run_repl_loop()` stays with a job the
turn handed off (`.repl_attach_jobs()`): it pumps on a short sleep and
prints the worker's steps, an approval question and the result as they
come. Ctrl+C returns the prompt and leaves the job running; `/jobs`
stays with it again. A terminal driven from a room does not stay.
`/provider` and `/max-tokens` are the doer's (`.repl_provider_command()`,
`doer_max_tokens`, carried in the job's spec); `/review`, `/doctor` and
`/config` use the work's model and provider (`.repl_work_route()`); a
settled job's usage joins `/spent` and its result the transcript; a
talker session starts no `/auto` run. Every terminal row of
`corteza_commands()` has a `talker` value, and `test_repl_talker.R`
types each command into a talker session and holds the table to it.

## Where a room works (`R/room-binding.R`)

`bot_room_cwd(cfg, room_id)`: the directory an operator bound the room
to with `/cd`, while it exists inside a root (`bot_project_roots()`:
the run's `project_roots`, else the saved ones, links resolved); the
scratch folder for `/cd none`, a binding that no longer holds, or one
that is unknown; else the bot's `cwd`, else the scratch folder. Only a
room with no binding on record takes the default: a state file that
cannot be read is copied aside and replaced, in one rename, by a
binding of state `unknown` (`bot_room_state_read()`), and a binding
there but malformed reads the same way, until an operator's `/cd`
writes a new one. A topic names no directory:
`bot_parse_topic()` gives a description only, and `bot_topic_path()`
reads a leading path only for the one-time notice. The binding is
`binding` in the room's state file (`R/room-conversation.R`), beside the
conversation records. `bot_cd_command()` archives each of the room's
sessions, then writes the binding and a new record for every timeline
in one write (`bot_room_move()`); `bot_poll()` answers and resets each
session. `moved` in the state gives a thread first seen later the same
start (`bot_conversation_open()`).

## A worker obeys its conversation (`R/job-authority.R`)

A job's worker (a doer or a reviewer) is a process whose session was
built when it started. Each job brings a spec, and the child applies
all of it at the job's start (`.job_worker_child_apply_spec()`):
provider, model, reasoning settings, plan mode, tools, turn limit.
Plan mode, approval mode and dry run can change while the job runs, so
the session that dispatched it writes them to `authority.json` beside
the job: in `job_dispatch()`, and from `job_pump()` when one changes
(`job_authority_sync()`). turn()'s handler reads the record
(`session$authority`, `.authority_apply()`) before each call and again
just before the call runs; a different `seq` the second time refuses
the call. A record that is missing, unreadable, or names another job or
conversation refuses every call. A failed write removes the record, so
the failure refuses rather than leaving a looser record standing.

Only the conversation that dispatched a job writes its record: the
cache on the session (`.job_authority`) names the job and the
conversation (`session_conversation()`), and is not in
`JOB_SESSION_STATE`, so neither the terminal's `/clear` nor a room's
replacement session updates an old job's record. A worker cannot end
plan mode (`plan_mode_locked`): it is not offered `exit_plan_mode`, the
handler refuses it by name, and its plan-mode text and refusals say the
person ends it. The monitor runs no job and has neither.

## A worker follows its conversation (`R/job-conversation.R`)

A conversation's id is `session_conversation()`: the session id at the
terminal, and for a room or a thread the id in its record (below). Each
job's spec and origin carry it. A warm worker holds the conversation it
built its session for (`.job_worker_state$conversation`); a job from
another one rebuilds the session in the same process
(`.job_worker_child_session()`: config, trust, prompt, tools,
callbacks) with an empty history, and leaves the R objects. A
checkpoint records its conversation, and a restore takes the history
only from its own conversation's checkpoint: one from before 0.8.1
names none and gives its R objects only, except to a session with no
conversation of its own.

A surface's `/clear` gives the session its new id and calls
`job_conversation_clear()`, and touches no worker or checkpoint itself.
The terminal keeps a checkpoint for each conversation (`repl:<id>`), so
its new conversation gets a starting checkpoint: the R objects and no
history, written by the idle doer, copied from the old checkpoint
(`job_checkpoint_copy()`) when none is alive, or, with a job in flight,
by `job_pump()` once it settles (`.job_starts`). A job in flight
finishes under the conversation that asked for it, and the terminal
sends its result to that conversation's transcript, not to the new
talker's history. A job that conversation queued and had not started
does the same: just before the conversation ends, the surface calls
`job_conversation_hold()`, which keeps the spec and the authority the
conversation gives each such job (`session$.job_held`, moved with the
jobs by `job_state_move()`), and `job_dispatch()` starts it under them.
A job from an ended conversation with nothing held does not run.

A room or a thread keeps its record in the bot's room-state file
(`R/room-conversation.R`, `<state>/rooms/`, one file per room and bot):
an id, the `/clear` that started the conversation (`since`, `at`) and
the bot's answer to it (`ack`). `bot_get_or_create_session()` takes the
record, or makes one, so a restart goes on with the conversation. The
`/clear` command saves the new record (`bot_conversation_begin()`)
before it answers or discards anything; one it cannot save clears
nothing, and the room is told. The answer's ids are added once it is
sent (`bot_conversation_acked()`). Then `bot_reset_session()` takes the
record and the old session: it holds the old conversation's queued
jobs, clears the old context, moves the jobs and worker to the
replacement (the authority cache stays behind, which freezes an old
job's record) and calls `job_conversation_clear()`. A room keeps
one checkpoint for each session key, so its clear writes none. A result
after a clear is posted marked as asked before it and goes into the new
history, since a restart seeds the timeline from there. The backfill
seeds nothing from before a conversation's start
(`bot_conversation_before()`), each thread by its own record. Retention
never removes the file.

## A turn can be asked to stop (`R/turn-stop.R`)

A session may carry `stop_requested`, a function of no arguments.
`turn()` asks it before each model request (wrapped around the audit's
`request_callback`, at phase `start`), before each tool call (in the
handler, beside `.remote_check_stop()`), again just before the call
runs, after any wait at the gate or the approval prompt (audited as
`stopped`), and on each streamed delta
(`.turn_stop_sink()`). The first three raise `corteza_stop`, classed
`interrupt` so the `error` handlers around tool dispatch and llm.api's
notifier let it through; the sink raises `llm_cancelled`, and `agent()`
returns with the history so far. `turn()` catches both, repairs the
history with `apply_exit_marker()` and returns status `cancelled`.
Ctrl+C and remote control's stop keep their own path. Nothing sets the
function yet; ACP's `session/cancel` will. No turn waits on a doer
today (`delegate` returns at once), and a loop that does must read the
same function and call `job_cancel()`. A cancelled job's reason
(`job_cancel_reason()`) names the checkpoint the next job starts from,
read through the identity in the job's dispatch record, since the
session's may have moved with a `/clear`.

## Voice: the brain and AgentVoice (`R/voice-turn.R`)

`voice_turn()` and `voice_turn_report()` are the voice brain: a turn
streams deltas to a `send` function (TRUE means delivered; anything
else cancels the generation), posts the whole reply to the room, and
records the turn; a report trims the post to the code points that were
heard. One media backend calls them:

- **AgentVoice** (`R/voice.R`, `voice_serve()`): the 1:1 mode for the
  FluffyChat fork over rgrpc. Audio never crosses corteza; the client
  streams PCM to the STT/TTS hosts named in the `AllocateVoice` grant
  from gpu.ctl, and only text and session control reach the brain.

Call mode (the bot as a participant in a LiveKit room, `R/call-*.R`)
is parked on the `livekitr-calls` branch until livekitr is on CRAN. It
needs livekitr and the call surfaces of chat.api and mx.client, none of
which CRAN carries.

## Configuration

Config merges global (`tools::R_user_dir("corteza", "config")/config.json`)
with project (`.corteza/config.json`); project overrides global. The
project file is read in a trusted directory only (Project trust above).
Key fields: `provider`, `port` (default 7850), `context_warn_pct` /
`context_high_pct` / `context_crit_pct` / `context_compact_pct`,
`context_files`, `approval_mode`, `dangerous_tools`, `permissions`.

**When a session compacts** (`R/compact-trigger.R`): at the smaller of
`context_compact_pct` of the model's window and `context_compact_tokens`
(200,000), or ahead of a turn after `context_idle_minutes` (60) with
`context_idle_tokens` (100,000) of context. A session that edits its
own context (`context_edit` with `strategy: "clm"`, which a bot's
talker config can set) is never compacted, so none of these numbers
reach it. `context_trigger()` is the one resolver: the compactor
decides with it and `/context` prints it, so add a display through it
and not through `context_limit_for_model()`.
The windows are a table in `R/context-budget.R`; each row cites the
provider's page, and `context_window` in the config overrides it.
`test_compact_trigger.R` fails when a default model of llm.api has no
row. The summary is written from the newest part of the turns it
replaces, up to `.compact_summary_chars()` characters: 120,000 at a
128,000-token window and the same share of a larger one, so raise the
cap's default only with that budget in mind. After a summary that
failed, and in a recovery from a request refused for its size, a
summary reads 120,000 (`.compact_summary_budget()`).

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

- `run_r` returns what the code writes (`cat`, `print`, messages,
  warnings) followed by the print of a visible final value, like a
  console. Output written before an error is kept ahead of the
  `Error:` line. The supervised worker path calls the same function,
  so both `run_r_mode`s behave alike.
- Large outputs from either tool are captured as handles (e.g.
  `.h_001`) for the agent to reference later; those are read-only
  snapshots, not workspace state.
- Any tool result over the cap (`R/tool-output-cap.R`) is cut to a
  preview and stashed the same way; `read_handle` searches it
  (`op = "grep"`) or pages it (`op = "lines"`). A session with
  `handle_store` set keeps its cut results there, named `.o_NNN`, out of
  reach of `run_r` and of other sessions in the process. Matrix room
  sessions set it (`bot_new_session()`), since a bot's rooms share one
  process. The cut notice names only a tool in the session's
  `tools_filter`, so a role that gets neither `read_handle` nor `run_r`
  is told it cannot open the handle.
- Same isolation rationale as `run_r_script` applies to subagents:
  each `subagent_spawn()` opens a private `callr::r_session` so child
  work can't leak into the parent's R session.

Every other tool runs under `skill_run()`'s `setTimeLimit()`
(`skill_timeout`, 30 s). R checks that limit only between R calls,
never inside `readLines()` or `grep()`, so a limit passed in a long C
call fires at the next R code, wherever that is. A calling handler
outside the tool (`.agent_call()`'s for warnings, a host's
`suppressMessages()`) runs with the tool's `tryCatch` out of reach, and
a limit that fired there ended the turn, not the tool. So
`.held_conditions()` holds the tool's warnings and messages while the
limit is armed and passes them on once it is off, and the limit goes off
as soon as the tool returns or fails. Where warnings are errors
(`options(warn = 2)`), a warning stops the tool where it is raised, as
R stops it, inside the `tryCatch`; a held warning passes on under the
`warn` setting it was raised under. A tool that can run long bounds
itself as well: `grep_files` skips binary files and files over 10 MB and
stops at 500 matches or 20 s (`.grep_limits`).

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

## Session journal (`R/journal.R`)

The openclaw transcript (`<id>.jsonl`) records what people exchanged,
as text; the trace (`<id>_trace.jsonl`) records tool calls cut to a
preview. The journal is the full provider-native record:
`<id>.journal/live.json` is the live history as last mirrored (written
after every `history_callback`), and `segment-NNNN.json` holds the
exact history a rewrite replaced, written before the rewrite so a
failed write blocks it. `history_replace()` (`R/history-replace.R`) is
the one call every rewrite site makes (compaction, context edits,
archival, the provider bridge, restores, a resumed job): it removes the
thinking blocks from the replacement, calls `journal_rewrite()`, then
installs the history. Nothing else replaces `session$history`, and
`test_history_replace.R` fails on an assignment to a history that its
table does not list. A provider accepts a thinking block only in the
conversation that produced it; `turn()` answers a refusal by removing
them all and continuing once. `journal_turn_slice()` returns
the entries a turn added even when a compaction inside the turn rewrote
the live history; do not slice by before/after length. A session with
no `sessionId` plus `journal_agent_id`, and no explicit `journal_dir`,
has no journal, so bare sessions in tests see no change.
`journal_enabled: false` turns it off.

## Tool calls left open (`R/turn-repair.R`)

llm.api mirrors a tool call into `session$history` before the tool
runs, so an error that leaves a turn mid-call leaves the call without
a result, and the provider refuses every later request made from that
history (Anthropic: 400, "tool_use ids were found without tool_result
blocks"). `turn()` closes such calls twice: in its error handler,
before the error goes on, and at the start of each turn, before
anything reads the history, for a process stopped mid-call or a
history from before the fix. `.history_close_open_calls()` puts a
placeholder result right after the message that made each call, where
the provider looks for it (rooms and jobs add messages between turns,
so that is not always the end), and `.turn_close_open_calls()`
installs the result through `history_replace()`. A turn that fails
before anything is mirrored leaves the history alone.

## Retention (`R/retention.R`)

Once a day, from `run_repl_loop()` and `bot_run_step()`,
`retention_tick()` removes the journal and the tool trace of every
session unused for `retention$session_days` (default 90, 0 for off).
The setting is read from the user's config only (`retention_config()`),
and a value it cannot read turns retention off. Transcripts always stay.
`sessions_prune()` is the exported form.

The same run clears the state stores (`R/retention-state.R`): job
chains (jobs linked by `resume_of`, `review_of` or `parent`) once all
ended, worker folders with no checkpoint, and remote-control sessions
that closed or whose process here ended. A chain with an incomplete job
nothing resumed waits `retention$incomplete_days` (180). A worker folder
with a checkpoint (kind `"checkpoint"`) goes once it and the place its
identity names have both gone unused: `repl:<id>` is the terminal's
session `<id>`, any other key a room's or thread's journal under agent
`matrix` (`retention_checkpoint_place()`, by `bot_session_id()`'s
naming, a sidebar's epoch taken off the key). Any worker folder is
held (`retention_worker_held()`) while its worker lives
(`worker.json`, written by `.job_worker_child_mark()`), its session is
in the caller's `keep` (the terminal's ids, a bot's registry through
`bot_retention_keep()`), or a job is open in its workspace or runs
under its identity; the holds and both last uses are read again just
before removal (`retention_checkpoint_moved()`). A folder is
judged only when its name and record are corteza's; anything else stays
and is listed as `"unknown"`. A record is read only where it is a file
of the folder itself (`retention_record()`), never through a link. A
removal takes the record that makes a folder live first (`intent.json`,
`request.json`), and where that record is there but may not go (a
link, a folder), it removes nothing. Checkout lock generations are
never removed (`R/job-lock.R`).

A test that hands retention a clock in the future points
`CORTEZA_STATE_DIR` and `CORTEZA_REMOTE_DIR` into its own temporary
folder, as `test_retention.R` does: run where either names a bot's live
state, it would otherwise remove that.

Two things to keep in mind when changing it or the journal:

- A session's audit records live in its journal folder
  (`<id>.journal/audit/`, `.audit_root()`), and their expiry never runs
  by itself. So a journal is removed file by file
  (`retention_journal_files()`), never as a folder.
- The process that holds a session may outlive its journal. A journal
  whose live snapshot is gone starts again from nothing
  (`.journal_removed()`), and a mark taken on the old one counts as
  before every segment of the new (`.journal_mark_seq()`).

## The archive (`R/archive.R`)

What a room said is filed by the host, through the `on_archive` hook
(the Hooks section of `bot_run_init()`). With no hook nothing is filed.
corteza keeps the books:

- A range the host took is recorded by its Matrix event ids, per room
  (`bot_archive_state_write()`), so a restart's backfill is not offered
  again.
- One it did not take waits as `<session id>.pending.json` beside those
  ids, written before the host is asked, and goes again at the next
  `/clear` or flush. A range from a flush stays open and takes the
  conversation's later entries under the same `id`; one from `/clear`
  is closed and goes on its own. `on_clear` follows only a range the
  host took.

## Session records (`R/session.R`)

`sessions.json` is each agent's index of conversations, and other
programs read it (`vignette("configuration")`, "Session records"): add
fields, never rename one.

- A session belongs to the directory it was started in. `session_list()`
  and `session_latest()` take `cwd` and `all`, and every surface
  (`--resume`, `--list`, `/sessions`, `chat(session = TRUE)`) asks for
  its own directory unless told otherwise. Compare directories through
  `session_cwd()`, never as strings.
- `session_new()` writes `cwd` and `surface`, `session_title_once()`
  writes `title` from the first prompt, and `session_save()` leaves all
  3 alone. An entry from before 0.8.1 gets its `cwd` from the
  transcript's header the first time sessions are listed.
- A Matrix room's session has a journal and no transcript. Its entry is
  under agent `matrix`, keyed by the journal folder's name
  (`bot_session_record()`).
- `store_save()` writes beside the store and renames. Every session of
  an agent shares the file: keep it that way, and go through
  `store_update()` so an unreadable store is set aside, not replaced.
  2 processes can still each read, change and write it, and the later
  write wins.

## Test conventions and gotchas

- `corteza:::env[[key]] <- value` doesn't parse the way you'd expect
  for a package-private environment. Use
  `assign(key, value, envir = corteza:::env)`.
- `llm.api::agent` calls hit the network. Gate any test that calls it
  with both `at_home()` and `nzchar(Sys.getenv("ANTHROPIC_API_KEY"))`.
- A test file that writes under `tools::R_user_dir()` starts with
  `source("isolate.R", local = TRUE)`. R CMD check runs the tests
  through `tests/tinytest.R`, which points those folders at temporary
  ones; `run_test_file()`, `run_test_dir()` and `test_package()` do
  not, and a file they run would write beside a live session's
  records. `tools/check_files_alone.sh` runs each file alone with an
  empty home and lists any that write there, stop or fail.

## CLI vs `chat()` slash-command parity

Both surfaces share one slash-command implementation. `inst/bin/corteza`
(shell binary) and `corteza::chat()` (in-R) each build a `ctx` and hand
off to `run_repl_loop()` (`R/repl.R`), which dispatches every slash
command, so the command set is identical across both. The
`/status /doctor /config /diff /review /last /outputs` commands (once
CLI-only) now work in `chat()` too.

`corteza_commands()` (`R/commands.R`) is the table of every surface's
commands: a row a name, with what it is the same as, its argument, a
line saying what it does, and whether the terminal and `chat()`, a bot's
room, or a room controlling a terminal takes it. Nothing dispatches
from it. `test_commands.R` reads `run_repl_loop()`, `.remote_input()`
and `.remote_bot_route()` for the names they compare with, asks each
room parser, holds its list of parsers to the ones `bot_poll()` calls
(a name matching `bot_is_*_command`, `bot_parse_*` or `*_parse_command`),
and checks both `/help` texts, so a new command needs a row, a line in
both help texts, and its surface's match.

`/remember` and `/recall` no longer exist. `/flush` is the live memory
command (manual memory flush), sharing one `run_memory_flush()`
implementation (`R/chat-slash.R`) across both surfaces via
`run_repl_loop()`. The legacy `memory_store` / `memory_search` tools are
off by default behind `config$legacy_memory_tools_enabled`.

## Project context loading

Project context comes from `saber::briefing()` and
`saber::context_manifest()` (recent commits, package summary, the
shared instructions file, AGENTS.md or CLAUDE.md) plus any files
explicitly listed in `config$context_files`. In an untrusted directory
only the shared file and the user's own context files load. The
`/context` slash command shows live token usage broken into system /
tools / history, whether the directory was trusted, and what was left
alone.

## Matrix credentials rotate: derive, never cache

The Matrix access token in `cfg` is **not stable for the life of a
process**. It rotates whenever `mx.client` relogins, which happens on at
least two paths:

- `chat_poll()` wraps its sync in `mx_with_relogin()`
- `mx_set_displayname()` does the same, so every `/model` or `/clear`
  badge rename is a rotation point

**Rule: anything derived from `cfg` is derived at the point of use, not
cached.** `matrix_mx_session()` is pure field validation over `cfg` — no
network, no state — so deriving it per call is free and caching it only
buys a stale token. `matrix_poll()` does this through local `sess_now()`
and `chat_now()` helpers.

The failure mode is silent, which is what makes it expensive. A request
on a rejected token throws, `chat_send()` does no relogin of its own, and
the surrounding best-effort `tryCatch` converts it to `NULL` or `FALSE`.
The user sees a `/model` switch that never acknowledged, or an approval
prompt the model reads as declined.

Two non-obvious specifics, both learned the hard way (four review rounds
on the model badge, each finding the same defect in a different holder):

1. **`mx_with_relogin()` persists the refreshed client *before* it
   retries.** So a relogin whose retry then fails — rate limit, transient
   5xx — still leaves the live token on disk even though the call threw.
   Never gate adopting a refreshed config on the call succeeding; disk is
   authoritative either way.

2. **`mx_set_displayname()` returns `invisible(TRUE)`, discarding the
   refreshed client** (mx.client#18). Until that is fixed, re-read the
   config after any attempted rename rather than trusting the return.

When reviewing a change in this area, the question is not "did I refresh
this object" but "can this object exist long enough to go stale". Closures
count: `matrix_approval_cb()` closed over its session's `cfg` and a
session outlives many rotations.

## Things to avoid

- **No drive-by cosmetic edits** — don't bundle whitespace or comment
  reformats into substantive PRs.
- **Don't hide real dependencies behind `requireNamespace()`**. If a
  package is needed for core functionality, declare it in `Imports`.

## Repo layout (orientation)

```
R/
├── turn.R             # Shared agent turn (entry point for all surfaces)
├── policy.R           # Tool-call policy engine (allow/ask/deny)
├── trust.R            # Project trust: what loads from the working directory
├── supervisor.R       # Gate for unattended calls: rules, monitor, person
├── exec-scan.R        # Reads shell commands and R code for the rules
│                      #   (shell-parse.R, shell-scan.R, r-code-scan.R)
├── job-monitor.R      # The monitor worker process
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
├── retention.R        # What leaves the data directory, and when
├── archival.R         # Retroactive-extraction runtime (opt-in)
├── log.R              # Structured JSON logging
├── tasks.R            # Task-tracker intercept in turn()
├── permissions.R      # /permissions surface helpers
├── plan-mode.R        # Plan-mode gate helpers
├── workspace.R        # Workspace dir management
├── voice.R            # AgentVoice gRPC service (1:1 voice; voice-alloc.R, voice-auth.R)
├── voice-turn.R       # The voice brain: voice_turn(), voice_turn_report()
└── (many more helpers: chunk.R, handles.R, retrieval.R, etc.)

inst/
├── bin/corteza        # CLI binary (Rscript shebang)
└── tinytest/          # Tests (1300+ asserts)

vignettes/
├── configuration.md
├── retroactive-extraction.md
└── skills.md
```
