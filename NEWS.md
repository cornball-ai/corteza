# corteza 0.8.1

Resubmission after the archival of 0.8.0, batching the 0.8.0.1 to
0.8.0.53 development cycles below.

## The archival

- No function creates a directory in the home filespace. A chat room
  with no directory worked in `~/<bot name>`, which corteza created on
  first use, and a test made one on CRAN's check machine. A room now
  works in a directory its bot's operator names, which must already
  exist, or else in a scratch folder under the session's temporary
  directory (0.8.0.1, 0.8.0.19, 0.8.0.31).
- `install_cli()` with no `path` installs under
  `tools::R_user_dir("corteza", "data")` and no longer writes to
  `~/bin` (0.8.0.2).

## Changed defaults

- A bot's room works in the bot's `cwd`, or where an operator binds it
  with `/cd` inside the bot's `project_roots`. A path in a room's topic
  names no directory. A room with no directory reads and talks only
  (0.8.0.1, 0.8.0.19, 0.8.0.31).
- A bot with no `operators` accepts no invites (0.8.0.5).
- A project's own files load only in a directory you trust:
  `.corteza/config.json`, its skills, `AGENTS.md` or `CLAUDE.md`, its
  lessons and the briefing (0.8.0.6).
- "Allow always for this project" is kept in the user's config, by
  directory. A project's `.corteza/approvals.json` is not read
  (0.8.0.12).
- Every tool has a class. A tool with no class asks, and plan mode
  refuses it. `send_room_file` asks, as a write does (0.8.0.11).
- No package function is registered as a tool unless a config lists it:
  `skill_packages` is empty (0.8.0.15).
- With no `on_archive` hook, nothing of a room's conversation is filed.
  corteza no longer calls `pensar::ingest()` (0.8.0.13).
- Once a day, the journal and tool trace of each session unused for 90
  days are removed, with finished job chains, idle worker folders and
  closed remote-control sessions. Setting `retention.session_days` to 0
  disables idle-session cleanup. Earlier rewrites, audit payloads and
  legacy workspace snapshots each have a separate opt-in lifetime.
  Running turns retain all their entries across history expiry.
- `--resume`, `--list`, `/sessions` and `chat(session = TRUE)` look at
  the sessions of the current directory. `--all` covers every directory
  (0.8.0.4).
- A session compacts at the smaller of `context_compact_pct` of its
  window and 200,000 tokens (`context_compact_tokens`), and before a
  request that follows 60 idle minutes with 100,000 tokens of context
  (0.8.0.8).
- A bot's bound room uses a talker by default with unrestricted tools.
  The terminal and `chat()` run tools directly by default. In a
  talker session `/model`, `/effort`, `/provider` and `/max-tokens` act
  on the doer, and `/auto` needs talker mode off (0.8.0.30, 0.8.0.34).
- At the terminal, `/clear` starts a new conversation for a job's worker
  too: its R objects stay, and its history goes (0.8.0.24).
- A session's system text and tool list stay fixed for the session.
  Plan mode, an `/auto` run and the task list reach the model as notes
  (0.8.0.32).
- The task list is off (`task_list_enabled`), and so is the memory flush
  ahead of an automatic compaction (`memory_flush_enabled`) (0.8.0.42).
- A session on `anthropic` or `anthropic_claude` caches its prompt for 5
  minutes (`cache`), and a turn's token total counts the cached input
  (0.8.0.42).

## New features

- `corteza guide` and `/guide` explain subscriptions, approvals,
  delegation, context and storage. The guide works before sign-in.
- Set `context_window_cap` to use a smaller working window across providers.
  A cap of 400,000 leaves smaller models at their own limit.
- The Codex subscription route supports model-managed history. Approval
  reviewers can continue a verified conversation across decisions.
- `corteza acp` serves an Agent Client Protocol client on standard input
  and output, with cancellation that reaches a running tool and the
  session's commands. Not on Windows (0.8.0.39 to 0.8.0.41).
- `approval_mode: "review"` at the terminal and in `chat()`: a reviewer
  model rules on what would prompt, reading the conversation (0.8.0.14).
- `bot_run_init()` takes the hooks `on_message` and `on_archive`, so a
  host can take a room's messages and file its conversation (0.8.0.10,
  0.8.0.13).
- A picture sent to a room is kept where the session's tools can read it
  (0.8.0.9).
- `session$stop_requested` lets a program stop a turn (0.8.0.26).
- A job's worker follows its conversation's plan mode, approval mode and
  dry run while it runs (0.8.0.21).
- `sessions_prune()` removes what old sessions no longer need, and
  reports it with `dry_run = TRUE` (0.8.0.3).

## Fixes

- A Claude model that thinks by default no longer stops answering after
  a compaction, a context edit, a restart or a task-list change
  (0.8.0.7).
- A turn that ends on an error no longer leaves a tool call without a
  result, which made every later request fail (0.8.0.23).
- A tool's time limit stays inside the tool, and `grep_files` reads
  within bounds (0.8.0.28).
- A call `policy()` refused is refused before any gate (0.8.0.33).
- A job whose worker exits as its reply is read is settled
  `indeterminate`, not `failed` (0.8.0.37).
- On Windows, retention of the state stores removed nothing (0.8.0.38).

## Changes

- No exported function was removed, and 1 was added:
  `sessions_prune()`. Arguments added, each last: `cwd` and
  `project_roots` to `bot_configure()`, `bot_run()`, `bot_run_init()`
  and `bot_poll()`; `operators` to `bot_configure()`; `on_archive` and
  `cfg` to `bot_archive_all()`; `class` to `register_skill_from_fn()`.
  The `matrix_*` aliases take the same.

# corteza 0.8.0.53

- The terminal and `chat()` offer `/guide` at startup. It explains model
  subscriptions, tool approval, delegation, history, caching and storage.
  Run `corteza guide` before choosing a provider or signing in.
- Personal retention settings can expire unpinned audit payloads, older
  committed journal segments and legacy workspace snapshots. Each new age
  defaults to zero. Preview eligible files with `sessions_prune(dry_run = TRUE)`.
- Warm approval reviewers can use a measured token count for a verified
  request prefix. New inputs retain the conservative byte bound.
- Idle subagents report their context usage in `/agents`. The runtime no
  longer scans the global R environment or writes unused workspace copies
  on exit. Old RDS snapshots can be recovered with `readRDS()`.
- Removed the unused experimental context index, heartbeat, rate-limit
  settings and their dedicated tests. The configuration vignette lists
  the supported settings.

# corteza 0.8.0.52

- Foreground and background archive summaries and room consultation workers
  skip the working directory's `.Rprofile` at startup.

# corteza 0.8.0.51

- Subagents spawned without a preset use the read-only tools, including
  `read_file`, `web_search` and `fetch_url`. An explicit tool list still applies.
- Tool calls no longer copy their results or new R bindings into the shared
  workspace. R bindings still persist between calls in the same evaluator.
  This removes the package's `codetools` import.
- Job workers, supervised `run_r` workers and spawned subagents skip the
  working directory's `.Rprofile` by default, including any `corteza.policy`
  option it sets. Job and `run_r` workers can opt in with `user_profile = "project"`.
  For `renv`, their `libpath` must include the project library followed by the
  libraries containing corteza and its dependencies.

# corteza 0.8.0.50

- An approval reviewer reads the full transcript afresh when journaling is off.
  A call too large to review goes to a person, and the next eligible call
  can reuse the verified conversation.
- In `/auto`, a tool argument over 2,000 characters as JSON is refused before
  the monitor is asked. The worker can continue with a smaller call.
- When switching providers, corteza keeps tool output distinct from the
  person's words and assigns fresh context IDs to the converted history.
- Skill discovery skips dot-prefixed subdirectories such as `.archive/`.
  An explicitly configured hidden directory can still serve as a skill root.

# corteza 0.8.0.49

- Codex subscription sessions can edit their native Responses history with CLM.
  A tool call and its result stay together when a session retires them.
- Approval reviewers can keep working notes between decisions in a conversation.
  A warm reviewer accepts up to 100,000 new characters after verifying its
  earlier inputs. An incomplete record or a request over budget goes to a person.
- Anthropic sessions using CLM now use the same 5-minute prompt cache as other
  Anthropic sessions. The 1-hour and off settings still apply.

# corteza 0.8.0.48

- The CLI runs tools directly by default, matching `chat()`. Talker/doer
  mode remains available with `"talker": {"enabled": true}`. Bot-room
  defaults are unchanged.

# corteza 0.8.0.47

- Provider switches and quota fallbacks keep a talker and its doer on
  the same provider, using remembered or configured model pairs. The
  model menu includes configured fallback routes.
- Routes that cannot edit their native history, including Codex's
  Responses format, use standard compaction when CLM is requested.
  Compaction uses the selected route's model and context limits.

# corteza 0.8.0.46

- Leaving plan mode requires the person's approval. Saved approvals,
  permissive settings, and model reviewers cannot answer for them.
  Refusing a plan keeps plan mode on.

# corteza 0.8.0.45

- `r_help` finds a requested package without scanning every installed
  package. An unsupported provider leaves an attached `llm.api` alone
  and asks for an R restart.
- The offline flush example runs for the deprecated alias too. Help
  pages describe return values and live-service requirements, and the
  sample systemd unit calls `bot_run()`.
- Examples and tests use generic host, account and project names.

# corteza 0.8.0.44

- An empty `CORTEZA_REMOTE_DIR` counts as unset, as `CORTEZA_STATE_DIR`
  does. Read as a folder, it put remote control's records at the root
  of the file system, and the package's own tests tried to write there
  after tinytest had left the variable empty.

# corteza 0.8.0.43

- A history's token estimate counts tool calls and their results in
  full. It read a block list for its text alone, so an Anthropic tool
  result counted as one line: a session whose tools returned long text
  compacted far past `context_compact_tokens`, and `/context`
  understated it. A chat-completions call's `tool_calls` count too, and
  so does a plain-text document's text.

# corteza 0.8.0.42

- The task list is off by default: no `task_create` or `task_update`
  tool and no task text in the system prompt. `"task_list_enabled": true`
  in the config turns it on, and `/tasks` says so.
- The memory flush ahead of an automatic compaction in the terminal is
  off by default (`"memory_flush_enabled": true` turns it on). `/flush`
  runs it by hand, as before.
- A session on `anthropic` or `anthropic_claude` caches its prompt for 5
  minutes unless `cache` says otherwise (`"none"` turns it off). A
  session that edits its own context gets no default cache.
- On those providers a turn's token total counts the input read from
  the cache and written to it, so `/spent`, `/auto`'s token cap and
  `subagents.mcp_spend_cap_tokens` count all a session sends.

# corteza 0.8.0.41

- An ACP client is offered the session's commands: `/help`, `/trust`,
  `/status`, `/tools`, `/tasks`, `/spent` and `/compact`. A prompt that
  starts with one runs it in place of a turn, and a cancel reaches
  `/compact`.

# corteza 0.8.0.40

- Over ACP, a cancel reaches a tool while it runs. `bash` and `cmd` end
  the command and its children, and `run_r_script` ends its process.
  `run_r`, which runs in a worker under ACP, interrupts it: the worker
  keeps its R objects, and one that does not answer within 5 seconds is
  ended and loses them. Other tools finish first. Elsewhere a tool runs
  as before.
- `bash` and `cmd` run their command through processx, and
  `run_r_script` through `callr::rscript_process()`. Their output and
  their time limits are as before.

# corteza 0.8.0.39

- `corteza acp` serves an Agent Client Protocol client (protocol
  version 1) on standard input and output: a session for each directory
  the client names, prompts of text and file links, the reply streamed,
  tool calls and their results, the terminal's approval answers, and the
  `ask` and `review` modes. A session keeps the terminal's records, so
  `corteza --resume` and a client's resume take up each other's
  conversations in the same directory. Sessions in one process share no
  R objects: each session's `run_r` runs in a worker of its own. Not on
  Windows.
- The reviewer (`approval_mode: "review"`) is shown everything the agent
  said since the prompt before the call, a job's result included. A
  reply missing from the transcript hands the call to the person.
- The terminal writes a failed turn's error to the transcript, so every
  prompt there has an answer.
- A compaction asked to stop before or during its summary leaves the
  conversation as it was.

# corteza 0.8.0.38

- On Windows, retention of the state stores removed nothing: no job
  chain, worker folder or remote-control session ever went. A path
  there mixes `\` and `/`, and the check that it lies inside the data
  folder compared it as text with one that does not. The check now
  compares with forward slashes, and on Windows in any case.

# corteza 0.8.0.37

- A job whose worker exits as its reply is read is settled
  `indeterminate`, as one whose worker is seen dead first: it may have
  acted before it stopped. It was settled `failed`, as though the job
  had reported an error.

# corteza 0.8.0.36

- A talker's job takes the terminal's "allow always" answers, as the
  session's own calls do, before the reviewer is asked. Never for a
  call the credential rule asks about: the rule now goes with the
  job's request, through the supervisor too.
- The job's prompt offers "always" (`Approve? [y/a(lways)/N]`): the
  terminal saves it for the directory, and `chat()` holds it for the
  session. A room's jobs are answered in the room, as before.

# corteza 0.8.0.35

- Retention removes a worker's folder with a checkpoint once the folder
  and the session its checkpoint belongs to have both gone unused for
  `retention$session_days`: the terminal session, or the room. A folder
  whose checkpoint names no session of its own stays and is listed as
  `"unknown"`.
- A worker folder stays while its worker is alive, its session is open
  in the process that prunes, or a job is open in it. A worker writes
  `worker.json` when it starts and with each checkpoint, so the folder a
  `/clear` moves it to is marked too.

# corteza 0.8.0.34

- The terminal answers through a talker by default, as a bot's room
  does: its own session only reads and hands work to a doer, a worker
  process of the conversation's own. It says so when it starts.
  `"talker": {"enabled": false}` in the corteza config turns it off, and
  a session whose tools are restricted (`--tools`, the config's `tools`)
  is no talker unless the config turns it on. `chat()` stays without
  talker mode unless its config turns it on.
- After a turn that hands work off, the terminal stays with the job: it
  prints the worker's steps, its questions and its result as they come.
  Ctrl+C returns the prompt and leaves the job running; `/jobs` lists
  the jobs and stays with an open one again. A Ctrl+C at a job's
  question is a no.
- `approval_mode: "review"` covers the doer's calls at the terminal and
  in `chat()`: the reviewer judges a job's call by the conversation that
  asked for the job and the task the talker wrote, and `/permissions`
  typed while a job runs reaches its next call.
- In a talker session `/provider` and `/max-tokens` change the doer's,
  as `/model` and `/effort` do; `/review`, `/doctor` and `/config` use
  the doer's model and provider. `/spent` counts each job in the
  conversation that asked for it, and a job's result goes into that
  conversation's transcript, so `--resume` brings it back. `/auto`
  starts no run in a talker session and says how to turn talker mode
  off. `/r` runs R in the terminal's own process. A resumed
  conversation's doer starts on the `jobs` block's model where the
  config names one.
- The command table's `talker` column covers every command of the
  terminal.

# corteza 0.8.0.33

- A call `policy()` denied is refused before any gate (a room's
  supervisor, the terminal's reviewer, an `/auto` run's monitor). The
  rule that nothing after `policy()` lifts a deny lived in each gate,
  and a gate that answered otherwise would have let a denied call run.
  A gate is still shown the call, so an `/auto` run's decision log keeps
  the deny, but its answer is not read.

# corteza 0.8.0.32

- A session's system text and tool list stay fixed for the session. A
  change to either made the provider read the whole history again, and
  a model that signs its thinking against the request refused the
  replay. Plan mode, an `/auto` run and the task list now change
  neither: each change reaches the model as a note at the head of the
  next prompt, and permission is checked at the call. `exit_plan_mode`
  is refused outside plan mode, and what an `/auto` run may not call is
  refused when called.
- `send_room_file` and `ask_room` stay listed in a room's session
  whoever sent the message, and refuse anyone but an operator at the
  call.
- The git and search tools are looked for once per session, so a
  `git init` mid-session adds no tool.
- The terminal's `/clear` builds the next conversation's config, tool
  table and system text again, so `/trust yes` takes effect at the next
  conversation. What a flag or a command set in this process
  (`--dry-run`, `--base-url`, `/permissions`, `/dryrun`,
  `/context-edit`) carries over. A talker keeps its guidance when
  `/clear` or the archival runtime builds the text again.
- `/context`, `/status`, `/tools`, `/doctor` and the meter count the
  system text and tools the request sends.

# corteza 0.8.0.31

- A bot's room works where an operator binds it with `/cd`, inside the
  bot's project roots, and no longer in a directory its topic names.
  Anyone allowed to change a topic could change that directory, and with
  it the checkout where the supervisor lets writes through.
  `project_roots`, a new last argument of `bot_configure()`, `bot_run()`,
  `bot_run_init()` and `bot_poll()` and a key of the bot config, names
  the roots. Each must exist; corteza creates none.
- An operator's `/cd` in a room says where the room works, and binds it:
  `/cd <dir>` to an existing directory inside a root, `/cd none` to no
  directory, `/cd default` back to the bot's `cwd`. `/ls` lists the
  roots, and `/ls <dir>` the directories in one. A move refuses while a
  job of the room is open, files each conversation as `/clear` does, and
  starts each conversation again in the new directory, with a new doer.
- A room's directory is its binding, else the bot's `cwd`, else the
  scratch folder. A binding to a directory that is gone, or outside every
  root, gives the scratch folder. A room whose topic starts with a path
  works in the bot's `cwd` until an operator binds it, and is told so
  once.
- A room state file that is there and cannot be read is set aside, and
  the room works in the scratch folder until an operator's `/cd` binds it
  again; it does not fall back to the bot's `cwd`.

# corteza 0.8.0.30

- A bot's room answers through a talker by default: a session with
  read-only tools that hands work needing edits, commands or tests to a
  doer, a worker process of the conversation's own. `"talker":
  {"enabled": false}` in the bot's config turns it off. A bot whose
  tools are restricted (`tools_filter`) and a room with no directory
  are no talker unless the config turns talker mode on.
- The talker runs on `talker.model`, else on the bot's own model;
  `talker.provider` without a model takes that provider's default.
  Up to 0.8.0 it ran on a fast model of its provider's and stopped with
  an error on any other provider. A `talker` entry that is no object
  stops with a message saying what it should be.
- In a talker session `/model` and `/effort` change the doer's model
  and effort, which the next job runs with. The doer starts on the
  model, provider and reasoning settings the config's `jobs` block
  names, where it names them. The talker follows `/model` unless its
  config names a model or a provider of its own, and takes its
  reasoning settings from the `talker` block alone. `/status` names
  both models. Up to 0.8.0 both commands changed the talker's alone.
- The command table (`corteza:::corteza_commands()`) has a `talker`
  column: what a command acts on in a talker session.

# corteza 0.8.0.29

- The command table (`corteza:::corteza_commands()`) says that a bot's
  room takes `/effort` and `/context-edit`, as it does; it listed both
  for the terminal alone.

# corteza 0.8.0.28

- A tool's time limit (`skill_timeout`) stays inside the tool. R checks
  the limit only between R calls, so a limit passed inside `readLines()`
  or `grep()` fired at the next R code; when that was a warning handler
  outside the tool, it ended the turn and left the call open. Now the
  tool's warnings and messages wait until the tool returns or fails and
  then pass on, in order. Where warnings are errors (`options(warn =
  2)`), a warning stops the tool where it is raised, and the error is
  the tool's result.
- `grep_files` reads within bounds. It skips a binary file (a NUL byte
  in the first 8000 bytes) and a file over 10 MB, matches a line that is
  not UTF-8 by its bytes, cuts lines at 500 characters, and stops at 500
  matches or after 20 seconds. The result says what it skipped and
  where it stopped.

# corteza 0.8.0.27

- A room's monitor (`auto_approve_asks: true`) reads the room's
  conversation, as the terminal's reviewer does, where it was told the
  task in one line. Each conversation keeps a record in the room's
  journal folder that a flush, `/clear` or restart leaves whole. The
  monitor sends the call to a person when it cannot read the record
  whole: nobody may approve, the record has a break (a restart whose
  backfill did not reach its last event, or failed), or it is too long.

# corteza 0.8.0.26

- A program that runs turns for someone else can let them stop one:
  `session$stop_requested`, a function the turn asks before each
  request to the model, before each tool call, again just before a call
  runs and on each streamed piece of the reply. A stop ends the turn
  with status `cancelled` and a history the next turn can use. A
  cancelled job's reason says what it leaves: the files it wrote stay,
  and the next job starts from its worker's last checkpoint.

# corteza 0.8.0.25

- A Matrix room and each of its threads keep a record of the
  conversation they hold, so a restart goes on with it and seeds
  nothing from before its `/clear`. A room's `/clear` saves the new
  record before it answers or replaces the session; when it cannot,
  nothing is cleared and the room is told. A job the room asked for
  finishes in the conversation that asked, and its result is posted
  marked as asked before the `/clear`.

# corteza 0.8.0.24

- At the terminal, `/clear` starts a new conversation for a job's
  worker too: its R objects stay and its history does not, and the
  next job gets the instructions and tools a session started now would
  have. A job still running finishes, and its result goes to the
  conversation that asked for it; a job still queued runs under the
  settings its conversation had when it ended. Each conversation keeps
  its own checkpoint, so `--resume` brings back that conversation's
  worker as its last job left it.

# corteza 0.8.0.23

- A turn that ends on an error no longer leaves a tool call without a
  result. Every later request from that history failed (Anthropic
  answers 400), so a room stayed broken until `/reset`. Before the
  error leaves `turn()`, each call with no result gets a placeholder
  and a note says the turn stopped on that error; a turn also closes,
  before its first request, any call an earlier turn or process left
  open. Anthropic, OpenAI and Responses histories alike.

# corteza 0.8.0.22

- Retention also clears corteza's state stores, in the same daily run
  and in `sessions_prune()`: a chain of jobs once every job in it has
  ended and none of its folders has changed for `retention.session_days`
  (an incomplete job nothing resumed waits `retention.incomplete_days`,
  180 by default), a worker's folder with no checkpoint, and a
  remote-control session that was closed or whose process is gone. An
  open job keeps its chain, and audit records, checkout lock numbers,
  room state, the archive's files and transcripts stay. A folder whose
  name or record corteza did not make is listed and left, and nothing is
  read or removed through a link.

# corteza 0.8.0.21

- A job's worker obeys its conversation's plan mode, approval mode and
  dry run. Each job applies its whole spec when it starts (provider,
  model, reasoning settings, plan mode, tools, turn limit), and the
  worker reads what its conversation allows before each tool call and
  again just before the call runs, so a `/plan`, `/permissions` or
  `/dryrun` typed while a job runs reaches its next call. A record the
  worker cannot read refuses every call. A worker cannot end plan mode;
  the person does.

# corteza 0.8.0.20

- Each session takes its tools once, when it starts: corteza's own and
  any a host registered, the user's skills, the config's package tools
  and, in a trusted directory, the project's skills. A bot's rooms
  share one process, and each now sees only its own project's tools. A
  name is taken once: a later source never replaces an earlier one's
  tool, so a skill named like a built-in tool is skipped, with a
  message. Policy, the supervisor, the monitors, the reviewer and the
  approval prompt judge the class of the tool that runs, as does the
  gate of a room with no directory. A child process (a subagent, a
  job's worker) now gets the config's package tools, as its parent
  does.

# corteza 0.8.0.19

- A Matrix room with no directory (its topic names none, and the bot
  has no `cwd`) reads and talks only. It works in a scratch folder, and
  until now it could write files there, run commands, start subagents
  and hand off or take jobs. Writes, commands, work in a child, a
  worker or another room, and tools with no class are refused before
  they run, and no later rule, setting or approval lifts the refusal,
  which says how the room gets a directory. Room listings say "no
  directory" in place of the scratch folder's path.

# corteza 0.8.0.18

- A compaction's summary ends with the names of the R objects `run_r`
  made that are still in the session, and the handles still open, so
  the model does not rebuild what it has. Names only, read with `ls()`,
  so no object's code runs; at most 30 names. The terminal, `chat()`
  and a child process list them; a room, whose objects are the bot's,
  and a session whose `run_r` runs in a worker do not.

# corteza 0.8.0.17

- A room's trust messages go out as Markdown, with what a session left
  alone as a list: the notice a room gets once, the `/trust` reply and
  the trust lines of `status`. A client that renders Markdown had run
  the indented items together. The terminal and `chat()` are unchanged.

# corteza 0.8.0.16

- `/help` lists every command. The terminal's had left out `/q`,
  `/reset` and `/new` (the same as `/quit` and `/clear`), `/refine`,
  `/job resume` and `/skills`, and `chat()`'s had left out `/skills`.
  corteza keeps a table of the commands of each surface, and a test
  holds both help texts and the dispatchers to it.

# corteza 0.8.0.15

- **Changed default: no package functions are registered as tools
  unless a config lists them.** `skill_packages` is now empty. Up to
  0.8.0 the default registered 8 functions of base and utils in every
  session: `file.exists`, `Sys.glob`, `read.csv`, `head` and `tail`,
  and `file.copy`, `file.remove` and `dir.create`. To keep any of them,
  list it with its class, as in `{"skill_packages": [{"package":
  "utils", "functions": ["head"], "class": "read"}]}`. A config that
  sets `skill_packages` is unchanged.

# corteza 0.8.0.14

- **A reviewer can answer what would prompt, at the terminal and in
  `chat()`.** With `approval_mode: "review"`, from `/permissions
  review` or the user's config, a call that would prompt goes to the
  rules in code, then to a reviewer, then to the person. The reviewer
  is a model in a process of its own with read-only tools, set by the
  `supervisor` settings. It approves, refuses and tells the model why,
  or passes the call on. It rules on reads, writes and commands; a
  call the rules flag, a tool with no class and a credential path
  still prompt, and a call allowed always runs without it. A reviewer
  that fails or has not ruled within `supervisor.timeout_sec` passes
  the call on, and an interrupt cancels its request. A project's config
  cannot set `"review"`.
- The reviewer reads the conversation from the session's transcript:
  every prompt whole, each after the whole reply it answers. It never
  reads a compaction's summary, output staged with `/r` or `!`, or an
  `/auto` run's driver prompts. A conversation it cannot read whole
  goes to the person: one past 100,000 characters, one with a line it
  cannot read, or one with prompts written before this version.
- Each prompt in a transcript says how it was sent: `staged`, and
  `typed` when more went with it than the person typed.
- The approval prompt says why it asks when the reviewer passed a call
  on or the rules flagged it.

# corteza 0.8.0.13

- **The host files a room's conversation.** `bot_run_init()` takes a
  fourth hook, `on_archive(chat, cfg, room_id, archive)`, handed each
  range of a room's or thread's conversation that nothing has filed, at
  `/clear` and at a flush (`bot_request_flush()`). `list(archived =
  TRUE, ref)` takes the range, and `ref` goes to `on_clear`. Anything
  else, an error included, leaves the range waiting in
  `<state>/archive/<session id>.pending.json`, written before the host
  is asked, and the next `/clear` or flush offers it again.
- **Changed default: with no `on_archive` hook, nothing is filed.**
  corteza no longer calls `pensar::ingest()`.
- `/clear` keeps a session whose conversation is neither with the host
  nor on disk, and says so in the room.
- After each flush the log says how many ranges wait for the host, the
  size of their files and how long ago the oldest was first kept.
  Retention never removes a waiting range.
- `thread_context` may return `kind = "summary"`, and a thread restored
  from one is told it continues a conversation archived as a summary.
- `bot_archive_all()` and `matrix_archive_all()` take `on_archive` and
  `cfg`.

# corteza 0.8.0.12

- **"Allow always for this project" is kept in the user's config, by
  directory.** The terminal keeps those answers in `approvals.json`
  under `tools::R_user_dir("corteza", "config")`, keyed by the
  directory with links resolved, and no longer reads a project's own
  `.corteza/approvals.json`, trusted or not, so a repository cannot
  arrive with answers already given. A tool allowed always through the
  project's file asks once more.
- The terminal offers "Allow always for this project" in every
  directory, an untrusted one included. Saving makes no folder in the
  project.
- Where a project holds the old file, the terminal says at start that
  it is not read. `/permissions` and `/status` list what is saved for
  the directory.
- Terminals that save at once each keep their answer. A store that
  cannot be read is set aside as `approvals.json.unreadable`, and an
  answer that cannot be saved holds for the session.
- In a room, a worker's R code that names the approvals file goes to a
  person.

# corteza 0.8.0.11

- **Every tool has a class, and approvals follow it.** A tool reads,
  writes, runs code (`exec`), acts on the session's own work
  (`control`), or runs work in a child, a worker or another room
  (`continue`).
  `skill_spec()` and `register_skill_from_fn()` take `class`, and a
  `skill_packages` entry takes one for all its functions. A tool that
  names none is `unknown` and asks: a skill, or a package tool whose
  entry names no class, used to run unasked.
- **Changed default: `send_room_file` asks**, as any write does. A
  person answers by reaction, or the monitor where `auto_approve_asks`
  is on. `"permissions": {"send_room_file": "allow"}` in the user's
  config lets it send unasked.
- Plan mode refuses `write`, `exec`, `continue` and `unknown` tools,
  among them `query_subagent`, `delegate`, `job_resume` and
  `harness_note`, and a write under a credential path, which used to
  ask.
- A read or write under a credential path asks each time. No saved
  "always" answers it, and its prompt offers none.
- `base::file.copy`, `base::file.remove`, `base::dir.create` and `cmd`
  ask, as `write_file` does. For a tool corteza did not write, every
  string argument counts as a path.
- A `permissions` entry replaces its tool's verdict, unless that
  verdict refuses: nothing in the config lifts a refusal. Taking a tool
  off `dangerous_tools` no longer stops it asking; a `permissions`
  entry does.
- In a room, `harness_note` and a tool with no class go to a person,
  never to the monitor.

# corteza 0.8.0.10

- **A host can take a room's messages for itself.** `bot_run_init()`
  takes a third hook, `on_message(chat, cfg, room_id, message, session,
  backfill)`, offered each message from someone else before the loop
  does anything else with it. When it returns `list(handled = TRUE)`,
  corteza marks the message seen and does nothing more: no turn, no
  history entry, no read receipt. Anything else, an error included,
  takes the normal path.
- Once a host has taken a message in a room, the bot's own messages
  there are recorded nowhere for the life of the process, so the
  host's posts do not fill a history no turn reads.
- A restart's backfill offers each message it would record with
  `backfill = TRUE`.

# corteza 0.8.0.9

- **A picture sent to a bot's room is kept where its tools can read it.**
  It used to be given to the model and removed, so no tool could use it.
  A room's session now keeps each picture in `<id>.attachments/` beside
  its journal, readable by this user alone, and the receipt the model
  gets names the file and the size actually downloaded. A picture the
  model is not shown, too big for the provider or with no vision, is
  kept all the same, up to 25 MiB. `keep_attachments: false` in the bot
  config turns keeping off.
- A picture in the history a restart reads back is kept the same way,
  and the history gets its receipt; the model is not shown it and
  nothing is posted. chat.api 0.2.0's Matrix history does not yet
  return pictures.
- Retention removes a session's kept pictures with its journal. It
  removes no link, wherever one is in a session's folders, and counts
  no link's target in a size.

# corteza 0.8.0.8

- **Compaction follows the windows of current models, with a cap.** The
  table of context windows had stopped at Claude Opus 4.7 and gpt-5.6, and a
  model it did not know compacted at 115,200 tokens. It now holds the
  current models of each provider, and `context_compact_tokens` (200,000 by
  default) caps the trigger: a session compacts at the smaller of
  `context_compact_pct` of its window and the cap. `0` turns the cap off.
- A session that comes back after `context_idle_minutes` (60) with at least
  `context_idle_tokens` (100,000) of context compacts before its next
  request. `"context_idle_minutes": 0` turns that off.
- `/context` says where compaction starts and which rule set it. A
  `context_window` in the config now shows there and in the context meter.
- A `context_window` that is not a number above 0 is ignored. A quoted
  number there stopped every turn.
- A compaction's summary reads more of what it replaces on a model with a
  large window: up to 937,500 characters at 1M, which holds a context at the
  default cap. It read the newest 120,000 characters on any model, and still
  does in a recovery from a request refused for its size and after a summary
  that failed.
- On the Codex backend the summary's request keeps `context_compact_bytes`
  (900,000 by default), measured as it is sent. A request that cannot fit,
  or has room for no message of the conversation, is not sent, and the
  history stays as it is.
- `context_limit_for_model()` resolves a dated or suffixed id to its
  family's row, and an ambiguous short id to the smallest window among its
  matches. The OpenAI rows hold the input limit.

# corteza 0.8.0.7

- **A session on a Claude model that thinks by default no longer stops
  answering after a compaction, a context edit, a restart or a change to its
  task list.** Those models sign each thinking block against the
  conversation that produced it, and the provider refuses a request that
  replays one after the system text, the tools or an earlier message
  changed. corteza now removes the thinking blocks from a history whenever
  it replaces one. When a provider refuses replayed thinking all the same,
  the turn removes the blocks, records that in the journal and continues
  once, without running a tool twice.
- A session that sets `thinking_budget_tokens` keeps its thinking blocks
  through a rewrite.
- `new_session(history = )` removes the thinking blocks of the history it
  is given.
- A fallback to another provider converts a Claude reply to plain text
  whether or not it came with thinking.
- A request-buffer error on a turn's first request sends the prompt again.
  The retry went out without it.

# corteza 0.8.0.6

- **A project's own files load only from a directory you have trusted.**
  That covers `.corteza/config.json`, the skills under `.corteza/skills/`,
  `AGENTS.md` or `CLAUDE.md`, the stored lessons and the briefing. Trust a
  directory with `/trust yes`, or list it under `trusted_projects` in your
  config; every directory inside it is trusted with it. The command and
  `chat()` ask once at start. A room is told once, and only an operator may
  trust its directory. `serve()` reports an untrusted directory on stderr.
  `/trust` and `/status` show where a directory stands and what was not
  loaded. Up to 0.8.0 a cloned repository could set the approval mode and
  the provider for a session started in it, and ran its own R files at
  start.
- In an untrusted directory the terminal's "Allow always" lasts for the
  session and writes no file.
- An unattended session's write to a project's `.corteza` folder goes to a
  person, and so does R code that names the list of trusted directories. Up
  to now only `.corteza/config.json` did.
- `harness_trust_project` is no longer read. A trusted directory's lessons
  are trusted.

# corteza 0.8.0.5

- **A bot with no `operators` accepts no invites.** Up to 0.8.0 it accepted
  every one, which gave a session with tools to anyone who could invite it.
  Rooms it is already in work as before. Each refused invite is reported in
  the log with what would change it.
- `bot_configure()` gains `operators`: the people a bot answers to, each by
  a full Matrix id. Before, the key could only be added to the bot config by
  hand. A bot that should go on accepting invites needs it.

# corteza 0.8.0.4

- `corteza --resume`, `corteza --list`, `/sessions` and
  `chat(session = TRUE)` look at the sessions started in the current
  directory. `--all`, or `/sessions all`, covers every directory.
  `corteza --resume --all` is what `--resume` did before.
- A session's entry in `sessions.json` records its directory, where it runs
  and a title, the first prompt cut to 100 characters. A list shows the
  title. Sessions from earlier versions get their directory from the
  transcript the first time sessions are listed.
- A session started with `--session <key>` is listed. Only keys beginning
  with `corteza:` were.
- A Matrix room's session has an entry too, under agent `matrix`.
- `vignette("configuration")` has a "Session records" section that states
  the fields for programs that read the file.
- `sessions.json` is written through a temporary file with mode 0600, and a
  store that cannot be read is set aside as `sessions.json.unreadable` where
  it used to be replaced by the entry being written.

# corteza 0.8.0.3

- corteza now removes what old sessions no longer need. Once a day, when a
  terminal session starts and while a bot runs, the journal and the tool
  trace of every session unused for 90 days are removed. Transcripts, session
  stores, workspaces and audit records stay, and a session whose journal is
  gone still resumes. Set `retention$session_days` in the user's config to
  change the number of days; 0 keeps everything.
  `sessions_prune(dry_run = TRUE)` reports what a run would remove. Nothing
  reached through a link is removed.
- A room that restores its context from its journal, and has been silent for
  90 days, starts its next conversation fresh.
- A journal removed while its session is running starts again from nothing.
  Before, the session's next compaction was refused.

# corteza 0.8.0.2

- `install_cli()` no longer writes to `~/bin` by default. With no `path`, the
  command goes to `tools::R_user_dir("corteza", "data")/bin` on every system,
  as it already did on Windows, and the function prints the line that puts
  that directory on `PATH`. A named directory works as before. An install
  made by 0.8.0 or earlier stays in `~/bin`: `install_cli()` and
  `uninstall_cli()` say so and leave it alone, and
  `install_cli("~/bin", force = TRUE)` refreshes it.

# corteza 0.8.0.1

- corteza no longer creates a directory in the home directory for a chat bot.
  Up to 0.8.0 a room whose topic named no directory worked in `~/<bot name>`,
  created on first use; a test did the same on CRAN's check machine. Such a
  room now works in the bot's `cwd`, a new last argument of `bot_configure()`,
  `bot_run()`, `bot_run_init()` and `bot_poll()`, and of the `matrix_*` names
  that forward to them. It must name an existing directory. With no `cwd`,
  the room works in a scratch folder under the session's temporary directory
  and is told it has no project directory. A bot that relied on
  `~/<bot name>` now needs `cwd`.

# corteza 0.8.0

CRAN release, batching the 0.7.1.1 to 0.7.1.94 development cycles below.

## New features

- Chat rooms run on the chat.api transport contract. The room functions are
  renamed `bot_configure()`, `bot_send()`, `bot_poll()`, `bot_run()`,
  `bot_run_init()`, `bot_run_step()`, `bot_request_flush()` and
  `bot_archive_all()`; the `matrix_*` names still work and are deprecated.
- Delegated jobs in rooms: a talker answers while supervised worker
  processes do the work, with a reviewer, per-job approval, resumable
  attempts, and `tool_delegate()`, `tool_job_status()`, `tool_job_cancel()`
  and `tool_job_resume()`. Unattended tool calls pass through rules in code,
  an optional monitor, and a person.
- Remote control: `/rc` gives each CLI session a private room from which it
  can be driven.
- Audit records: turns, tool decisions, room deliveries and request payloads
  are recorded, and read with `audit_read()` and the `audit_payload_*()`
  functions.
- A session journal keeps the full provider-native history through every
  rewrite, and opt-in context editing lets the model drop or shorten
  completed entries.
- `voice_serve()` runs the AgentVoice gRPC service for one-to-one voice.
- Provider fallback when the primary hits a limit, bounded and narrated
  `/auto` runs, a continual harness of approved lessons, per-tool output
  caps, and an opt-in supervised `run_r` worker.
- `/model` lists models and sets reasoning effort; images in a room are
  passed to the model as images.

## Changes

- Requires llm.api 0.2.0 and saber 0.8.0. The optional room transport
  requires chat.api 0.2.0 and mx.client 0.3.0. Every dependency is on CRAN.
- `run_r` returns what the code prints along with its value.
- No exported function was removed.

# corteza 0.7.1.94

- Call mode is removed until livekitr is on CRAN: the bot no longer joins a
  room's call, `/call` and `/hangup` are gone, and the `voice.stt`,
  `voice.tts`, and `voice.call` settings are no longer read. The code is kept
  on the `livekitr-calls` branch. AgentVoice (`voice_serve()`) is unchanged.
- Every dependency now comes from CRAN: livekitr is dropped from Suggests and
  `Additional_repositories` is removed.

# corteza 0.7.1.93

- Require llm.api 0.2.0 and saber 0.8.0, both on CRAN, and drop the pinned
  saber GitHub remote.
- An interrupted turn on the OpenAI Responses wire (`openai_codex`, and
  `openai` models that llm.api routes there) now closes its unfinished tool
  calls with `function_call_output` entries. They were given Chat Completions
  `role = "tool"` messages, which a Responses history does not accept.

# corteza 0.7.1.92

- Close remote-control mailboxes before emitting disconnect notices, and drop
  stale room-index entries whose binding is missing or unreadable.
- `/rc` now creates a private Matrix room per CLI session by default and
  reuses it when reconnecting or resuming. Explicit room overrides remain
  available. The courier enables encryption before inviting operators when
  configured for E2EE, and recovers rooms after interrupted creation/setup.
- `/clear`, `/reset`, and `/new` in remote control start a fresh conversation
  in a new room, leaving a link in the old room. Disconnected session rooms
  ask for a CLI reconnect instead of starting an independent bot conversation.
- Threaded messages in session rooms join the same CLI conversation. Room
  lookups use a process-local index, and invitation retries check Matrix
  membership before inviting again. New-room links include the bot's server
  as a routing hint. Session-room creation requires chat.api 0.1.0.2 or later.

# corteza 0.7.1.91

- Keep one audit writer when a new storage directory is beneath a symlink,
  including macOS temporary directories.
- Reuse the caller's writer for synchronous archival dispatch/exit records;
  async archival uses separate background and model-subprocess streams.
  Report write failures and quota omissions once
  per kind per process so one warning does not suppress the other.
- Preserve the reported `turn()$usage` total when request records are partial;
  terminal records retain both totals and identify the canonical choice.
  Complete request coverage returns normalized input/output/total token,
  cache and cost fields. Transcript usage is explicitly tagged `llm.api`;
  its pi-style envelope does not imply pi usage field names.
- Reuse the caller's audit writer for in-process summaries, reviews and memory
  flushes. Release writer reservation directories after the first write, notify
  once on payload-quota exhaustion, and keep basic turn records when optional
  metadata enrichment fails. Coverage counts successfully written request records.
- `auto_log_read()` now counts blank lines as malformed and reports their
  positions rather than silently ignoring them.
- Capture final provider request bodies and context manifests when llm.api
  supports request callbacks, including transport retries and fallback usage.
  Keep the 0.1.9.13 dependency floor and mark unavailable request accounting.
- Preserve full captured tool output and admitted previews in private,
  content-addressed payload stores with explicit size/quota omissions. Add
  redacted readers, payload pins and explicit expiry with a dry-run default.
- Record auxiliary summaries, reviews and consultations as canonical turns,
  with parent links and inherited audit settings. Link supervised R captures
  from separate worker streams and retain known usage on failed worker turns.

# corteza 0.7.1.90

- Create transcripts and traces with owner-only permissions before writing and
  restrict existing files before appending. Mark trace truncation explicitly
  for arguments and results, including nested and vector character values.

# corteza 0.7.1.89

- Persist room delivery attempts, failures and all returned event IDs without
  requiring a vault. Link replies and progress to their originating turns and
  hash posted text separately from generated text. Audit-write failures do not
  cause duplicate sends or change transport error handling.

# corteza 0.7.1.88

- Record canonical turn accounting across interactive, room, voice and worker
  sessions, including errors and interrupts. Link tool decisions, approvals,
  worker outcomes and monitor verdicts by stable turn and call IDs. Record
  observational write failures as gaps on recovery without changing execution.
- Preserve actual CLI usage and route metadata, use null for unknown usage,
  and mark copied usage as derived from canonical turns. Audit streams remain
  private and independent of verbosity and history-journal configuration.

# corteza 0.7.1.87

- Add explicit `/job resume <id>` and talker `job_resume` for incomplete local
  doer jobs. Save immutable per-attempt history and workspace snapshots, validate
  identity and file integrity, and restore them in a fresh worker under the
  normal checkout lock. Preserve newer checkout files and identify unavailable
  workspace objects. Resume requests require the original requester or operator.
- Link each continuation to its prior attempt, deduplicate repeated requests,
  retain per-attempt limits and normal approval checks, and record cumulative usage. No
  automatic continuation is enabled. Old jobs without snapshots cannot resume.

# corteza 0.7.1.86

- Retain attributed attachment receipts in room context, transcripts, journals,
  and delegated text. Acknowledge unsupported or unreadable contents explicitly
  without exposing transport URLs or keys. Keep successful image ingestion;
  non-image files are acknowledged without downloading or extracting them.

# corteza 0.7.1.85

- Allow diagnostic strings such as `cat("token:", value)` through remote
  approval while retaining credential-value and known-secret checks. Explain
  credential and oversized-call refusals as declines, with no pending local
  approval, and suggest credential references or smaller reviewable calls.

# corteza 0.7.1.84

- Record tool-turn and output limits as incomplete worker outcomes, preserving
  partial results, termination reasons, and known usage across retries and
  fallbacks. Release checkout locks without scheduling a reviewer or claiming
  completion. Detect limits from the agent loop rather than quoted reply text.

# corteza 0.7.1.83

- Add `send_room_file(path)` for bot operators to send a requested attachment
  to the current conversation through the host's chat client. Models and workers
  do not receive chat credentials. Preserve tool policy and supervision, refuse
  credential/configuration files, and record confirmed attachment event IDs.
- Use the transport's encrypted attachment support when available. Limit files
  to 25 MiB and refuse thread sends until file routing can preserve them.

# corteza 0.7.1.82

- Add opt-in `ask_room(room, question, thread)` for one bot to consult its own
  conversation in another room. Answer asynchronously with the destination's
  conversational model and a snapshot of its current text context, with no
  tools, recursive requests, or rewrites of its live window. Record the
  question and answer in both conversations; retain cancellation and recovery.
- Add opt-in bot-to-bot delegation over the existing chat transport, using
  explicit outgoing destinations and incoming delegator permissions. Keep
  remote jobs in the durable ledger, correlate authenticated results, and
  handle duplicate delivery, cancellation, review addenda, and restart probes.
- Run accepted work through the existing supervised doer/reviewer lifecycle.
  Per-delegator tool restrictions apply to each worker run, including native
  web search. Bot requesters cannot approve tool calls; human approvers can
  answer by message where reactions are unavailable.

# corteza 0.7.1.81

- Add `context_edit.retain_turns` for conversation owners: protect the recent
  human-led turns, including assistant replies and tools, from CLM edits.
  Doers do not inherit this retention floor. The package default remains 0.
- Keep an unrecovered output-token cutoff's visible reply in active history
  and the session journal so later turns can refer to it. Incomplete tool
  calls remain discarded.

# corteza 0.7.1.80

- Restore the saved provider and model together when resuming in the CLI or
  `chat()`, rather than combining the saved provider with a configured model.
  Honor explicit overrides; a provider change uses its matching configured
  model or provider default.

# corteza 0.7.1.79

- Preserve image turns when conversation context editing adds labels, rewrites
  the journal, or files retired messages to a vault. Multimodal records retain
  their classes and image bytes across save/restore, so later requests still
  translate correctly for the selected provider. Existing plain JSON journals
  remain readable.

# corteza 0.7.1.78

- R code is ASCII: the non-ASCII characters in string literals (the
  ellipsis in the sentence-break pattern, the em dashes in the context
  meter, the instruction catalog and the fallback banner, the banner's
  emoji, the curly apostrophe in the sidebar pattern) are `\u` escapes
  now, which R turns into the same characters at parse time. This clears
  the one WARNING `R CMD check` had been reporting, and CI now fails on a
  check WARNING (`WARNINGS_ARE_ERRORS`).

# corteza 0.7.1.77

- `bot_run_init()` takes `hooks`, a named list of callbacks for policy the
  loop asks the host about: `on_clear(chat, cfg, room_id, title, vault_ref)`
  decides whether a cleared conversation becomes a room of its own, and
  `thread_context(chat, room_id, thread_root)` names the archive a thread
  continues. `bot_segment_from_clear()` and `bot_thread_archive()`, which
  made that decision and read that index here, are gone: a bare bot
  (`bot_run()`, or no `hooks`) archives and resets at `/clear` and never
  rehydrates. The `segment_rooms` config gate moves with the segment code
  into the host.

# corteza 0.7.1.76

- Add `/sidebar [N]` and "Let's take this offline" for copying recent
  conversation into an operator's private room. Route active job progress,
  approvals and results there, with private-destination checks and worker
  retirement to keep subsequent shared-room work separate.
  Import the excerpt as one user-role quotation with source attribution.

# corteza 0.7.1.75

- Let room operators use the explicit `/approve-all [job-id]` command
  to authorize remaining requests for one active job. Add status and revoke
  commands; preserve policy denials and per-call credential approvals, and
  verify operator membership on use. Ordinary words and `/auto` do not grant it.

# corteza 0.7.1.74

- Mask credentials only at display boundaries using a session-local registry.
  Preserve exact tool data and journals, immediate voice delivery, and complete
  approval arguments. Credential-bearing remote requests require local review.
- Default room progress to summaries; `activity_detail: true` enables previews.
- Show CLI result previews and visible commentary. Emit factual updates at
  tool boundaries after 60 seconds of silence and explain tool-turn limits.

# corteza 0.7.1.73

* `/model` now lists current and configured models plus the active provider's
  catalog, with numeric selection tied to the last displayed list. The CLI,
  `chat()`, and Matrix rooms share the menu. Live discovery uses llm.api's
  optional `list_models()` API; discovery failures keep configured choices.
* Add `/effort` to show or change a conversation's reasoning effort.
  `/effort default` restores its configuration/provider default. Split
  talkers change independently of their doers.
* Keep gateway URL overrides scoped to `openai_compatible` during model
  discovery. Check Responses routing before offering context editing;
  reject unsupported pure-CLM requests before generation and skip incompatible
  fallback models.

# corteza 0.7.1.72

- Add `/remote-control` (alias `/rc`) to continue the live CLI or `chat()`
  session from a Matrix room through a running local bot. Keep the same model, history,
  workspace, and tool policy, with operator-only input and request-specific
  approvals. Relay bounded progress and replies without a second sync client.
- Support remote `/auto`, `/max-tokens`, `/stop`, and disconnect, with queued
  output delivery and callback restoration when control returns locally.
- Include remote room prompts and replies in the normal conversation archive,
  deduplicated by Matrix event ID.

# corteza 0.7.1.71

- Bare `/auto` continues the current conversation. Add auto status and resume,
  expose the execution setting, and hide tools unavailable to the auto monitor.
- Classify `read_handle` as a read operation for approval policy on every
  channel. Auto runs omit tools whose read/write/exec operation is unknown,
  including unclassified package and MCP tools.
- Recover output-token cutoffs with bounded larger requests from completed
  history, without replaying completed tools or executing partial calls.
  Add `/max-tokens` and report an unrecovered cutoff as a stopped auto run.

# corteza 0.7.1.70

- Add opt-in pure CLM context management for bot conversations and doers.
  `context_edit.strategy = "clm"` disables automatic summary compaction and
  holder archival. Doers inherit the editing policy; reviewers and monitors
  keep their existing configuration.
- Conversation owners, either split talkers or combined conversation/work
  sessions, can file retired context into an explicit `context_vault` before
  removal. Original entries are preserved as Markdown and provider-native
  JSON. Older human turns may be filed and dropped, never rewritten; the
  latest human turn remains protected. Workers never inherit vault filing.
- Restore configured conversation windows and consumed event IDs together
  from the journal so Matrix backfill does not revive retired context.
- Invalid conversation or vault settings warn and disable context editing.
  Failed vault writes reject the edit while the turn continues with the
  original entries intact. An exhausted pure-CLM room window requires `/clear`.

# corteza 0.7.1.69

- Detect downloaded image formats from their bytes rather than trusting upload
  MIME metadata, preventing rejected requests when PNG images are labeled JPEG.
- Show public commentary between expandable tool groups in room progress, with
  commands or paths, bounded result previews, and failed or refused outcomes.
  Delegated workers forward the same progress to their room or thread before
  posting the final result. Updates remain coalesced, with a plain-text fallback.

# corteza 0.7.1.68

- Serialize the `edit_context` schema's required fields as a JSON array.
  The single required field previously became a string, causing providers
  to reject requests with context editing enabled before generating a reply.

# corteza 0.7.1.64

- Add experimental conversation context editing, off by default. The
  `edit_context` tool drops or summarizes completed entries and maintains a
  task note while protecting instructions, human messages, and the current
  tool batch. Edits commit through the session journal at a checkpoint;
  compact edit segments share immutable entries, and failed writes prevent
  the live rewrite. `context_edit.enabled` / `cadence`, session overrides,
  `/context-edit on|off|status`, and `--context-edit` control the feature.
  Anthropic and OpenAI chat-completions shapes are supported; threshold
  compaction remains the fallback. Subagents inherit the effective setting,
  and CLI/room reports and structured events show each applied edit.
- Journal provider-history bridges before fallback replaces the native
  window, preserving completed tool traffic across provider changes.

# corteza 0.7.1.63

- **A session journal keeps the full history through every compaction.**
  The transcript records what people exchanged as text and the trace cuts
  tool calls to a preview, so compacting the live history used to destroy
  the only complete copy of the tool traffic the model had seen.
  `<id>.journal/` beside the transcript now holds `live.json`, the live
  history as last mirrored after every assistant message and tool result,
  and `segment-NNNN.json`, the exact history each compaction replaced,
  written before the rewrite so a write failure blocks the compaction
  rather than losing the prefix. The record is the segments past their
  inherited heads plus the live tail: every entry once, no synthetic
  summaries. The same record covers archival, subagents (whose per-turn
  transcript slice now comes from the journal and is correct across a
  compaction inside the turn), Matrix rooms and threads, and interrupted
  turns. `journal_enabled: false` turns it off. This is the pattern a
  downstream host already ran for long autonomous sessions, now built in
  for every surface.

# corteza 0.7.1.62

- **The talker can read a URL itself (`fetch_url`).** The talker's
  toolset gains `fetch_url`, so a question about a page the user names
  (a URL, a GitHub issue and its comments, a raw file) is answered by
  reading it, not by handing off a whole job. Without it, a talker asked
  to read an upstream issue had no way to fetch it and escalated to
  cloning the repository through the doer, which then tripped the
  write-outside-project gate. The guidance tells the talker to fetch and
  answer, and never to clone a repository just to read something.

# corteza 0.7.1.61

- **Delegated work can carry pasted content by reference.** When the
  talker hands off work about something the user just provided (an
  article, a document, a log), it no longer has to copy that content into
  the task, which ran through its own output budget and truncated the
  reply once the content was large. `delegate` gains `attach_context`:
  the message the turn is answering is written beside the job as
  `context.md` and the task points the worker at it, so the content never
  passes through the talker's tokens.

# corteza 0.7.1.60

- **Reviewer and talker prompt calibration.** The reviewer now approves
  work that is correct and does what the task asked, listing minor points
  as notes, and requests changes only for a defect that matters; it no
  longer blocks usable work over nitpicks. The talker keeps its handoff
  to the doer invisible to the user: it acknowledges in one natural line
  and does not name the doer, the task it wrote, a reviewer, or that work
  was delegated, and does not paste long content back into the chat.

# corteza 0.7.1.59

- **Matrix tool calls are supervised; `auto_approve_asks` no longer
  approves everything.** The setting used to answer "yes" to every call
  policy asked about, in a room's turns and in its jobs, the
  credential-path rule's "ask" included. Now every call in a room goes
  through three steps. Rules in code read it first, shell commands and R
  code included, and put to a person: credentials, writes outside the
  project, elevated privileges, other machines, publishing, and git
  commands that throw work away. With `auto_approve_asks: true`, what the
  rules leave open goes to a monitor, a model with read-only tools and
  the project's instructions in a process of its own, which approves,
  refuses with a reason the working agent reads, or passes the call to a
  person. A person answers by reaction in the room, as before; without
  the setting a person is asked in place of the monitor. A job's result
  says how its calls were answered. The monitor's model and any extra
  writable directories are set under `supervisor` in the user's own
  config (a project config cannot set them); see
  `vignette("configuration")`. The monitor is started on first need,
  retired when idle, and counted against `jobs.max_workers` like a doer
  or reviewer. The rules read text and are not a sandbox.

- **A tool result that was cut for length can be searched.**
  `read_handle` gains `op = "grep"` (the lines matching `pattern`, with
  line numbers and a count of how many lines were searched) and
  `op = "lines"` (`start` to `end`, 40 at a time). The talker, the
  reviewer, and the monitor now have `read_handle`: the talker was
  shown the first 40 lines of a long result and told to read the rest
  with a tool it did not have, and the other two had no way at all. The
  notice on a cut result now names only a tool the session has, and
  says so when it has none. In a Matrix bot each room keeps its own cut
  results (named `.o_001`, ...), so one room cannot open what another
  room's tools returned; they shared the process's store before.

- **Matrix approvals only accept an operator or the room's one human.**
  Any reaction other than the bot's own used to answer an approval prompt,
  so a second bot in the room could approve this bot's tool calls.
  Approvers are now the configured `operators`, or, with none configured,
  the room's one human when it has exactly one. Other reactions are
  ignored. A room with no possible approver gets a notice and the request
  is declined. Bots not listed under `bots` count as humans.

- **Talker mode: delegate work and keep talking.** With
  `"talker": {"enabled": true}` in a bot config or the corteza config, a
  session runs on a fast model (Haiku 4.5 for anthropic, gpt-6-luna for
  openai, or `talker$model`) with read-only tools plus `delegate`,
  `job_status`, and `job_cancel`. Delegated work runs on a persistent
  per-session worker using the session's configured model. In Matrix the
  bot keeps answering while jobs run, posts each result to the room or
  thread it came from, and takes approvals as reactions on later polls.
  In the CLI and `chat()`, results and approvals appear before the next
  prompt; `/jobs` and `/cancel` manage them. Off by default.

- **The doer starts with the project's context.** A delegated job's
  worker gets, after its role, what a session opened in its directory
  gets: the shared instructions file, the project's AGENTS.md or
  CLAUDE.md, the briefing, and configured context files. The task text
  is all a talker hands over, so without this a project's rules reached
  the doer only when the talker restated them. A reviewer gets the
  shared and project instructions after its own prompt, so it can say
  when the work broke one; its tools stay read-only and confined to the
  checkout.

- **A Matrix bot's rooms can hand work to each other.** `delegate`
  takes `room` (a room's name, id, or project directory). The job is
  then that room's: it runs on that room's doer, in its directory, under
  its checkout lock, with approvals asked there. That room is told when
  the job is accepted, in the room and in its talker's history, and the
  result is posted in both rooms. Either room can ask about the job or
  cancel it. Only a configured operator's request is handed over. A room
  whose topic names another directory than its running session works in
  is not handed work until the two agree (`/clear` there starts a
  session in the new directory). When no room matches, the talker is
  told that every room was checked, shown the nearest names, and told
  when the project directory exists with no room working in it; the
  reply no longer lists every room, which a bot in many rooms had cut
  short.

- **A job's result is retried until it is posted.** A result, or the
  notice of a hand-off, that could not be sent (homeserver down, token
  mid-rotation) was dropped and reported as delivered. It is now kept in
  the state directory and retried by the poll loop with a growing delay,
  and set aside after a day. The job itself is never resubmitted.

- **A job runs in the directory its record names.** A job queued for one
  directory fails without running if its session has since moved to
  another, and a live worker is reused only while it is in the session's
  directory. `/clear` and `/model` no longer orphan a running job: the
  job and its worker move to the replacement session.

- **A Matrix bot retires idle job workers.** Each room and thread kept a
  doer process, and a reviewer once it had one, for as long as the bot
  ran. A worker idle for `jobs$worker_idle_minutes` (default 30; 0 to
  disable) is now closed, and no more than `jobs$max_workers` (default
  8) are kept, longest idle closed first. Both are read from the corteza
  config for the bot's own directory. A worker running a job, or waiting
  on an approval, is never closed, so the count can exceed the limit
  while that many jobs run. The next job in that room starts a new
  process that restores the worker's saved workspace and conversation.

- **A `thinking` setting names the Anthropic thinking type.** `thinking`
  in the corteza config, in a `talker` block, or under `jobs` and
  `jobs$reviewer`, is sent as the request's `thinking.type`. Claude
  Sonnet 5.5 thinks unless told otherwise and refuses `"disabled"`;
  `"between_tools"` is its setting for answering without thinking first,
  as in `"talker": {"enabled": true, "model": "claude-sonnet-5-5",
  "thinking": "between_tools"}`. The value belongs to one model, so a
  fallback to another model does not carry it and `/model` clears a
  talker's. It cannot be combined with `thinking_budget_tokens`.
  Requires llm.api 0.1.9.13, whose `agent()` takes `thinking`.

- **Jobs are durable and never re-run blindly.** Each job's intent is
  written before a worker sees it and its outcome beside it. After a
  restart, a job that never started is still queued; one that started
  with no outcome is reported `indeterminate` and not re-run. The worker
  checkpoints its R workspace and history at each job boundary and a
  replacement restores them. Jobs carry an owner, so bots sharing a
  machine or a room never settle or start each other's jobs.

- **One writing job per checkout.** A job that edits takes its checkout's
  lock (the git toplevel) and later writers queue until it ends, across
  every bot on the machine. Edits made outside jobs are not covered.

- **Optional review of delegated work.** `delegate(task, review = TRUE)`,
  or `"jobs": {"review": true}` as the default, follows a finished job
  with a review by a second worker that has read-only tools, no network,
  no access outside the checkout, and no way to run code. It reads the
  job's diff and report and answers with a verdict and findings. The
  diff is taken between snapshots of the checkout as the job started and
  ended, so it leaves out changes that were already there, and it covers
  untracked files up to `"jobs": {"review_snapshot_max_mb": 20}` in total
  (ignored files are never covered). Past that limit the two snapshots
  still cover the same files: files that were untracked at the start are
  left out of both, or the job's new files are left out of the end, and
  the reviewer is told which. The checkout stays locked from the
  start of the job to the end of the review. If the bot restarts in
  between and another job takes the checkout first, the review is
  cancelled instead of run on a changed tree.
  `"jobs": {"reviewer": {"provider": ..., "model": ...}}` runs the
  reviewer on a different model. The reviewer cannot run tests, and a
  review does not start a revision by itself.

- **`git_status`, `git_diff`, and `git_log` no longer run commands or
  read outside `allowed_paths`.** A ref or path could carry shell
  commands, a ref could be an option, and `path` could name any
  repository on the machine. Git now runs without a shell, and a
  session confined with `allowed_paths` is held to it, including inside
  a subdirectory of a repository. Three things behave differently in
  every session:
  - The file filter of `git_diff` is a literal path. Globs and pathspec
    magic such as `:(top)` are not interpreted.
  - A ref has to resolve to commits. `<rev>:<path>`, the object id of a
    file or a tree, a tag on one, and anything starting with `-` are
    refused.
  - Git runs none of the programs a repository's configuration names:
    no fsmonitor hook, clean/smudge filter, `post-index-change` hook,
    external diff, textconv, or gpg, and no fetch of objects a partial
    clone lacks. A file under a clean filter (git-lfs) is compared as
    its raw content, and a submodule by its commit only.

- **Matrix `/model` now changes the model replies use.** It set a field
  `turn()` never read, so it only renamed the badge. The badge also
  showed the provider's default model for sessions created from
  `cfg$model`; it now shows the model in use.

# corteza 0.7.1.58

- **A bot can take part in a call.** The voice brain that serves the
  1:1 AgentVoice mode (`voice_turn()`, `voice_turn_report()`) now has a
  second media backend: a call over LiveKit, where the bot is a
  participant and hears one audio stream per person. The call runs in a
  worker process beside the bot (`R/call-loop.R`, `R/call-worker.R`):
  it cuts each person's audio into utterances by signal energy, posts
  each to an OpenAI-shaped transcription route, answers through the
  room's session, synthesizes the reply a sentence at a time through an
  OpenAI-shaped synthesis route, and publishes it in 200 ms chunks.
  Someone talking over the bot stops it within a chunk, cancels the
  generation, and trims the reply posted to the room to what was heard.
  With several people the bot answers when addressed by one of its
  names or as a follow-up within 20 s; with one, always. The bot
  process posts and edits on the worker's behalf and tells it the
  call's media keys; the worker never holds a Matrix credential.
  Configured under `voice.stt`, `voice.tts` and `voice.call` in the bot
  config (see `vignette("configuration")`); needs `livekitr` (Suggests).
  Each route speaks one of two wires: the OpenAI audio API, or
  `wire: "gpu-host"`, a gpu.ctl host's `POST /infer` spoken directly
  (whisper and chatterbox entries; the bearer token from `key_file`,
  the voice cloned from the reference WAV named by `voice.tts.voice`).
  The bot joins a room's MatrixRTC call when someone else is in it (a
  call button posts a membership event and nothing else; chat.api
  0.1.0.1 reports those from the poll), in the rooms `voice.call.rooms`
  allows, and hangs up when everyone else has left; `/call` joins by
  hand and `/hangup` leaves. Joining goes through chat.api's call
  surface (`chat_call_join()`, an encrypted client). The bot process
  forwards the media keys that arrive on each poll to the worker and
  shortens its long poll to half a second while a call is on, so the
  worker's replies are posted promptly. `voice.call.greeting` is said
  once on joining; `voice.call.connect_delay_s` holds the worker
  between the membership and the media connection for clients that
  only notice participants who arrive after them. Tried live against
  FluffyChat 2.10 on a LiveKit SFU: the bot joins, is heard, and hears
  the room once the peers' media keys arrive; the log names each key's
  sender and recipients, never the keys.

# corteza 0.7.1.56

- **`run_r` releases its output sink when a worker call is interrupted.**
  The supervised worker aborts an over-deadline `run_r` with an interrupt,
  which unwound past the stream cleanup and left a sink, its file connection,
  and the temp file open in the persistent worker. With no ceiling, repeated
  timeouts accumulate sinks, connections, and files over a long run. Cleanup
  now runs on interrupts as well as on errors and normal returns.

# corteza 0.7.1.55

- **`/auto` runs continuously when no `--loops` is given.** Previously a
  bare `/auto <goal>` stopped after the configured iteration cap (default
  10). It now runs continuously, governed by the monitor subagent and the
  resource caps -- tool calls, tokens, time, cost, and the stall guard --
  rather than an iteration count, matching the "work until done, stop at
  the boundary" model of an Auto approval mode. `--loops N` still sets an
  explicit iteration cap. The run is never truly unbounded: `max_tool_calls`
  and `stall_loops` stay finite by validation, so a continuous run always
  stops on some cap or a monitor verdict.
- **Auto runs are framed by an autonomy policy.** The first prompt now states
  the run's operating policy explicitly -- act on reasonable assumptions
  rather than pausing to ask or report progress, stop only for an ambiguity
  that would materially change the intended outcome (and say so for a human
  rather than guessing), and judge completion by evidence such as passing
  tests. The per-iteration continuation prompt carries only a one-line
  reminder, so the stance survives compaction without repeating the whole
  policy every turn.
- **Narration guidance targets belief state, not keystrokes.** The system
  prompt's "communicating while you work" policy now asks for a brief update
  keyed to meaningful events -- a chunk of work finished, evidence that
  changes the working hypothesis, a substantially different approach, or
  several tool calls with nothing said -- rather than a line before every
  call, and to keep working after an update rather than stop for
  acknowledgement. Routine calls that only confirm an expectation need no
  narration. The silent-streak backstop (a nudge after
  `corteza.narration_streak` tool-call turns, default 3) is reworded to
  match.
- **Running commentary is rendered, not just nudged.** The model's narration
  now prints in the live progress stream on both the CLI and `chat()`, once
  per model response and independent of the approval prompt -- previously it
  surfaced only inside an approval prompt, so an auto-approved call (every
  call in an unattended run) showed nothing. See `cli_commentary_lines()`.
- **Auto runs no longer block on plan approval.** Inside an auto run,
  `task_create` is accepted automatically instead of prompting a human (which
  would hang an interactive run or default-deny a non-interactive one), and
  the task guidance drops its "ask clarifying questions first / wait for plan
  approval" steps in favour of the unattended flow. Attended sessions still
  prompt for plan approval exactly as before.
- **A worker can stop an auto run for a human decision.** `AUTO_STATUS:
  blocked` is a recognised status: the loop treats it as an escalation and
  stops immediately, rather than routing the request through the monitor as
  ordinary progress where a `continue` verdict would run another turn on an
  unanswered question. The autonomy prompt tells a blocked worker to report
  it rather than emit `continue` with prose.

# corteza 0.7.1.54

- **Worker `run_r` starts with R's default packages.** A supervised worker
  (`run_r_mode = "worker"`) is a bare `callr::r_session`, which loads only
  `base` plus callr's own tools, so common helpers from `utils`/`stats`/
  `methods` (`str`, `tail`, `head`, `setNames`, `capture.output`, ...) were
  missing, unlike in-process `run_r` which inherits the front end's default
  packages. The worker now sets `R_DEFAULT_PACKAGES` to the normal default
  set so both modes see the same base environment. A host that sets
  `R_DEFAULT_PACKAGES` in `run_r_worker_options$env` still wins.

# corteza 0.7.1.53

- **`run_r` returns what the code prints.** Text written with `cat()`,
  `print()`, `message()`, or a warning comes back ahead of the final value's
  print, like a console, in both `run_r_mode`s. Streams interleave in
  emission order, so a `message()` before a `cat()` prints before it.
  Warnings are muffled only under the default `warn < 2`; with
  `options(warn = 2)` a warning still becomes an error and halts. Output
  written before an error is kept ahead of the `Error:` line. `tool_run_r()`
  also returns an `r_error` flag so a caller can tell a failed evaluation
  from a successful one, without changing the model-facing text or
  `isError`. Previously only the final value's print was returned and
  everything else was lost.

# corteza 0.7.1.52

- **Per-tool output caps.** `config$tool_output_caps` raises the 50-line /
  5000-character tool-result cap for named tools whose results the model must
  see whole (a host-supplied game board, a table), keyed by tool name with
  `max_chars` and/or `max_lines`. Other tools keep the universal cap, and a
  result past the raised cap still stashes to a handle.
- **Worker process options.** `config$run_r_worker_options` passes named
  arguments to `callr::r_session_options()` for the supervised `run_r` worker
  (`env`, `libpath`, `cmdargs`, `arch`), so a strict host can run only the
  model's R under a sandbox wrapper at `R.home("bin")/<arch>/R`.

# corteza 0.7.1.51

- **Supervised persistent R execution is available without breaking embedded
  sessions.** `run_r_mode = "worker"` gives a session one private persistent
  `callr::r_session`, applies `skill_timeout`, bounds model requests with
  `skill_timeout_max`, retains partial state after a clean interrupt, and routes
  handle inspection back to the owning worker. The historical in-process
  `tool_run_r(code, envir=)` contract remains the default. Timeout results tell
  the agent to inspect retained state and optimize, split, parallelize, or
  approximate before retrying. The session's `skill_timeout` now also supplies
  the default argument to self-bounded `bash`, `cmd`, and `run_r_script` calls;
  explicit caller values continue to win.
- **Subagents can return Cursor-style structured handoffs.** An optional
  `report = TRUE` request asks for status, notes, findings, concerns,
  deviations, open questions, and feedback. The parent still receives a
  character reply for compatibility; the report is rendered for the model and
  retained in a `corteza_report` attribute for R callers.

# corteza 0.7.1.50

- **Subagent queries fire by default, and the sync wait is bounded.**
  The `query_subagent` tool now defaults to `wait = FALSE`: the prompt
  starts at once and `collect_subagent` fetches the reply, so several
  subagents can work in parallel while the parent continues. With
  `wait = TRUE`, `subagent_query()` fires the same one-shot call and
  collects with a real deadline (`timeout`, default 60 s; `Inf` opts out
  of the bound). A child that has not replied by then leaves the query
  pending for `subagent_collect()` or `subagent_kill()` and the call
  returns NULL, where it previously blocked for as long as the child
  took. A timeout that is not a single non-negative number is refused
  before the prompt is fired. `/ask` in the REPL says so and points at
  `/collect`; the hall monitor reads a timed-out query as no verdict and
  escalates, as its contract already said it should.

# corteza 0.7.1.49

- **Context compaction uses provider-default sampling.** Internal summary calls
  no longer send an unnecessary `temperature`, allowing compaction to work with
  subscription endpoints and models such as Claude Opus 5 that reject it.

# corteza 0.7.1.48

- **The default room workspace uses the bot's localpart as written.**
  `bot_default_cwd()` capitalised the Matrix localpart, so a bot logged in
  as `@helper:example.org` fell back to an empty `~/Helper` beside the
  `~/helper` instance directory its host runs it from, and created it.
  The fallback is now `~/helper`. An instance directory with other
  capitalisation, such as `~/MyHelper`, was mismatched before and still is.

# corteza 0.7.1.47

- **Instruction roots accept top-level symlink aliases.** A symlink directly
  under a configured `instruction_roots` entry may point at a skill directory
  outside the root, such as a package's `inst/skills/<skill>`. corteza walks
  the target as a bounded alias and derives the id from the link name, so one
  flat hub of package links can serve corteza, Claude Code, and Codex alike.
  Deeper symlinks must still stay inside their root, skill resources still
  cannot reach outside their bundle, and a skill reached through two roots
  stays a single entry owned by the more specific root.

# corteza 0.7.1.46

- **Provider cooldowns remain recognizable as limit failures.** The internal
  limit classifier now recognizes corteza's own all-providers-cooling error,
  allowing durable hosts to wait for the recorded deadline instead of
  misclassifying a transient cooldown as a permanent failure.

# corteza 0.7.1.45

- **Codex request-buffer overflow is now recoverable.** Long Responses-wire
  histories are measured by serialized bytes as well as semantic tokens, so
  opaque encrypted reasoning cannot silently exceed the gateway buffer before
  normal context compaction fires. A 507 request-buffer rejection forces a
  safe-cut compaction and prompt-free continuation of the authoritative
  provider-native history, bounded to three retries by default. The same guard
  covers the retry after a model context-window overflow, preserves completed
  turn and usage accounting, and exposes byte/retry provenance to durable
  hosts.

# corteza 0.7.1.44

- **Context compaction now belongs to the shared agent lifecycle.** `turn()`
  checks the live context before a run and after each complete assistant/tool
  result batch, so direct callers cannot bypass it. A single long autonomous
  prompt can be split at a safe call/result boundary while retaining a recent
  token tail; Anthropic, OpenAI Chat, and Responses/Codex native histories are
  all pairing-aware. A real context-window overflow gets one compact-and-
  continue retry without inventing a user message, and usage from both attempts
  is retained. CLI and `chat()` manual/automatic compaction now use this same
  path and durably record summaries through a pre-rewrite lifecycle hook.
  Codex compaction omits unsupported sampling controls, and caught summarizer
  failures are exposed through a lifecycle hook instead of remaining silent.
- **Compaction briefs can be domain-specific.** `new_session(compaction_prompt = )`
  lets a long-running host state what evidence and working state its summary must
  preserve, while NULL retains corteza's existing coding-agent brief.
- **Context accounting understands Codex and provider products.** Responses
  output, function-call results, and opaque reasoning state are no longer
  reported as near-empty history. Context-window lookup can distinguish the
  public API's model limit from the smaller ChatGPT Codex working window.

- **Instruction skills are now compact, lazy, and session-scoped.** Startup
  discovers exact `SKILL.md` files recursively, puts only their names,
  one-line descriptions, and stable ids in the prompt, and exposes the
  read-only `skill_instructions` tool for exact body or relative-resource
  retrieval. Each CLI, console, Matrix room, and subagent owns an immutable
  hash snapshot; changed, foreign, traversal, and symlink-escaping resources
  are refused. Explicit `instruction_roots` merge by stable root id and
  `instruction_disabled` ids union across global and project config.
  Instruction documents remain separate from executable tools, and the
  legacy full-body `format_skill_docs()` contract remains available.

- **System context is now a provenance-carrying saber manifest.** Corteza
  requires saber 0.7.2.3 at its published commit, renders compact shared and
  project instructions through its manifest API, and retains the source
  metadata on CLI, `chat()`, and Matrix sessions. `/context` reports included
  and skipped sources, their token estimates, reasons, and paths/origins
  without exposing their bodies. The migration removes corteza's obsolete
  inlined `saber::agent_context()` fallback and the eager package-help layer
  that duplicated tool schemas at substantial token cost; package help remains
  available on demand. Claude-specific global instructions and basename-
  matched memory are no longer guessed implicitly—operators can add any such
  files explicitly through `context_files`.

- **`run_r` can now be capability-scoped without changing its default.**
  `tool_run_r(code, envir = ...)` accepts a caller-owned persistent R
  environment whose assignments and large-result handles remain private to
  that scope. Existing calls still evaluate in `globalenv()` and retain
  workspace auto-capture; the model-facing tool schema still exposes only
  `code`, so hosts—not models—select the execution boundary.

- **Limit failover now finishes interrupted tool runs without replaying
  completed tools.** Provider-native histories are bridged to portable text
  when the next candidate uses a different wire. Matrix configs also honor
  `reasoning_effort` and can set `fallback_primary_retry_at` (for example,
  `"Mon 03:00"`) so a limited primary is retried at its weekly reset while
  the fallback serves requests in the meantime. Every fallback reply now names
  the actual model/provider; crossing from subscription providers onto an
  API-key provider produces an intentionally loud billable-usage banner.

# corteza 0.7.1.43

- **Anthropic prompt caching is plumbed through.** `new_session(cache =
  "5m")` (or `"1h"`, or `"none"`) reaches `llm.api::agent(cache = )`,
  with `config$cache` as the fallback. Anthropic-only, and stripped for
  other providers -- per candidate during a fallback too -- so a codex
  session never carries it and llm.api's off-Anthropic warning stays
  meaningful. Validated as a closed set, because llm.api's `match.arg`
  would otherwise partial-match `"5"` to `"5m"` silently. Billing-only:
  the model sees byte-identical input, so this cannot change a result.
  With llm.api 0.1.9.6, now the Imports floor, the marker covers the
  message history as well as the system prompt, so each request in a
  tool loop reads the previous request's context from cache and pays
  fresh input only for what was appended since. The floor is raised
  because an older llm.api marks the system prompt alone and would
  silently deliver a fraction of that.

# corteza 0.7.1.42

- **`reasoning_effort` now works on Anthropic too.** Claude's depth
  control is `output_config.effort`, not the thinking budget, and its
  scale is `"low"`, `"medium"`, `"high"`, `"xhigh"`, `"max"` -- the API
  names it in the 400 when refused. Rather than add a second setting,
  `reasoning_effort` is routed to whichever field the provider's wire
  uses, so a session sets it once and it means the same thing on either
  family. Routing is symmetric, because a limit fallback re-routes per
  candidate and a one-hop-only value would silently vanish on the
  provider that answered. `thinking_budget_tokens` is unchanged and
  still Anthropic-only; the two are independent.

# corteza 0.7.1.41

- **A limit fallback no longer sends a provider a history it cannot
  read.** A conversation's history carries the content vocabulary of
  the wire that produced it, and llm.api replays what it is handed, so
  an Anthropic history diverted onto a Responses-wire fallback came
  back as `API error (400): Invalid value: 'thinking'` -- and since a
  400 is not a limit error, that ended the turn instead of continuing
  down the chain. A Matrix bot on `anthropic_claude` hit this the
  moment an Anthropic usage limit sent it to its `openai_codex`
  fallback mid-conversation. Candidates whose wire cannot replay the
  history's shape are now skipped (with a message), so the walk reaches
  one that can.
- **Reasoning depth is now a session setting.** `new_session()` takes
  `reasoning_effort` (`"low"`/`"medium"`/`"high"`, forwarded on the
  `openai` and `openai_codex` wires) and `thinking_budget_tokens`
  (Anthropic extended thinking). Both resolve session-field-then-config
  like `max_tokens`, so `reasoning_effort` or `thinking_budget_tokens`
  in `.corteza/config.json` reaches the CLI, `chat()`, and Matrix
  surfaces without further wiring. Each is dropped for providers whose
  wire cannot carry it -- including after a limit fallback rewrites the
  provider, where an effort setting meant for codex would otherwise
  reach the Anthropic Messages body as an unknown field and 400.

# corteza 0.7.1.40

- **Harness hardening** (review follow-up to the previous release).
  A project `.corteza/harness.json` travels with a cloned repo, so it
  is third-party data: entries are validated as they load, and
  project lessons render in a separate "Untrusted project notes"
  block that tells the model to treat them as reference material and
  never as instructions, unless the project is trusted via
  `harness_trust_project` in the *global* config. Rollback is now a
  compare-and-swap that refuses when an entry changed after the
  refinement being reverted, instead of silently destroying the later
  edit. Store writes use a unique temp file, check `file.rename()`,
  and hold a lock across the whole read-modify-write so concurrent
  sessions can't discard each other's entries. `/refine` shows
  evidence, kind, and path with each proposed edit and enforces the
  8-edit maximum in code.
- **`harness_auto`**: config flag for recording lessons without
  prompting (implemented as the default for the `harness_note`
  permission, so there is one enforcement path; explicit permissions
  still win). Auto-recorded notes remain in the ledger and reversible.
- Fixed: the in-process tool executor passed no `cwd`, so
  `harness_note` wrote to the process working directory instead of
  the session's project. Found by the new end-to-end approval tests.

# corteza 0.7.1.39

- **Continual harness**: a self-improving store of one-line lessons
  the agent records as it works and that ride into every later
  session's context. `harness_note(title, fact, evidence)` is a tool
  the model calls the moment it verifies a durable fact (approval-
  gated by default); `/refine` reviews the conversation into proposed
  edits shown one at a time for approval, with `/refine list`,
  `/refine rollback <id>`, and `/refine promote <id>`. Entries carry
  ids, versions, provenance, and evidence; every applied edit records
  before/after snapshots so rollback is a generated inverse; the base
  system prompt is never in the store. Project scope lives in
  `.corteza/harness.json`, global in the user data dir. Injection is
  the whole store while small (empty store adds nothing to context).

# corteza 0.7.1.38

- **`new_session()` gains `max_tokens`**: a per-response output-token
  budget forwarded to `llm.api::agent` (NULL defers to
  `config$max_tokens`, then llm.api's provider default — 4096 on
  Anthropic). Agent turns that write long tool calls, big `run_r`
  bodies especially, need more than that default: a response the
  budget cuts off ends the turn with `[Output truncated: max_tokens]`
  (llm.api#39) instead of the model's answer. Resolution follows the
  `web_search`/`base_url` idiom via `.session_max_tokens()`; both
  boundaries validate (single positive whole number — `as.integer()`
  alone would truncate fractions and pass NA through). Requires
  llm.api >= 0.1.9.5: forwarding a budget onto an older llm.api walks
  straight into the silent-truncation bug #39 fixes.

# corteza 0.7.1.37

- **Auto runs write a record of authority**: one append-only JSONL per
  run under `agents/<agent>/auto/<run_id>.jsonl`. `run_start` is
  written before validation (a refused start still appears), each gate
  consultation gets a record naming the deciding authority (budget /
  envelope / monitor / accounting) — envelope-refused calls previously
  left no trace anywhere — each monitor progress verdict is recorded,
  and `run_end` carries a mechanical stop category, spend, executed
  calls, and the worktree delta. A file with no `run_end` is a run
  whose process died. The `auto_run_id` also stamps main-session trace
  entries written during the run and the monitor's transcript header,
  so the three surfaces join; attended formats are unchanged.

# corteza 0.7.1.36

- **`turn()` falls back to other providers on limit errors.** A new
  `fallback` config key (Matrix config and `.corteza/config.json`; a
  `bot_configure()` argument) lists `"model provider"` pairs to try when
  the primary answers with HTTP 429/503/529 or a rate, usage, or quota
  message. The provider that tripped is skipped process-wide for
  `fallback_cooldown_minutes` (default 30), then the primary is tried
  again. A limit hit after tool calls have run is not retried, so no
  tool call executes twice. Other errors pass through unchanged.

# corteza 0.7.1.35

- **`/auto [--loops N] <goal>` runs bounded unattended iterations**,
  supervised by a read-only "hall monitor" subagent. The monitor is the
  point: `turn()` routes only `"ask"` verdicts to `approval_cb`, and the
  default tensor resolves ordinary project writes to `"allow"` outside
  `~/projects`, so a supervisor wired there would never be consulted for
  the edits that matter. Every call surviving `policy()` now passes a
  `session$auto_gate` instead, whatever the verdict.
- **The monitor's authority is mechanically bounded** before it is asked
  anything: credential-path safety verdicts, paths escaping the project
  (resolved through symlinks), unknown tools, and write calls whose
  target cannot be resolved all stop for a human rather than going to a
  model for an opinion. Project config can tighten the envelope but
  never widen it, since a `.corteza/config.json` travels with a cloned
  repo. Shell and R execution is off unless enabled per run with
  `--exec` or in your own global config, because those tools' effects
  cannot be resolved from their arguments.
- Budgets are enforced at the gate, immediately before each brokered
  call and again after each monitor query, not merely between turns: one
  turn makes many calls, and asking the monitor costs tokens of its own.
  Errors and interrupts end a run rather than continuing against the
  previous turn's reply.
- **Subagent presets can now withhold provider-native web search and
  confine file tools to the project.** `subagent_turn_init()` previously
  built its session without a `web_search` argument, so every subagent
  had server-side search on regardless of its tool preset.
- Auto-run progress is measured by content hash rather than size and
  mtime, against a baseline that keeps pre-existing uncommitted work out
  of the run's ledger; spend counts the monitor's own queries, and an
  unpriced model stops the run rather than counting as zero.

# corteza 0.7.1.34

- **`/permissions` switches approval mode for the session**
  (`/permissions allow|ask|deny`), instead of only printing settings.
  Changing how much corteza asks before running a tool previously meant
  editing `config.json` and restarting. Session-scoped like `/dryrun`;
  the reply names the config file for anyone who wants it permanent,
  and says plainly that `allow` does not waive the credential-path
  guard, which short-circuits ahead of the config overlay.

# corteza 0.7.1.33

- **Follows the grpc package's rename to rgrpc** (their #27, same
  `grpc_*` function surface): Suggests, namespace qualifiers, the
  runtime floor (rgrpc >= 0.1.0, the first release under the new
  name), and the refusal messages.

# corteza 0.7.1.32

- **The verifier dials the agent's own homeserver directly.** When a
  voice caller names the agent's own homeserver domain (the domain of
  the bot's `user_id`), OpenID verification goes straight to the
  configured `server` URL instead of through Matrix discovery — a
  tailnet homeserver is deliberately not publicly discoverable. Every
  other server name still discovers normally, and the bypass changes
  only where the question is asked: the sub-domain binding and
  membership check are untouched. Derived entirely from the bot
  config; no hostname constant in code or config. Replaces the
  hostname-rewriting shadow in the launch script.

# corteza 0.7.1.31

- **Voice turns generate in spoken register.** A voice session's
  system prompt now carries an instruction to write plain speakable
  prose: no markdown syntax, numbers and abbreviations said aloud,
  short sentences, visual content described in words. The reply is
  synthesized verbatim by contract (the client may not alter it, or
  the heard-text accounting breaks), so speakable text has to be born
  speakable — the first live run read a markdown table aloud as
  "asterisk five bend forward". `voice.speech_style` in the config
  replaces the instruction's text; typed room chat keeps markdown.

# corteza 0.7.1.30

- **The allocator caller speaks the settled `gpu-voice-alloc/1`
  contract.** The request body carries the protocol version, and every
  mint presents the service credential from the new
  `voice.allocator_token` config key (a configured allocator without
  its token refuses with a named config error). The response envelope
  (`ok`, `v`, `allocation_id`, `room_id`) is validated before the
  grant is read, a refusal's `error` sentence is surfaced, a 401 is
  reported as the config problem it is, and the allocation id is
  logged so log lines and revocation can name the grant.

# corteza 0.7.1.29

- **A barge-in stops the generation, not just the stream.** When the
  voice client hangs up on a Converse stream mid-reply, the delta
  relay now calls `llm.api::llm_cancel()`: the provider stops
  generating and the turn returns its partial reply immediately,
  instead of running out a tail nobody is listening to. The voice
  server is single-threaded, so that tail was exactly the delay before
  the user's next turn could start -- and it billed for every token.
  The room record still keeps everything generated up to the cut.

# corteza 0.7.1.28

- **corteza serves live voice.** `voice_serve()` runs the AgentVoice
  gRPC service a fluffychat live-voice client talks to: `AllocateVoice`
  verifies the caller's Matrix OpenID token against their own
  homeserver, checks room membership, and relays a media grant from the
  fleet allocator (`voice.allocator`, HTTP); `Converse` runs one turn
  through the shared `turn()` machinery, streaming text deltas as the
  model produces them; `ReportTurn` cuts the stored room message down
  to the prefix the user actually heard, counted in code points. The
  reply is posted when generation ends and edited down on report, so a
  session that dies unreported leaves the full text standing -- the
  documented fallback. Audio never crosses corteza: the client streams
  PCM to the model hosts directly. Runs as its own R process, next to
  the room poll loop, and needs the `grpc` and `RProtoBuf` Suggests.
  The contract is `inst/proto/cornball/agent/v1/agent_voice.proto`,
  vendored from cornball-ai/fluffychat.

# corteza 0.7.1.27

- **A config can name the transport.** `cfg$transport` picks the client
  constructor behind `bot_chat_client()`: absent or `"matrix"` is the
  Matrix adapter as before; a `{"constructor": "pkg::fn", "args": {...}}`
  list builds any other `chat.api` client, and the whole room loop runs
  on it. The adapter must hold its own poll cursor and report
  `first_run`; the existing guard now says so.

# corteza 0.7.1.26

- **A segment room is not named after the command that ended it.**
  The fork auto-prepends a bare `/clear` as its own message, so a
  conversation opening with one produced a permanent room titled
  `/clear (2026-08-20)`. Commands are skipped when picking the title.

- **A conversation the user said nothing in gets no room.** A `/clear`
  straight after a `/clear` archives fine and used to become a sidebar
  entry that will never be worth reopening. The bar is "said anything at
  all", not a turn count -- a threshold would be a number nobody chose.

# corteza 0.7.1.25

- **The per-image ceiling is per provider.** It was one 5 MB constant,
  taken from Anthropic's documented limit on the reasoning that the
  strictest provider is the safe one to assume. That refused the first
  real photograph anyone sent -- 5.58 MB, 6.5% over -- to a bot running
  gpt-5.5, which takes four times that. Anthropic keeps 5 MB, OpenAI
  and `openai_codex` get 20 MB, and a provider that is not listed takes
  the conservative default rather than a guess. `image_max_bytes` still
  overrides all of it. The "skipping" message now names the limit as
  well as the size, since a line saying a picture was refused without
  saying what would have passed is the one thing a reader has to go
  find out.

# corteza 0.7.1.24

- **A picture posted in a room reaches the model as a picture.** An
  image arrives as its own message carrying an attachment that names
  where the bytes live; corteza fetches it through the transport
  (`chat.api::chat_download()`), encodes it, and sends it alongside the
  text as `llm.api::llm_content()`. Before this the bot saw only the
  filename, and answered that no image was attached.

  On by default for the cloud providers, off for `ollama` (a local
  build is as likely to be a text-only 9b as a vision model). Both are
  overridable with `images: true`/`false` in the Matrix config, and
  `image_max_bytes` sets the per-image ceiling (5 MB by default). One
  unfetchable picture -- an encrypted attachment, most likely -- costs
  the message its image and nothing else. Requires chat.api >= 0.0.1.24
  and an llm.api with `llm_content()`; without either, the text goes
  through as it did before and a message on stderr says why.

# corteza 0.7.1.23

- **A folded conversation continues from its thread.** Sessions are
  keyed by room and thread root rather than room alone, so a room's
  main timeline and each of its threads are separate conversations
  (without a thread the key is the room id exactly, so existing rooms
  are unaffected). The first message in a thread seeds its session from
  the archive the thread stands for, found through the
  `ai.cornball.fold` index the fold writes into the topic room. Replies
  go back into the thread they answer, falling back to the room where
  the transport cannot route one. Requires chat.api >= 0.0.1.23.

# corteza 0.7.1.22

- **A cleared conversation can become a room of its own.** When a
  cleared room is listed in the config's `segment_rooms`, `/clear`
  files the ended conversation as a "segment": a private room named by
  the conversation's opening line, carrying the archived-transcript
  pointer as a notice and an `ai.cornball.lifecycle` state event that
  clients group and demote by. Off unless configured. Requires
  chat.api >= 0.0.1.22 (`chat_set_state()`); older transports skip
  with a message.

# corteza 0.7.1.19

- **`matrix_*` is now `bot_*`, and `R/matrix.R` is `R/rooms.R`.** corteza
  reaches every transport through the `chat.api` contract and makes no
  `mx.api` or `mx.client` call of its own, so the prefix named a coupling
  that no longer exists. Of the 74 functions renamed, roughly 20 were ever
  transport; the rest are agent behaviour that had been wearing a Matrix
  prefix since the day it was written.

  The eight exported names stay as wrappers that warn and forward:
  `matrix_configure`, `matrix_send`, `matrix_poll`, `matrix_run`,
  `matrix_run_init`, `matrix_run_step`, `matrix_archive_all`,
  `matrix_request_flush`. Nothing about behaviour differs. They are
  scheduled for removal at 1.0.0.

- **Message records use the contract's field names**: `room_id` is
  `channel`, `event_id` is `id`, matching `chat.api::chat_message()`.
  `cfg$room_id` is unchanged -- it is a key in a JSON file on disk, and
  renaming it would be a config migration rather than a rename.

# corteza 0.7.1.18

- **The last `mx.*` calls are gone.** Credentials and current-state reads
  go through `chat.api`: `chat_matrix_config()`, `chat_config_save()`,
  `chat_matrix_configure()` and `chat_set_identity()` for the credential
  lifecycle, `chat_channels()`, `chat_history()`, `chat_pending()` and
  `chat_mark_read()` for the state the poll cursor cannot answer.

  The startup invite catch-up was the last raw sync, and it was raw
  precisely because an invitation is standing state that a cursor cannot
  deliver. `chat_pending()` is the verb for it.

  `mx.api` leaves Suggests, and `matrix_require_mx()` checks `chat.api`
  alone: repeating a downstream package's own requirements is asserting
  facts only that package can keep true. `mx.client` stays in Suggests
  because the integration tests stub it to drive a real relogin through
  the whole stack.

- **`chat_poll()` no longer returns `$client`.** corteza used to take the
  post-sync config out of it and rebuild a client per send, because a
  `/model` rename could rotate the access token and the config file the
  two shared was the only thing keeping them in step.
  `chat_set_identity()` leaves the rotation on the client that performed
  it, so there is one client for the poll and every send after it.

# corteza 0.7.1.15

- **Invitations ride the transport contract, and `res$raw` goes unread.**
  The poll loop reads `chat_poll()$invites` and joins with `chat_join()`.
  That was the last thing it took off the raw sync response, so the
  migration's stated finish line is reached: every message, decrypted
  message, reaction and invitation now arrives as a record.

  `matrix_invite_inviters()` is deleted. Reading who sent an invitation
  meant walking the stripped `invite_state`, which put Matrix sync-shape
  knowledge two packages away from the sync; `mx.client` owns it now and
  the record carries it.

  The operator gate is unchanged in behaviour and clearer in reporting: an
  invitation from someone not on the list and one whose sender the
  homeserver never named both refuse, and now say which is which.

  One invite that cannot be joined no longer risks the others. `chat_join()`
  raises so each failure is visible, and `matrix_accept_invites()` catches
  per room so a revoked invitation cannot take the whole poll with it.

  The startup catch-up still reads a raw sync. It needs a full no-since
  sync to see invitations issued while the bot was down, and `chat_poll()`
  only ever resumes from a cursor -- so it stays until the contract grows
  a verb for that. It goes through the same gate, so the operator policy
  is written once.

  Requires `chat.api >= 0.0.1.14` and `mx.client >= 0.2.0.4`.

# corteza 0.7.1.14

- **Room metadata rides the transport contract.** `mx_room_name()`,
  `mx_room_topic()` and `mx_room_members()` are gone from the Matrix
  loop, replaced by `chat_channel_info()` and `chat_members()`. Three of
  the nine remaining direct mx.api calls go with them.

  `matrix_new_session()` made three state reads and fetched the topic
  twice: once inside `matrix_room_cwd()` for the working directory, and
  again for the room description. It makes one now, and
  `matrix_room_cwd()` takes the topic rather than fetching one.

  `matrix_archive_session()`, `matrix_archive_all()` and
  `matrix_handle_flush_signal()` take a `chat` client where they took an
  `mx_sess`; `matrix_room_members_cached()` likewise. `matrix_archive_all()`
  is exported, so that is a signature change.

  A failed member lookup still keeps the previous cache rather than
  emptying it. `chat_members()` raises instead of returning `character()`
  precisely so the two can be told apart -- an empty room and an
  unanswerable question are different, and only one of them means the
  room emptied.

  Also drops the unused `mx_sess` parameter from
  `matrix_default_system()`, which no caller passed.

  Requires `chat.api >= 0.0.1.13`, enforced at runtime.

# corteza 0.7.1.13

- **Reaction approval rides the transport contract.**
  `matrix_reaction_approval()` no longer calls `mx.api::mx_react()` or
  runs its own private `mx_sync` loop; it uses `chat_react()` and
  `chat_poll()$reactions`. Two of the four direct mx.api sites are gone
  with it. The approve/deny vocabulary stays in corteza, where the prompt
  that teaches a user which emoji to tap also lives, and is now
  overridable per config via `approve_keys` / `deny_keys`.

  Three bugs fixed on the way:

  - **Approvals outside the default room could only time out.** The
    prompt went to the session's room but the verdict was read from
    `cfg$room_id`, so in every room but one no tap was ever seen.
  - **The fastest tap was the one most likely to be lost.** The baseline
    cursor was taken *after* the prompt was sent, so a reaction placed in
    between landed in the baseline sync and was discarded with it. The
    baseline is taken first now; nothing can react to an event that does
    not exist yet.
  - The poll loop runs on a client built `save_cursor = FALSE`, so its
    private cursor cannot be written over the bot's own position. This
    was not reachable before -- the old loop drove `mx_sync` directly and
    never touched the stored token -- but it becomes possible the moment
    the loop goes through `chat_poll()`, and it is the failure that would
    have silently eaten every message sent while a user was deciding.

  `mx_extract_reaction_verdict()`'s corteza wrapper is deleted: it
  delegated the approve/deny vocabulary to the transport package.

  Requires `chat.api >= 0.0.1.12` and `mx.client >= 0.2.0.3`, both
  enforced at runtime.

# corteza 0.7.1.12

- **End-to-end encryption moves into the chat.api Matrix adapter.**
  `R/matrix_crypto.R` is gone and `mx.crypto` has left Suggests.
  corteza no longer builds an Olm account, carries Megolm sessions,
  tracks which rooms are encrypted, or keeps a second send path for
  encrypted rooms. Setting `e2ee` in the Matrix config now enables
  encryption in the adapter, which encrypts and decrypts on both sides
  of `chat_poll()` / `chat_send()`; decrypted messages arrive in
  `chat_poll()$messages` like any other traffic, so the poll loop stops
  reading `m.room.encrypted` out of the raw sync. `matrix_poll()` drops
  its `crypto` argument and `matrix_run_init()` no longer returns a
  crypto context. Requires `chat.api >= 0.0.1.9`, enforced at runtime.

  corteza names no crypto store. The adapter derives one per Matrix
  device, `(user_id, device_id)`, and records that identity in the store
  so another device cannot open it. This replaces
  `dirname(config)/crypto`, which tied the device identity to wherever
  the config file sat, so a moved config silently minted a new device and
  lost every Megolm session with it.

- **`mx.client` floor raised to 0.2.0.2.** That release stops
  `mx_send_encrypted()` reporting success for a message nobody can
  read: it filters its own device by `(user_id, device_id)` rather than
  `device_id` alone, refuses to post when every recipient device was
  skipped, and treats a partial `/keys/query` or `/keys/claim` as
  unanswered rather than as a user with no devices. corteza does not
  call it directly, but every encrypted reply this loop sends goes
  through it.

- **CI installs and version-checks the Matrix stack.** It is all
  Suggests, and `_R_CHECK_FORCE_SUGGESTS_=false` let a run pass without
  any of it -- which it did, silently, because every Matrix test sits
  behind a `requireNamespace()` guard that skips the whole integration
  file. The workflow now installs `mx.api`, `mx.client`, and `chat.api`
  from source and fails if either declared floor is unmet, so the
  transport tests cannot skip themselves into a green tick.

# corteza 0.7.1.4

- **`Additional_repositories` declares the cornball drat.** `chat.api`
  is a Suggests and is not on CRAN, so CRAN could not resolve it and
  the check NOTE listed it with no availability line -- which blocked a
  release. It is now published at
  <https://cornball-ai.github.io/drat/> and declared here, so the NOTE
  reports `chat.api yes` against the declared repository, the form
  Writing R Extensions describes for exactly this case.

# corteza 0.7.1.3

- **Model badge fixes.** A display-name relogin left the caller holding
  the rejected token, so the next `/model` acknowledgement was dropped
  silently; `matrix_update_displayname()` now returns the refreshed
  config. Startup and `/clear` advertised `cfg$model` rather than the
  runtime override the next session is built with. The
  `mx.client >= 0.2.0` floor is now enforced at runtime, not only
  declared, so a stale installed copy fails loudly instead of losing
  the rename to a best-effort `tryCatch`.

# corteza 0.7.1.2

- **Opt-in Matrix model badge.** New `matrix_configure(model_badge =)`
  key: `"never"` (default, unchanged behavior), `"non_default"`, or
  `"always"`. When a badge applies, deterministic post-turn code — not
  the model — prepends a `⚡ <model> (<provider>)` first line to each
  reply and renames the bot to `<name> ⚡ <model>`, so a room session
  that `/model`-switched to a paid provider is visible on every message
  and in the sender line. In `"non_default"` mode silence means the
  configured default: the badge appears only while a switch is live and
  disappears on `/clear` or a process restart (both reset the profile
  name too). The display name is account-global, so with sessions in
  several rooms the most recent switch wins; the per-reply badge line
  is always room-accurate. `display_name` optionally sets the base name
  the rename builds on (default: the user-id localpart).
  The profile update goes through the new mx.client
  `mx_set_displayname()` (>= 0.2.0), which refreshes a rotated token
  instead of silently failing the rename.

# corteza 0.7.1.1

- **The Matrix message plane rides the chat.api transport contract.**
  Sends, receives, and the typing indicator go through `chat_send()`,
  `chat_poll()`, and `chat_typing()` instead of calling mx.client and
  mx.api directly. Encrypted rooms stay on the mx.crypto path, and
  corteza still reads invites, reactions, and encrypted events off the
  raw sync response itself. `chat.api` is a Suggests, gated by the same
  check as the rest of the Matrix stack, so it is only needed on hosts
  that enable the channel.

# corteza 0.7.1

## New

- **`openai_compatible` provider for generic OpenAI-compatible
  endpoints** (#149). Point corteza at OpenRouter, DeepSeek, DeepInfra,
  a local proxy, or a corporate gateway via a `base_url` config key or
  the `--base-url` CLI flag (falling back to
  `OPENAI_COMPATIBLE_BASE_URL`). The model id is passed through
  untouched and is required; the API key comes from
  `OPENAI_COMPATIBLE_API_KEY` or `OPENAI_API_KEY`, and a keyless
  gateway works with neither. A missing endpoint or model is reported
  at session setup rather than at the first request. `turn()` applies
  the endpoint through `llm.api::llm_base()` scoped to each agent call,
  so a mid-session `/provider` switch or a subagent on another provider
  cannot inherit it. Requires `llm.api >= 0.1.9`. See
  `vignette("configuration")`.

- **Provider-native web search.** The agent loop enables
  `llm.api::agent()`'s server-side web search for the hosted providers
  that support it (`anthropic`, `anthropic_claude`, `openai`,
  `openai_codex`, `moonshot`). On by default, no Tavily key required;
  the model searches inside its own turn. Toggle with `--web-search` /
  `--no-web-search`, `chat(web_search = )`, or a `web_search` key in
  `.corteza/config.json`. The Tavily `web_search` tool remains the
  fallback for local models.

- **Matrix `/model` lists available models and switches by number.**
  Bare `/model` (or `//model` where the client eats single slashes)
  replies with a numbered menu instead of echoing the current settings:
  the configured default, the live local Ollama inventory, and any
  hosted models declared via the new `matrix_configure(models =)` key
  (`"model provider"` strings, since hosted providers cannot be
  enumerated remotely). `/model 3` switches the room session to entry
  3, setting model and provider together, so switching from a phone
  client never needs a typed model name. The typed form
  `/model <name> [provider]` is unchanged, and an unreachable Ollama
  degrades the menu to the configured entries.

## Matrix access control

- **Private conversations and invites are restricted to configured
  operators.** The new `operators` key lists the Matrix ids allowed a
  one-on-one conversation with the bot. A room with one human is
  answered only when that human is an operator; everyone else gets
  silence rather than a mention-gated session. Invites are gated on the
  same list, read from the inviter in the stripped `invite_state`, and
  an invite whose inviter cannot be determined is refused. Group rooms
  are unaffected. Leaving `operators` unset preserves the previous
  behavior.

- **Reply gating counts humans, not members.** Rooms whose only non-bot
  member is one human are answered without a mention; rooms with two or
  more humans require a mention (replies count, since clients put the
  replied-to user in `m.mentions`) or an open engagement window: after
  the bot answers someone, that person's plain messages keep getting
  replies for 5 minutes, ending early when they address someone else.
  Other bot accounts are declared via the new `matrix_configure(bots =)`
  parameter; their messages always require a mention. Room membership is
  cached per session and refreshed on unknown senders or every 10
  minutes, replacing the boot-time DM snapshot that left rooms stuck on
  stale gating after membership changed. With `bots` unset, behavior
  matches the old member-count rule minus the staleness; during a
  members-API outage the gate fails open for human senders instead of
  going silent.

## Matrix fixes

- **Non-triggering messages are ingested instead of dropped.** In a room
  with more than one human, a message that did not warrant a reply was
  marked seen, given a read receipt, then dropped with a bare `next`:
  never appended to the session's history and, because it was already
  marked seen, never reconsidered. The agent genuinely never saw other
  people's non-mention messages in a busy room, even though it had
  "read" them. Gated messages are now appended to history as context;
  the reply gate and its bot-loop protection are unchanged, only
  ingestion is. Turns are attributed with a `[sender]` prefix in rooms
  with more than one human (both the live poll and the startup
  backfill), and known bot senders are labeled even in one-human rooms,
  so the model can tell speakers apart. A lone-human DM is unchanged.

- **Rooms load project context into the room system prompt.**
  `matrix_new_session()` passed a non-NULL `system` to `session_setup()`
  with `load_project_context = FALSE`, so Matrix bots ran without the
  project briefing, agent context files, `.corteza` `context_files`, and
  package tool docs that other channels receive. The new
  `matrix_room_system()` composes the Matrix identity header with
  `load_context()` for the room cwd.

## Archiving

- **Archives are built from a Matrix-visible event ledger, not from
  provider history.** History holds tool calls and tool results that
  were never Matrix events, so a restarted process (which rebuilds
  history from the server's visible messages) could not align with what
  it had archived, and tool-using rooms re-archived their whole backfill
  on every restart. Sessions keep a transcript of
  `{event_id, role, content}` and a queue of pending events, written
  where events are seen or sent and drained once archived. Archival keys
  on Matrix event ids, recognized against a bounded per-room tail of
  already-archived ids. Transcripts no longer contain tool turns.

- **Archive progress is persisted per room instead of tracked in
  memory.** `matrix_archive_session()` used `ingested_through`, an index
  into the in-memory session history, which reset on every restart while
  startup backfill refilled history from the server. Each restart
  therefore re-archived the backfilled tail. Progress is now an ordered
  key tail persisted per room under `CORTEZA_STATE_DIR`, advanced only
  after a successful ingest, so a failed archive retries rather than
  loses turns.

- **Archives are keyed by room id, not display name.** Room display
  names and avatars are mutable Matrix state, so archived transcripts
  use the global Matrix room id for `title` and `source`. The room name
  is retained only as archive-time metadata, which keeps renames from
  splitting one chat into multiple raw archive identities.

# corteza 0.7.0

CRAN release consolidating the 0.6.9.1-0.6.9.8 development cycle plus a
runtime-hardening batch. Last CRAN release: 0.6.9. Per-cycle detail follows.

## Highlights since 0.6.9

- **Matrix loop split** into `matrix_run_init()` / `matrix_run_step()`, with
  the channel delegating session and transport plumbing to `mx.client`.
- **`anthropic_claude` provider** (Claude on a subscription via OAuth),
  requiring llm.api (>= 0.1.8).
- **Tool-timeout hardening.** bash/cmd/run_r/run_r_script and the network
  tools (`fetch_url`/`web_search`, now bounded by curl's own timeout) are no
  longer wrapped in an R-level `setTimeLimit`; the residual flush moved to
  `skill_run` entry so a validation/dry-run early return can't leak a queued
  interrupt (#142, #139).
- **Narration guard.** Every tool outcome (executed, denied, declined,
  dry-run, task) routes through one nudge finalizer; the streak resets at
  each agent-run boundary.
- **Approval-prompt hardening.** Model-controlled fields rendered into the
  approval prompt and the model-visible history (paths, tool names, args,
  policy reasons) are sanitized so a crafted value can't forge a prompt line.
  Untrusted Matrix room metadata is sanitized and framed as informational in
  the system prompt.
- **`/compact` survives provider-native history** (Anthropic, OpenAI chat, and
  role-less OpenAI Codex Responses entries), including tool calls and outputs.

# corteza 0.6.9.8

## Changes

- `/compact` now renders Anthropic, OpenAI chat, and role-less OpenAI Codex
  Responses history safely, including tool calls and tool outputs. This prevents
  provider-native history entries from wedging compaction.

- Matrix documentation now matches the `anthropic_claude` provider default and
  documents the optional `crypto` polling context.

# corteza 0.6.9.7

## Changes

- Accept the `anthropic_claude` provider, which drives Claude on a Claude
  subscription (OAuth) via llm.api instead of an API key. Added to
  `matrix_configure()`'s provider whitelist and routed through the Anthropic
  message shape for interrupt-time tool-result repair. Requires llm.api
  (>= 0.1.4.3).

# corteza 0.6.9.6

## Changes

- The Matrix long-poll loop is split into two new exports,
  `matrix_run_init()` (build the session registry, catch up on invites,
  backfill history, init E2EE) and `matrix_run_step(state, timeout)` (one
  poll-and-reply iteration). `matrix_run()` is now a thin wrapper over
  them. Lets an external scheduler own the main process and interleave
  the Matrix poll with its own work. No behavior change for
  `matrix_run()`.

# corteza 0.6.9.5

## Changes

- Matrix channel now delegates config persistence, session construction,
  message sending, sync/cursor handling, relogin, and encrypted-room
  detection to the `mx.client` package instead of hand-rolled plumbing
  over `mx.api`. Behavior is unchanged; ~37 fewer lines in the adapter.

# corteza 0.6.9.1

## Changes

- The system prompt now includes a runtime-guidance block describing the
  live, persistent R session, `run_r`/`run_r_script` semantics, and that
  the bash tool makes corteza a general-purpose agent rather than R-only.

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
  CLAUDE.md`, `Run command in /srv/work/repo`); the redundant
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
  grant. Aligns with the rest of the cornball.ai toolchain (saber,
  pensar, hacer). The LICENSE stub file is removed; Apache 2.0
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
