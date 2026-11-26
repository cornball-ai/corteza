## Resubmission after archival

corteza 0.8.0 was archived on 2026-11-14 for a policy violation: its
checks created `~/bot` in the home directory of the check machine.

**What was wrong.** A chat room with no working directory set used
`~/<bot name>`, and `bot_default_cwd()` created it on first use. Two test
files reached that function with a test bot named `@bot:ex`, so the check
made `~/bot`. With the slow test files on (CRAN skips them), the suite
made 3 more the same way: `~/agent`, `~/courier` and `~/fixture-bot`. Our
own checks missed it: `tests/tinytest.R` redirected the three
`R_USER_*_DIR` roots, which a literal `~/` path ignores, and `~/bot`
already existed on the development machine, so later runs created
nothing new there.

**What changed.**

* No function creates a directory in the home filespace. A room works in
  a directory the bot's operator gives it (`cwd`, or `/cd` inside the
  bot's `project_roots`), each of which must already exist; without one,
  it works in a scratch folder under `tempdir()`.
* `install_cli()` no longer writes to `~/bin` by default. With no
  `path`, it installs under `tools::R_user_dir("corteza", "data")`.
* The command-line interface keeps its saved approvals under
  `tools::R_user_dir("corteza", "config")`, and no longer in a folder it
  made inside the project.

**How it is tested now.** Continuous integration runs
`R CMD check --as-cran --run-donttest` with an empty home directory and a
bare environment, once as CRAN runs it and once with every test file
(`NOT_CRAN=true`). It fails when anything is left in the home directory
or when the check reports new files in other directories, the temp
directory or the check directory. A second script runs each test file
alone with an empty home and lists any that writes there.

## Submission summary

Resubmission of 'corteza' (0.8.0 -> 0.8.1), an agent runtime that lets
Large Language Models (LLMs) drive an R session through a policy-gated
tool-use loop. Entry points: an interactive console
read-eval-print-loop (`chat()`), a shell command-line interface
(`corteza`), and a Model Context Protocol (MCP) server (`serve()`) for
external clients like Claude Code or Codex.

0.8.1 batches the 0.8.0.1 to 0.8.0.53 development cycles. No exported
function was removed and one was added, `sessions_prune()`. New
arguments come last in their functions' argument lists, so existing
calls still run. Several defaults changed; NEWS.md lists them.

## Dependency note

Every dependency is on CRAN, as in 0.8.0.

* `Imports: llm.api (>= 0.2.0)` and `saber (>= 0.8.0)`.
* `Suggests: chat.api (>= 0.2.0)` and `mx.client (>= 0.3.0)`, by the same
  maintainer.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE: "Package was archived on
  CRAN", with the archival comment "Creates and leaves behind '~/bot'".
  This submission is the fix.

`R CMD check --as-cran --run-donttest` on:

* Ubuntu 24.04.5 LTS, R 4.6.1, using released dependencies including
  curl 8.2.0. All four runs returned Status: OK with incoming and network
  clock checks disabled.
  The CRAN selection passed 6,974 assertions and full mode passed 13,749.
  With `_R_CHECK_DEPENDS_ONLY_=true`, 6,388 assertions passed. With
  SIGINT ignored and umask 077, 6,974 assertions passed.
* Windows 10, R 4.6.0, on the same source tarball with incoming checks
  enabled. Both runs returned Status: 1 NOTE (the archival notice above).
  The CRAN selection passed 6,855 assertions in 51 seconds. The entire
  check took 4 minutes 6 seconds. Full mode passed 13,244 assertions.
* Each run used a separate empty home and temporary directory. Both were
  empty afterwards. All three R checks for leftover files also passed.
* The win-builder R-devel check of this source tarball is pending upload.
* Ubuntu and macOS GitHub Actions passed on earlier development builds.

## Notes

* The default selection still runs the empty-home regression for `~/bot`.
  To shorten check time, 37 longer test files covering workers, rooms,
  approvals, tools and context now run in full mode. CI selects the full
  suite with `NOT_CRAN=true`.
* 20 help pages keep `\dontrun`. 6 need a Matrix homeserver and a
  bot account (`bot_configure()` also writes the bot's configuration,
  and `bot_run()` blocks). 1, `bot_archive_all()`, acts on a running
  bot's archive state: it reads the bot's configuration and files the
  bot's pending ranges through the host's hook. 7 are deprecated aliases
  of those 7. 3 need a model provider's API key and the network
  (`turn()`, `subagent_query()`, `subagent_collect()`). `serve()` starts
  a server that blocks, and `mcp_tool_executor()` needs one running.
  `voice_serve()` blocks as well, and needs 'rgrpc' and a media
  allocator. The examples that can run offline in the temporary
  directory now do.
* Console output. `chat()`, the `corteza` command and their slash
  commands are interactive read-eval-print loops and write to the
  console with `cat()` by design. Elsewhere, progress and diagnostics
  print only when `getOption("corteza.verbose", interactive())` is
  `TRUE`, or go to standard error or a log file. `serve()` and
  `corteza acp` write protocol messages to standard output, as the Model
  Context Protocol and the Agent Client Protocol require.
* `tool_installed_packages()`, the agent's tool for listing what is
  installed, calls `installed.packages()`: listing every package is its
  purpose. Its test does not run on CRAN. `tool_r_help()` finds a single
  package with `system.file()`.
* `worker_init()` calls `setwd()` without restoring it, and so does the
  background process `archival_summarize_bg()` starts. Both run in a
  `callr` process, never in the user's session. `worker_init()` sets a
  worker's directory for its lifetime, so every tool the worker runs
  starts there.
* The agent evaluates R code in the global environment only at the
  user's request: through `run_r` in a session the user started, or the
  `/r` command the user types. `run_r` also places the outputs it has
  stored there under hidden names (`.h_001`, ...), so the code can use
  them, and removes them when they are cleared. Letting an agent work in
  the user's live session is the package's purpose.
* Model providers' API keys come from environment variables only. A
  chat-room bot's login, which `bot_configure()` saves, and the voice
  allocator's token, which the user sets, are kept in the user's
  configuration directory (`tools::R_user_dir()`). No credentials are
  bundled, and nothing is written outside `tempdir()` during checks: the
  test runner redirects R's user cache, data, and config directories
  into `tempdir()`.
* Optional integrations (chat rooms via 'chat.api' and 'mx.client',
  voice via 'rgrpc' and 'RProtoBuf', knowledge-vault archiving via
  'pensar', clipboard via 'clipr') are in Suggests and guarded at every
  call site, so the package is fully functional without them.
* There are no reverse dependencies on CRAN.
