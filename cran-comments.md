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

0.8.1 batches the 0.8.0.1 to 0.8.0.42 development cycles. No exported
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

* Ubuntu 24.04 LTS, R 4.6.1, from an empty home directory, once as CRAN
  runs the tests and once with every test file: Status: OK, the home
  still empty.
* GitHub Actions, Ubuntu and macOS, R-release: Status: OK.
* Windows 10, R 4.6.0: Status: 1 NOTE (the note above).
* win-builder, R-release and R-devel: to be run just before submission.

## Notes

* Eighteen slow test files, which start worker processes, are skipped on
  CRAN to keep the check near 5 minutes on Windows. Continuous
  integration runs every file.
* 20 of 109 help pages keep `\dontrun`. 6 need a Matrix homeserver and a
  bot account (`bot_configure()` also writes the bot's configuration,
  and `bot_run()` blocks). 1, `bot_archive_all()`, acts on a running
  bot's archive state: it reads the bot's configuration and files the
  bot's pending ranges through the host's hook. 8 are deprecated aliases
  of those 7. 3 need a model provider's API key and the network
  (`turn()`, `subagent_query()`, `subagent_collect()`). `serve()` starts
  a server that blocks, and `mcp_tool_executor()` needs one running. The
  examples that can run offline in the temporary directory now do.
* `worker_init()` calls `setwd()` without restoring it. It runs inside a
  `callr` worker process, never in the user's session, and sets that
  worker's directory for its lifetime, so every tool the worker runs
  starts there.
* The agent evaluates R code in the global environment only at the
  user's request: through `run_r` in a session the user started, or the
  `/r` command the user types. Letting an agent work in the user's live
  session is the package's purpose.
* The package reads API keys from environment variables only. No
  credentials are bundled, and nothing is written outside `tempdir()`
  during checks: the test runner redirects R's user cache, data, and
  config directories into `tempdir()`.
* Optional integrations (chat rooms via 'chat.api' and 'mx.client',
  voice via 'rgrpc' and 'RProtoBuf', knowledge-vault archiving via
  'pensar', clipboard via 'clipr') are in Suggests and guarded at every
  call site, so the package is fully functional without them.
* There are no reverse dependencies on CRAN.
