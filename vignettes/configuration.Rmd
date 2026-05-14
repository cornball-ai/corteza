<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Configuration}
-->
---
title: Configuration
---

<img src="../man/figures/corteza.png" alt="corteza logo" align="right" width="160" />

# Configuration

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
| `--provider <p>` | LLM provider: `anthropic`, `openai`, `moonshot`, `ollama` | `anthropic` |
| `--model <name>` | Model name | provider default |
| `--tools <filter>` | Tool filter: `core`, `file`, `code`, `git`, `r`, `data`, `web`, `chat` | all |
| `--session <key>` | Session key; resumes if exists | none |
| `--resume` | Resume most recent session | `false` |
| `--list` | List sessions and exit | `false` |
| `--dry-run` | Preview tool calls without executing | `false` |
| `--trace` | Print structured tool-call events to stderr | `false` |
| `--help` | Show help and exit | - |

Flags override config values for the current run.

## JSON config keys

All keys shown with type and default, current as of corteza 0.6.3. Most defaults live in `R/config.R::load_config()`; a few (memory keys and the CLI port fallback) live in `inst/bin/corteza`.

### Core

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `provider` | string | `"anthropic"` | LLM provider |
| `model` | string or null | null | Model name (null = provider default) |
| `port` | integer | `7850` | MCP server port |

### Context

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `context_files` | string[] | `[]` | Extra files to load into the system prompt |
| `context_warn_pct` | integer | `75` | Token-usage % to start showing warnings |
| `context_high_pct` | integer | `90` | Token-usage % for orange indicator |
| `context_crit_pct` | integer | `95` | Token-usage % for red indicator + hint to `/clear` |
| `context_compact_pct` | integer | `90` | Auto-compaction threshold |
| `context_include_soul` | boolean or null | null | Include `SOUL.md` (null = saber default) |
| `context_include_user` | boolean or null | null | Include `USER.md` (null = saber default) |

### Safety

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `approval_mode` | string | `"ask"` | `"ask"`, `"allow"`, or `"deny"` |
| `dangerous_tools` | string[] | `["bash", "run_r", "run_r_script", "write_file", "replace_in_file", "base::writeLines"]` | Tools requiring approval |
| `permissions` | object | `{}` | Per-tool overrides, e.g. `{"bash": "deny"}` |
| `denied_paths` | string[] | `["~/.ssh", "~/.gnupg", "~/.aws", "~/.config/gcloud", "~/.kube", "~/.docker"]` | Blocked paths |
| `allowed_paths` | string[] or null | null | If set, only these paths allowed |

### Skills

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `skill_paths` | string[] | `[]` | Extra directories for `SKILL.md` files |
| `skill_packages` | object[] | `[{"package":"base", ...}, {"package":"utils", ...}]` | R packages registered as tools |
| `skill_timeout` | integer | `30` | Default skill execution timeout (seconds) |

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

#### Matrix

Matrix channel requires `mx.api` (Suggests). Configured via a separate helper, not the main JSON config.

```r
corteza::matrix_configure(
  server = "https://matrix.example.com",
  user = "corteza_bot",
  password = "verysecure",
  room = "#general:example.com",
  model = "llama3.2",
  provider = "ollama"
)
```

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
| `/model <name>` | Switch model |
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

## Skills

Skills are `SKILL.md` files loaded at startup. Built-in R skills are always registered.

**Search paths:**

| Scope | Path |
|-------|------|
| Global | `tools::R_user_dir("corteza", "data")/skills/` |
| Project | `.corteza/skills/` |

Nested (`skill/SKILL.md`) and flat (`skill.md`) layouts both work.

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

## systemd service

`matrix_run()` is designed for a systemd user unit.

`~/.config/systemd/user/corteza-matrix.service`:

```ini
[Unit]
Description=corteza Matrix bot
After=network.target

[Service]
Type=simple
ExecStart=Rscript -e 'corteza::matrix_run()'
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
