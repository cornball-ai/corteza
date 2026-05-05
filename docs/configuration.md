# Configuration Guide

How to configure corteza for your workflow.

---

## config files

Two JSON files, merged: project overrides global.

| Layer | Path |
|-------|------|
| Global | `tools::R_user_dir("corteza", "config")/config.json` |
| Project | `.corteza/config.json` |

**Merge semantics:** project keys replace global keys. Top-level replace; no deep merge.

Example `.corteza/config.json`:

```json
{
  "provider": "ollama",
  "model": "llama3.2",
  "context_files": ["README.md", "PLAN.md"],
  "approval_mode": "ask",
  "workspace": {
    "scan_globalenv": true
  }
}
```

---

## CLI flags

```bash
corteza [options]
```

| Flag | Description | Default |
|------|-------------|---------|
| `--provider <p>` | LLM provider: `anthropic`, `openai`, `moonshot`, `ollama` | `anthropic` |
| `--model <name>` | Model name | provider default |
| `--port <n>` | MCP server port (ignored by CLI) | `7850` |
| `--tools <filter>` | Tool filter: `core`, `file`, `code`, `git`, `r`, `data`, `web`, `chat`, or comma-separated | all |
| `--session <key>` | Session key; resumes if exists | none |
| `--resume` | Resume most recent session | `false` |
| `--list` | List sessions and exit | `false` |
| `--dry-run` | Preview tool calls without executing | `false` |
| `--trace` | Print structured tool-call events to stderr | `false` |
| `--help` | Show help and exit | - |

Flags override config values for the current run.

---

## JSON config keys

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
| `context_include_soul` | boolean or null | null | Include `SOUL.md` in context (null = use saber default) |
| `context_include_user` | boolean or null | null | Include `USER.md` in context (null = use saber default) |

### Safety

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `approval_mode` | string | `"ask"` | `"ask"`, `"allow"`, or `"deny"` |
| `dangerous_tools` | string[] | `["bash", "run_r", "run_r_script", "write_file", "replace_in_file", "base::writeLines"]` | Tools that require approval when `approval_mode` is `"ask"` |
| `permissions` | object | `{}` | Per-tool overrides, e.g. `{"bash": "deny"}` |
| `denied_paths` | string[] | `["~/.ssh", "~/.gnupg", "~/.aws", "~/.config/gcloud", "~/.kube", "~/.docker"]` | Paths always blocked by policy |
| `allowed_paths` | string[] or null | null | If set, only these paths are accessible |

### Skills

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `skill_paths` | string[] | `[]` | Extra directories to scan for `SKILL.md` files |
| `skill_packages` | object[] | `[{"package":"base", ...}, {"package":"utils", ...}]` | R packages registered as tools |
| `skill_timeout` | integer | `30` | Default skill execution timeout in seconds |

### Diagnostics

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `dry_run` | boolean | `false` | Preview tools without executing |
| `trace` | boolean | `false` | Emit structured trace events |

### Rate limiting

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `rate_limits` | object | `{}` | Per-provider limits, e.g. `{"anthropic": {"tokens_per_hour": 100000}}` |

### Subagents

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `subagents.enabled` | boolean | `true` | Enable subagent commands (`/spawn`, `/ask`, `/kill`) |
| `subagents.max_concurrent` | integer | `3` | Max parallel subagents |
| `subagents.timeout_minutes` | integer | `30` | Subagent kill timeout |
| `subagents.allow_nested` | boolean | `false` | Allow subagents to spawn subagents |
| `subagents.default_tools` | string[] | `["base::readLines", "base::writeLines", "bash", "grep_files"]` | Tools available to subagents |
| `subagents.base_port` | integer | `7851` | Starting port for subagent MCP servers |

### Workspace

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `workspace.enabled` | boolean | `true` | Enable managed workspace |
| `workspace.budget_chars` | integer | `32000` | Context budget in characters |
| `workspace.capture_results` | boolean | `true` | Capture large results as handles |
| `workspace.max_result_size` | integer | `50000` | Max result size before handle promotion |
| `workspace.scan_globalenv` | boolean | `true` | Scan `.GlobalEnv` on startup for workspace objects |
| `workspace.scan_max_bytes` | integer | `52428800` | Max bytes to scan from `.GlobalEnv` (50 MB) |
| `workspace.max_object_summary_chars` | integer | `2000` | Max summary length per object |

### Channels

#### Signal

Signal channel requires `mx.api` (Suggests) and `signal-cli`.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `channels.signal.enabled` | boolean | `false` | Enable Signal bot |
| `channels.signal.httpHost` | string | `"127.0.0.1"` | signal-cli HTTP host |
| `channels.signal.httpPort` | integer | `8080` | signal-cli HTTP port |
| `channels.signal.httpUrl` | string or null | null | Optional full URL (overrides host/port) |
| `channels.signal.account` | string | null | Signal account number (E.164) |
| `channels.signal.allowFrom` | string[] | null | Allowed sender numbers (E.164) |
| `channels.signal.cliPath` | string or null | null | Custom path to `signal-cli` |
| `channels.signal.textChunkLimit` | integer | `4000` | Max characters per outbound chunk |
| `channels.signal.chunkMode` | string | `"length"` | `"length"` or `"newline"` |

### Matrix

Matrix channel requires `mx.api` (Suggests). Configured via `matrix_configure()`, stored separately from main JSON config.

```r
corteza::matrix_configure(
  server = "https://matrix.example.com",
  user = "corteza_bot",
  password = "verysecure",
  room = "#general:example.com",
  model = "llama3.2",    # optional
  provider = "ollama"    # optional
)
```

Config is stored at `tools::R_user_dir("corteza", "config")/matrix.json`.

### Legacy memory

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `legacy_memory_tools_enabled` | boolean | `false` | Show `/remember`, `/recall`, `/flush` commands |
| `memory_flush_enabled` | boolean | `false` | Auto-flush memories before compaction |
| `context_include_memory_logs` | boolean | `false` | Include daily memory logs in context |

---

## Slash commands

In-chat commands prefixed with `/`.

| Command | Description |
|---------|-------------|
| `/quit`, `/exit` | Exit corteza |
| `/status` | Show runtime and session status |
| `/doctor` | Check provider, git, MCP, context health |
| `/tools` | List available tools |
| `/diff [ref]` | Show git diff against HEAD or ref |
| `/review [ref]` | Review local changes with LLM |
| `/config` | Show active runtime configuration |
| `/permissions` | Show tool approval and sandbox settings |
| `/clear` | Clear conversation (keeps session) |
| `/compact` | Summarize conversation to free context |
| `/sessions` | List sessions |
| `/context` | Show live context usage and loaded files |
| `/model <name>` | Switch model |
| `/provider <p>` | Switch provider |
| `/dryrun` | Toggle dry-run mode |
| `/trace [N]` | Show last N tool executions |
| `/skill list` | List installed skills |
| `/skill install <path\|url>` | Install a skill |
| `/skill remove <name>` | Remove a skill |
| `/skill test <path>` | Run skill tests |
| `/spawn <task>` | Spawn a subagent |
| `/agents` | List active subagents |
| `/ask <id> <prompt>` | Query a subagent |
| `/kill <id>` | Terminate a subagent |
| `/remember <fact> #tags` | Remember with auto-categorization |
| `/remember --global <fact>` | Remember globally |
| `/recall <query>` | Search memories |
| `/recall --tags` | List memory tags |
| `/flush` | Flush durable memories to daily log |
| `/last [N]` | Show Nth most recent tool output |
| `/outputs` | List recent tool outputs |
| `/help` | Show help |

---

## Skills

Skills are `SKILL.md` files loaded at startup.

**Search paths:**

| Scope | Path |
|-------|------|
| Global | `~/.corteza/skills/` |
| Project | `.corteza/skills/` |

Both nested (`skill/SKILL.md`) and flat (`skill.md`) layouts work. Built-in R skills are always registered.

**Package skills:** register R packages as tools via `skill_packages` in config (see JSON config keys above).

---

## MCP server

Expose corteza tools to external MCP clients (Claude Code, Claude Desktop).

```r
corteza::serve(port = 7850, cwd = getwd())
```

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

Tools execute in a persistent R session. Objects persist across calls, packages stay loaded.

---

## Session tuning

Programmatic session construction.

```r
library(corteza)

s <- new_session(
  channel = "cli",        # "cli", "console", or "matrix"
  provider = "anthropic",
  max_turns = 20L,
  verbose = FALSE
)

# Run one turn
result <- turn("What packages are loaded?", session = s)
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `channel` | string | `"cli"` | `"cli"`, `"console"`, `"matrix"` |
| `provider` | string | `"anthropic"` | LLM provider |
| `max_turns` | integer | `10` | Max LLM tool-use turns per call |
| `verbose` | boolean | `FALSE` | Print tool-call progress |
| `tools_filter` | string[] or null | null | Restrict available tools |
| `system` | string or null | null | System prompt override |
| `approval_cb` | function or null | null | Custom approval callback |

---

## systemd service

`matrix_run()` is designed to run as a systemd user unit.

### User service file

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

---

## Environment variables

| Variable | Required for | Description |
|----------|-------------|-------------|
| `ANTHROPIC_API_KEY` | Anthropic provider | API key |
| `OPENAI_API_KEY` | OpenAI provider | API key |
| `MOONSHOT_API_KEY` | Moonshot provider | API key |
| `TAVILY_API_KEY` | `web_search` tool | Optional |
| `NO_COLOR` | CLI | Disable ANSI colors |
| `FORCE_COLOR` | CLI | Force ANSI colors |
| `CORTEZA_STATE_DIR` | Matrix bot | Out-of-band signal directory |

Set API keys in `~/.Renviron`:

```
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
MOONSHOT_API_KEY=sk-...
TAVILY_API_KEY=tvly-...
```

---

## R options

| Option | Default | Description |
|--------|---------|-------------|
| `corteza.model` | null | Default model (overrides provider default) |
| `corteza.local_models` | `c("gpt-oss:120b", "gpt-oss:20b")` | Candidates for `default_local_model()` |
| `corteza.max_turns` | null | Default for `max_turns` in `chat()` |
| `corteza.trace` | `FALSE` | Enable structured trace events |

Set in `~/.Rprofile` or per-session via `options()`.

---

*Guide version: 0.6.3*
