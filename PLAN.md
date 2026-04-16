# llamaR Development Plan

## Vision

A CLI-first AI agent runtime for R. Self-hosted, model-agnostic, tinyverse.

## Guiding Principles

- Base R over tidyverse
- Minimal dependencies
- MCP-native (tools are portable)
- Local-first (Ollama priority)
- Composable over monolithic

---

## Phase 1: Core Loop ✅

Get the fundamental agent cycle working.

### 1.1 R Execution Engine
- [x] `run_r()` — execute R code, capture output + errors
- [x] Stateful session — environment persists across calls
- [x] `r_help()` — query R documentation
- [x] `installed_packages()` — list/search packages

### 1.2 LLM Backend (llm.api)
- [x] Anthropic support (Claude)
- [x] OpenAI support
- [x] **Ollama support** — priority for local inference
- [x] Tool/function calling interface

### 1.3 CLI Agent (`llamar`)
- [x] Interactive REPL loop
- [x] Message → LLM → tool calls → response cycle
- [x] Basic conversation history (in-memory)
- [x] Clean exit handling

### 1.4 File Operations
- [x] `read_file()` / `write_file()` / `list_files()` / `grep_files()`
- [x] Working directory awareness

---

## Phase 2: MCP Foundation ✅

Make llamaR speak the protocol.

### 2.1 MCP Server
- [x] `llamaR::serve()` — expose tools via MCP
- [x] JSON-RPC over stdio and TCP socket
- [x] Tool discovery (`tools/list`)
- [x] Tool invocation (`tools/call`)
- [x] Works with Claude Desktop, Claude Code
- [x] Tool category filtering (`--tools core`, `--tools file,git`)

### 2.2 MCP Client (in llm.api)
- [x] `mcp_connect()` — connect to external MCP servers
- [x] `mcp_tools()` — list available tools
- [x] Tool call forwarding
- [ ] Aggregate tools from multiple servers

### 2.3 Skill System ✅
- [x] SKILL.md format — portable skill definitions
- [x] `register_skill()` / `list_skills()` — skill registry
- [x] Skills export MCP-compatible tool definitions
- [x] Built-in R skills registered on server startup
- [x] User skills loaded from `~/.llamar/skills/` and `.llamar/skills/`
- [x] `/skill install`, `/skill remove`, `/skill test` — CLI management
- [x] Skill docs loaded into system prompt context

---

## Phase 3: Context & Memory ✅

Make the agent remember.

### 3.1 Conversation Persistence ✅
- [x] JSONL transcripts + JSON metadata (openclaw-compatible)
- [x] Session IDs (UUID format)
- [x] `llamar --session <key>` to create or resume
- [x] `llamar --list` / `/sessions` to list sessions
- [x] Global storage (`~/.llamar/agents/main/sessions/`)
- [x] Tool execution trace per session

### 3.2 Context Injection ✅
- [x] Global context: SOUL.md, USER.md, MEMORY.md from `~/.llamar/workspace/`
- [x] Project briefing via `saber::briefing()` and `saber::agent_context()`
- [x] User-specified project files via `context_files` config key (default: empty)
- [x] `/context` command to show loaded files

### 3.3 Long-term Memory ✅
- [x] `/remember` with tags and auto-categorization
- [x] `/recall` with keyword and tag search
- [x] Project-scoped (`<cwd>/.llamar/MEMORY.md`) and global (`~/.llamar/workspace/MEMORY.md`)
- [x] Daily memory logs (`~/.llamar/workspace/memory/YYYY-MM-DD.md`)
- [x] `/flush` — manual memory flush to daily logs
- [x] Pre-compaction auto-flush
- [x] SQLite FTS5 memory index
- [x] Claude Code session import (`memory_import_claude`)
- [ ] Auto-inject relevant memories into context (RAG)

### 3.4 Context Compaction ✅
- [x] `/compact` — manual conversation summarization
- [x] Auto-compact when context usage exceeds threshold (default 80%)
- [x] Context usage tracking with color-coded indicator
- [x] Configurable thresholds (`context_warn_pct`, `context_high_pct`, etc.)

---

## Phase 4: Channels

Messaging integrations. Personal use first.

### 4.1 Signal ✅
- [x] signal-cli integration (`llamar-signal`)
- [x] Inbound message handling
- [x] Outbound replies
- [x] Allowlist for senders

### 4.2 iMessage (macOS only)
- [ ] AppleScript or `imessage-cli` bridge
- [ ] Same inbound/outbound pattern

### 4.3 Future Channels
- [ ] Telegram
- [ ] Discord
- [ ] Slack

---

## Phase 5: Proactive Behavior

Let the agent initiate. Earn trust first.

### 5.1 Scheduled Tasks (removed in 0.4.0)

The SQLite-backed scheduler was removed in 0.4.0 in favor of cron. Users
who want recurring agent runs invoke `llamar` from a crontab.

### 5.2 Event Triggers
- [ ] File watchers
- [ ] Webhook endpoints
- [ ] Email (Gmail Pub/Sub pattern)

### 5.3 Autonomous Actions (partial)
- [x] Tool approval gate (allow once / allow always / deny)
- [x] Project-local approval persistence (`.llamar/approvals.json`)
- [x] Tool execution trace with timing and approval audit
- [x] Dry-run mode (`/dryrun`, `--dry-run`)

---

## Phase 6: Subagents ✅

Parallel work via child agent processes.

- [x] `subagent_spawn()` — fork agent for a task
- [x] `subagent_query()` — send prompts to running subagent
- [x] `subagent_kill()` — terminate subagent
- [x] `/spawn`, `/agents`, `/ask`, `/kill` CLI commands
- [x] MCP tools: `spawn_subagent`, `query_subagent`

---

## Phase 7: Voice Mode (removed in 0.4.0)

Speech input/output via local TTS/STT servers. Shipped in 0.3.0, removed in
0.4.0 to drop stt.api and tts.api from the dependency surface. Rebuild on top
of an external voice wrapper if needed.

---

## Phase 8: Operational

### 8.1 Rate Limiting ✅
- [x] Token bucket rate limiter (`R/rate-limit.R`)

### 8.2 Remaining Work
- [ ] Aggregate tools from multiple MCP servers (Phase 2.2)
- [ ] Auto-inject relevant memories into context via RAG (Phase 3.3)
- [ ] Event triggers (file watchers, webhooks)

Scheduled tasks were removed in 0.4.0 along with the SQLite-backed scheduler
in favor of cron. The kill-switch idea went with them.

---

## Architecture

```
User (terminal / Signal)
     │
     ▼
┌─────────────────┐
│  llamar CLI     │  ← inst/bin/llamar (Rscript shebang)
└────────┬────────┘
         │
         ├── llm.api (provider abstraction)
         │      ├── Anthropic
         │      ├── OpenAI
         │      └── Ollama
         │
         └── llamaR MCP Server (port 7850)
                ├── File tools (read/write/list/grep)
                ├── R tools (run_r, r_help, installed_packages)
                ├── System tools (bash, git_*)
                ├── Web tools (web_search, fetch_url)
                ├── Chat tools (chat, chat_models via llm.api)
                ├── Memory tools (memory_store, memory_recall, memory_get)
                └── Subagent tools (spawn, query)
```

---

## Non-Goals (for now)

- Shiny UI
- RStudio integration
- Cloud deployment
- Multi-user / auth
- Paid API wrappers
