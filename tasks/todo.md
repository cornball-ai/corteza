# CLI / worker split — decouple CLI from internal MCP

## Context

The `corteza` CLI spawns an R subprocess running `corteza::serve()` and speaks MCP over stdio. The MCP protocol is the wrong choice for a private internal worker: we control both ends, capability negotiation is unnecessary, and `tools/list` dumps every JSON Schema into the LLM system prompt. That's the "MCP context tax" the cerebro narrative calls out, paid inside our own runtime.

The fix is to split the public interop protocol (MCP, via `serve()`, used by external clients like Claude Desktop / VS Code) from the private runtime transport (CLI ↔ worker, used only by the `corteza` binary). `serve()` stays fully MCP-compliant. The CLI moves to a `callr::r_session` persistent worker with R-native dispatch.

## Goal (one sentence)

Replace the CLI's use of `serve()`/MCP as its internal transport with a private persistent worker driven by `callr::r_session`, keep `serve()` unchanged as the external MCP surface, and unify both under a single in-process tool registry.

## Final architecture

- `chat()` — in-process tools (unchanged).
- `serve()` — external MCP server, spec-compliant (unchanged).
- CLI — private `callr::r_session` worker, R-native dispatch.
- All three paths read from one shared tool registry.

## Design invariants — do not revisit during execution

1. Transport is `callr::r_session`. Not raw `processx`, not `mirai`, not forks, not custom JSON-RPC. callr's dependency footprint has been audited (4 packages, all clean).
2. `serve()`'s MCP wire behavior does not change.
3. Schema generation is CLI-side only, built from the registry via saber introspection. Schemas never cross the worker pipe.
4. One shared in-process tool registry, single source of truth for `chat()`, `serve()`, and CLI.
5. Errors cross the boundary as a normalized `corteza_tool_error` condition class.
6. No streaming event channel in v1. Stderr JSON lines are acceptable as a side channel for observability.
7. No progressive disclosure / `describe_tool` meta-tool. Context-aware pruning only.
8. Base R only. No pipes, no tidyverse. Match existing corteza conventions.
9. R version gate: whatever `callr::r_session` requires. Confirm against current `Depends: R (>= 4.4.0)` is sufficient.

## Follow-up after all 8 phases ship

- Subagents (R/subagent.R) currently spawn child agents that talk MCP via `mcp_connect`/`mcp_call`. Same argument applies: we own both ends, MCP buys us nothing internally. Move them to `callr::r_session` in a later refactor, separate from the CLI phases.

## Non-goals for this refactor

- Changing `serve()`'s MCP protocol behavior.
- `describe_tool` / progressive disclosure.
- Streaming/async event protocol on the RPC path.
- Worker pools or per-call-ephemeral workers.
- Binary framing, `serialize()`-over-pipe, shared memory.
- Changing `chat()`'s in-process model.
- Replacing MCP externally.
- Token-budget optimization beyond basic context-aware pruning.

## Preflight (Phase 0)

Confirm repo layout before Phase 1:

- [ ] CLI entry point: `inst/bin/corteza`.
- [ ] `serve()` MCP implementation path.
- [ ] `chat()` path (known: `R/chat.R`).
- [ ] How tools are currently registered — ad hoc in `serve()`, or an existing registry structure.
- [ ] `callr` in DESCRIPTION? (known: no, needs adding in Phase 2.)
- [ ] `saber` in DESCRIPTION? (known: yes, Imports.)
- [ ] Current CLI subprocess bootstrap: trace CLI startup → subprocess spawn → MCP handshake.

## Phase 1 — Conceptual split (this PR)

**Goal:** separate CLI-worker code from `serve()` MCP surface in the source tree. No runtime behavior changes.

**Changes:**
- If tool definitions live inside `serve()`, extract them to `R/tools.R` (or similar). Each tool is a plain R function with roxygen docs.
- Create `R/registry.R` holding the shared registry. Registry = named list / environment mapping `tool_name → list(fn, meta)`. `serve()` and `chat()` consume it; CLI will consume it in Phase 3.
- Rename CLI-side code currently coupled to MCP terminology (e.g. `cli_mcp_client` → `cli_worker`, `tools_list_request` → `worker_call`). The rename is load-bearing: it forces conceptual separation before the transport changes.
- `serve()` sources its tool surface from the registry. MCP wire behavior unchanged.

**Acceptance:**
- Package check passes (`tinypkgr::check()` with 0 errors, 0 warnings).
- `chat()` and `serve()` both work, using the shared registry.
- CLI still works end-to-end via the existing MCP subprocess path.
- No `MCP` / `mcp_` identifiers remain in code paths that will become the private CLI transport.

**Not in scope for Phase 1:**
- No `callr` yet. CLI still speaks MCP to the subprocess.
- No schema generation changes.
- No handles.
- No context-aware pruning.
- No new condition classes.

**Stop here. Check in before Phase 2.**

## Phase 2 — Swap CLI transport to `callr::r_session`

**Goal:** CLI drives the worker via `callr::r_session`. MCP no longer used between CLI and subprocess. `serve()` still speaks MCP to external clients.

**Changes:**
- Add `callr` to Imports in DESCRIPTION.
- Replace CLI's MCP-client code with a callr-backed implementation:
  - `callr::r_session$new()` starts the worker; `session$run(function() library(corteza))` initializes corteza.
  - Tool dispatch: `session$call(function(name, args) corteza:::dispatch_tool(name, args), list(name = name, args = args))`.
- Add `R/tool_error.R` with `make_tool_error()` constructor producing a `c("corteza_tool_error", "error", "condition")` class.
- Worker-side `dispatch_tool(name, args)` wraps tool invocation in `tryCatch`, rethrows as `corteza_tool_error`.
- CLI side catches `corteza_tool_error`, formats for the LLM.

**Acceptance:**
- CLI starts, spawns a callr worker, executes a tool call (e.g. `bash 'echo hi'`), returns output.
- A tool that throws (e.g. `read_file('/does/not/exist')`) produces a `corteza_tool_error` on the CLI side with the original condition's message preserved.
- `serve()` still passes whatever MCP compliance checks existed before.
- No MCP protocol code remains on the CLI ↔ worker path.

**Stop here. Check in before Phase 3.**

## Phase 3 — CLI-side schema generation from registry

**Goal:** CLI constructs the LLM API `tools` payload locally from the registry via saber introspection. Schemas never requested from the worker.

**Changes:**
- `R/schema.R`:
  - `schema_from_registry(registry)` iterates and calls `schema_from_fn(name, entry)` per tool.
  - `schema_from_fn(name, entry)` uses saber to extract signature + `@description` + `@param` entries with types/descriptions. Emits JSON-Schema-shaped R list for the Anthropic API `tools` parameter.
- Two roxygen conventions (parsed as prefixed text, no new tinyrox features):
  - `@llmDescription` — override when `@description` isn't LLM-friendly.
  - `@llmDefault <value>` — explicit default when R default is unserializable.
- CLI's previous tools-payload construction replaced with `schema_from_registry(registry)`.
- `...` excluded from generated schemas; tools that need variadic input redesign to take a named list.

**Acceptance:**
- LLM tools payload shape-equivalent to MCP's output (minus envelope) for at least 3 representative tools (bash, read_file, run_r).
- Removing a tool from the registry removes it from the payload. Adding a new tool with proper roxygen docs produces a working schema.

**Stop here. Check in before Phase 4.**

## Phase 4 — Build-time schema validation via saber

**Goal:** a saber AST pass validates that generated schemas match what tool functions actually do.

**Changes:**
- `inst/tinytest/test_tool_schemas.R`: for each tool in the registry, walk the body AST via saber. Check every `@param` is referenced; check every top-level symbol matching a formal is documented. Type-hint consistency → warning only.

**Acceptance:**
- Deliberately break one tool's `@param` docs → tests fail with a pointer. Restore → tests pass.

**Stop here. Check in before Phase 5.**

## Phase 5 — Handle-based large results

**Goal:** tool results that would be large R objects stay in the worker; only summary + handle crosses to the CLI / LLM.

**Changes:**
- `R/handles.R`: worker maintains a `handle_store` environment. Handles are short opaque strings (`.h_001`, ...).
- `with_handle(value, summary_fn)` helper returns `list(summary, handle)`.
- `run_r` is primary consumer: non-scalar / over-threshold results wrapped with `with_handle`.
- `read_handle(handle, op = c("str", "head", "summary", "print"))` as a first-class tool.
- Subsequent `run_r` calls can reference handles by name; worker substitutes real object into eval env.

**Acceptance:**
- `run_r('mtcars')` returns `list(summary, handle)`, not the full frame.
- `read_handle('<handle>', 'head')` returns head output.
- Session memory stays in worker; CLI RSS doesn't grow with result size.

**Stop here. Check in before Phase 6.**

## Phase 6 — Context-aware tool pruning

**Goal:** registry surface adapts to the runtime environment.

**Changes:**
- Each registry entry gains an optional `available()` predicate. If absent, tool always included.
- Concrete: git tools gated on `dir.exists(".git")`; matrix tools on configured homeserver; R-package-dev tools on `file.exists("DESCRIPTION")`; web tools on env var / option.
- `schema_from_registry()` filters by availability before emitting.

**Acceptance:**
- CLI in non-git dir → no git tools in payload. CLI in git dir → included. Measurable drop in system-prompt tokens for bare environments.

**Stop here. Check in before Phase 7.**

## Phase 7 — Observability side channel

**Goal:** CLI surfaces structured events from the worker without coupling to RPC.

**Changes:**
- Worker-side `cz_log(event, ...)` writes one JSON line to stderr per event from the dispatch wrapper.
- CLI-side drains `session$read_error_lines()`, parses as JSON, dispatches to a CLI logger.
- `--trace` flag / `options(corteza.trace = TRUE)` pretty-prints events inline.

**Acceptance:**
- `--trace` shows tool-started / tool-finished with timing. Without, nothing visible changes.

**Stop here. Check in before Phase 8.**

## Phase 8 — Benchmark and stop

**Goal:** confirm no further optimization is needed. Resist adding more.

**Changes:**
- `inst/bench/` scripts measuring: CLI startup (invocation → first-tool-ready), round-trip for bash/read_file/run_r (small)/run_r (handled), system-prompt token count in typical environments.
- **Specifically measure worker warm-start cost** (`r_session$new()` + `library(corteza)`). If 1s+, users notice; document but do not preemptively build persistent-daemon architecture.
- Compare against pre-refactor baseline if captured.

**Acceptance:**
- Numbers in `inst/bench/README.md`. Decision documented: ship, or specific measured reason to continue.

## Checkpoint discipline

Each phase = one branch / one PR. Phase 1 first, merge, then Phase 2 on a fresh branch from main. Do not chain phases.
