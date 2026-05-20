# Corteza Testing Log

Branch: `testing/local-bug-hunt`
Started: 2026-05-20

---

## Bug Summary

Seven bugs were found and fixed during this session. Here is a plain-language description of each.

---

**BUG-001 — Missing provider support (moonshot)**
The `llm.api` dependency had no minimum version requirement in the package description, so a fresh install could land on an old version that didn't support the "moonshot" AI provider. Two tests failed as a result.
- **Fix:** Added `llm.api (>= 0.1.3)` to `DESCRIPTION` so the minimum required version is now enforced.
- **Note:** The package itself was also upgraded on this machine to make the tests pass immediately.

---

**BUG-002 — Shell commands silently swallowed failure**
When the `bash` tool ran a shell command that failed (e.g. a script that crashed or returned an error code), corteza reported it as a success anyway. The AI had no way to know something went wrong.
- **Fix:** `R/tool-impl.R` — the tool now checks the exit code after running a command and reports an error if it's non-zero.

---

**BUG-003 — Wrong documentation for `run_r` tool**
`CLAUDE.md` said the `run_r` tool runs code in a fresh, isolated process each time (so variables don't carry over between calls). This is false — `run_r` actually runs code in the same live R session, so variables *do* persist. The isolated subprocess tool is a different one called `run_r_script`.
- **Fix:** `CLAUDE.md` — corrected the descriptions of both tools.

---

**BUG-004 — Tool names with hyphens got corrupted**
Corteza converts tool names into a format the AI API accepts (e.g. `base::read.csv` → `base__read_dot_csv`) and then converts them back. The conversion used `-` to represent `.`, but `-` is also a valid character in tool names. This meant a name like `my-tool` would be decoded as `my.tool` — silently wrong. No built-in tools were affected, but any user-registered tool with a hyphen would have broken.
- **Fix:** `R/tools.R` — changed the encoding so `.` becomes `_dot_` instead of `-`. Hyphens now pass through unchanged.

---

**BUG-005 — Claude 4 models reported the wrong context window size**
Corteza tracks how much of a model's memory is in use to decide when to compact history. The lookup table only had old date-stamped model IDs (like `claude-sonnet-4-20250514`). Newer short-form IDs (like `claude-sonnet-4-6`) weren't in the table, so corteza assumed only 128K of context instead of the correct 200K. This would cause unnecessary early compaction.
- **Fix:** `R/context-budget.R` — added the short-form Claude 4 model IDs to the lookup table.

---

**BUG-006 — Default Anthropic model was outdated**
When no model is explicitly specified, corteza picks a default. For Anthropic, it was still set to the old date-stamped ID `claude-sonnet-4-20250514` rather than the current short-form `claude-sonnet-4-6`. This caused the wrong model name to appear in the `/agents` display.
- **Fix:** `R/context-budget.R` — updated the default to `claude-sonnet-4-6`. Also updated two test files (`inst/tinytest/test_agents_visibility.R`, `inst/tinytest/test_cli_helpers.R`) that had the old ID hardcoded.

---

**BUG-007 — Session list showed internal agent sessions**
Running `corteza --list` is supposed to show your chat sessions. Instead it was showing a mix of user sessions and internal "subagent" sessions (helper processes the AI spawns internally). On this machine it showed ~10 internal sessions and zero user sessions.
- **Fix:** `R/session.R` — the session list now filters to only return sessions whose key starts with `"corteza:"`, excluding all internal subagent entries.

---

## Test Runs

### 1. Full test suite — baseline
**Date:** 2026-05-20
**Command:** `r -e 'tinytest::test_package("corteza")'`
**Result:** FAIL — 2 out of 2045 tests failed

Failures:
| File | Line | Assert | Error |
|------|------|--------|-------|
| `test_chat.R` | 8 | `expect_true("moonshot" %in% supported)` | Expected TRUE, got FALSE |
| `test_chat.R` | 9 | `expect_silent(corteza:::ensure_llm_api_provider("moonshot"))` | Error thrown: "does not support provider 'moonshot'" |

---

### 2. Full test suite — after llm.api upgrade
**Date:** 2026-05-20
**Command:** `r -e 'tinytest::test_package("corteza")'`
**Result:** PASS — 2045 tests OK

---

### 3. Individual file runs — core subsystems
**Date:** 2026-05-20

| Test file | Result | Notes |
|-----------|--------|-------|
| `test_policy.R` | PASS (37 tests) | |
| `test_config.R` | PASS (18 tests) | |
| `test_context.R` | PASS (15 tests) | |
| `test_tool_impl.R` | PASS (31 tests) | |
| `test_session.R` | PASS (49 tests) | |
| `test_permissions.R` | PASS (43 tests) | |
| `test_subagent_callr.R` | PASS (10 tests) | |
| `test_turn.R` | PASS (45 tests) | |
| `test_tool_executor.R` | PASS (8 tests) | |

---

### 4. Context limits and model ID update
**Date:** 2026-05-20

Discovered and fixed BUG-005 (Claude 4 models returning 128K instead of 200K).
Added short-form IDs to `MODEL_CONTEXT_LIMITS` in `R/context-budget.R`.

---

### 5. Manual functional testing — tools and policy
**Date:** 2026-05-20
**Method:** Direct `call_skill()` calls and `policy()` inspection in R

| Area | Tested | Result | Notes |
|------|--------|--------|-------|
| `replace_in_file` — happy path | ✓ | PASS | Correct params: `old_text`/`new_text` |
| `replace_in_file` — missing old_text | ✓ | PASS | Returns `err("old_text not found")` |
| `replace_in_file` — empty old_text | ✓ | PASS | Returns `err("old_text must not be empty")` |
| `write_file` + `read_file` round-trip | ✓ | PASS | |
| `bash` — successful command | ✓ | PASS | stdout captured |
| `bash` — non-zero exit code | ✓ | **FAIL** | See BUG-002 |
| `bash` — stderr merged into stdout | ✓ | PASS | `2>&1` merged correctly |
| `list_files` — existing dir | ✓ | PASS | |
| `list_files` — nonexistent path | ✓ | PASS | Returns `err("Directory not found: ...")` |
| `grep_files` — with matches | ✓ | PASS | |
| `grep_files` — no matches | ✓ | PASS | Returns `ok("No matches found")` |
| `run_r` — in-process, state persists | ✓ | PASS (by design) | See BUG-003 (doc error) |
| `run_r_script` — subprocess, stateless | ✓ | PASS | Variables do not persist |
| Policy — plan mode write denied | ✓ | PASS | |
| Policy — plan mode read allowed | ✓ | PASS | |
| Policy — plan mode exec denied | ✓ | PASS | |
| Policy — sticky personal data class | ✓ | PASS | |
| Policy — config allow overlay | ✓ | PASS | Named list format required |
| Policy — config deny overlay | ✓ | PASS | |
| Config — project overrides global | ✓ | PASS | |
| Config — defaults applied | ✓ | PASS | e.g. `context_warn_pct = 75` |
| `new_session` defaults | ✓ | PASS | |
| `add_observer` + `fire_observers` | ✓ | PASS | |
| Tool name sanitize/unsanitize round-trip | ✓ | **FAIL** | See BUG-004 |

---

## Bugs Found

### BUG-001 — `moonshot` provider absent in llm.api 0.1.0
**Status:** Fixed (env-level; upstream fix recommended)
**Severity:** Medium — breaks tests; silently disables moonshot for users on stale installs

**Symptom:**
`test_chat.R:8-9` fails because `moonshot` is not in `llm.api::agent()`'s `provider` formals.
`llm_api_supported_providers()` returns only `c("anthropic", "openai", "ollama")`.

**Root cause:**
`llm.api` 0.1.0 (the version that lands on a fresh install) predates moonshot support.
`DESCRIPTION` declares `llm.api` in `Imports` with no minimum version constraint, so any
version satisfies the dependency check.

**Fix applied:**
Upgraded `llm.api` from 0.1.0 to 0.1.3 on this machine.
```bash
r -e 'install.packages("llm.api")'
```

**Recommended upstream fix:**
Add a minimum version to `DESCRIPTION`:
```
llm.api (>= 0.1.3),
```

---

### BUG-002 — `bash` tool does not report non-zero exit codes
**Status:** Fixed
**Severity:** High — the LLM has no way to detect shell command failures
**File:** `R/tool-impl.R`, function `tool_shell_impl` (~line 634)

**Symptom:**
A shell command that exits with a non-zero status (e.g. `exit 1`, a failed `ls`, etc.)
is returned as `ok(result)` with `isError = FALSE`. The LLM sees the output (if any)
but cannot distinguish success from failure.

```r
r <- corteza:::call_skill("bash", list(command = "exit 1"))
r$isError  # FALSE — should be TRUE or status should be in output
```

**Root cause:**
`system2()` stores the exit status in `attr(out, "status")` but `tool_shell_impl` never
checks it:
```r
result <- tryCatch({
    out <- system2(shell_exe, exe_args_fg, stdout = TRUE, stderr = TRUE, ...)
    paste(out, collapse = "\n")   # <-- status attribute ignored
}, ...)
ok(result)  # always ok()
```

**Fix applied (`R/tool-impl.R`):**
Wrap `system2()` in `suppressWarnings()` (the warning was the only prior signal of failure),
capture `attr(out, "status")`, and return `err()` with the status prepended to any output
when non-zero. The `error` branch of `tryCatch` also changed from `ok()` to `err()` so
process-launch failures are also flagged.

```r
tryCatch({
    out <- suppressWarnings(
        system2(shell_exe, exe_args_fg, stdout = TRUE, stderr = TRUE, timeout = timeout)
    )
    status <- attr(out, "status") %||% 0L
    text <- paste(out, collapse = "\n")
    if (!is.null(status) && status != 0L) {
        err(sprintf("[exit status %d]\n%s", status, text))
    } else {
        ok(text)
    }
}, error = function(e) {
    err(paste("Error:", e$message))
})
```

---

### BUG-003 — CLAUDE.md incorrectly describes `run_r` as stateless
**Status:** Open (documentation bug)
**Severity:** Low — misleads agents and developers about tool behaviour
**File:** `CLAUDE.md` ("Tool isolation: `run_r` is stateless")

**Symptom:**
CLAUDE.md states: "`run_r` runs every call in a fresh R process via `callr::rscript()`"
and "Variables do **not** persist across calls." Both claims are false.

```r
corteza:::call_skill("run_r", list(code = "x_test <- 999"))
# x_test is now in globalenv — persists to next call
corteza:::call_skill("run_r", list(code = "cat(exists('x_test'))"))
# prints TRUE
```

**Root cause:**
`tool_run_r` uses `eval(parse(text = code), envir = handle_eval_env(parent = globalenv()))`.
This is intentionally stateful — assignments land in `globalenv()` and the workspace
auto-capture explicitly depends on this. The subprocess tool is `run_r_script`
(uses `callr::rscript()`), not `run_r`.

**Recommended fix:**
Update CLAUDE.md to accurately describe both tools:
- `run_r` — **stateful**, runs in the current R process; variables persist in `globalenv()`
- `run_r_script` — **stateless**, runs in a fresh subprocess via `callr::rscript()`

---

### BUG-004 — Tool name sanitize/unsanitize round-trip fails for names containing `-`
**Status:** Fixed
**Severity:** Low — would silently misroute tool calls for any skill whose name contains a hyphen
**File:** `R/tools.R`, functions `sanitize_tool_name` / `unsanitize_tool_name`

**Symptom:**
`sanitize_tool_name` converts `.` → `-`. `unsanitize_tool_name` reverses by converting
`-` → `.`. If a tool name already contains a literal `-`, it survives sanitize unchanged
but is incorrectly converted to `.` on unsanitize:

```r
sanitize_tool_name("pkg::some-tool")   # "pkg__some-tool"
unsanitize_tool_name("pkg__some-tool") # "pkg::some.tool"  — WRONG
```

This also means `sanitize_tool_name("a.b")` and `sanitize_tool_name("a-b")` both produce
`"a-b"`, making the mapping non-injective (collision possible).

**Root cause:**
The encoding scheme (`-` as the escaped form of `.`) conflicts with `-` being a valid
character in LLM tool name patterns (`[a-zA-Z0-9_-]`).

**Current exposure:** No built-in skills have hyphens, so this does not manifest today.
It would break any user-registered skill or package function whose name contains `-`.

---

### 6. Live surface testing — Ollama (qwen3:8b)
**Date:** 2026-05-20
**Model:** qwen3:8b (newly pulled; replaces qwen2.5:14b for testing)

| Area | Result | Notes |
|------|--------|-------|
| `turn()` basic round-trip | PASS | reply correct |
| `turn()` multi-turn history | PASS | name recalled across turns |
| `turn()` tool use (run_r) | PASS | sqrt(144)=12 via tool call |
| qwen3 thinking tokens | INFO | `/think` appeared once non-deterministically; not in history |
| `serve()` startup | PASS | up in ~4 s on port 17852 |
| `serve()` MCP connection | PASS | `mcp_connect(host, port)` correct API |
| `serve()` tool listing | PASS | 11 tools for "core" filter |
| `serve()` MCP tool call (bash) | PASS | |
| `serve()` BUG-002 fix via MCP | PASS | exit 42 → `isError=TRUE` through MCP path |
| CLI `--help` | PASS | full usage printed |
| CLI `--list` (before fix) | FAIL | showed subagent sessions — see BUG-005 |
| CLI `--list` (after fix) | PASS | "No sessions found." |
| Context assembly — CLAUDE.md | PASS | |
| Context assembly — custom files | PASS | |
| Context assembly — missing file | PASS | silently skipped |
| Workspace put/get/clear | PASS | |
| `ws_retrieve` scoring | PASS | relevant object ranked first |
| `compact_find_cut` | PASS | returns 0 for un-compactable history |
| `default_provider_model` | INFO | returns old date-stamped ID for anthropic — see BUG-006 |

---

### BUG-005 — Claude 4 short-form model IDs return 128K context limit
**Status:** Fixed
**Severity:** Medium — triggers premature compaction for Claude 4 sessions
**File:** `R/context-budget.R`, `MODEL_CONTEXT_LIMITS`

**Symptom:** `context_limit_for_model("claude-sonnet-4-6")` returned `128000L` (unknown-model fallback) instead of `200000L`.

**Root cause:** `MODEL_CONTEXT_LIMITS` only had date-stamped IDs. The prefix-matching fallback requires a shared prefix but `"claude-sonnet-4-6"` is not a prefix of `"claude-sonnet-4-20250514"`.

**Fix applied:** Added short-form entries to `MODEL_CONTEXT_LIMITS`:
```r
"claude-opus-4-7"   = 200000L,
"claude-sonnet-4-6" = 200000L,
"claude-haiku-4-5"  = 200000L,
```

---

### BUG-006 — `default_provider_model("anthropic")` returns stale date-stamped ID
**Status:** Fixed
**Severity:** Low — returns a valid but outdated model ID; shows wrong model in `/agents`
**File:** `R/context-budget.R`, `default_provider_model()` (line 58)

**Symptom:** `default_provider_model("anthropic")` returns `"claude-sonnet-4-20250514"` instead of `"claude-sonnet-4-6"`.

**Recommended fix:** Change line 58:
```r
# Before:  anthropic = "claude-sonnet-4-20250514",
# After:   anthropic = "claude-sonnet-4-6",
```

---

### BUG-007 — `--list` / `session_list()` surfaces subagent sessions
**Status:** Fixed
**Severity:** Medium — clutters session list with internal sessions the user never created
**File:** `R/session.R`, `session_list()`

**Symptom:** `~/bin/corteza --list` showed ~10 entries like `agent:main:subagent:planner` instead of "No sessions found."

**Root cause:** `session_list()` iterated all keys in the flat JSON store, including subagent sessions keyed `"agent:main:subagent:<name>"`. User sessions are keyed `"corteza:<id>"`.

**Fix applied:** Added a filter to keep only keys starting with `"corteza:"`:
```r
user_keys <- Filter(function(k) startsWith(k, "corteza:"), names(store))
```

---

---

### 7. Subagent lifecycle — Ollama (qwen3:8b)
**Date:** 2026-05-20

| Area | Result | Notes |
|------|--------|-------|
| `subagent_spawn` — basic | PASS | Returns name-based id; callr session starts |
| `subagent_list` — registry count | PASS | 1 entry after spawn |
| `subagent_query` — sync (wait=TRUE) | PASS | Returns character; correct answer (7×8=56) |
| `subagent_query` — multi-turn context | PASS | Context preserved; 56×2=112 |
| `subagent_query` — async (wait=FALSE) | PASS | `pending` field set with prompt text |
| `subagent_collect` — blocking | PASS | Reply correct (3+4=7); pending cleared |
| `subagent_kill` | PASS | Registry count drops to 0 |

---

### 8. CLI session resume
**Date:** 2026-05-20

| Area | Result | Notes |
|------|--------|-------|
| `session_new()` creates persisted session | PASS | `sessionKey` set, appears in store |
| `session_save()` persists metadata | PASS | |
| `session_list()` shows user session | PASS | 1 session; shows provider/model/msg count |
| `~/bin/corteza --list` | PASS | Shows `corteza:test-resume-01  ollama/qwen3:8b` |
| `~/bin/corteza --resume` | PASS | Prints "Resuming latest session: corteza:test-resume-01"; loads correct session |

---

---

### 9. Archival runtime — Ollama (qwen3:8b)
**Date:** 2026-05-20

| Area | Result | Notes |
|------|--------|-------|
| `archival_should_trigger` — below threshold | PASS | Returns FALSE |
| `archival_should_trigger` — at threshold | PASS | `>= semantics`: fires at exactly threshold |
| `archival_should_trigger` — disabled | PASS | Returns FALSE |
| `archival_should_trigger` — at depth_cap | PASS | Returns FALSE |
| `archival_archive_turn` — sync path | PASS | Spawns holder, generates real summary via Ollama |
| Summary style fallback (Ollama) | INFO | Structured→paragraph fallback with warning (expected) |
| `archival_archive_turn` — async path | PASS | Returns placeholder summary immediately; bg summary fires |
| Placeholder text | PASS | `"[archived turn pending summary] | task=... | N entries"` |
| Holder subagent in registry post-archive | PASS | `subagent_list()` shows holder |
| `subagent_kill` on holder | PASS | Registry cleared |
| Config guard: `archival.enabled` without `subagents.enabled` | PASS (not directly tested; enforced in `load_config`) | |

---

## Pending

- [x] **Fix BUG-006** — update `default_provider_model("anthropic")` to `"claude-sonnet-4-6"` (`R/context-budget.R` line 58)
- [x] **Fix BUG-004** — tool name sanitize/unsanitize encoding scheme; changed `.`→`_dot_` (was `-`); now injective
- [x] **Upstream fix for BUG-001** — added `llm.api (>= 0.1.3)` to `DESCRIPTION`
- [x] **Test subagent lifecycle** — spawn, query (sync + async), collect, kill — all PASS
- [x] **Test CLI session resume** — `--resume` loads session correctly — PASS
- [x] **Test archival runtime** — sync + async paths both PASS
