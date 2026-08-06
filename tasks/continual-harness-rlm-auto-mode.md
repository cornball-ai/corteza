# Task: continual harness, RLM tool mode, evals, and auto mode

Date: 2026-08-05
Status: untracked planning note

## Goal

Add the missing runtime pieces surfaced by the Prime Agent comparison:

1. Continual harness state with safe refinement proposals.
2. `tools = "rlm"` one-tool mode for comparison against `tools = "all"`.
3. A tiny executable eval suite.
4. Bounded long-running auto mode.

## Plan

### 1. RLM mode

- [ ] Add a `tools = "rlm"` preset that exposes only `run_r`.
- [ ] Preload an R control environment in the live session:
  - [ ] `fs$read()` / `fs$write()` / `fs$grep()`
  - [ ] `sh$run()`
  - [ ] `git$status()` / `git$diff()`
  - [ ] `agent$spawn()` / `agent$query()`
  - [ ] `harness$propose()` once harness exists
- [ ] Add focused docs in runtime guidance for this mode.

### 2. Continual harness

- [ ] Add `.corteza/harness/` loader.
- [ ] Support `memories/`, `prompts/`, `skills/`, `subagents/`, and `proposals/`.
- [ ] Add `harness_load()`.
- [ ] Add `harness_format_context()`.
- [ ] Add proposal-only `harness_propose_update()`.
- [ ] Add guarded `harness_apply_update()` and `harness_reject_update()`.
- [ ] Add `harness_audit()`.
- [ ] Add CLI commands:
  - [ ] `/harness`
  - [ ] `/refine`
  - [ ] `/refine apply <id>`
  - [ ] `/refine reject <id>`

Safety rule: the agent may propose harness updates, but human approval applies them.

### 3. Eval suite

- [ ] Inspect Posit `vitals` / `are` and Simon Couch `helperbench` for task format ideas.
- [ ] Keep the corteza runner tinyverse and based on executable checks, not an LLM judge.
- [ ] Add eval case shape:

```text
evals/
  add-function/
    prompt.md
    setup.R
    grade.R
    fixture/
```

- [ ] Add 5 starter cases.
- [ ] Compare modes:
  - [ ] `tools = "all"`
  - [ ] `tools = "rlm"`
  - [ ] optional hybrid preset
- [ ] Record pass/fail, wall time, tokens, cost, tool calls, files touched, dependency changes, and instruction violations.

### 4. Long-running mode

- [ ] Add loop mode first:

```r
chat(auto = TRUE, max_loops = 10)
```

- [ ] Stop conditions:
  - [ ] assistant reports done
  - [ ] no progress after N loops
  - [ ] spend/time/token cap hit
  - [ ] destructive, privileged, or ambiguous action requested
  - [ ] tests keep failing with no new diff
- [ ] Feed test logs back into the next loop when configured.
- [ ] Defer detached worker until loop mode proves useful.

Future detached shape:

```bash
corteza run --goal "fix failing CI" --detach
corteza agents
corteza attach <id>
corteza stop <id>
```

## Notes

corteza already has strong context engineering via saber, local context files, package/tool docs, sessions, compaction, and `callr::r_session` subagents. This work should build on those pieces rather than replacing them.
