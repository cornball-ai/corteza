# Benchmarks

End of the CLI / worker split refactor (Phases 1-8). These numbers justify stopping here.

## How to run

```bash
r inst/bench/bench_warm_start.R      # CLI worker spawn + init
r inst/bench/bench_tool_dispatch.R   # per-call round trip on a warm worker
r inst/bench/bench_schema_tokens.R   # LLM tools-payload token footprint
```

## Results

Measured on Linux (Ubuntu 24.04 LTS, R 4.5, local `~/cerebro` checkout, warm disk cache). Numbers will vary across hardware; use these as an order-of-magnitude baseline.

### Worker warm-start (n=5)

```
median:  0.254s
min/max: 0.231s / 0.264s
```

About a quarter of a second from `cli_worker_spawn()` to a worker ready for its first tool call. One-shot cost, amortized over the rest of the session.

### Per-call dispatch (n=20, warm worker)

```
bash 'echo hi':         17.5 ms (min 17.1, max 40.5)
read_file (50 lines):   11.9 ms (min 11.3, max 32.8)
run_r '2+2':            10.8 ms (min 10.3, max 12.2)
run_r data.frame:       10.9 ms (min 10.6, max 12.2)
```

Round-trip time from `session$run()` to result. Dominated by callr's native-serialization IPC. `bash` pays the extra cost of forking a shell; pure-R tools are all ~11 ms.

For context: the Anthropic API round trip for an LLM turn is typically 1-3 seconds. Per-call dispatch is well under 1% of that.

### System-prompt tools payload (Phase 6 pruning)

```
non-git dir, no TAVILY_API_KEY            18 tools   ~1666 tokens
git repo, no TAVILY_API_KEY               21 tools   ~1970 tokens
git repo, TAVILY_API_KEY set              22 tools   ~2040 tokens
unpruned baseline                         22 tools   ~2040 tokens
```

Context-aware pruning drops three git tools in non-git directories and `web_search` without a Tavily key — about 18% token savings in a bare environment (1666 vs 2040). The cost of each additional tool is small (~70 tokens) because the schemas are terse R-derived descriptions rather than verbose MCP boilerplate.

## Decision: ship

All three dimensions land well inside the budget any reasonable usage would want:

- **Warm-start**: 250 ms is low enough that it's below the noise floor of the first LLM call. No need for a persistent daemon.
- **Dispatch**: 10-20 ms per tool call isn't a bottleneck against multi-second LLM turns. No need to move off `callr::r_session` to shared memory or native serialize over pipes.
- **Token budget**: 1.7-2k tokens of tools surface is small in practice. No need for progressive disclosure (the `describe_tool` meta-tool pattern).

The plan's non-goals stay non-goals. The refactor is done.

## Non-goals

- Progressive disclosure (`describe_tool` meta-tool): not needed until the tool surface exceeds ~10k tokens.
- Binary framing / `serialize()` over pipes: not needed until dispatch is a measurable bottleneck.
- Worker pools / per-call-ephemeral workers: not needed at these round-trip times.
- Subagent re-architecture (moving off MCP): worthwhile cleanup, but a separate project — covered in `tasks/todo.md`.
