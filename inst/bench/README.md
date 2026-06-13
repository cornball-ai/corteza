# Benchmarks

## How to run

```bash
r inst/bench/bench_schema_tokens.R   # LLM tools-payload token footprint
```

## Results

Measured on Linux (Ubuntu 24.04 LTS, R 4.5, local source checkout, warm disk cache). Numbers will vary across hardware; use these as an order-of-magnitude baseline.

### System-prompt tools payload (Phase 6 pruning)

```
non-git dir, no TAVILY_API_KEY            18 tools   ~1666 tokens
git repo, no TAVILY_API_KEY               21 tools   ~1970 tokens
git repo, TAVILY_API_KEY set              22 tools   ~2040 tokens
unpruned baseline                         22 tools   ~2040 tokens
```

Context-aware pruning drops three git tools in non-git directories and `web_search` without a Tavily key — about 18% token savings in a bare environment (1666 vs 2040). The cost of each additional tool is small (~70 tokens) because the schemas are terse R-derived descriptions rather than verbose MCP boilerplate.

## Non-goals

- Progressive disclosure (`describe_tool` meta-tool): not needed until the tool surface exceeds ~10k tokens.
