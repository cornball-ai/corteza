<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Package as Skill}
-->
---
title: Package as Skill
---

# Package as Skill

An R package that documents its functions properly is already what MCP is trying to wire together: tools (functions), descriptions (`.Rd` files), and an invocation mechanism (`library()`). corteza walks the `.Rd` tree at session start and turns the exports into JSON-Schema tool definitions — same shape the Anthropic / OpenAI / Moonshot APIs expect. No server, no schema by hand, no protocol.

This vignette walks the setup with [fortunes](https://cran.r-project.org/package=fortunes), quotes from the R-help archives. It returns a structured S3 object, and the demo is more fun than configuring a JSON parser.

## Setup

Two steps. Total wiring: one config line.

### 1. Install the package

```r
install.packages("fortunes")
```

### 2. Register it as a skill

Add a `skill_packages` entry to corteza's config. Either project-local at `<cwd>/.corteza/config.json` or global at `tools::R_user_dir("corteza", "config")/config.json`:

```json
{
  "skill_packages": ["fortunes"]
}
```

That's it. The string form registers every export. For larger packages, the object form picks specific functions:

```json
{
  "skill_packages": [
    {"package": "fortunes", "functions": ["fortune"]}
  ]
}
```

## Use

```r
corteza::chat()
```

The startup banner shows the tool count climbing — `corteza::chat()` reports `30 tools` instead of `29`. Then ask the agent to use it:

```
> Find a fortune by Brian Ripley about types or coercion.
```

You'll see the progress hint fire as the agent invokes the tool:

```
  [fortunes::fortune] author=Brian Ripley (8 lines)
```

The `fortune` object comes back with `quote`, `author`, `context`, `source`, and `date` fields, and the agent paraphrases or quotes from there. Try variations:

- "Pick a random fortune and tell me which R personality you'd most love to argue with about it."
- "Use `fortune(showMatches = TRUE)` to find anything about NULL, then summarize the consensus."
- "Show me three fortunes about S4 classes and rank them by snark."

## What makes a package a good fit

The R community has 20,000 CRAN packages but not all of them work cleanly as agent skills. The shape that fits:

- **Returns structured data** (data.frames, lists, S3 objects). The LLM consumes the result programmatically.
- **No printing to stderr** on the happy path. Anything that `cat()`s status updates or paints with `crayon::` pollutes the tool result.
- **No `oops()` or `stop()` for non-error cases**. If the function calls `stop()` because you're not in the right directory, it's designed for a human at the console, not a tool harness.
- **`.Rd` parameters that match `formals()`**. CRAN already enforces this with `R CMD check`, so any current CRAN package qualifies.

Counter-example: [gitr](https://cran.r-project.org/package=gitr) wraps `system2("git", ...)` cleanly enough but `cat()`s colored output and calls `oops()` when not in a git repo. Wrapping it as a skill needed `utils::capture.output()` and grew the code instead of shrinking it. Built for humans, not for plumbing.

The test: would you call this function from another function and trust the return value? If yes, it's a candidate.

## Why no MCP server

A live MCP server ships every tool's JSON schema into the system prompt at connect time. Twenty tools at ~400 tokens each is 8,000 tokens of startup overhead before the agent has done anything. The CLI / package-as-skill path pays roughly zero startup tax (corteza already has `bash`, `run_r`, `read_file` baked in), and lazy lookup via `saber::pkg_help()` costs ~200 tokens for a tool the agent actually needs.

Same tool surface, different cost curve. corteza's MCP server (`corteza::serve()`) still exists for clients that need it (Claude Code, Codex, etc), but it's no longer the only path — the same skill registry feeds both.

## Where to go next

- The R as CLI Agent Harness post explains the derivation pipeline: <https://cornball.ai/posts/2026-03-06-r-as-cli-agent-harness/>
- Discover what's in a package the same way the agent does: `Rscript --vanilla -e 'saber::pkg_exports("fortunes")'` and `Rscript --vanilla -e 'saber::pkg_help("fortune", "fortunes")'`.
- For your own functions, drop an `.R` file in `<project>/.corteza/skills/` calling `register_skill_from_fn("name", my_fn)`. Loaded on every session start, no installation required.
