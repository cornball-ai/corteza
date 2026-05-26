<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Retroactive-Extraction Runtime}
-->
---
title: Retroactive-Extraction Runtime
---

<img src="../man/figures/corteza.png" alt="corteza logo" align="right" width="160" />

# Retroactive-Extraction Runtime

An opt-in runtime where finished turns collapse into subagents that
hold the full transcript, while the parent's conversation history
keeps only a short summary plus the subagent id. The LLM sees the live
subagents in its system prompt and picks `query_subagent` or
`spawn_subagent` as a normal tool decision.

Default off. Existing CRAN users see no behavior change.

## Why

Long sessions accumulate raw tool-use / tool-result blocks in the
parent's history. Eventually the model hits `[Max turns reached]` (in
`llm.api::agent`) and the work-so-far freezes as a string the parent
can't recover from. The retroactive-extraction runtime turns that into
a recoverable state: the full transcript moves into a held subagent,
the parent gets a compact summary, and the LLM can `query_subagent`
for detail when (and only when) it actually needs it.

This stays compatible with the tinyverse philosophy: no graph
framework, no router, no role taxonomy. The same model that picks
`read_file` vs `bash` picks `query_subagent` vs `spawn_subagent`.

## Enabling

Add to your project or global config:

```json
{
  "subagents": { "enabled": true },
  "archival": {
    "enabled": true,
    "trigger": {
      "on_max_turns": true,
      "token_threshold": 8000,
      "tool_call_threshold": 5,
      "depth_cap": 3
    },
    "summary": {
      "style": "structured",
      "model": null
    }
  }
}
```

`archival.enabled = true` requires `subagents.enabled = true`. corteza
errors at config-load time rather than silently overriding.

## How it works

After every CLI turn (in `chat()` or `inst/bin/corteza`), the runtime
calls `maybe_archive_turn()`:

1. If `archival.enabled` is FALSE, return immediately.
2. Compute the just-finished turn's history slice.
3. Evaluate triggers:
   - `on_max_turns`: did the turn end with `[Max turns reached]`?
   - `token_threshold`: did the slice exceed N estimated tokens?
   - `tool_call_threshold`: did the slice contain M+ tool-use pairs?
   - `depth_cap`: at the cap, never archive (recursion stop).
4. Refuse to archive if the slice ends with an unfinished `tool_use`
   (turn isn't really finished).
5. Spawn a holder subagent with no tools (`tools = character(0)`).
   Seed it with the turn's history via `subagent_seed_history()`.
6. Generate a summary via a one-shot `llm.api::agent` call using the
   parent's provider and model (or the override in
   `archival.summary.model`).
7. Persist the holder's transcript to
   `agents/subagent-<id>/sessions/<id>.jsonl` using the same
   `transcript_append` infra parent sessions use.
8. Replace the parent's slice with one synthetic assistant message:
   `[archived turn]\nsubagent_id: <id>\n\n<summary>`.
9. Refresh `turn_session$system` so the next turn's system prompt
   shows the new subagent in the live-subagents block.

## What the LLM sees

The system prompt grows a section that looks like this:

```
# Live Subagents

These subagents hold archived prior turns of this conversation. Use
the `query_subagent(id, prompt)` tool to retrieve detail; spawn a new
one with `spawn_subagent(task)` to fan out. Do not query unless you
actually need the detail.

- id: 4f9d2a... | task: Archive: Find the auth handler
- id: 8b1c33... | task: Archive: Add validation to the login form
```

The LLM uses normal tool selection to decide between
`query_subagent(id, "what did you find at line 42?")` and
`spawn_subagent("...")` for new work.

## Permission model

Holder subagents (the ones the archival runtime creates) carry
`tools = character(0)`: they hold transcript context for
`query_subagent`, nothing more. Subagents spawned via
`spawn_subagent(task)` inherit only the tools the parent grants at
spawn time. There is no interactive approval channel back to the
parent or user, and no way to grant capability mid-run — the child's
`approval_cb` denies by default. If a child may need shell, write, or
network capability, pick a preset or explicit tool list that includes
it when calling `spawn_subagent()`; otherwise the child should report
that it is blocked rather than retry.

## Recursion

A subagent finishing its own query re-evaluates the same triggers
(inside `subagent_turn_prompt()`). If they fire, the subagent archives
into a sub-subagent. The `depth_cap` (default 3) is the hard stop.

Each subagent's `.subagent_registry` is process-local: the parent
won't see grandchild subagents until it queries the child. The parent
still has the original child's id and can drill in via
`query_subagent` to follow the chain.

## Recovery

The full transcript of every archived turn is on disk under:

```
tools::R_user_dir("corteza", "data")/agents/subagent-<id>/sessions/<id>.jsonl
```

Same JSONL format as parent sessions. You can inspect them with the
same tools that read parent transcripts. The parent transcript also
gets one `[archived: <id>]` line so a single-file read of the parent
transcript shows the full session shape.

## Summary styles

- `structured` (default): JSON object with keys `outcome`,
  `key_findings`, `files_touched`, `tools_used`, `open_questions`.
  Easier for the LLM to reason over field by field.
- `paragraph`: 3-5 sentences, no bullets.

The Ollama provider has unreliable JSON output, so when
`provider == "ollama"` the runtime forces paragraph style with a
warning.

## Known limitations

1. **Token estimator underestimates** large JSON tool results. Default
   `token_threshold = 8000` may correspond to true ~12000 tokens. If
   you care, lower it.
2. **Summary becomes the parent's only memory**: a poorly-summarized
   critical detail is recoverable only by querying the holder.
   Structured summaries hedge with `key_findings` and
   `open_questions`.
3. **Subagent registry leaks across `chat()` exits**. Pre-existing
   issue; archival makes it more visible. Restart your session to
   clean up.
4. **Recursion across processes**: each subagent has its own
   `.subagent_registry`. The parent doesn't see sub-subagents until it
   queries.
5. **Mutated R session state isn't unwound by archival**. If a tool
   loaded a package or set an option in the parent process, that
   mutation survives. Most corteza tools don't mutate session state;
   `r_help` and arbitrary `library()` calls inside `run_r` are the
   leakers, and they're benign in practice.

## Disabling

Set `archival.enabled` back to `false` (or remove the block). Existing
on-disk subagent transcripts stay where they are; new turns return to
the pre-archival behavior.
