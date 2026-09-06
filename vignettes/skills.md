<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Skills}
-->
---
title: Skills
---

<img src="../man/figures/corteza.png" alt="corteza logo" align="right" width="160" />

# Skills

An instruction skill is a `SKILL.md` file that teaches the agent how to do
something. It is documentation, not an executable tool and not a capability
grant. It may explain shell commands, R workflows, or how to use tools the
session already exposes.

corteza has three ways to add tools, and they target different audiences:

| Form | Audience | Config key | Vignette |
|------|----------|------------|----------|
| Package skills | R packages as tools | `skill_packages` | `vignette("package-as-skill")` |
| Instruction skills | Portable documentation | `instruction_roots` | this one |
| R skills | R functions registered as tools | standard skill roots | this one |

## Format

A `SKILL.md` is plain markdown with YAML frontmatter:

````markdown
---
name: weather
description: Get current weather and forecasts (no API key required)
metadata: {"requires":{"bins":["curl"]}}
---

# Weather

Get weather using wttr.in:

```bash
curl -s "wttr.in/London?format=3"
```

## Options

- `?format=3`: one-line format
- `?0`: current weather only
````

### Frontmatter

| Field | Required | Description |
|-------|----------|-------------|
| `name` | yes | snake-case identifier |
| `description` | yes | One-line catalog summary; folded YAML is supported |
| `metadata` | no | Optional metadata; the `requires` block declares external deps |

The `metadata.requires` field is openclaw-compatible, so skills written for openclaw work in corteza without modification (and vice versa). Conceptually `requires.bins` is to a skill what `SystemRequirements` is to an R package: a declaration of external binaries the skill needs (`curl`, `jq`, etc). `requires.env` lists environment variables (API keys, tokens). corteza stores both for documentation; it doesn't gate skill loading on them.

## Where skills live

| Scope | Path |
|-------|------|
| Global | `tools::R_user_dir("corteza", "data")/skills/` |
| Project | `.corteza/skills/` |

Discovery is recursive and includes only files named exactly `SKILL.md`.
Names do not override each other: lookup uses the stable id derived from the
configured root id plus the skill directory's relative path. Additional
roots are configured explicitly through `instruction_roots`; corteza does
not scan `~/skills` implicitly.

## How skills get invoked

The agent receives only a compact catalog at session start, then retrieves
instructions as needed:

1. **Discover**: corteza recursively finds exact `SKILL.md` files.
2. **Snapshot**: it records the document and relative-resource hashes for
   this session.
3. **Advertise**: only name, description, and stable id enter the prompt.
4. **Read**: `skill_instructions(id)` returns the selected `SKILL.md`;
   `skill_instructions(id, resource)` reads a snapshotted relative file.
5. **Act**: the model uses its normal, separately granted tools.

Absolute paths, traversal, foreign ids, symlink escapes, and files changed
after session start are refused. Opening another session or Matrix room does
not mutate an existing session's catalog.

## R skills

R one-liners run via `Rscript`:

````markdown
---
name: r-eval
description: Execute one-shot R code
metadata: {"requires":{"bins":["Rscript"]}}
---

# R one-liners

```bash
Rscript --vanilla -e 'mean(1:100)'
```

## Multi-line

```bash
Rscript --vanilla -e '
df <- mtcars[1:5, 1:3]
print(df)
'
```
````

This skill is **stateless**: each `Rscript` call starts a fresh R session.

For **stateful** R (objects persist across turns), corteza's built-in `run_r` tool maintains a long-lived R session in the MCP server process. The agent picks `run_r` for interactive analysis and `Rscript` for portable one-shots.

| Use case | Approach |
|----------|----------|
| One-off calculation | Stateless |
| Data pipeline | Stateless, write intermediate state to files |
| Interactive analysis | Stateful (`run_r`) |
| Package development | Stateful (`run_r`) |
| Portable skill | Stateless |

## Built-in tools

corteza ships with tools that don't have a `SKILL.md`:

| Tool | Purpose | Stateful? |
|------|---------|-----------|
| `run_r` | Execute R in the persistent session | Yes |
| `run_r_script` | Execute R in a subprocess | No |
| `r_help` | Query R documentation via saber | No |
| `installed_packages` | List installed R packages | No |

You don't need to write these as skills; they're always available.

## Authoring a skill

```bash
mkdir -p .corteza/skills/my-skill
$EDITOR .corteza/skills/my-skill/SKILL.md
```

Add frontmatter and documentation, then restart the session. Catalogs are
immutable snapshots, not live filesystem views.

## Best practices

- **Show complete commands**, not snippets. The agent copies what it sees.
- **Document the flags**. `?format=3` is opaque; "one-line format" is not.
- **Declare external dependencies** in `metadata.requires.bins`. The `requires.env` block lists API keys that need to be set.
- **One skill per task domain**. Splitting beats stuffing.
- **Show error handling** if the command can fail in non-obvious ways.

## Sharing

Skills are just files. Commit them, symlink them, copy them.

```bash
# Personal collection of skills, used across projects
ln -s ~/skills $(Rscript -e 'cat(file.path(tools::R_user_dir("corteza","data"),"skills"))')

# Or just clone a collection into the project
git clone <your-skills-repo-url> .corteza/skills
```
