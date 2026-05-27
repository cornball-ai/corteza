<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Skills}
-->
---
title: Skills
---

<img src="../man/figures/corteza.png" alt="corteza logo" align="right" width="160" />

# Skills

A skill is a `SKILL.md` file that teaches the agent how to do something using shell commands. Skills are markdown documentation: the agent reads them and runs `bash` to execute. No code, no compilation, no installation.

corteza has three ways to add tools, and they target different audiences:

| Form | Audience | Config key | Vignette |
|------|----------|------------|----------|
| Package skills | R packages as tools | `skill_packages` | `vignette("package-as-skill")` |
| `SKILL.md` files | Portable, shell-based | `skill_paths` | this one |
| R skills | R functions, registered directly | `skill_paths` (`.R` files) | this one |

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
| `description` | yes | One-liner that goes into the system prompt |
| `metadata` | no | Optional metadata; the `requires` block declares external deps |

The `metadata.requires` field is openclaw-compatible, so skills written for openclaw work in corteza without modification (and vice versa). Conceptually `requires.bins` is to a skill what `SystemRequirements` is to an R package: a declaration of external binaries the skill needs (`curl`, `jq`, etc). `requires.env` lists environment variables (API keys, tokens). corteza stores both for documentation; it doesn't gate skill loading on them.

## Where skills live

| Scope | Path |
|-------|------|
| Global | `tools::R_user_dir("corteza", "data")/skills/` |
| Project | `.corteza/skills/` |

Both nested (`my-skill/SKILL.md`) and flat (`my-skill.md`) layouts work. Project-local skills override global ones with the same name.

## How skills get invoked

The agent doesn't call skills directly. It reads the markdown into the system prompt at session start, then generates `bash` commands when the conversation calls for them.

1. **Load**: corteza scans the skill paths at session start
2. **Inject**: frontmatter and body land in the system prompt
3. **Use**: the LLM reads the docs and generates the right command
4. **Execute**: the `bash` tool runs it

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

Add frontmatter and documentation, then restart the session. Skills load at startup, not on the fly.

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
