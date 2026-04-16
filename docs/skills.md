# Skills: Isomorphic Agent Extensions

llamaR uses the same skill format as [openclaw](https://github.com/mariozechner/openclaw), making skills portable between R and TypeScript agent runtimes.

## What is a Skill?

A skill is a **SKILL.md** file that teaches the LLM how to accomplish a task using shell commands. Skills are:

- **Human-readable**: Markdown documentation you can read and edit
- **Portable**: Same file works in llamaR, openclaw, or any compatible agent
- **Shell-based**: Uses `bash` as the universal executor
- **Zero-code**: No programming required to create a skill

## Skill Format

```markdown
---
name: skill-name
description: One-line description
metadata: {"openclaw":{"emoji":"🔧","requires":{"bins":["curl"]}}}
---

# Skill Title

Documentation and examples in Markdown.

## Usage

```bash
command --with --flags
```

## Examples

More examples...
```

### Frontmatter Fields

| Field | Required | Description |
|-------|----------|-------------|
| `name` | Yes | Skill identifier (snake-case) |
| `description` | Yes | Short description for LLM context |
| `metadata` | No | openclaw-compatible metadata |

### Metadata (optional)

```json
{
  "openclaw": {
    "emoji": "🔧",
    "requires": {
      "bins": ["curl", "jq"],
      "env": ["API_KEY"]
    }
  }
}
```

## Skill Locations

llamaR loads skills from:

```
~/.llamar/skills/           # Global skills
    weather/
        SKILL.md
    github/
        SKILL.md
.llamar/skills/             # Project-local skills
    my-project-tool/
        SKILL.md
```

Both nested (`skillname/SKILL.md`) and flat (`skillname.md`) layouts work.

## How Skills Work

1. **Load**: llamaR parses SKILL.md files at startup
2. **Inject**: Skill content is added to the system prompt
3. **Use**: LLM reads the docs and generates shell commands
4. **Execute**: The `bash` tool runs the commands

The LLM doesn't call skills directly—it reads the documentation and uses `bash` to execute the commands described.

## Example: Weather Skill

```markdown
---
name: weather
description: Get current weather and forecasts (no API key required)
metadata: {"openclaw":{"emoji":"🌤️","requires":{"bins":["curl"]}}}
---

# Weather

Get weather using wttr.in:

```bash
curl -s "wttr.in/London?format=3"
```

## Options

- `?format=3` - one-line format
- `?0` - current weather only

## Examples

```bash
# Current weather
curl -s "wttr.in/NYC?format=3"

# Full forecast
curl -s "wttr.in/Tokyo"
```
```

## R Skills

R code runs via `r -e '...'` (littler) or `Rscript -e '...'`:

```markdown
---
name: r-eval
description: Execute R code and return results
metadata: {"openclaw":{"emoji":"📊","requires":{"bins":["r"]}}}
---

# R Code Execution

```bash
r -e 'mean(1:100)'
```

## Multi-line

```bash
r -e '
df <- mtcars[1:5, 1:3]
print(df)
'
```

## Run script file

```bash
r -f analysis.R
```
```

This skill works identically in llamaR and openclaw.

## Sharing Skills

Skills are just files. Share them by:

- Copying files between machines
- Git repositories
- Symlinks to a shared directory

```bash
# Clone a skill collection
git clone https://github.com/user/skills ~/.llamar/skills/community

# Symlink openclaw skills
ln -s ~/openclaw/skills/github ~/.llamar/skills/github
```

## Isomorphism with openclaw

| Aspect | llamaR | openclaw |
|--------|--------|----------|
| Format | SKILL.md | SKILL.md |
| Location | `~/.llamar/skills/` | `~/.openclaw/skills/` |
| Frontmatter | YAML | YAML |
| Metadata | `openclaw.requires` | `openclaw.requires` |
| Executor | `bash` tool | `bash` tool |

To use the same skills in both:

```bash
# Option 1: Symlink
ln -s ~/.openclaw/skills ~/.llamar/skills

# Option 2: Copy
cp -r ~/.openclaw/skills/* ~/.llamar/skills/
```

## Why Isomorphism Matters

**For R users:**
- Access to the entire openclaw skill ecosystem
- Skills created for llamaR work in openclaw (and vice versa)
- No lock-in to a single runtime

**For skill authors:**
- Write once, run anywhere
- Larger audience (R + TypeScript communities)
- Simpler format (Markdown, not code)

**For the ecosystem:**
- Shared skill libraries
- Community contributions benefit everyone
- Focus on documentation quality, not implementation details

## Creating a New Skill

1. Create directory: `mkdir ~/.llamar/skills/my-skill`
2. Create file: `touch ~/.llamar/skills/my-skill/SKILL.md`
3. Add frontmatter and documentation
4. Restart llamar (or start new session)

The skill is immediately available—no compilation, no installation.

## Best Practices

1. **Clear examples**: Show complete, runnable commands
2. **Explain flags**: Document what each option does
3. **Error handling**: Show how to check for failures
4. **Dependencies**: List required binaries in metadata
5. **Keep it focused**: One skill = one task domain

## Stateful vs Stateless R

### Stateless (Portable)

Shell-based R skills use `r -e` or `Rscript -e`. Each call starts a fresh R session:

```bash
r -e 'x <- 1:10; mean(x)'   # x is gone after this
r -e 'print(x)'              # Error: object 'x' not found
```

This is **portable**—works in llamaR, openclaw, or any agent with a `bash` tool.

### Stateful (llamaR)

llamaR's built-in `run_r` MCP tool maintains a persistent R session:

```r
# First call
run_r("x <- 1:10")

# Second call - x still exists
run_r("mean(x)")  # Returns 5.5
```

This is **llamaR-specific**—the R session lives in the MCP server process.

### When to Use Which

| Use Case | Approach |
|----------|----------|
| One-off calculations | Stateless (`r -e`) |
| Data pipelines | Stateless (save intermediate results to files) |
| Interactive analysis | Stateful (`run_r`) |
| Package development | Stateful (`run_r`) |
| Portable skills | Stateless (`r -e`) |

### Stateful R from openclaw

openclaw users who need stateful R can connect to llamaR as an MCP server:

```bash
# Terminal 1: Start llamaR MCP server
Rscript -e 'llamaR::serve(port = 7850)'

# Terminal 2: Configure openclaw to use it
# Add llamaR as an MCP tool provider in openclaw config
```

This gives openclaw access to `run_r`, `r_help`, and other R-native tools while keeping skills portable.

## Built-in MCP Tools

llamaR includes these built-in tools (not skills):

| Tool | Purpose | Stateful? |
|------|---------|-----------|
| `run_r` | Execute R in persistent session | Yes |
| `run_r_script` | Execute R in subprocess (via littler) | No |
| `r_help` | Query R documentation via saber | No |
| `installed_packages` | List installed R packages | No |

These are MCP tools, not SKILL.md files. They're always available in llamaR and can be exposed to other agents via the MCP server.
