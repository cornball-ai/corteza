# llamaR

<img src="man/figures/llamaR.png" alt="llamaR logo" width="200" />

**An AI agent runtime for R.** Self-hosted, model-agnostic, tinyverse.

In Spanish, *llamar* (pronounced ["Ya Mar"](https://www.youtube.com/watch?v=p-2EZXOoFt8)) means "to call."

-----

## Quick Start

```bash
# Install the CLI
r -e 'llamaR::install_cli()'

# Start the agent
llamar
```

That's it. You're talking to an AI agent that can read files, run shell commands, query git, search the web, and execute R code in a persistent session.

```
llamar | claude-sonnet-4-20250514 @ anthropic | 24 tools

> What R packages are loaded?
  [run_r] Running: loadedNamespaces()
12 packages loaded: base, utils, stats, ...

> llamar --provider ollama --model llama3.2
```

-----

## Why R?

In the agent world, tools, skills, and the servers that expose them are three separate things you have to design, host, and wire together yourself. R packages bundle all three: the capability, the documentation that teaches you how to use it, and the mechanism that makes it available to your session. Versioned, tested, and delivered in a single `install.packages()` call. CRAN has been doing integrated tool distribution for 20 years. The agent ecosystem is just now figuring out that's hard.

- **`install.packages()`** works the same on Windows, Mac, and Linux. No brew, no nvm, no Docker, no venv.
- **Packages are tools, skills, and servers in one artifact.** The 20,000+ packages on CRAN are agent capabilities waiting to be wired up.
- **CRAN** reviews packages by hand and tests on 3 OSs. That's a quality gate most tool registries don't have.

-----

## Three Ways to Use llamaR

### 1. CLI agent (`llamar`)

The primary interface. A terminal agent with session management, voice mode, and context compaction. Uses the MCP server internally for tool execution.

```bash
llamar                    # Start agent
llamar --resume           # Resume last session
llamar --provider ollama  # Use local models
llamar --provider moonshot --model kimi-k2
```

### 2. MCP server (`serve()`)

Exposes a persistent R session to external MCP clients (Claude Code, Claude Desktop, VS Code). This is how other tools get stateful R access: objects persist across tool calls, packages stay loaded, and runtime inspection tools like `mirar` work on real session state.

```json
{
  "mcpServers": {
    "llamaR": {
      "command": "Rscript",
      "args": ["-e", "llamaR::serve()"]
    }
  }
}
```

### 3. In-session agent (`chat()`)

Runs inside your R console. Tools execute as direct function calls, no MCP server, no subprocess. Your `.GlobalEnv` objects are the agent's objects.

```r
chat()                           # Claude (default)
chat(provider = "openai")        # GPT-4o
chat(provider = "moonshot")      # Kimi K2
chat(provider = "ollama",        # Local
     model = "llama3.2")
```

This is the most interesting mode architecturally (the agent lives in the same process as the data), but current LLMs are trained on bash-style CLIs, so they perform better through the terminal interface. The `/r <code>` command lets you eval R directly without an LLM roundtrip.

-----

## Tools

|Tool                |Description                           |
|--------------------|--------------------------------------|
|`bash`              |Run shell commands                    |
|`read_file`         |Read file contents                    |
|`write_file`        |Write or create files                 |
|`replace_in_file`   |Make exact text replacements in files |
|`list_files`        |List directory contents               |
|`grep_files`        |Search file contents                  |
|`run_r`             |Execute R code in the session         |
|`r_help`            |Query R documentation via saber       |
|`installed_packages`|List and search R packages            |
|`git_status`        |Git working tree status               |
|`git_diff`          |Git diff                              |
|`git_log`           |Git commit history                    |
|`web_search`        |Search the web (requires Tavily key)  |
|`fetch_url`         |Fetch web content                     |

-----

## Installation

```r
# llamaR (not yet on CRAN)
remotes::install_github("cornball-ai/llamaR")

# LLM provider abstraction (not yet on CRAN)
remotes::install_github("cornball-ai/llm.api")
```

### API Keys

Set in `~/.Renviron`:

```
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
MOONSHOT_API_KEY=sk-...
TAVILY_API_KEY=tvly-...   # Optional, for web search
```

### CLI (Optional)

```r
# Install the llamar CLI to ~/bin
llamaR::install_cli()
```

-----

## Landscape

Anthropic's [Claude Agent SDK](https://platform.claude.com/docs/en/agent-sdk/overview) gives you Claude Code as a library, in Python and TypeScript. [nanoclaw](https://github.com/qwibitai/nanoclaw) builds on it. There's no R equivalent.

llamaR fills that gap. Not by wrapping Anthropic's SDK, but by building an R-native agent runtime from scratch. Model-agnostic (Anthropic, OpenAI, Moonshot, Ollama), small enough to read in an afternoon.

|Role           |Posit (tidyverse)                      |cornyverse|
|---------------|---------------------------------------|----------|
|LLM API client |[ellmer](https://ellmer.tidyverse.org/)|llm.api   |
|Context tools  |[btw](https://posit-dev.github.io/btw/)|saber     |
|MCP bridge     |mcptools                               |llamaR    |

mcptools integrates R into the broader MCP ecosystem (Claude Desktop, VS Code, Positron). It's polished, on CRAN, and backed by Posit.

llamaR is a standalone agent runtime. `chat()` runs inside your R session. The CLI runs from your terminal. No external client required.

-----

## Design Philosophy

- **Small enough to understand**: one package, one import (jsonlite), readable source
- **Model-agnostic**: Anthropic, OpenAI, Moonshot, Ollama
- **R-native**: packages are skills, `install.packages()` is the marketplace
- **Hackable**: add tools by writing R functions

-----

## Platform Support

|Platform|Status                          |
|--------|--------------------------------|
|Linux   |Fully supported                 |
|macOS   |Expected to work                |
|Windows |Partial (stdin handling pending) |

-----

## Status

Experimental. Interfaces may change.

-----

## License

MIT
