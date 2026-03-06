# llamaR

<img src="man/figures/llamaR.png" alt="llamaR logo" width="200" />

**An AI agent runtime for R.** Self-hosted, model-agnostic, tinyverse.

In Spanish, *llamar* (pronounced ["Ya Mar"](https://www.youtube.com/watch?v=p-2EZXOoFt8)) means "to call."

-----

## Quick Start

```r
library(llamaR)
chat()
```

That's it. You're talking to an AI agent inside your R session. It can read files, run shell commands, query git, search the web, and execute R code directly in your environment.

```
llamaR chat | claude-sonnet-4-20250514 @ anthropic | 24 tools | /r to eval R | /quit to exit

> Create a data frame with 10 random numbers and their squares
  [run_r] (11 lines)
Done. The data frame `df` is in your workspace.

> /r df
    random    squared
1  0.2876 0.08270083
2  0.7883 0.62142499
...

> /quit
Bye.
```

The agent evals R code in `.GlobalEnv`. Your objects are its objects. When you `/r df`, you're running R directly, no LLM roundtrip.

Switch providers with one argument:

```r
chat(provider = "openai")   # GPT-4o
chat(provider = "ollama")   # Local models
```

-----

## Why R?

In the agent world, tools, skills, and the servers that expose them are three separate things you have to design, host, and wire together yourself. R packages bundle all three: the capability, the documentation that teaches you how to use it, and the mechanism that makes it available to your session. Versioned, tested, and delivered in a single `install.packages()` call. CRAN has been doing integrated tool distribution for 20 years. The agent ecosystem is just now figuring out that's hard.

- **`install.packages()`** works the same on Windows, Mac, and Linux. No brew, no nvm, no Docker, no venv.
- **Packages are tools, skills, and servers in one artifact.** The 20,000+ packages on CRAN are agent capabilities waiting to be wired up.
- **CRAN** reviews packages by hand and tests on 3 OSs. That's a quality gate most tool registries don't have.

-----

## How It Works

llamaR has two modes:

### 1. In-session agent (`chat()`)

Runs inside your R session. Tools execute as direct function calls via the skill registry. No MCP server, no subprocess.

```r
chat()                           # Claude (default)
chat(provider = "openai")        # GPT-4o
chat(provider = "ollama",        # Local
     model = "llama3.2")
chat(tools = "core")             # Minimal tools (file + code + git)
```

In-session commands:
- `/r <code>` evaluates R code directly (auto-prints like the console)
- `/quit`, `/exit`, `/q` exits the chat

### 2. MCP server (`serve()`)

Exposes tools to external MCP clients (Claude Desktop, VS Code, other agents).

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

### 3. CLI agent (`llamar`)

A terminal agent with full session management, voice mode, and context compaction.

```bash
llamar                    # Start agent
llamar --resume           # Resume last session
llamar --provider ollama  # Use local models
```

-----

## Tools

|Tool                |Description                           |
|--------------------|--------------------------------------|
|`bash`              |Run shell commands                    |
|`read_file`         |Read file contents                    |
|`write_file`        |Write or create files                 |
|`list_files`        |List directory contents               |
|`grep_files`        |Search file contents                  |
|`run_r`             |Execute R code in the session         |
|`r_help`            |Query R documentation via fyi         |
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

llamaR fills that gap. Not by wrapping Anthropic's SDK, but by building an R-native agent runtime from scratch. Model-agnostic (Anthropic, OpenAI, Ollama), small enough to read in an afternoon.

|Role           |Posit (tidyverse)                      |cornyverse|
|---------------|---------------------------------------|----------|
|LLM API client |[ellmer](https://ellmer.tidyverse.org/)|llm.api   |
|Context tools  |[btw](https://posit-dev.github.io/btw/)|fyi       |
|MCP bridge     |mcptools                               |llamaR    |

mcptools integrates R into the broader MCP ecosystem (Claude Desktop, VS Code, Positron). It's polished, on CRAN, and backed by Posit.

llamaR is a standalone agent runtime. `chat()` runs inside your R session. The CLI runs from your terminal. No external client required.

-----

## Design Philosophy

- **Small enough to understand**: one package, one import (jsonlite), readable source
- **Model-agnostic**: Anthropic, OpenAI, Ollama
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
