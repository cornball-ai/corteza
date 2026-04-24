# corteza

<img src="man/figures/corteza.png" alt="corteza logo" width="200" />

**An AI agent runtime for R.** Self-hosted, model-agnostic, tinyverse.

*Corteza* is Spanish for the brain's cortex: the outer layer where processing happens.

-----

## Quick Start

```bash
# Install the CLI
r -e 'corteza::install_cli()'

# Start the agent
corteza
```

That's it. You're talking to an AI agent that can read files, run shell commands, query git, search the web, and execute R code in a persistent session.

```
corteza | claude-sonnet-4-6 @ anthropic

> What R packages are loaded?
  [run_r] Running: loadedNamespaces()
12 packages loaded: base, utils, stats, ...

> corteza --provider ollama --model llama3.2
```

-----

## Why R?

In the agent world, tools, skills, and the servers that expose them are three separate things you have to design, host, and wire together yourself. R packages bundle all three: the capability, the documentation that teaches you how to use it, and the mechanism that makes it available to your session. Versioned, tested, and delivered in a single `install.packages()` call. CRAN has been doing integrated tool distribution for 29 years. The agent ecosystem is just now figuring out that's hard.

- **`install.packages()`** works the same on Windows, Mac, and Linux. No brew, no nvm, no Docker, no venv.
- **Packages are tools, skills, and servers in one artifact.** The 20,000+ packages on CRAN are agent capabilities waiting to be wired up.
- **CRAN** reviews packages by hand and tests on 3 OSs. That's a quality gate most tool registries don't have.

-----

## Three Ways to Use corteza

### 1. CLI agent (`corteza`)

The primary interface. A terminal agent with session management and context compaction. 

```bash
corteza                    # Start agent
corteza --resume           # Resume last session
corteza --provider ollama  # Use local models
corteza --provider moonshot --model kimi-k2
```

### 2. MCP server (`serve()`)

Exposes a persistent R session to external MCP clients (Claude Code, Claude Desktop, VS Code). This is how other tools get stateful R access: objects persist across tool calls, packages stay loaded, and `saber` introspection tools see real session state.

```json
{
  "mcpServers": {
    "corteza": {
      "command": "Rscript",
      "args": ["-e", "corteza::serve()"]
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

This is the most interesting mode architecturally (the agent lives in the same process as the data), but current LLMs are trained on bash-style CLIs, so they perform better through the terminal interface. The `/r <code>` command evaluates R locally, shows you the output, and stages that result into the context for your next message (a fast way to load data or inspect state before asking the model about it, without burning an LLM roundtrip).

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
# corteza (not yet on CRAN)
remotes::install_github("cornball-ai/corteza")

# Dependencies are on CRAN
install.packages(c("llm.api", "saber"))
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
# Install the corteza CLI to ~/bin
corteza::install_cli()
```

-----

## Landscape

Anthropic's [Claude Agent SDK](https://platform.claude.com/docs/en/agent-sdk/overview) gives you Claude Code as a library, in Python and TypeScript. [nanoclaw](https://github.com/qwibitai/nanoclaw) builds on it. There's no R equivalent.

corteza fills that gap. Not by wrapping Anthropic's SDK, but by building an R-native agent runtime from scratch. Model-agnostic (Anthropic, OpenAI, Moonshot, Ollama), small enough to read in an afternoon.

|Role             |Posit (tidyverse)                      |cornyverse|
|-----------------|---------------------------------------|----------|
|LLM API client   |[ellmer](https://ellmer.tidyverse.org/)|llm.api   |
|Context tools    |[btw](https://posit-dev.github.io/btw/)|saber     |
|Matrix API client|n/a                                    |mx.api    |
|Agent runtime    |mcptools                               |corteza   |

mcptools integrates R into the broader MCP ecosystem (Claude Desktop, VS Code, Positron). It's polished, on CRAN, and backed by Posit.

corteza is a standalone agent runtime. `chat()` runs inside your R session. The CLI runs from your terminal. No external client required.

-----

## Design Philosophy

- **Small enough to understand**: a handful of imports (curl, jsonlite, processx, codetools, llm.api, saber), readable source
- **Model-agnostic**: Anthropic, OpenAI, Moonshot, Ollama
- **R-native**: packages are skills, `install.packages()` is the marketplace
- **Hackable**: add tools by writing R functions

-----

## Platform Support

|Platform|Status                          |
|--------|--------------------------------|
|Linux   |Fully supported                 |
|macOS   |Expected to work                |
|Windows |Supported (see Windows setup below) |

### Windows setup

Install, in order:

1. **R 4.4 or 4.5** from CRAN. The installer adds R to PATH by default; verify with `where R`.
2. **Rtools** matching your R version ([Rtools45](https://cran.r-project.org/bin/windows/Rtools/rtools45/) for R 4.5.x, [Rtools44](https://cran.r-project.org/bin/windows/Rtools/rtools44/) for R 4.4.x). Rtools gives you a real bash, GCC, and the build toolchain for Rcpp or any compiled code.
3. **git** — either install [Git for Windows](https://git-scm.com/downloads/win) system-wide, or drop it into Rtools' MSYS2 from a Rtools bash shell:

   ```bash
   pacman -Sy git
   ```

corteza resolves bash to an absolute path (Rtools first, then Git for Windows) so `C:\Windows\System32\bash.exe` (the WSL launcher stub) never intercepts shell commands. Minimal installs without any bash still get a working `cmd` shell tool.

User environment variables live in `C:\Users\<user>\.Renviron`. Use that for `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, etc.

-----

## Status

Experimental. Interfaces may change.

-----

## License

MIT
