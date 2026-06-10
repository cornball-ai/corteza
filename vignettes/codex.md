<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Using Codex with corteza}
-->
---
title: Using Codex with corteza
---

<img src="../man/figures/corteza.png" alt="corteza logo" align="right" width="160" />

# Using Codex with corteza

corteza is provider-plural R-native agent tooling. It can run with Anthropic, OpenAI, Moonshot, Ollama, and ChatGPT-account-backed Codex access through `llm.api`.

Codex support currently needs development builds of `tinyoauth` and `llm.api`, then corteza from CRAN:

```r eval=FALSE
remotes::install_github("cornball-ai/tinyoauth")
remotes::install_github("cornball-ai/llm.api")
install.packages("corteza")
```

The practical value is that a current ChatGPT subscription can drive the same R package workflow as API-key-backed models: inspect a project, edit files, run R code, run tests, and review a git diff. The provider changes, the R development loop stays portable.

## Prerequisites

You need:

- corteza installed.
- A development build of `llm.api` with `openai_codex` support installed. corteza imports `llm.api`.
- `tinyoauth` installed for ChatGPT-account-backed Codex login through `llm.api::openai_codex_login()`.
- Access to the OpenAI or Codex model you plan to use.

corteza talks to OpenAI and Codex through `llm.api`. It does not require the Codex CLI for the examples in this vignette.

There are 2 useful provider names:

- `openai_codex`: ChatGPT-account-backed Codex access. Log in once with `llm.api::openai_codex_login()`; `tinyoauth` caches and refreshes the token.
- `openai`: OpenAI API-key access through `OPENAI_API_KEY`.

## Authentication

For ChatGPT-account-backed Codex access, run the device-code login once:

```r eval=FALSE
llm.api::openai_codex_login()
```

The login prints a verification URL and code. `tinyoauth` handles the token cache, so later corteza sessions can use `provider = "openai_codex"` without another login.

For OpenAI API-key access, set `OPENAI_API_KEY` in your environment. A common place is `~/.Renviron`:

```text
OPENAI_API_KEY=sk-...
```

For a one-session fallback, set it inside R before starting corteza:

```r eval=FALSE
Sys.setenv(OPENAI_API_KEY = "sk-...")
```

Never print real tokens in your console history, transcripts, tests, or vignettes.

## A minimal Codex-backed corteza session

Start the terminal agent with the `openai_codex` provider:

```sh
corteza --provider openai_codex
```

Or start the in-session R console agent:

```r eval=FALSE
corteza::chat(provider = "openai_codex")
```

You can pass a model when you want one explicitly:

```r eval=FALSE
corteza::chat(
  provider = "openai_codex",
  model = "gpt-5.3-codex-spark"
)
```

Use `openai` when you want OpenAI API-key-backed access instead:

```r eval=FALSE
corteza::chat(provider = "openai")
```

## Using Codex on an R package

A typical package-development session starts from a project root:

```sh
cd path/to/your/package
corteza --provider openai_codex
```

Then give corteza a scoped prompt, for example:

```text
Inspect this package, run the tests, and propose the smallest change needed to fix failing checks.
```

A safer workflow is explicit:

1. Inspect `DESCRIPTION`, `NAMESPACE`, vignettes, R files, and tests.
2. Propose a plan before editing.
3. Edit the smallest set of files.
4. Run targeted tests, then broader checks when needed.
5. Review `git diff` before committing.

Inside R, the same idea works with `chat()`:

```r eval=FALSE
setwd("path/to/your/package")
corteza::chat(provider = "openai_codex")
```

Then paste the package-development prompt at the chat prompt.

## Tooling expectations

corteza exposes project tools to the model. The exact set can be filtered by configuration and by the `tools` argument. Common built-in tools include:

| Area | Tools |
|------|-------|
| Files | `read_file`, `write_file`, `replace_in_file`, `list_files` |
| Code | `run_r`, `run_r_script`, `bash` (or `cmd` on Windows) |
| Search | `grep_files` |
| R help | `r_help`, `installed_packages` |
| Git | `git_status`, `git_diff`, `git_log` |
| Web | `web_search`, `fetch_url` |
| Handles | `read_handle` |
| Background jobs | `bg_status`, `bg_kill` |
| Planning | `task_create`, `task_update`, `exit_plan_mode` |
| Subagents | `spawn_subagent`, `query_subagent`, `collect_subagent`, `list_subagents`, `kill_subagent` |

Use a smaller tool set for focused work:

```r eval=FALSE
corteza::chat(provider = "openai_codex", tools = "core")
```

`core` expands to file, code, and git tools. `all` enables the full available set.

## CRAN-safe examples

This vignette does not make live model calls while building. Networked examples are marked as non-evaluated.

Local inspection is safe. For example, you can ask `llm.api` which model corteza will use by default for a provider:

```r
llm.api::provider_default_model("openai_codex")
llm.api::provider_default_model("openai")
```

You can also inspect the corteza chat interface without starting a session:

```r
args(corteza::chat)
```

## Troubleshooting

### Missing OpenAI credentials

For `openai`, set `OPENAI_API_KEY`. Put it in `~/.Renviron` for normal R sessions, then restart R or call `readRenviron("~/.Renviron")`.

For `openai_codex`, run:

```r eval=FALSE
llm.api::openai_codex_login()
```

### Expired OAuth token

Run `llm.api::openai_codex_login()` again. The token cache is managed by `tinyoauth` through `llm.api`.

### Model unavailable

Model access depends on your OpenAI or ChatGPT account. Try the provider default first:

```r eval=FALSE
corteza::chat(provider = "openai_codex")
```

Then specify a model only after confirming it is available to your account.

### Package missing or old

Install or update corteza from CRAN:

```r eval=FALSE
install.packages("corteza")
```

If `provider = "openai_codex"` or `llm.api::openai_codex_login()` is missing, install the development builds shown at the top of this vignette before starting the chat session.

### Tool execution disabled or sandboxed

corteza applies tool policy before execution. If a tool call is blocked, check your `.corteza/config.json`, global corteza config, and any approval prompts shown by the CLI or R console.

### API-key auth versus ChatGPT-account auth

Use `provider = "openai"` with `OPENAI_API_KEY`.

Use `provider = "openai_codex"` with `llm.api::openai_codex_login()` and the cached ChatGPT-account token.

## Provider-plural workflow

The same project workflow can target another provider by changing the provider and, optionally, the model:

```r eval=FALSE
corteza::chat(provider = "anthropic")
corteza::chat(provider = "moonshot")
corteza::chat(provider = "ollama", model = "llama3.2")
```

Project config can make that choice persistent:

```json
{
  "provider": "openai_codex",
  "model": "gpt-5.3-codex-spark",
  "tools": "core"
}
```

Save that as `.corteza/config.json` in the project root when you want the CLI and `chat()` defaults to follow the project.
