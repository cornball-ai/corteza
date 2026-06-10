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

For most developers, subscription access via ChatGPT-account-backed Codex is dramatically more cost effective than metered API keys. Claude Code no longer offers this. A current ChatGPT subscription can drive the same R package workflow as API-key-backed models: inspect a project, edit files, run R code, run tests, and review a git diff. The provider changes, the R development loop stays portable.

## Prerequisites

You need:

- corteza installed.
- The development build of `llm.api` with `openai_codex` support installed. corteza imports `llm.api`.
- `tinyoauth` installed for ChatGPT-account-backed Codex login through `llm.api::openai_codex_login()`.
- Access to the OpenAI or Codex model you plan to use.

corteza talks to OpenAI and Codex through `llm.api`. It does not require the Codex CLI for the examples in this vignette.

## Authentication

For ChatGPT-account-backed Codex access, run the device-code login once:

```r eval=FALSE
llm.api::openai_codex_login()
```

The login prints a verification URL and code. `tinyoauth` handles the token cache, so later corteza sessions can use `provider = "openai_codex"` without another login.

## Using Codex on an R package

A typical package-development session starts from a project root with:

```sh
corteza::chat(provider = "openai_codex")
```

Then give corteza a scoped prompt, for example:

```text
Inspect this package, run the tests, and propose the smallest change needed to fix failing checks.
```

Then you're off to the races!

If you're entirely new to LLM agents in your CLI (aka, an agent harness), you may want to start with a more established harness like codex or Claude Code.


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
