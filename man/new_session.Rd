% tinyrox says don't edit this manually, but it can't stop you!
\name{new_session}
\alias{new_session}
\title{Create a new turn session}
\usage{
new_session(
  channel = c("cli", "console", "matrix"),
  history = NULL,
  model_map = NULL,
  provider = "anthropic",
  tools_filter = NULL,
  system = NULL,
  approval_cb = NULL,
  max_turns = 10L,
  verbose = FALSE,
  plan_mode = FALSE,
  web_search = NULL,
  base_url = NULL,
  max_tokens = NULL,
  reasoning_effort = NULL,
  thinking_budget_tokens = NULL,
  cache = NULL
)
}
\arguments{
\item{channel}{Character, one of "cli", "console", "matrix".}

\item{history}{List of prior messages, or NULL.}

\item{model_map}{Named list with \code{cloud} and \code{local} model
names. Defaults to configured defaults.}

\item{provider}{LLM provider passed to \code{llm.api::agent}.}

\item{tools_filter}{Character vector passed to \code{get_tools()}.}

\item{system}{System prompt override (NULL for built-in default).}

\item{approval_cb}{Function called when policy returns \code{"ask"}.
Signature: \code{function(call, decision) -> TRUE|FALSE}. Default
denies (safe fallback).}

\item{max_turns}{Maximum LLM turns per call.}

\item{verbose}{Print tool call progress.}

\item{plan_mode}{Logical. When TRUE, the session is in plan mode:
the LLM is told to research and propose without acting, the policy
engine denies write/exec tool calls (except \code{exit_plan_mode}),
and \code{exit_plan_mode} is added to the tool list. A successful
\code{exit_plan_mode} call flips this back to FALSE.}

\item{web_search}{Logical or NULL. When TRUE and the provider
supports it, \code{\link{turn}} enables \code{llm.api::agent}'s
provider-native (server-side) web search for the session: the
model searches inside its own turn, no Tavily key required. NULL
defers to \code{turn}'s default (on for supported providers).}

\item{base_url}{Character or NULL. Endpoint for the
\code{"openai_compatible"} provider (OpenRouter, DeepSeek, a
corporate gateway). \code{\link{turn}} applies it via
\code{llm.api::llm_base()} for the duration of the agent call.
NULL falls back to \code{config$base_url}, then llm.api's own
\code{OPENAI_COMPATIBLE_BASE_URL} environment variable. Ignored
for every other provider.}

\item{max_tokens}{Integer or NULL. Per-response output-token budget,
forwarded to \code{llm.api::agent}. NULL falls back to
\code{config$max_tokens}, then llm.api's provider default (4096
on Anthropic). Agent turns that write long tool calls -- big
\code{run_r} bodies especially -- need more than that default:
a response the budget cuts off ends the whole turn with
\code{[Output truncated: max_tokens]} instead of the model's
answer.}

\item{reasoning_effort}{Character or NULL. How hard a reasoning
model should think before answering, e.g. \code{"low"},
\code{"medium"}, \code{"high"}. One setting, routed to whichever
field the provider's wire uses: \code{reasoning_effort} on
\code{"openai"} / \code{"openai_codex"}, \code{output_config$effort}
on \code{"anthropic"} / \code{"anthropic_claude"}. Dropped for
providers with no effort control. NULL falls back to
\code{config$reasoning_effort}, then the provider default. Not
validated against a fixed set, because the scales differ: Anthropic
accepts \code{"low"}, \code{"medium"}, \code{"high"},
\code{"xhigh"} and \code{"max"}, and an unknown value is refused by
the API with a message naming the alternatives.}

\item{thinking_budget_tokens}{Integer or NULL. Extended-thinking
budget for Anthropic models, forwarded to \code{llm.api::agent}
for the \code{"anthropic"} and \code{"anthropic_claude"}
providers only. Must be at least 1024 and below
\code{max_tokens}, both enforced by llm.api. NULL falls back to
\code{config$thinking_budget_tokens}, then the provider default
(thinking off).}

\item{cache}{Character or NULL. Anthropic prompt-cache TTL for the
cacheable prefix: \code{"none"}, \code{"5m"} or \code{"1h"}.
Anthropic-only, and dropped for other providers so they never see
a field their wire rejects. NULL falls back to
\code{config$cache}, then llm.api's default (\code{"none"}).
Caching is a billing setting, not a behavioural one: the model
receives byte-identical input either way, so turning it on cannot
change a result. What it saves depends entirely on how much of
the request sits inside the cached prefix -- llm.api places its
only breakpoint on the system message, so a session whose cost is
dominated by a long message history will see little from this.}
}
\value{
An environment holding the session state.
}
\description{
Returns an environment with sensible defaults. Adapters set channel-
specific fields (e.g. \code{approval_cb}, \code{tools_filter}) before
calling \code{\link{turn}}.
}
\examples{
# Build a stateless session for the CLI channel without making any
# network calls. The returned environment carries history, the
# active provider/model, and the approval callback.
s <- new_session(channel = "cli", provider = "anthropic")
is.environment(s)
identical(s$provider, "anthropic")
}
