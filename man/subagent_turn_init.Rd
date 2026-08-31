% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_turn_init}
\alias{subagent_turn_init}
\title{Initialize the child-side turn session.}
\usage{
subagent_turn_init(
  provider = "anthropic",
  model = NULL,
  tools_filter = NULL,
  system = NULL,
  max_turns = 10L,
  depth = 0L,
  plan_mode = FALSE,
  web_search = NULL,
  allowed_paths = NULL
)
}
\arguments{
\item{provider}{LLM provider name (see [new_session()]).}

\item{model}{Optional model override.}

\item{tools_filter}{Optional character vector of tool names to
expose. NULL uses the subagent config defaults.}

\item{system}{Optional system prompt string.}

\item{max_turns}{Max tool-use turns per query.}

\item{depth}{Archival depth this child sits at (0 means a direct
child of the CLI parent). Used by recursion in
[subagent_turn_prompt()] to avoid archiving past the configured
depth_cap.}

\item{web_search}{Logical or NULL. Provider-native (server-side) web
search for the child. NULL (default) preserves the historical
behavior: [new_session()] leaves it unset and [turn()] enables it
for providers that support it. Pass FALSE for a child that must
have no outbound channel -- dropping `web_search` from
`tools_filter` does not achieve that on its own, because the
provider's own search tool is injected by `llm.api::agent`, not
drawn from the skill registry.}

\item{allowed_paths}{Character or NULL. Confines this child's file
tools to the given roots for the life of the process. NULL (default)
leaves it unrestricted, as before. A read-only `tools_filter` is not
a filesystem sandbox on its own -- `read_file` reaches anything the
process can -- so a child that should only see its project needs
this as well.}
}
\value{
Invisible TRUE.
}
\description{
Called once per child just after [worker_init()]. Creates a
`new_session()` configured with the subagent's provider/model/tools
and stores it where [subagent_turn_prompt()] can find it. The
child's `approval_cb` denies by default: subagents have no
interactive approval channel back to the parent or user, and tool
permissions are fixed at spawn time via `tools_filter` (derived
from the parent's `preset` or explicit `tools` argument to
[subagent_spawn()]). There is no way to grant additional capability
mid-run.
}
\keyword{internal}
