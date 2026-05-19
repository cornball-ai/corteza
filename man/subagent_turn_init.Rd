% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_turn_init}
\alias{subagent_turn_init}
\title{Initialize the child-side turn session.}
\usage{
subagent_turn_init(provider = "anthropic", model = NULL, tools_filter = NULL,
                   system = NULL, max_turns = 10L, depth = 0L, plan_mode = FALSE)
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
