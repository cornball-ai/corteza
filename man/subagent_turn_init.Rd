% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_turn_init}
\alias{subagent_turn_init}
\title{Initialize the child-side turn session.}
\usage{
subagent_turn_init(provider = "anthropic", model = NULL, tools_filter = NULL,
                   system = NULL, max_turns = 10L)
}
\arguments{
\item{provider}{LLM provider name (see [new_session()]).}

\item{model}{Optional model override.}

\item{tools_filter}{Optional character vector of tool names to
expose. NULL uses the subagent config defaults.}

\item{system}{Optional system prompt string.}

\item{max_turns}{Max tool-use turns per query.}
}
\value{
Invisible TRUE.
}
\description{
Called once per child just after [worker_init()]. Creates a
`new_session()` configured with the subagent's provider/model/tools
and stores it where [subagent_turn_prompt()] can find it. Subagents
deny all tool approvals by default so a subagent can't run bash
without the parent opting in.
}
\keyword{internal}
