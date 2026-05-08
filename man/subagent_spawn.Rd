% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_spawn}
\alias{subagent_spawn}
\title{Spawn a subagent.}
\usage{
subagent_spawn(task, model = NULL, tools = NULL, preset = NULL,
               parent_session = NULL, config = NULL)
}
\arguments{
\item{task}{Task description (stored for bookkeeping; not yet fed
into an agent loop).}

\item{model}{Optional model override (reserved for later use).}

\item{tools}{Optional explicit tool filter (character vector).
Overrides `preset` when provided.}

\item{preset}{Preset name: `"investigate"` (read/search only, default),
`"work"` (investigate + bash + write/edit), or `"minimal"`
(read_file + grep_files only).}

\item{parent_session}{Parent session object; read for
nested-spawning control and session-key derivation.}

\item{config}{Config list.}
}
\value{
Subagent ID (character).
}
\description{
Starts a fresh `callr::r_session` with corteza loaded and its tool
registry set up. Stores the handle in the package-level registry
keyed by subagent id.
}
