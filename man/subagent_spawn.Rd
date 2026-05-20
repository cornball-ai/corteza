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
Overrides `preset` when provided. Fixed for the lifetime of the
child -- cannot be expanded after spawn.}

\item{preset}{Preset name (fixed for the lifetime of the child).
`"investigate"` (default): `read_file`, `grep_files`, `r_help`,
`web_search`, `fetch_url`. `"work"`: investigate + `bash`,
`write_file`, `replace_in_file`, `list_files`, `git_status`,
`git_diff`, `git_log`, `run_r`. `"minimal"`: `read_file`,
`grep_files`.}

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
\details{
Permissions: subagents have no interactive approval channel back
to the parent or user. The child's `approval_cb` denies by default
and there is no mid-run escalation path. Whatever capability the
child needs must be granted at spawn time through `preset` or
`tools`. If a task may need shell, write, or network capability,
pick a preset that includes it (or pass an explicit `tools` list);
otherwise the child should report that it is blocked rather than
retry.

}
\examples{
\donttest{
if (interactive()) {
    # Spawns a callr::r_session child loaded with corteza; the
    # registry is in-memory and dies with the parent R session, so
    # we wrap in interactive() to keep R CMD check from leaving
    # children behind.
    id <- subagent_spawn("look up the package version",
                         preset = "minimal")
    subagent_kill(id)
}
}
}
