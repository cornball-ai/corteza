% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_list}
\alias{subagent_list}
\title{List active subagents.}
\usage{
subagent_list()
}
\value{
List of subagent info objects.
}
\description{
Returns a list of info objects per agent: id/seq/task/started_at/
time_remaining/pending plus model/age/cumulative usage and a
best-effort live token count for idle agents (`NA` for busy).
}
\examples{
# Empty when no subagent has been spawned yet -- safe to call any time.
subagent_list()
}
