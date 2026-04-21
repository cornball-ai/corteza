% tinyrox says don't edit this manually, but it can't stop you!
\name{worker_tool_list}
\alias{worker_tool_list}
\title{Worker-side tool listing.}
\usage{
worker_tool_list(filter = NULL, cwd = getwd())
}
\arguments{
\item{filter}{Optional tool-name or category filter; see get_tools().}

\item{cwd}{Project root for project-local skill discovery.}
}
\value{
List of tool definitions.
}
\description{
Returns the full tool definition list the CLI needs to build its LLM
API `tools` payload. Ensures built-in skills and user skills are
loaded before listing.
Exported (with `@keywords internal`) for the same reason as
`worker_dispatch()`.
}
\keyword{internal}
