% tinyrox says don't edit this manually, but it can't stop you!
\name{schema_from_registry}
\alias{schema_from_registry}
\title{Build the LLM API `tools` payload from the tool registry.}
\usage{
schema_from_registry(filter = NULL)
}
\arguments{
\item{filter}{Optional tool-name or category filter; see `get_tools()`.}
}
\value{
List of tool definitions.
}
\description{
Returns a list of tool definitions in the shape Anthropic's chat
completion API expects (name, description, input_schema). Used by
the CLI to avoid round-tripping schemas through the worker.
Exported with `@keywords internal`: the CLI calls this directly, but
it is not part of the public user-facing API.
}
\keyword{internal}
