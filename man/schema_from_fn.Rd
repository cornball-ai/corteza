% tinyrox says don't edit this manually, but it can't stop you!
\name{schema_from_fn}
\alias{schema_from_fn}
\title{Derive an LLM tool schema from an R function's signature and docs.}
\usage{
schema_from_fn(fn_name, pkg = "corteza", max_desc_chars = 200L)
}
\arguments{
\item{fn_name}{Name of the function to introspect (must be in `pkg`).}

\item{pkg}{Package that owns the function.}

\item{max_desc_chars}{Cap on the generated description length.}
}
\value{
A tool-definition list with `name`, `description`, and
  `input_schema` ready for the Anthropic chat-API `tools` parameter.
}
\description{
Derive an LLM tool schema from an R function's signature and docs.
}
\keyword{internal}
