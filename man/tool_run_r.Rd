% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_run_r}
\alias{tool_run_r}
\title{Execute R code in the session's global environment.}
\usage{
tool_run_r(code)
}
\arguments{
\item{code}{(character) R code to execute.}
}
\value{
An MCP tool-result list.
}
\description{
New bindings are auto-captured into the workspace cache. Large
result values (data frames, matrices, long vectors, objects over
~10 KB) are stashed via `with_handle()` and returned as a `str()`
summary plus a short `.h_NNN` handle the LLM can reference in a
later `run_r` call or inspect with `read_handle`.
}
\keyword{internal}
