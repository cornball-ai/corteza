% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_git_diff}
\alias{tool_git_diff}
\title{Show git diff for the current repository.}
\usage{
tool_git_diff(ref = "HEAD", path = ".", file_path = "", staged = FALSE,
              context_lines = 3L)
}
\arguments{
\item{ref}{(character) Diff against this ref.}

\item{path}{(character) Repository path or file path filter when combined with file_path.}

\item{file_path}{(character) Optional file path filter within the repository.}

\item{staged}{(logical) Diff staged changes instead of the worktree.}

\item{context_lines}{(integer) Number of context lines around changes.}
}
\value{
An MCP tool-result list.
}
\description{
Show git diff for the current repository.
}
\keyword{internal}
