% tinyrox says don't edit this manually, but it can't stop you!
\name{skill_test}
\alias{skill_test}
\title{Run skill tests}
\usage{
skill_test(path, verbose = TRUE)
}
\arguments{
\item{path}{Path to skill directory}

\item{verbose}{Print test output}
}
\value{
List with passed, failed, errors
}
\description{
Executes test_*.R files in a skill directory.
}
