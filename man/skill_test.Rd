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
\examples{
# A skill directory with no test_*.R files returns a zero-result
# summary rather than erroring.
p <- file.path(tempdir(), "skill_no_tests")
dir.create(p, showWarnings = FALSE)
skill_test(p, verbose = FALSE)
unlink(p, recursive = TRUE)
}
