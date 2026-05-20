% tinyrox says don't edit this manually, but it can't stop you!
\name{skill_list_installed}
\alias{skill_list_installed}
\title{List installed skills}
\usage{
skill_list_installed(skill_dir = NULL)
}
\arguments{
\item{skill_dir}{Skills directory}
}
\value{
Data frame with skill info
}
\description{
List installed skills
}
\examples{
# List skills from an empty tempdir; returns a zero-row data frame
# with the documented columns.
empty <- file.path(tempdir(), "empty_skills")
dir.create(empty, showWarnings = FALSE)
skill_list_installed(skill_dir = empty)
unlink(empty, recursive = TRUE)
}
