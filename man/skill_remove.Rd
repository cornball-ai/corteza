% tinyrox says don't edit this manually, but it can't stop you!
\name{skill_remove}
\alias{skill_remove}
\title{Remove an installed skill}
\usage{
skill_remove(name, skill_dir = NULL)
}
\arguments{
\item{name}{Skill name}

\item{skill_dir}{Skills directory}
}
\value{
Invisible TRUE on success
}
\description{
Remove an installed skill
}
\examples{
# Install a demo skill into a tempdir, then remove it. The
# installed name is the source directory's basename.
src <- file.path(tempdir(), "demo_skill")
dir.create(src, showWarnings = FALSE)
writeLines(c("---", "name: demo_skill",
             "description: A demo skill", "---"),
           file.path(src, "SKILL.md"))
dest <- file.path(tempdir(), "skills_lib")
name <- skill_install(src, target_dir = dest)
skill_remove(name, skill_dir = dest)

unlink(src, recursive = TRUE)
unlink(dest, recursive = TRUE)
}
