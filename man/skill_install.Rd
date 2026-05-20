% tinyrox says don't edit this manually, but it can't stop you!
\name{skill_install}
\alias{skill_install}
\title{Install a skill from a path or URL}
\usage{
skill_install(source, target_dir = NULL, force = FALSE)
}
\arguments{
\item{source}{Path to skill directory or URL}

\item{target_dir}{Installation directory. Default is
\code{tools::R_user_dir("corteza", "data")/skills}.}

\item{force}{Overwrite if exists}
}
\value{
Installed skill name
}
\description{
Install a skill from a path or URL
}
\examples{
# Install into a throwaway directory rather than the user's
# R_user_dir, so this example doesn't mutate state.
src <- file.path(tempdir(), "demo_skill")
dir.create(src, showWarnings = FALSE)
writeLines(c(
               "---",
               "name: demo",
               "description: A demo skill",
               "---",
               "Demo body."
            ),
           file.path(src, "SKILL.md"))

dest <- file.path(tempdir(), "skills_lib")
skill_install(src, target_dir = dest)

unlink(src, recursive = TRUE)
unlink(dest, recursive = TRUE)
}
