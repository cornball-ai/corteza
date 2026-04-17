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
