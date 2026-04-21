% tinyrox says don't edit this manually, but it can't stop you!
\name{install_windows_deps}
\alias{install_windows_deps}
\title{Install Windows dependencies into Rtools}
\usage{
install_windows_deps(rtools_home = NULL)
}
\arguments{
\item{rtools_home}{Path to Rtools root. Defaults to the
\code{RTOOLS45_HOME} / \code{RTOOLS44_HOME} environment variable,
then \code{C:/rtools45}, then \code{C:/rtools44}.}
}
\value{
TRUE on success, invisibly.
}
\description{
Runs \code{pacman -Sy git} inside the Rtools MSYS2 environment so
corteza's git tools work without requiring a separate Git for Windows
install. Rtools itself must already be installed (Rtools45 for R
4.5.x, Rtools44 for R 4.4.x).
This is a convenience helper, not something \code{library(corteza)}
runs on your behalf. Packages should not mutate a user's toolchain
silently.
}
\examples{
\dontrun{
install_windows_deps()
}
}
