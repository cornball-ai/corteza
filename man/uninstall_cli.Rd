% tinyrox says don't edit this manually, but it can't stop you!
\name{uninstall_cli}
\alias{uninstall_cli}
\title{Uninstall corteza CLI}
\usage{
uninstall_cli(path = NULL)
}
\arguments{
\item{path}{Directory where corteza is installed. Default matches
\code{install_cli()}:
\code{tools::R_user_dir("corteza", "data")/bin}. An install made by
0.8.0 or earlier on Unix is in \code{~/bin}; remove it with
\code{uninstall_cli("~/bin")}.}
}
\value{
TRUE if removed, FALSE if not found, invisibly.
}
\description{
Remove the \code{corteza} command-line tool.
}
\examples{
bin <- file.path(tempdir(), "corteza-bin")
install_cli(bin)
uninstall_cli(bin)
unlink(bin, recursive = TRUE)
}
