% tinyrox says don't edit this manually, but it can't stop you!
\name{uninstall_cli}
\alias{uninstall_cli}
\title{Uninstall corteza CLI}
\usage{
uninstall_cli(path = NULL)
}
\arguments{
\item{path}{Directory where corteza is installed. Default matches
\code{install_cli()}: \code{~/bin} on Unix,
\code{tools::R_user_dir("corteza", "data")/bin} on Windows.}
}
\value{
TRUE if removed, FALSE if not found, invisibly.
}
\description{
Remove the \code{corteza} command-line tool.
}
\examples{
\dontrun{
uninstall_cli()
}
}
