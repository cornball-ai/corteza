% tinyrox says don't edit this manually, but it can't stop you!
\name{uninstall_cli}
\alias{uninstall_cli}
\title{Uninstall corteza CLI}
\usage{
uninstall_cli(path = "~/bin")
}
\arguments{
\item{path}{Directory where corteza is installed. Defaults to `~/bin`.}
}
\value{
TRUE if removed, FALSE if not found (invisibly).
}
\description{
Remove the `corteza` command-line tool.
}
\examples{
\dontrun{
uninstall_cli()
}
}
