% tinyrox says don't edit this manually, but it can't stop you!
\name{install_cli}
\alias{install_cli}
\title{Install corteza CLI}
\usage{
install_cli(path = "~/bin", force = FALSE)
}
\arguments{
\item{path}{Directory to install to. Defaults to `~/bin`.}

\item{force}{Overwrite existing installation. Defaults to FALSE.}
}
\value{
The path to the installed script (invisibly).
}
\description{
Install the `corteza` command-line tool to a directory in your PATH.
}
\details{
This copies the `corteza` script from the package to the specified directory.
The script requires:

- `r` (littler) for fast R script execution
- The `llm.api` package for LLM connectivity
- The `corteza` package itself

After installation, you can run `corteza` from any terminal.
}
\examples{
\dontrun{
# Install to ~/bin (default)
install_cli()

# Install to custom location
install_cli("/usr/local/bin")
}
}
