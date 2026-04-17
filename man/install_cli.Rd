% tinyrox says don't edit this manually, but it can't stop you!
\name{install_cli}
\alias{install_cli}
\title{Install corteza CLI}
\usage{
install_cli(path = NULL, force = FALSE)
}
\arguments{
\item{path}{Directory to install to. Default is \code{~/bin} on
Unix, \code{tools::R_user_dir("corteza", "data")/bin} on Windows.}

\item{force}{Overwrite existing installation.}
}
\value{
The installed script path, invisibly.
}
\description{
Install the \code{corteza} command-line tool to a directory in your
PATH. On Unix (Linux, macOS) installs the Rscript shebang binary.
On Windows installs a \code{.cmd} wrapper alongside the script so
\code{corteza} works from cmd.exe / PowerShell.
}
\details{
Requires:
\itemize{
  \item \code{r} (littler) for fast R script execution (Unix only —
    Windows uses \code{Rscript}).
  \item The \code{llm.api} package for LLM connectivity
  \item The \code{corteza} package itself
}

After installation, run \code{corteza} from any terminal (you may
need to add the install directory to PATH; the function prints the
PATH hint if it isn't already there).
}
\examples{
\dontrun{
install_cli()
install_cli("/usr/local/bin")
}
}
