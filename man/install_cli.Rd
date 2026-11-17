% tinyrox says don't edit this manually, but it can't stop you!
\name{install_cli}
\alias{install_cli}
\title{Install corteza CLI}
\usage{
install_cli(path = NULL, force = FALSE)
}
\arguments{
\item{path}{Directory to install to. Default is
\code{tools::R_user_dir("corteza", "data")/bin}, created when
missing. To put the command in a directory already in your PATH,
name it, as in \code{install_cli("~/bin")}. Up to 0.8.0 the default
on Unix was \code{~/bin}.}

\item{force}{Overwrite existing installation.}
}
\value{
The installed script path, invisibly.
}
\description{
Install the \code{corteza} command-line tool. On Unix (Linux, macOS)
installs the Rscript shebang binary. On Windows installs a
\code{.cmd} wrapper alongside the script so \code{corteza} works from
cmd.exe / PowerShell.
}
\details{
Requires:
\itemize{
  \item \code{r} (littler) for fast R script execution (Unix only —
    Windows uses \code{Rscript}).
  \item The \code{llm.api} package for LLM connectivity
  \item The \code{corteza} package itself
}

After installation, run \code{corteza} from any terminal. The
default directory is not in PATH, and the function prints the line
that adds it.

An install made by 0.8.0 or earlier stays in \code{~/bin} until you
replace it (\code{install_cli("~/bin", force = TRUE)}) or remove it
(\code{uninstall_cli("~/bin")}). A shell that finds that copy first
keeps running it, so both functions say when one is there.
}
\examples{
# Into a directory of your choosing, here a throwaway one. With no
# argument the command goes to R's user data directory.
bin <- file.path(tempdir(), "corteza-bin")
install_cli(bin)
uninstall_cli(bin)
unlink(bin, recursive = TRUE)
}
