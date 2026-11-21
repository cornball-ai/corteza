% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_request_flush}
\alias{matrix_request_flush}
\title{Deprecated: use bot_request_flush()}
\usage{
matrix_request_flush()
}
\value{
See \code{\link{bot_request_flush}}.
}
\description{
Deprecated: use bot_request_flush()
}
\examples{
# Use bot_request_flush(). Like it, this writes a sentinel file under
# CORTEZA_STATE_DIR, sent to a temporary folder here.
old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = file.path(tempdir(), "state"))
sig <- matrix_request_flush()  # warns that it is deprecated
file.exists(sig)
unlink(Sys.getenv("CORTEZA_STATE_DIR"), recursive = TRUE)
if (is.na(old)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old)
}
}
\seealso{
\code{\link{bot_request_flush}}
}
