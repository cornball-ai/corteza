% tinyrox says don't edit this manually, but it can't stop you!
\name{corteza_execute_in_chat}
\alias{corteza_execute_in_chat}
\title{Execute current line or selection in `corteza::chat()`}
\usage{
corteza_execute_in_chat()
}
\value{
Invisible NULL. Side effect: sends a line to the
  console.
}
\description{
RStudio addin. Reads the line or selection under the cursor in
the active source editor, prepends `/r` for `.R` files (or
`! ` for `.sh` / `.bash` files) when `corteza::chat()` is the
active console REPL, and sends the result to the console via
`rstudioapi::sendToConsole()`. After sending, the editor cursor
advances to the next line (mirroring RStudio's pre-assigned
Ctrl+Enter / Cmd+Return behavior).
}
\details{
When `chat()` is not running, no prefix is added -- the addin
is a superset of RStudio's default "execute line" behavior, so
you can bind it to Ctrl+Enter without losing normal R script
execution.

**Setup:** bind Ctrl+Enter to "Execute in corteza::chat()"
under RStudio's Tools -> Modify Keyboard Shortcuts. Choose
"Addins" in the dropdown to find the binding.

}
\keyword{internal}
