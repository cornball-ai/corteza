% tinyrox says don't edit this manually, but it can't stop you!
\name{corteza_execute_in_chat_retain}
\alias{corteza_execute_in_chat_retain}
\title{Execute current line or selection in `corteza::chat()` (retain cursor)}
\usage{
corteza_execute_in_chat_retain()
}
\value{
Invisible NULL.
}
\description{
Same routing logic as [corteza_execute_in_chat()] but the
editor cursor stays in place after sending, mirroring RStudio's
pre-assigned Alt+Enter / Option+Return behavior.
}
\details{
**Setup:** bind Alt+Enter to "Execute in corteza::chat()
(retain cursor)" under RStudio's Tools -> Modify Keyboard
Shortcuts.

}
\keyword{internal}
