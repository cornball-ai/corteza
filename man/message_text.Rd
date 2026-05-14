% tinyrox says don't edit this manually, but it can't stop you!
\name{message_text}
\alias{message_text}
\title{Best-effort flatten of a message's `content` field into one string.}
\usage{
message_text(message)
}
\arguments{
\item{message}{Single message list.}
}
\value{
Character.
}
\description{
Messages may have content as a plain string or a list of typed
blocks (text / tool_use / tool_result). For budget math we just
want the textual surface area.
}
\keyword{internal}
