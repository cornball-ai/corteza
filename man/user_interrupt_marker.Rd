% tinyrox says don't edit this manually, but it can't stop you!
\name{user_interrupt_marker}
\alias{user_interrupt_marker}
\title{History marker written when a turn is interrupted (Ctrl+C / Esc).}
\usage{
user_interrupt_marker()
}
\value{
Character scalar.
}
\description{
Carries the same "stop and ask the user" directive as
[user_deny_marker()] so an interrupt and a deny leave the LLM with
the same next-turn instruction -- matching the (Esc)/(Ctrl+C) hint on
the approval prompt, which converges on this interrupt path.
}
\keyword{internal}
