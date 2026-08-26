% tinyrox says don't edit this manually, but it can't stop you!
\name{voice_serve}
\alias{voice_serve}
\title{Serve AgentVoice for live voice clients}
\usage{
voice_serve(
  config = NULL,
  address = NULL,
  hooks = list(),
  poll_ms = 200L,
  max_events = Inf
)
}
\arguments{
\item{config}{Config list as from \code{bot_load_config()}, or
\code{NULL} to load it.}

\item{address}{\code{"host:port"} to bind, or \code{NULL} for the
config's \code{voice.listen}, or \code{"127.0.0.1:7851"}.}

\item{hooks}{Named list overriding side effects (tests/harnesses
only): \code{http}, \code{clock}, \code{run_turn}, \code{post},
\code{edit}, \code{members}, \code{ready}.}

\item{poll_ms}{Milliseconds each poll waits for events.}

\item{max_events}{Stop after handling this many events (tests only;
the default never stops).}
}
\value{
Invisibly, the number of events handled.
}
\description{
Runs the gRPC service a fluffychat live-voice client talks to
(\code{AllocateVoice}, \code{Converse}, \code{ReportTurn}). Blocks
until interrupted. Run it as its own R process, next to -- not inside
-- the room poll loop.
}
\details{
Requires the \code{rgrpc} and \code{RProtoBuf} packages (Suggests),
and a media allocator named in the config: \code{voice.allocator}
(the gpu.ctl base URL) plus \code{voice.allocator_token} (the
service credential its front requires on every mint). Without
either, \code{AllocateVoice} refuses and names the missing key.

Voice sessions generate in spoken register: their system prompt
carries an instruction to write plain speakable prose (no markdown,
numbers said aloud), because the reply is synthesized verbatim.
\code{voice.speech_style} in the config replaces that instruction's
text; typed room chat is unaffected either way.

}
