% tinyrox says don't edit this manually, but it can't stop you!
\name{default_local_model}
\alias{default_local_model}
\title{Detect the preferred local Ollama model}
\usage{
default_local_model()
}
\value{
Character scalar model name, or NULL.
}
\description{
Walks \code{getOption("corteza.local_models")} (default
\code{c("gpt-oss:120b", "gpt-oss:20b")}) and returns the first one that
is currently installed in the local Ollama server. Returns NULL if
Ollama is unreachable or none of the candidates are installed.
Cached per R process.
}
