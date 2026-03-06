% tinyrox says don't edit this manually, but it can't stop you!
\name{subagent_spawn}
\alias{subagent_spawn}
\title{Spawn a subagent}
\usage{
subagent_spawn(task, model = NULL, tools = NULL, parent_session = NULL,
               config = NULL)
}
\arguments{
\item{task}{Task description}

\item{model}{Optional model override}

\item{tools}{Optional tool filter (character vector)}

\item{parent_session}{Parent session object}

\item{config}{Config list}
}
\value{
Subagent ID
}
\description{
Creates a new agent process with its own MCP server.
}
