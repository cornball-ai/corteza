% tinyrox says don't edit this manually, but it can't stop you!
\name{scheduler_daemon}
\alias{scheduler_daemon}
\title{Run the scheduler daemon}
\usage{
scheduler_daemon(agent_id = "main", config = NULL, check_interval_secs = 60L,
                 verbose = TRUE)
}
\arguments{
\item{agent_id}{Agent identifier}

\item{config}{Config list}

\item{check_interval_secs}{Seconds between checks (default: 60)}

\item{verbose}{Print status messages}
}
\description{
Continuously checks for and runs due tasks.
}
