% tinyrox says don't edit this manually, but it can't stop you!
\name{task_create}
\alias{task_create}
\title{Create a new scheduled task}
\usage{
task_create(name, prompt, schedule = NULL, description = NULL,
            notification_sink = "console", agent_id = "main")
}
\arguments{
\item{name}{Task name}

\item{prompt}{Prompt to send to agent}

\item{schedule}{Cron expression or special string}

\item{description}{Optional description}

\item{notification_sink}{Where to send output: console, file, signal}

\item{agent_id}{Agent identifier}
}
\value{
Task ID
}
\description{
Create a new scheduled task
}
