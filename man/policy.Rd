% tinyrox says don't edit this manually, but it can't stop you!
\name{policy}
\alias{policy}
\title{Evaluate policy for a tool call}
\usage{
policy(call, config = NULL)
}
\arguments{
\item{call}{A list describing the tool call. See the file header in
\code{R/policy.R} for the expected fields.}

\item{config}{Optional config list from \code{load_config()}. When
\code{NULL} (default), only the table and user policy apply; this
matches the historical \code{policy(call)} contract.}
}
\value{
A decision list with fields \code{model}, \code{approval},
  \code{reason}.
}
\description{
Returns a decision \code{list(model, approval, reason)}. \code{model} is
\code{"cloud"} or \code{"local"}; nothing routes on it yet.
\code{approval} is \code{"allow"}, \code{"ask"}, or \code{"deny"}. A
verdict from the credential rule also carries
\code{rule = "credential"}, one that asks before leaving plan mode
\code{rule = "plan_exit"}, one from plan mode
\code{rule = "plan_mode"}, and one from a room with no directory
\code{rule = "unbound_room"}. A person answers the first two each
time: a saved "always" does not, and a prompt offers none.
}
\details{
Every tool has a class, declared where it is registered:
\code{"read"}, \code{"write"}, \code{"exec"}, \code{"control"} (acts on
the session's own work) or \code{"continue"} (runs work in a child, a
worker or another room). A tool that declares none is
\code{"unknown"}. By default \code{read}, \code{control} and
\code{continue} are allowed, and \code{write}, \code{exec} and
\code{unknown} ask. Plan mode refuses \code{write}, \code{exec},
\code{continue} and \code{unknown}. A Matrix room with no directory
(\code{context$unbound}) refuses everything but \code{read} and a few
named \code{control} tools. Nothing lifts a mode's refusal. A path
under a credential store asks next, whatever follows.

When \code{config} is supplied, the user's settings are overlaid on
the table: a \code{permissions} entry replaces the verdict for its
tool, a tool named in \code{dangerous_tools} asks, and
\code{approval_mode} says what "ask" means for a tool with no
\code{permissions} entry. None of them lifts a deny.

Both \code{corteza::chat()} and the CLI tool dispatch loop pass
their session's config through here so the \code{/permissions}
contract advertised by both surfaces is enforced consistently.
\code{serve()} does not: an MCP client asks its own user.

}
\examples{
# A bare-environment read_file call resolves under the default
# built-in policy without needing any session config.
decision <- policy(list(tool = "read_file",
                        args = list(path = "DESCRIPTION")))
decision$approval
}
