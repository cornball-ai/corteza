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
\code{NULL} (default), only the built-in tensor and user policy
apply; this matches the historical \code{policy(call)} contract.}
}
\value{
A decision list with fields \code{model}, \code{approval},
  \code{reason}.
}
\description{
Returns a decision \code{list(model, approval, reason)}. \code{model} is
\code{"cloud"} or \code{"local"}; \code{approval} is \code{"allow"},
\code{"ask"}, or \code{"deny"}.
}
\details{
When \code{config} is supplied, the project's
\code{approval_mode} / \code{dangerous_tools} / per-tool
\code{permissions} are overlaid on top of the default tensor: a
tool the user has configured as \code{"ask"} or \code{"deny"} will
have its decision promoted accordingly. Safety verdicts (credential
paths, plan mode) still win because those represent invariants the
user can't waive from config.

Both \code{corteza::chat()} and the CLI tool dispatch loop pass
their session's config through here so the \code{/permissions}
contract advertised by both surfaces is enforced consistently.

}
\examples{
# A bare-environment read_file call resolves under the default
# built-in policy without needing any session config.
decision <- policy(list(name = "read_file",
                        arguments = list(path = "DESCRIPTION")))
decision$approval
}
