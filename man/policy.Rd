% tinyrox says don't edit this manually, but it can't stop you!
\name{policy}
\alias{policy}
\title{Evaluate policy for a tool call}
\usage{
policy(call)
}
\arguments{
\item{call}{A list describing the tool call. See the file header in
\code{R/policy.R} for the expected fields.}
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
