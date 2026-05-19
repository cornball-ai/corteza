% tinyrox says don't edit this manually, but it can't stop you!
\name{mcp_tool_executor}
\alias{mcp_tool_executor}
\title{Build a tool executor that routes through an MCP connection}
\usage{
mcp_tool_executor(conn)
}
\arguments{
\item{conn}{An open MCP connection (from \code{llm.api::mcp_connect}).}
}
\value{
A function with signature \code{function(name, args)} that
  returns an MCP-format result list.
}
\description{
Returns a closure suitable for the \code{tool_executor} argument of
\code{\link{turn}}. Each tool call is forwarded to the connected MCP
server via \code{llm.api::mcp_call}.
}
