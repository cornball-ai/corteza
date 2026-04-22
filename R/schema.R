# LLM-API schema generation from the shared tool registry.
#
# The CLI builds the `tools` parameter for the Anthropic / OpenAI /
# Moonshot chat APIs by calling schema_from_registry() in its own
# process. The callr worker is not involved: nothing about schema
# production travels over the worker pipe, and the tool-definition
# shape lives in one place.
#
# chat() and serve() have their own tool-list paths (they need slightly
# different shapes — inputSchema vs input_schema, MCP protocol framing
# for serve()) and keep using those. schema_from_registry() is the
# CLI-side contract.
#
# Future work: when individual tool functions are rewritten with
# real signatures + @param docs, schema_from_registry can derive
# descriptions from those docs via saber::pkg_help() instead of the
# hand-authored skill_spec() metadata. Migration is tool-by-tool; for
# now every registered skill already has inputSchema baked in by
# skill_spec(), so we just reformat for the API.

#' Build the LLM API `tools` payload from the tool registry.
#'
#' Returns a list of tool definitions in the shape Anthropic's chat
#' completion API expects (name, description, input_schema). Used by
#' the CLI to avoid round-tripping schemas through the worker.
#'
#' Exported with `@keywords internal`: the CLI calls this directly, but
#' it is not part of the public user-facing API.
#'
#' @param filter Optional tool-name or category filter; see `get_tools()`.
#' @return List of tool definitions.
#' @keywords internal
#' @export
schema_from_registry <- function(filter = NULL) {
    mcp_tools <- get_tools(filter)
    lapply(mcp_tools, function(t) {
        list(
            name = sanitize_tool_name(t$name),
            description = t$description,
            input_schema = t$inputSchema
        )
    })
}
