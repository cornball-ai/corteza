# Derivation: function + roxygen docs -> JSON Schema for the LLM API.
#
# The bridge between an R package function and an LLM tool definition.
# Given a function name, schema_from_fn() pulls the function's formals
# (names, defaults, required-ness) and its Rd metadata (title,
# description, @param texts) via tools::Rd_db, and emits a JSON-Schema-
# shaped list suitable for the Anthropic chat-API `tools` parameter.
#
# Type hints come from an optional `(type)` parenthetical at the start
# of a @param doc line, e.g. `(character) Shell command to execute.`.
# R type names map to JSON Schema types:
#     character -> string
#     integer   -> integer
#     numeric   -> number (also: double)
#     logical   -> boolean
#     list      -> object
#     NULL      -> null
#     "<type> vector" -> array with items typed accordingly
# No hint? Fall back to inferring the type from the default value.
# Required params (no default) with no hint default to string.
#
# Enum support: `(character; one of: str, head, summary)` parses to
# type=string with enum=c("str","head","summary"). Other `(type; ...)`
# annotations are reserved for future extensions.
#
# ---

# Walk an Rd node tree, produce concatenated plain text.
.rd_text <- function(node) {
    if (is.character(node)) return(paste(node, collapse = ""))
    if (is.list(node)) {
        return(paste(vapply(node, .rd_text, character(1L)), collapse = ""))
    }
    ""
}

# Look up the Rd object for a function by name in the installed package
# Rd database. Returns NULL if the package isn't installed or the
# function lacks documentation.
.rd_for <- function(fn_name, pkg = "corteza") {
    db <- tryCatch(tools::Rd_db(pkg), error = function(e) NULL)
    if (is.null(db)) return(NULL)
    for (rd in db) {
        for (el in rd) {
            tag <- attr(el, "Rd_tag")
            if (identical(tag, "\\alias") || identical(tag, "\\name")) {
                if (identical(trimws(.rd_text(el)), fn_name)) return(rd)
            }
        }
    }
    NULL
}

# Extract \title + \description (collapsed whitespace, single line) up
# to max_chars. Capped at 1.5x the largest hand-written description in
# the pre-derivation codebase.
.rd_description <- function(rd, max_chars = 200L) {
    title <- ""
    desc <- ""
    for (el in rd) {
        tag <- attr(el, "Rd_tag")
        if (identical(tag, "\\title")) title <- trimws(.rd_text(el))
        else if (identical(tag, "\\description")) desc <- trimws(.rd_text(el))
    }
    title <- gsub("\\s+", " ", title)
    desc <- gsub("\\s+", " ", desc)
    combined <- if (nchar(desc) > 0L && !identical(title, desc)) {
        paste(title, desc, sep = " ")
    } else {
        title
    }
    combined <- trimws(combined)
    if (nchar(combined) > max_chars) {
        combined <- paste0(substr(combined, 1L, max_chars - 3L), "...")
    }
    combined
}

# Extract \arguments as a named list mapping param name -> raw text.
.rd_args <- function(rd) {
    out <- list()
    for (el in rd) {
        if (!identical(attr(el, "Rd_tag"), "\\arguments")) next
        for (child in el) {
            if (!identical(attr(child, "Rd_tag"), "\\item")) next
            if (length(child) < 2L) next
            nm <- trimws(.rd_text(child[[1]]))
            desc <- trimws(gsub("\\s+", " ", .rd_text(child[[2]])))
            out[[nm]] <- desc
        }
    }
    out
}

# Parse an optional `(type[; one of: a, b, c])` prefix from a @param
# description. Returns list(hint, enum, desc) with desc stripped of the
# prefix. Either hint or enum may be NULL.
.parse_type_hint <- function(text) {
    m <- regmatches(text, regexpr("^\\(([^)]+)\\)\\s*", text, perl = TRUE))
    if (length(m) == 0L || !nzchar(m)) {
        return(list(hint = NULL, enum = NULL, desc = text))
    }
    inner <- gsub("^\\(|\\)\\s*$", "", trimws(m))
    enum <- NULL
    hint <- inner
    if (grepl(";", inner, fixed = TRUE)) {
        parts <- strsplit(inner, ";", fixed = TRUE)[[1]]
        hint <- trimws(parts[1])
        for (p in parts[-1]) {
            p <- trimws(p)
            if (startsWith(p, "one of:")) {
                raw <- trimws(sub("^one of:\\s*", "", p))
                enum <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
                break
            }
        }
    }
    rest <- sub("^\\([^)]+\\)\\s*", "", text, perl = TRUE)
    list(hint = hint, enum = enum, desc = rest)
}

# Map an R type hint to a JSON Schema type skeleton.
.r_type_to_json <- function(hint) {
    simple <- c(
        character = "string", integer = "integer", numeric = "number",
        double = "number", logical = "boolean", list = "object",
        `NULL` = "null"
    )
    if (is.null(hint)) return(NULL)
    if (hint %in% names(simple)) return(list(type = unname(simple[hint])))
    if (endsWith(hint, " vector")) {
        inner <- sub(" vector$", "", hint)
        if (inner %in% names(simple)) {
            return(list(type = "array", items = list(type = unname(simple[inner]))))
        }
    }
    list(type = hint)
}

# Infer a schema skeleton from a default value (last-resort when the
# @param has no (type) hint).
.infer_from_default <- function(default) {
    if (is.null(default)) return(list(type = "null"))
    if (is.logical(default) && length(default) == 1L) return(list(type = "boolean"))
    if (is.integer(default) && length(default) == 1L) return(list(type = "integer"))
    if (is.double(default) && length(default) == 1L) return(list(type = "number"))
    if (is.character(default) && length(default) == 1L) return(list(type = "string"))
    list(type = "string")
}

#' Derive an LLM tool schema from an R function's signature and docs.
#'
#' @param fn_name Name of the function to introspect (must be in `pkg`).
#' @param pkg Package that owns the function.
#' @param max_desc_chars Cap on the generated description length.
#' @return A tool-definition list with `name`, `description`, and
#'   `input_schema` ready for the Anthropic chat-API `tools` parameter.
#' @keywords internal
#' @export
schema_from_fn <- function(fn_name, pkg = "corteza", max_desc_chars = 200L) {
    fn <- get(fn_name, envir = asNamespace(pkg))
    fml <- formals(fn)
    rd <- .rd_for(fn_name, pkg)
    arg_docs <- if (is.null(rd)) list() else .rd_args(rd)

    properties <- list()
    required <- character()

    for (nm in names(fml)) {
        if (nm == "...") next
        # An empty formal (required arg) is the empty symbol. Binding
        # it to a local triggers R's missing-argument handling, so we
        # keep it inside the list and only evaluate when optional.
        is_req <- is.symbol(fml[[nm]]) && !nzchar(as.character(fml[[nm]]))

        raw_desc <- arg_docs[[nm]] %||% ""
        parsed <- .parse_type_hint(raw_desc)

        schema_prop <- .r_type_to_json(parsed$hint)
        evaluated <- NULL
        if (!is_req) {
            evaluated <- tryCatch(eval(fml[[nm]]), error = function(e) NULL)
        }
        if (is.null(schema_prop)) {
            schema_prop <- if (!is_req) {
                .infer_from_default(evaluated)
            } else {
                list(type = "string")
            }
        }
        schema_prop$description <- parsed$desc
        if (!is.null(parsed$enum)) schema_prop$enum <- parsed$enum
        if (!is_req && is.atomic(evaluated) && length(evaluated) == 1L) {
            schema_prop$default <- evaluated
        }

        properties[[nm]] <- schema_prop
        if (is_req) required <- c(required, nm)
    }

    description <- if (is.null(rd)) "" else .rd_description(rd, max_desc_chars)

    list(
        name = fn_name,
        description = description,
        input_schema = list(
            type = "object",
            properties = properties,
            required = as.list(required)
        )
    )
}

#' Register a skill whose schema is derived from its function.
#'
#' @param tool_name Name the LLM sees.
#' @param fn The R function to introspect and execute.
#' @return Invisible tool name.
#' @keywords internal
#' @export
register_skill_from_fn <- function(tool_name, fn) {
    fn_name <- deparse(substitute(fn))
    derived <- schema_from_fn(fn_name)
    skill <- list(
        name = tool_name,
        description = derived$description,
        inputSchema = list(
            type = derived$input_schema$type,
            properties = derived$input_schema$properties,
            required = derived$input_schema$required
        ),
        handler = function(args, ctx) {
            valid <- intersect(names(args), names(formals(fn)))
            do.call(fn, args[valid])
        }
    )
    .skill_registry[[tool_name]] <- skill
    invisible(tool_name)
}

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
