# Package-as-Skills: turn any installed R package into agent tools
#
# An R package already bundles what agent frameworks treat as three separate
# things: the functions (tools), the documentation (skills/knowledge), and the
# delivery mechanism (MCP server). This module introspects installed packages
# and registers their exports as skills that the LLM can call.
#
# Config: "skill_packages": ["gitr", "basalt"]

#' Register all exports of an R package as skills
#'
#' Introspects the package namespace, builds JSON Schema parameters from
#' formals() + basalt docs, and registers each exported function as a skill.
#'
#' @param pkg Package name (must be installed)
#' @return Invisible character vector of registered tool names
#' @noRd
package_as_skills <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Package '%s' not installed", pkg), call. = FALSE)
    }

    # Use basalt if available (already filters to functions, gives arg sigs)
    has_basalt <- requireNamespace("basalt", quietly = TRUE)

    if (has_basalt) {
        exports_df <- basalt::pkg_exports(pkg)
        fn_names <- exports_df$name
    } else {
        ns <- getNamespace(pkg)
        all_exports <- getNamespaceExports(pkg)
        fn_names <- Filter(function(nm) {
            is.function(get(nm, envir = ns))
        }, all_exports)
    }

    ns <- getNamespace(pkg)
    registered <- character()

    for (fn_name in fn_names) {
        fn <- get(fn_name, envir = ns)

        # Get help text and parse params
        help_md <- if (has_basalt) {
            tryCatch(basalt::pkg_help(fn_name, pkg), error = function(e) NULL)
        }

        params <- build_params_from_formals(fn, help_md)
        desc <- extract_rd_title(help_md) %||% fn_name

        tool_name <- paste0(pkg, "::", fn_name)

        register_skill(skill_spec(
                                  name = tool_name,
                                  description = paste0("[", pkg, "] ", desc),
                                  params = params,
                                  handler = make_pkg_handler(pkg, fn_name)
            ))

        registered <- c(registered, tool_name)
    }

    invisible(registered)
}

#' Build skill params from formals + basalt docs
#'
#' Combines type inference from default values with parameter descriptions
#' parsed from basalt::pkg_help() markdown output.
#'
#' @param fn Function to introspect
#' @param help_md Markdown string from basalt::pkg_help(), or NULL
#' @return Named list of param specs (type, description, required)
#' @noRd
build_params_from_formals <- function(fn, help_md) {
    f <- formals(fn)
    if (is.null(f)) {
        return(list())
    }

    rd_descs <- parse_basalt_params(help_md)

    params <- list()
    for (i in seq_along(f)) {
        nm <- names(f)[i]
        if (nm == "...") {
            next
        }

        required <- is_missing_formal(f, i)
        if (required) {
            type <- "string"
        } else {
            type <- infer_param_type(f[[i]])
        }

        params[[nm]] <- list(
                             type = type,
                             description = rd_descs[[nm]] %||% "",
                             required = required
        )
    }
    params
}

#' Parse parameter descriptions from basalt help markdown
#'
#' Extracts name/description pairs from the Arguments section of
#' basalt::pkg_help() output.
#'
#' @param help_md Markdown string, or NULL
#' @return Named list of descriptions keyed by param name
#' @noRd
parse_basalt_params <- function(help_md) {
    if (is.null(help_md)) {
        return(list())
    }

    lines <- strsplit(help_md, "\n")[[1]]
    params <- list()

    for (line in lines) {
        m <- regmatches(
                        line,
                        regexec("^- \\*\\*`([^`]+)`\\*\\*:\\s*(.+)$", line)
        )[[1]]
        if (length(m) == 3) {
            params[[m[2]]] <- m[3]
        }
    }
    params
}

#' Create a dispatch handler for a package function
#'
#' Returns a closure that calls pkg::fn_name with the given args,
#' capturing output as text.
#'
#' @param pkg Package name
#' @param fn_name Function name
#' @return Handler function(args, ctx)
#' @noRd
make_pkg_handler <- function(pkg, fn_name) {
    function(args, ctx) {
        fn <- getExportedValue(pkg, fn_name)
        result <- tryCatch({
            out <- capture.output(do.call(fn, args))
            paste(out, collapse = "\n")
        }, error = function(e) {
            paste("Error:", e$message)
        })
        ok(result)
    }
}

#' Infer JSON Schema type from a formal's default value
#'
#' @param val Default value from formals()
#' @return Character: "boolean", "number", or "string"
#' @noRd
infer_param_type <- function(val) {
    if (identical(val, TRUE) || identical(val, FALSE)) {
        return("boolean")
    }
    if (is.numeric(val) && length(val) == 1) {
        return("number")
    }
    "string"
}

#' Check if a formal parameter has no default (is missing)
#'
#' @param formals_list Result of formals()
#' @param i Index into formals list
#' @return TRUE if the parameter has no default value
#' @noRd
is_missing_formal <- function(formals_list, i) {
    # The empty symbol (missing default) can't be assigned without error.
    # Compare via identical() against a known missing value.
    identical(formals_list[[i]], alist(x =)[[1]])
}

#' Extract the title line from basalt help markdown
#'
#' The first ### heading in basalt output is the Rd title.
#'
#' @param help_md Markdown string, or NULL
#' @return Title string, or NULL
#' @noRd
extract_rd_title <- function(help_md) {
    if (is.null(help_md)) {
        return(NULL)
    }
    lines <- strsplit(help_md, "\n")[[1]]
    for (line in lines) {
        if (grepl("^###\\s", line)) {
            return(sub("^###\\s+", "", line))
        }
    }
    NULL
}

