# Package-as-Skills: turn any installed R package into agent tools
#
# An R package already bundles what agent frameworks treat as three separate
# things: the functions (tools), the documentation (skills/knowledge), and the
# delivery mechanism (MCP server). This module introspects installed packages
# and registers their exports as skills that the LLM can call.
#
# Config: "skill_packages": ["gitr", "saber"]

#' Register all exports of an R package as skills
#'
#' Introspects the package namespace, builds JSON Schema parameters from
#' formals() + saber docs, and registers each exported function as a skill.
#'
#' @param pkg Package name (must be installed)
#' @return Invisible character vector of registered tool names
#' @noRd
package_as_skills <- function(pkg, functions = NULL) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Package '%s' not installed", pkg), call. = FALSE)
    }

    # Use saber if available (already filters to functions, gives arg sigs)
    has_saber <- requireNamespace("saber", quietly = TRUE)

    if (has_saber) {
        exports_df <- saber::pkg_exports(pkg)
        fn_names <- exports_df$name
    } else {
        ns <- getNamespace(pkg)
        all_exports <- getNamespaceExports(pkg)
        fn_names <- Filter(function(nm) {
            is.function(get(nm, envir = ns))
        }, all_exports)
    }

    # Filter to requested functions if specified
    if (!is.null(functions)) {
        fn_names <- intersect(functions, fn_names)
        if (length(fn_names) == 0) {
            warning(sprintf("No matching exports found in '%s'", pkg))
            return(invisible(character()))
        }
    }

    ns <- getNamespace(pkg)
    registered <- character()

    for (fn_name in fn_names) {
        fn <- get(fn_name, envir = ns)

        # Get help text and parse params
        help_md <- if (has_saber) {
            tryCatch(saber::pkg_help(fn_name, pkg), error = function(e) NULL)
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

#' Build skill params from formals + saber docs
#'
#' Combines type inference from default values with parameter descriptions
#' parsed from saber::pkg_help() markdown output.
#'
#' @param fn Function to introspect
#' @param help_md Markdown string from saber::pkg_help(), or NULL
#' @return Named list of param specs (type, description, required)
#' @noRd
build_params_from_formals <- function(fn, help_md) {
    f <- formals(fn)
    if (is.null(f)) {
        return(list())
    }

    rd_descs <- parse_saber_params(help_md)

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

#' Parse parameter descriptions from saber help markdown
#'
#' Extracts name/description pairs from the Arguments section of
#' saber::pkg_help() output.
#'
#' @param help_md Markdown string, or NULL
#' @return Named list of descriptions keyed by param name
#' @noRd
parse_saber_params <- function(help_md) {
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
    force(pkg)
    force(fn_name)
    function(args, ctx) {
        fn <- getExportedValue(pkg, fn_name)
        tryCatch({
            printed <- capture.output(val <- do.call(fn, args))
            if (length(printed) > 0 && any(nchar(printed) > 0)) {
                ok(paste(printed, collapse = "\n"))
            } else if (is.null(val)) {
                ok(sprintf("OK: %s::%s completed", pkg, fn_name))
            } else {
                ok(paste(capture.output(print(val)), collapse = "\n"))
            }
        }, error = function(e) {
            ok(paste("Error:", e$message))
        })
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

#' Extract the title line from saber help markdown
#'
#' The first ### heading in saber output is the Rd title.
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

#' Load skill packages from config
#'
#' Processes the skill_packages config entry. Supports two formats:
#' - String: load all exports from the package
#' - List with package + functions: selective loading
#'
#' @param config Config list from load_config()
#' @return Invisible NULL
#' @noRd
load_skill_packages <- function(config) {
    specs <- config$skill_packages %||% list()
    for (spec in specs) {
        tryCatch({
            if (is.character(spec)) {
                package_as_skills(spec)
            } else {
                package_as_skills(spec$package, functions = spec$functions)
            }
        }, error = function(e) {
            pkg_name <- if (is.character(spec)) spec else spec$package
            message(sprintf("  Skipping %s: %s", pkg_name, e$message))
        })
    }
    invisible(NULL)
}

#' Format package documentation for context injection
#'
#' Generates markdown documentation for skill packages to inject into
#' the system prompt. For selective loads, includes per-function docs.
#' For whole-package loads, includes only the package summary.
#'
#' @param config Config list from load_config()
#' @return Character string with formatted docs, or NULL
#' @noRd
format_pkg_skill_docs <- function(config) {
    if (!requireNamespace("saber", quietly = TRUE)) {
        return(NULL)
    }

    specs <- config$skill_packages %||% list()
    if (length(specs) == 0) {
        return(NULL)
    }

    parts <- character()
    for (spec in specs) {
        if (is.character(spec)) {
            pkg <- spec
            fns <- NULL
        } else {
            pkg <- spec$package
            fns <- spec$functions
        }

        if (!requireNamespace(pkg, quietly = TRUE)) {
            next
        }

        if (!is.null(fns)) {
            # Selective: per-function docs
            for (fn in fns) {
                doc <- tryCatch(
                                saber::pkg_help(fn, pkg),
                                error = function(e) NULL
                )
                if (!is.null(doc)) {
                    parts <- c(parts, sprintf("### %s::%s", pkg, fn),
                               "", doc, "")
                }
            }
        } else {
            # Whole package: summary only
            doc <- tryCatch(
                            paste(capture.output(saber::pkg_exports(pkg)), collapse = "\n"),
                            error = function(e) NULL
            )
            if (!is.null(doc)) {
                parts <- c(parts, sprintf("### %s (all exports)", pkg),
                           "", doc, "")
            }
        }
    }

    if (length(parts) == 0) {
        return(NULL)
    }
    paste(parts, collapse = "\n")
}

