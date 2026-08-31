# Permission System
# Handles per-tool permissions and filesystem sandboxing

#' Default dangerous tools (require approval by default)
#' @noRd
default_dangerous_tools <- function() {
    c("bash", "run_r", "run_r_script", "write_file", "replace_in_file",
        "base::writeLines")
}

#' Default denied paths for filesystem sandboxing
#' @noRd
default_denied_paths <- function() {
    c("~/.ssh", "~/.gnupg", "~/.aws", "~/.config/gcloud", "~/.kube",
        "~/.docker")
}

#' Get permission for a tool
#'
#' Checks config for per-tool permission, falls back to approval_mode.
#'
#' @param tool_name Name of the tool
#' @param config Config list from load_config()
#' @return "allow", "ask", or "deny"
#' @noRd
get_tool_permission <- function(tool_name, config) {
    # Check per-tool permissions first
    perms <- config$permissions
    if (!is.null(perms) && !is.null(perms[[tool_name]])) {
        return(perms[[tool_name]])
    }

    # Fall back to dangerous_tools + approval_mode
    dangerous <- config$dangerous_tools %||% default_dangerous_tools()
    approval_mode <- config$approval_mode %||% "ask"

    if (tool_name %in% dangerous) {
        approval_mode
    } else {
        "allow"
    }
}

#' Check if a tool requires approval
#'
#' @param tool_name Name of the tool
#' @param config Config list
#' @return TRUE if tool requires user approval
#' @noRd
requires_approval <- function(tool_name, config) {
    get_tool_permission(tool_name, config) == "ask"
}

#' Check if a tool is denied
#'
#' @param tool_name Name of the tool
#' @param config Config list
#' @return TRUE if tool is denied
#' @noRd
is_tool_denied <- function(tool_name, config) {
    get_tool_permission(tool_name, config) == "deny"
}

#' Normalize a path for comparison
#'
#' Expands ~, resolves symlinks, normalizes path.
#'
#' @param path Path to normalize
#' @return Normalized absolute path
#' @noRd
normalize_path_for_check <- function(path) {
    # Expand ~ first
    path <- path.expand(path)

    # Force forward slashes so prefix checks (startsWith) work identically
    # on Windows and POSIX. Without winslash="/" normalizePath returns
    # backslashes on Windows, but is_path_under appends a forward slash
    # to the base, so the comparison silently fails on Windows.
    # mustWork = FALSE so denied_paths entries that don't exist on the
    # current machine (e.g. ~/.ssh on a fresh VM) still compare cleanly.
    #
    # normalizePath() can only resolve symlinks in a path that exists,
    # and it returns a non-existent path unchanged. That asymmetry breaks
    # containment wherever an ancestor is a symlink: on macOS /var links
    # to /private/var, so a check on a not-yet-created file compared an
    # unresolved path against a resolved base and quietly never matched.
    # A containment check that stops matching is exactly the silent
    # failure to avoid -- for allowed_paths it spuriously denies, and for
    # denied_paths it spuriously permits.
    #
    # So resolve against the deepest ancestor that does exist, then
    # re-attach the part that doesn't. Same technique as .resolve_real()
    # in R/monitor.R, which needs it for the same reason.
    tryCatch({
        cur <- path
        tail <- character()
        # Bounded: dirname() reaches "/" (or a drive root) and fixpoints.
        while (nzchar(cur) && !file.exists(cur)) {
            tail <- c(basename(cur), tail)
            parent <- dirname(cur)
            if (identical(parent, cur)) {
                break
            }
            cur <- parent
        }
        if (!nzchar(cur) || !file.exists(cur)) {
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        base <- normalizePath(cur, winslash = "/", mustWork = FALSE)
        if (length(tail) > 0L) {
            paste(c(base, tail), collapse = "/")
        } else {
            base
        }
    }, error = function(e) {
        path
    })
}

#' Check if path is under a base path
#'
#' @param path Path to check
#' @param base Base path
#' @return TRUE if path is under base
#' @noRd
is_path_under <- function(path, base) {
    path <- normalize_path_for_check(path)
    base <- normalize_path_for_check(base)

    # Ensure base ends with /
    if (!endsWith(base, "/")) {
        base <- paste0(base, "/")
    }

    # Check if path starts with base or equals base (without trailing /)
    startsWith(path, base) || path == sub("/$", "", base)
}

#' Validate a file path against sandbox rules
#'
#' Checks if a path is allowed based on allowed_paths and denied_paths.
#'
#' @param path Path to validate
#' @param config Config list with allowed_paths and denied_paths
#' @param operation "read" or "write" (for error messages)
#' @return List with ok (logical) and message (character)
#' @noRd
validate_path <- function(path, config, operation = "access") {
    if (is.null(path) || nchar(path) == 0) {
        return(list(ok = FALSE, message = "Path is empty"))
    }

    norm_path <- normalize_path_for_check(path)

    # Check denied paths first (takes precedence)
    denied <- config$denied_paths %||% default_denied_paths()
    for (dp in denied) {
        if (is_path_under(norm_path, dp)) {
            return(list(
                        ok = FALSE,
                        message = sprintf("Access denied: %s is in restricted area (%s)",
                        path, dp)
                ))
        }
    }

    # Check allowed paths (if specified, path must be under one of them)
    allowed <- config$allowed_paths
    if (!is.null(allowed) && length(allowed) > 0) {
        is_allowed <- FALSE
        for (ap in allowed) {
            if (is_path_under(norm_path, ap)) {
                is_allowed <- TRUE
                break
            }
        }
        if (!is_allowed) {
            return(list(
                        ok = FALSE,
                        message = sprintf("Access denied: %s is outside allowed paths",
                        path)
                ))
        }
    }

    list(ok = TRUE, message = NULL)
}

#' Validate multiple paths
#'
#' @param paths Character vector of paths
#' @param config Config list
#' @param operation Operation type for error messages
#' @return List with ok and message (first error if any)
#' @noRd
validate_paths <- function(paths, config, operation = "access") {
    for (p in paths) {
        result <- validate_path(p, config, operation)
        if (!result$ok) {
            return(result)
        }
    }
    list(ok = TRUE, message = NULL)
}

#' Check if a command is safe (basic heuristics)
#'
#' Checks for obviously dangerous patterns in shell commands.
#'
#' @param command Shell command string
#' @return List with ok and message
#' @noRd
validate_command <- function(command) {
    # Check for dangerous patterns
    dangerous_patterns <- c(
                            "rm\\s+-rf\\s+/", # rm -rf /
                            "rm\\s+-rf\\s+~", # rm -rf ~
                            ":\\(\\)\\{.*:\\|:.*\\};:", # Fork bomb  :(){ :|:& };:
                            "> /dev/sd", # Write to disk device
                            "dd\\s+if=.*of=/dev", # dd to device
                            "mkfs", # Format filesystem
                            "chmod\\s+-R\\s+777\\s+/", # Recursive chmod on root
                            "curl.*\\|\\s*bash", # Pipe curl to bash
                            "wget.*\\|\\s*bash" # Pipe wget to bash
    )

    for (pattern in dangerous_patterns) {
        if (grepl(pattern, command, ignore.case = TRUE)) {
            return(list(
                        ok = FALSE,
                        message = sprintf("Potentially dangerous command pattern detected: %s",
                        pattern)
                ))
        }
    }

    list(ok = TRUE, message = NULL)
}

#' Valid approval modes, in order of increasing restriction.
#' @noRd
.approval_modes <- c("allow", "ask", "deny")

#' Handle the `/permissions` slash command for both surfaces.
#'
#' Bare `/permissions` renders the current settings. `/permissions
#' <mode>` switches the session's approval mode live, which is the
#' answer to "I am sitting here and tired of pressing Enter" -- the
#' alternative was editing JSON and restarting.
#'
#' Session-scoped on purpose, same as `/dryrun`: a mode you flipped once
#' to get through an afternoon should not quietly become the default for
#' every future run in every repo. The reply names the config file for
#' anyone who does want it permanent.
#'
#' What `allow` does NOT do is worth printing rather than leaving for
#' someone to discover: [check_safety()] short-circuits before the
#' config overlay, so credential paths keep prompting whatever this is
#' set to. A user relaxing approvals should be told exactly what they
#' kept.
#'
#' @param config Config list (read for the current mode and display).
#' @param arg Character or NULL. Requested mode.
#' @param cwd Project directory, for the approvals-file line.
#' @return List with `changed` (logical), `mode` (new mode or NULL), and
#'   `text` (what to print).
#' @noRd
permissions_command <- function(config, arg = NULL, cwd = getwd()) {
    current <- config$approval_mode %||% "ask"
    arg <- tolower(trimws(as.character(arg %||% "")[1]))
    if (is.na(arg)) {
        arg <- ""
    }

    if (nzchar(arg) && !arg %in% .approval_modes) {
        return(list(
                    changed = FALSE, mode = NULL,
                    text = sprintf("Unknown approval mode '%s'. Use one of: %s",
                                   arg, paste(.approval_modes, collapse = ", "))
            ))
    }

    if (nzchar(arg)) {
        detail <- switch(arg,
                         allow = c(
                                   "  Dangerous tools run without prompting for this session.",
                                   "  Credential paths still prompt: safety rules short-circuit",
                                   "  before config, so this can't waive them."),
                         ask = "  Dangerous tools prompt before running.",
                         deny = "  Dangerous tools are refused outright, no prompt.")
        if (identical(arg, current)) {
            head_line <- sprintf("Approval mode: %s (unchanged)", arg)
        } else {
            head_line <- sprintf("Approval mode: %s (was %s)", arg, current)
        }
        return(list(
                    changed = TRUE, mode = arg,
                    text = paste(c(
                                   head_line, detail,
                                   sprintf("  Permanent: set \"approval_mode\": \"%s\" in %s",
                            arg, corteza_config_path("config.json"))
                    ), collapse = "\n")
            ))
    }

    approvals_path <- file.path(cwd, ".corteza", "approvals.json")
    list(
         changed = FALSE, mode = NULL,
         text = paste(c(
                        format_permissions(config),
                        sprintf("Project approvals: %s",
                    if (file.exists(approvals_path)) {
                        approvals_path
                    } else {
                        "none"
                    }),
                        sprintf("Change for this session: /permissions %s",
                                paste(.approval_modes, collapse = "|"))
            ), collapse = "\n")
    )
}

#' Format permissions for display
#'
#' @param config Config list
#' @return Character string describing current permissions
#' @noRd
format_permissions <- function(config) {
    lines <- character()

    # Global approval mode
    mode <- config$approval_mode %||% "ask"
    lines <- c(lines, sprintf("Approval mode: %s", mode))

    # Dangerous tools
    dangerous <- config$dangerous_tools %||% default_dangerous_tools()
    lines <- c(lines,
               sprintf("Dangerous tools: %s", paste(dangerous, collapse = ", ")))

    # Per-tool permissions
    perms <- config$permissions
    if (!is.null(perms) && length(perms) > 0) {
        lines <- c(lines, "Per-tool permissions:")
        for (name in names(perms)) {
            lines <- c(lines, sprintf("  %s: %s", name, perms[[name]]))
        }
    }

    # Allowed paths
    allowed <- config$allowed_paths
    if (!is.null(allowed) && length(allowed) > 0) {
        lines <- c(lines,
                   sprintf("Allowed paths: %s", paste(allowed, collapse = ", ")))
    }

    # Denied paths
    denied <- config$denied_paths %||% default_denied_paths()
    lines <- c(lines,
               sprintf("Denied paths: %s", paste(denied, collapse = ", ")))

    paste(lines, collapse = "\n")
}
