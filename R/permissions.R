# Permission System
# Handles per-tool permissions and filesystem sandboxing

#' Default dangerous tools (require approval by default)
#' @noRd
default_dangerous_tools <- function() {
    c("bash", "run_r", "run_r_script", "base::writeLines")
}

#' Default denied paths for filesystem sandboxing
#' @noRd
default_denied_paths <- function() {
    c(
        "~/.ssh",
        "~/.gnupg",
        "~/.aws",
        "~/.config/gcloud",
        "~/.kube",
        "~/.docker"
    )
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

    # Try to normalize (resolve symlinks, etc.)
    # normalizePath fails if path doesn't exist, so handle that
    tryCatch({
        normalizePath(path, mustWork = FALSE)
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

