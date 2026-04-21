# Skill System for corteza
# Defines the standard interface for tools/skills. Registry itself lives
# in R/registry.R — this file owns skill_spec, skill_run, file-based
# skill loading (SKILL.md + .R files), and dry-run previews.
#
# Two types of skills:
# 1. SKILL.md files - markdown docs injected into context (shell-based)
# 2. R handlers - built-in tools (R-native, registered via skill_spec)

# Skill docs registry for SKILL.md files
.skill_docs <- new.env(parent = emptyenv())

#' Create a skill specification
#'
#' Defines a skill with its schema and handler function.
#'
#' @param name Tool name (snake_case)
#' @param description What the skill does
#' @param params Named list of parameter definitions, each with:
#'   - type: "string", "integer", "number", "boolean", "array", "object"
#'   - description: Parameter description
#'   - required: TRUE/FALSE (default FALSE)
#'   - enum: Optional list of allowed values
#' @param handler Function(args, ctx) that returns a result
#' @return Skill specification list
#' @noRd
skill_spec <- function(name, description, params = list(), handler) {
    # Build required list from params
    required <- names(params)[vapply(params, function(p) {
        isTRUE(p$required)
    }, logical(1))]

    # Strip 'required' from properties (not part of JSON Schema)
    properties <- lapply(params, function(p) {
        p$required <- NULL
        p
    })

    # Ensure empty properties serializes as {} not []
    if (length(properties) == 0) {
        properties <- setNames(list(), character(0))
    }

    list(
         name = name,
         description = description,
         inputSchema = list(
                            type = "object",
                            properties = properties,
                            required = if (length(required) > 0) as.list(required) else list()
        ),
         handler = handler
    )
}

#' Run a skill
#'
#' Executes a skill's handler with validation and optional timeout.
#' Logs tool calls and results for observability.
#'
#' @param skill Skill spec from skill_spec()
#' @param args Named list of arguments
#' @param ctx Context list (cwd, session, config, etc.)
#' @param timeout Timeout in seconds (default 30, NULL for no timeout)
#' @param dry_run If TRUE, validate only without executing (default FALSE)
#' @return Result from handler (should be ok() or err())
#' @noRd
skill_run <- function(skill, args, ctx = list(), timeout = 30L,
                      dry_run = FALSE) {
    args <- args %||% list()
    start_time <- Sys.time()

    # Log tool call
    log_tool_call(skill$name, args)

    # Validate required params
    required <- skill$inputSchema$required
    if (length(required) > 0) {
        missing <- setdiff(unlist(required), names(args))
        if (length(missing) > 0) {
            result <- err(paste("Missing required parameters:",
                                paste(missing, collapse = ", ")))
            log_tool_result(skill$name, success = FALSE, elapsed_ms = 0)
            return(result)
        }
    }

    # Validate parameter types (basic type checking)
    validation_result <- validate_skill_args(skill, args)
    if (!validation_result$ok) {
        result <- err(validation_result$message)
        log_tool_result(skill$name, success = FALSE, elapsed_ms = 0)
        return(result)
    }

    # Dry-run mode: return what would happen without executing
    if (isTRUE(dry_run)) {
        preview <- format_dry_run_preview(skill, args)
        log_event("dry_run", tool = skill$name, args = args, level = "info")
        return(ok(preview))
    }

    # Execute with optional timeout
    if (!is.null(timeout) && timeout > 0) {
        result <- tryCatch({
            setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
            on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
            skill$handler(args, ctx)
        }, error = function(e) {
            if (grepl("time limit|elapsed time", e$message,
                      ignore.case = TRUE)) {
                log_error(sprintf("Skill timed out after %d seconds", timeout),
                          error_type = "timeout", tool = skill$name)
                err(sprintf("Skill timed out after %d seconds", timeout))
            } else {
                log_error(e$message, error_type = "skill_error",
                          tool = skill$name)
                err(paste("Skill error:", e$message))
            }
        })
    } else {
        result <- tryCatch(
                           skill$handler(args, ctx),
                           error = function(e) {
            log_error(e$message, error_type = "skill_error", tool = skill$name)
            err(paste("Skill error:", e$message))
        }
        )
    }

    # Log result
    elapsed_ms <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
    success <- !is.null(result$isError) && !result$isError
    result_lines <- if (!is.null(result$content[[1]]$text)) {
        length(strsplit(result$content[[1]]$text, "\n")[[1]])
    } else {
        NULL
    }
    log_tool_result(skill$name, success = success, result_lines = result_lines,
                    elapsed_ms = round(elapsed_ms))

    # Capture result into workspace (skip run_r, it handles its own capture)
    if (skill$name != "run_r" && !is.null(result$content[[1]]$text)) {
        result_text_ws <- result$content[[1]]$text
        ws_capture_tool_result(skill$name, args, result_text_ws,
                               ws_current_turn())

        # File write invalidation
        if (skill$name == "base::writeLines" && !is.null(args$con)) {
            ws_invalidate_file(args$con)
        }
        if (skill$name %in% c("write_file", "replace_in_file") &&
            !is.null(args$path)) {
            ws_invalidate_file(args$path)
        }
    }

    # Add trace entry if session context available
    if (!is.null(ctx$session_id)) {
        result_text <- if (!is.null(result$content[[1]]$text)) {
            result$content[[1]]$text
        } else {
            ""
        }
        trace_add(
                  session_id = ctx$session_id,
                  tool = skill$name,
                  args = args,
                  result = result_text,
                  success = success,
                  elapsed_ms = round(elapsed_ms),
                  approved_by = ctx$approved_by,
                  turn = ctx$turn,
                  agent_id = ctx$agent_id %||% "main"
        )
    }

    result
}

#' Validate skill arguments against schema
#'
#' Basic type validation for skill parameters.
#'
#' @param skill Skill spec
#' @param args Arguments to validate
#' @return List with ok (logical) and message (character if error)
#' @noRd
validate_skill_args <- function(skill, args) {
    props <- skill$inputSchema$properties
    if (is.null(props) || length(props) == 0) {
        return(list(ok = TRUE))
    }

    for (name in names(args)) {
        if (!name %in% names(props)) {
            # Unknown param - allow for flexibility
            next
        }

        prop <- props[[name]]
        value <- args[[name]]
        expected_type <- prop$type

        # Check enum constraint
        if (!is.null(prop$enum) && !value %in% prop$enum) {
            return(list(
                        ok = FALSE,
                        message = sprintf("Parameter '%s' must be one of: %s",
                        name, paste(prop$enum, collapse = ", "))
                ))
        }

        # Basic type checking
        type_ok <- switch(expected_type,
                          "string" = is.character(value),
                          "integer" = is.numeric(value) && (is.integer(value) ||
                          value == as.integer(value)),
                          "number" = is.numeric(value),
                          "boolean" = is.logical(value),
                          "array" = is.list(value) || is.vector(value),
                          "object" = is.list(value),
                          TRUE # Unknown type, allow
        )

        if (!type_ok) {
            return(list(
                        ok = FALSE,
                        message = sprintf("Parameter '%s' should be %s, got %s",
                        name, expected_type, class(value)[1])
                ))
        }
    }

    list(ok = TRUE)
}

#' Format dry-run preview
#'
#' Creates a human-readable preview of what a skill would do.
#'
#' @param skill Skill spec
#' @param args Arguments
#' @return Character string preview
#' @noRd
format_dry_run_preview <- function(skill, args) {
    lines <- c(
               sprintf("[DRY RUN] Would execute: %s", skill$name),
               sprintf("Description: %s", skill$description)
    )

    if (length(args) > 0) {
        lines <- c(lines, "Arguments:")
        for (name in names(args)) {
            value <- args[[name]]
            value_str <- if (is.character(value) && nchar(value) > 100) {
                paste0(substr(value, 1, 97), "...")
            } else {
                as.character(value)
            }
            lines <- c(lines, sprintf("  %s: %s", name, value_str))
        }
    }

    # Add skill-specific preview hints based on tool name
    preview_hint <- get_dry_run_hint(skill$name, args)
    if (!is.null(preview_hint)) {
        lines <- c(lines, "", "Preview:", preview_hint)
    }

    paste(lines, collapse = "\n")
}

#' Get dry-run hint for specific tools
#'
#' Provides tool-specific preview information.
#'
#' @param tool_name Tool name
#' @param args Tool arguments
#' @return Preview hint string or NULL
#' @noRd
get_dry_run_hint <- function(tool_name, args) {
    switch(tool_name,
           "base::writeLines" = {
        if (!is.null(args$text)) {
            sprintf("Would write to: %s", args$con %||% "(stdout)")
        }
    },
           "write_file" = {
        if (!is.null(args$path)) {
            sprintf("Would write to file: %s", args$path)
        }
    },
           "replace_in_file" = {
        if (!is.null(args$path)) {
            sprintf("Would replace text in file: %s", args$path)
        }
    },
           "bash" = {
        if (!is.null(args$command)) {
            sprintf("Would execute shell command: %s", args$command)
        }
    },
           "git_diff" = {
        sprintf("Would diff against: %s", args$ref %||% "HEAD")
    },
           "run_r" = {
        if (!is.null(args$code)) {
            code_preview <- if (nchar(args$code) > 200) {
                paste0(substr(args$code, 1, 197), "...")
            } else {
                args$code
            }
            sprintf("Would execute R code:\n%s", code_preview)
        }
    },
           NULL
    )
}

# register_skill / get_skill / list_skills / clear_skills /
# .skill_registry live in R/registry.R.

#' Load skills from a directory
#'
#' Sources all .R files in the directory. Each file should call
#' register_skill() to add skills to the registry.
#'
#' @param path Directory path containing skill files
#' @param pattern File pattern to match (default "*.R")
#' @return Character vector of loaded file names (invisible)
#' @noRd
load_skills <- function(path, pattern = "*.R") {
    path <- path.expand(path)
    if (!dir.exists(path)) {
        return(invisible(character()))
    }

    files <- Sys.glob(file.path(path, pattern))

    # Create environment with skill functions available
    skill_env <- new.env(parent = globalenv())
    skill_env$skill_spec <- skill_spec
    skill_env$register_skill <- register_skill
    skill_env$ok <- ok
    skill_env$err <- err

    for (f in files) {
        tryCatch(
                 source(f, local = skill_env),
                 error = function(e) {
            warning(sprintf("Failed to load skill from %s: %s", f, e$message))
        }
        )
    }

    invisible(basename(files))
}

#' Get all skills as MCP tool list
#'
#' Returns skills in the format expected by MCP tools/list.
#'
#' @return List of tool definitions (name, description, inputSchema)
#' @noRd
skills_as_tools <- function() {
    skill_names <- list_skills()
    lapply(skill_names, function(name) {
        skill <- get_skill(name)
        list(
             name = skill$name,
             description = skill$description,
             inputSchema = skill$inputSchema
        )
    })
}

#' Call a skill by name
#'
#' Looks up skill in registry and executes it.
#'
#' @param name Skill name
#' @param args Named list of arguments
#' @param ctx Context list
#' @param timeout Timeout in seconds
#' @param dry_run If TRUE, validate only without executing
#' @return Result from skill handler
#' @noRd
call_skill <- function(name, args, ctx = list(), timeout = 30L,
                       dry_run = FALSE) {
    skill <- get_skill(name)
    if (is.null(skill)) {
        return(err(paste("Unknown skill:", name)))
    }
    skill_run(skill, args, ctx, timeout, dry_run)
}

# SKILL.md Support ----
# Isomorphic with openclaw - markdown files with YAML frontmatter

#' Parse a SKILL.md file
#'
#' Extracts YAML frontmatter and markdown body from a skill file.
#'
#' @param path Path to SKILL.md file
#' @return List with name, description, metadata, and body
#' @noRd
parse_skill_md <- function(path) {
    if (!file.exists(path)) {
        return(NULL)
    }

    lines <- readLines(path, warn = FALSE)
    if (length(lines) == 0) {
        return(NULL)
    }

    # Check for YAML frontmatter (starts with ---)
    if (!grepl("^---\\s*$", lines[1])) {
        # No frontmatter, treat entire file as body
        body <- paste(lines, collapse = "\n")
        body <- gsub("\\{baseDir\\}", dirname(path), body)
        return(list(
                    name = tools::file_path_sans_ext(basename(dirname(path))),
                    description = "",
                    metadata = list(),
                    body = body,
                    path = path
            ))
    }

    # Find end of frontmatter
    end_idx <- which(grepl("^---\\s*$", lines[-1]))[1] + 1
    if (is.na(end_idx)) {
        # No closing ---, treat as no frontmatter
        body <- paste(lines, collapse = "\n")
        body <- gsub("\\{baseDir\\}", dirname(path), body)
        return(list(
                    name = tools::file_path_sans_ext(basename(dirname(path))),
                    description = "",
                    metadata = list(),
                    body = body,
                    path = path
            ))
    }

    # Extract frontmatter and body
    frontmatter_lines <- lines[2:(end_idx - 1)]
    if (end_idx < length(lines)) {
        body_lines <- lines[(end_idx + 1):length(lines)]
    } else {
        body_lines <- character()
    }

    # Parse YAML frontmatter (simple key: value parsing)
    frontmatter <- parse_yaml_simple(frontmatter_lines)

    # Template {baseDir} to skill directory (openclaw compatibility)
    skill_dir <- dirname(path)
    body <- paste(body_lines, collapse = "\n")
    body <- gsub("\\{baseDir\\}", skill_dir, body)

    list(
         name = frontmatter$name %||% tools::file_path_sans_ext(basename(dirname(path))),
         description = frontmatter$description %||% "",
         metadata = frontmatter$metadata %||% list(),
         body = body,
         path = path
    )
}

#' Simple YAML parser for frontmatter
#'
#' Parses basic YAML (key: value, no nesting beyond JSON in metadata).
#'
#' @param lines Character vector of YAML lines
#' @return Named list
#' @noRd
parse_yaml_simple <- function(lines) {
    result <- list()

    for (line in lines) {
        # Skip empty lines and comments
        if (grepl("^\\s*$", line) || grepl("^\\s*#", line)) {
            next
        }

        # Match key: value
        match <- regmatches(line,
                            regexec("^([a-zA-Z_][a-zA-Z0-9_]*):\\s*(.*)$", line))[[1]]
        if (length(match) == 3) {
            key <- match[2]
            value <- match[3]

            # Remove surrounding quotes if present
            value <- gsub("^[\"']|[\"']$", "", value)

            # Try to parse JSON for metadata field
            if (key == "metadata" && grepl("^\\{", value)) {
                result[[key]] <- tryCatch(
                    jsonlite::fromJSON(value, simplifyVector = FALSE),
                    error = function(e) list()
                )
            } else {
                result[[key]] <- value
            }
        }
    }

    result
}

#' Load SKILL.md files from a directory
#'
#' Scans directory for SKILL.md files and loads them into the docs registry.
#' Supports both flat structure (skill.md files) and nested (skill/SKILL.md).
#'
#' @param path Directory path
#' @return Character vector of loaded skill names (invisible)
#' @noRd
load_skill_docs <- function(path) {
    path <- path.expand(path)
    if (!dir.exists(path)) {
        return(invisible(character()))
    }

    loaded <- character()

    # Pattern 1: path/skillname/SKILL.md (nested, like openclaw)
    subdirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
    for (d in subdirs) {
        skill_file <- file.path(d, "SKILL.md")
        if (file.exists(skill_file)) {
            skill <- parse_skill_md(skill_file)
            if (!is.null(skill)) {
                .skill_docs[[skill$name]] <- skill
                loaded <- c(loaded, skill$name)
            }
        }
    }

    # Pattern 2: path/*.md (flat, simple)
    md_files <- Sys.glob(file.path(path, "*.md"))
    for (f in md_files) {
        skill <- parse_skill_md(f)
        if (!is.null(skill)) {
            # Use filename as skill name for flat files
            skill$name <- tools::file_path_sans_ext(basename(f))
            .skill_docs[[skill$name]] <- skill
            loaded <- c(loaded, skill$name)
        }
    }

    invisible(loaded)
}

#' List loaded skill docs
#'
#' @return Character vector of skill doc names
#' @noRd
list_skill_docs <- function() {
    ls(.skill_docs)
}

#' Get a skill doc by name
#'
#' @param name Skill name
#' @return Skill doc list or NULL
#' @noRd
get_skill_doc <- function(name) {
    if (exists(name, envir = .skill_docs, inherits = FALSE)) {
        .skill_docs[[name]]
    } else {
        NULL
    }
}

#' Clear all skill docs
#'
#' @return Invisible NULL
#' @noRd
clear_skill_docs <- function() {
    rm(list = ls(.skill_docs), envir = .skill_docs)
    invisible(NULL)
}

#' Format skill docs for context injection
#'
#' Creates markdown text suitable for system prompt.
#'
#' @param names Optional character vector of skill names to include.
#'   If NULL, includes all loaded skills.
#' @return Character string with formatted skill docs
#' @noRd
format_skill_docs <- function(names = NULL) {
    if (is.null(names)) {
        names <- list_skill_docs()
    }

    if (length(names) == 0) {
        return("")
    }

    parts <- character()

    for (name in names) {
        skill <- get_skill_doc(name)
        if (is.null(skill)) {
            next
        }

        # Format as markdown section
        header <- sprintf("## Skill: %s", skill$name)
        if (nchar(skill$description) > 0) {
            header <- paste0(header, "\n\n", skill$description)
        }

        parts <- c(parts, header, "", skill$body, "")
    }

    paste(parts, collapse = "\n")
}

# ============================================================================
# Skill Packaging
# ============================================================================

#' Parse SKILL.json metadata file
#'
#' @param path Path to SKILL.json
#' @return List with skill metadata, or NULL on error
#' @noRd
parse_skill_json <- function(path) {
    if (!file.exists(path)) {
        return(NULL)
    }

    tryCatch({
        meta <- jsonlite::fromJSON(path, simplifyVector = FALSE)

        list(
             name = meta$name %||% basename(dirname(path)),
             version = meta$version %||% "0.0.0",
             schema_version = meta$schema_version %||% "1",
             description = meta$description %||% "",
             tools = meta$tools %||% list(),
             dependencies = meta$dependencies %||% list(),
             author = meta$author,
             license = meta$license
        )
    }, error = function(e) {
        warning(sprintf("Failed to parse SKILL.json at %s: %s", path,
                        e$message))
        NULL
    })
}

#' Validate a skill package
#'
#' Checks that a skill directory has required files and valid structure.
#'
#' @param path Path to skill directory
#' @return List with valid (logical), errors (character vector)
#' @noRd
validate_skill_package <- function(path) {
    errors <- character()

    if (!dir.exists(path)) {
        return(list(valid = FALSE, errors = "Directory does not exist"))
    }

    # Check for SKILL.json or SKILL.md
    has_json <- file.exists(file.path(path, "SKILL.json"))
    has_md <- file.exists(file.path(path, "SKILL.md"))

    if (!has_json && !has_md) {
        errors <- c(errors, "Missing SKILL.json or SKILL.md")
    }

    # If has SKILL.json, validate it
    if (has_json) {
        meta <- parse_skill_json(file.path(path, "SKILL.json"))
        if (is.null(meta)) {
            errors <- c(errors, "Invalid SKILL.json")
        } else {
            if (is.null(meta$name) || nchar(meta$name) == 0) {
                errors <- c(errors, "SKILL.json missing 'name' field")
            }
        }
    }

    # Check for skill.R (optional but recommended)
    if (!file.exists(file.path(path, "skill.R")) && has_json) {
        # SKILL.json implies R handlers expected
        if (length(errors) == 0) {
            # Just a warning, not an error
        }
    }

    list(valid = length(errors) == 0, errors = errors)
}

#' Install a skill from a path or URL
#'
#' @param source Path to skill directory or URL
#' @param target_dir Installation directory. Default is
#'   \code{tools::R_user_dir("corteza", "data")/skills}.
#' @param force Overwrite if exists
#' @return Installed skill name
#' @export
skill_install <- function(source, target_dir = NULL, force = FALSE) {
    if (is.null(target_dir)) {
        target_dir <- corteza_data_path("skills")
    }
    target_dir <- path.expand(target_dir)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

    # Handle URL (git clone or download)
    if (grepl("^https?://", source)) {
        # For GitHub URLs, try git clone
        if (grepl("github.com", source)) {
            temp_dir <- tempfile("skill_")
            result <- system2("git", c("clone", "--depth", "1", source,
                                       temp_dir),
                              stdout = TRUE, stderr = TRUE)
            if (!dir.exists(temp_dir)) {
                stop("Failed to clone repository: ", paste(result,
                        collapse = "\n"),
                     call. = FALSE)
            }
            source <- temp_dir
        } else {
            stop("URL installation only supported for GitHub repositories",
                 call. = FALSE)
        }
    }

    source <- path.expand(source)

    # Validate source
    validation <- validate_skill_package(source)
    if (!validation$valid) {
        stop("Invalid skill package: ", paste(validation$errors,
                collapse = "; "),
             call. = FALSE)
    }

    # Get skill name
    meta_path <- file.path(source, "SKILL.json")
    if (file.exists(meta_path)) {
        meta <- parse_skill_json(meta_path)
        skill_name <- meta$name
    } else {
        skill_name <- basename(source)
    }

    # Check if already installed
    dest <- file.path(target_dir, skill_name)
    if (dir.exists(dest)) {
        if (!force) {
            stop("Skill '", skill_name, "' already installed. Use force=TRUE to overwrite.",
                 call. = FALSE)
        }
        unlink(dest, recursive = TRUE)
    }

    # Copy to target
    dir.create(dest, recursive = TRUE)
    files <- list.files(source, full.names = TRUE, all.files = TRUE)
    files <- files[!basename(files) %in% c(".", "..")]
    file.copy(files, dest, recursive = TRUE)

    log_event("skill_install", skill = skill_name, source = source)

    skill_name
}

#' Remove an installed skill
#'
#' @param name Skill name
#' @param skill_dir Skills directory
#' @return Invisible TRUE on success
#' @export
skill_remove <- function(name, skill_dir = NULL) {
    if (is.null(skill_dir)) {
        skill_dir <- file.path(get_workspace_dir(), "..", "skills")
    }
    skill_dir <- path.expand(skill_dir)

    path <- file.path(skill_dir, name)
    if (!dir.exists(path)) {
        stop("Skill not found: ", name, call. = FALSE)
    }

    unlink(path, recursive = TRUE)

    # Clear from registries
    if (exists(name, envir = .skill_registry, inherits = FALSE)) {
        rm(list = name, envir = .skill_registry)
    }
    if (exists(name, envir = .skill_docs, inherits = FALSE)) {
        rm(list = name, envir = .skill_docs)
    }

    log_event("skill_remove", skill = name)

    invisible(TRUE)
}

#' List installed skills
#'
#' @param skill_dir Skills directory
#' @return Data frame with skill info
#' @export
skill_list_installed <- function(skill_dir = NULL) {
    if (is.null(skill_dir)) {
        skill_dir <- file.path(get_workspace_dir(), "..", "skills")
    }
    skill_dir <- path.expand(skill_dir)

    if (!dir.exists(skill_dir)) {
        return(data.frame(
                          name = character(),
                          version = character(),
                          description = character(),
                          stringsAsFactors = FALSE
            ))
    }

    dirs <- list.dirs(skill_dir, recursive = FALSE, full.names = TRUE)

    skills <- lapply(dirs, function(d) {
        json_path <- file.path(d, "SKILL.json")
        md_path <- file.path(d, "SKILL.md")

        if (file.exists(json_path)) {
            meta <- parse_skill_json(json_path)
            if (!is.null(meta)) {
                return(data.frame(
                                  name = meta$name,
                                  version = meta$version,
                                  description = substr(meta$description, 1, 50),
                                  stringsAsFactors = FALSE
                    ))
            }
        }

        if (file.exists(md_path)) {
            skill <- parse_skill_md(md_path)
            if (!is.null(skill)) {
                return(data.frame(
                                  name = skill$name,
                                  version = "md",
                                  description = substr(skill$description, 1, 50),
                                  stringsAsFactors = FALSE
                    ))
            }
        }

        # Directory without valid skill files
        NULL
    })

    skills <- Filter(Negate(is.null), skills)

    if (length(skills) == 0) {
        return(data.frame(
                          name = character(),
                          version = character(),
                          description = character(),
                          stringsAsFactors = FALSE
            ))
    }

    do.call(rbind, skills)
}

#' Run skill tests
#'
#' Executes test_*.R files in a skill directory.
#'
#' @param path Path to skill directory
#' @param verbose Print test output
#' @return List with passed, failed, errors
#' @export
skill_test <- function(path, verbose = TRUE) {
    path <- path.expand(path)

    if (!dir.exists(path)) {
        stop("Skill directory not found: ", path, call. = FALSE)
    }

    test_files <- Sys.glob(file.path(path, "test_*.R"))

    if (length(test_files) == 0) {
        if (verbose) {
            message("No test files found")
        }
        return(list(passed = 0L, failed = 0L, errors = character()))
    }

    if (verbose) {
        message(sprintf("Running %d test file(s)...", length(test_files)))
    }

    passed <- 0L
    failed <- 0L
    errors <- character()

    for (tf in test_files) {
        if (verbose) {
            message(sprintf("  %s", basename(tf)))
        }

        result <- tryCatch({
            source(tf, local = TRUE)
            list(ok = TRUE)
        }, error = function(e) {
            list(ok = FALSE, error = e$message)
        })

        if (result$ok) {
            passed <- passed + 1L
        } else {
            failed <- failed + 1L
            errors <- c(errors, sprintf("%s: %s", basename(tf), result$error))
        }
    }

    if (verbose) {
        if (failed == 0) {
            message(sprintf("All %d test(s) passed", passed))
        } else {
            message(sprintf("%d passed, %d failed", passed, failed))
            for (e in errors) {
                message(sprintf("  Error: %s", e))
            }
        }
    }

    list(passed = passed, failed = failed, errors = errors)
}

#' Format installed skills for display
#'
#' @param skills Data frame from skill_list_installed()
#' @return Character string for display
#' @noRd
format_skill_list <- function(skills) {
    if (nrow(skills) == 0) {
        return("No skills installed.")
    }

    lines <- c("Installed skills:")

    for (i in seq_len(nrow(skills))) {
        s <- skills[i,]
        lines <- c(lines, sprintf("  %s (v%s) - %s",
                                  s$name, s$version, s$description))
    }

    paste(lines, collapse = "\n")
}

