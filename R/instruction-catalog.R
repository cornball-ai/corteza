# Session-scoped instruction catalog.
#
# Instruction documents are deliberately separate from the executable skill
# registry and from the legacy process-global .skill_docs registry. A catalog
# is an immutable snapshot created for one turn session: compact metadata
# enters the system prompt, while bodies and referenced resources are available
# only through the read-only skill_instructions tool.

.instruction_root_id_rx <- "^[A-Za-z][A-Za-z0-9._-]*$"
.instruction_builtin_root_ids <- c("corteza-data", "project")

instruction_diagnostic <- function(level, code, message, path = "") {
    list(level = level, code = code, message = message, path = path)
}

instruction_config_values <- function(value, config_path = "") {
    if (is.null(value)) {
        return(list(values = character(), diagnostics = list()))
    }
    if (is.character(value)) {
        return(list(values = unname(value), diagnostics = list()))
    }
    if (is.list(value) && !is.null(names(value))) {
        vals <- unlist(value, use.names = FALSE)
        if (is.character(vals)) {
            return(list(values = vals, diagnostics = list()))
        }
    }
    list(
         values = character(),
         diagnostics = list(instruction_diagnostic(
                "error", "invalid_disabled",
                "instruction_disabled must be an array of exact instruction ids.",
                config_path
            ))
    )
}

instruction_root_entries <- function(value, base, origin, config_path) {
    diagnostics <- list()
    if (is.null(value)) {
        return(list(roots = list(), diagnostics = diagnostics))
    }
    if (is.character(value) && length(value) > 0L &&
        !is.null(names(value)) && all(nzchar(names(value)))) {
        value <- as.list(value)
    }
    if (!is.list(value) || is.null(names(value)) ||
        any(!nzchar(names(value)))) {
        diagnostics[[1L]] <- instruction_diagnostic(
            "error", "invalid_roots",
            "instruction_roots must be a named JSON object mapping root ids to paths.",
            config_path
        )
        return(list(roots = list(), diagnostics = diagnostics))
    }

    roots <- list()
    for (id in sort(names(value))) {
        path <- value[[id]]
        if (!grepl(.instruction_root_id_rx, id)) {
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "error", "invalid_root_id",
                sprintf("Invalid instruction root id '%s'.", id), config_path
            )
            next
        }
        if (id %in% .instruction_builtin_root_ids) {
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "error", "reserved_root_id",
                sprintf("Instruction root id '%s' is reserved.", id), config_path
            )
            next
        }
        if (!is.character(path) || length(path) != 1L || is.na(path) ||
            !nzchar(trimws(path))) {
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "error", "invalid_root_path",
                sprintf("Instruction root '%s' must have one non-empty path.",
                        id),
                config_path
            )
            next
        }
        path <- path.expand(path)
        if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
            path <- file.path(base, path)
        }
        roots[[id]] <- list(id = id, path = path, origin = origin,
                            config_path = config_path)
    }
    list(roots = roots, diagnostics = diagnostics)
}

# Resolve explicit instruction configuration without changing load_config's
# historical top-level replacement semantics. Named roots merge by stable id;
# project roots replace global roots with the same id, while disabled ids union.
instruction_session_config <- function(cwd = getwd()) {
    cwd <- normalizePath(cwd, winslash = "/", mustWork = FALSE)
    global_path <- corteza_config_path("config.json")
    project_path <- file.path(cwd, ".corteza", "config.json")
    global <- load_config_file(global_path)
    project <- load_config_file(project_path)

    g <- instruction_root_entries(global$instruction_roots,
                                  dirname(global_path), "global", global_path)
    p <- instruction_root_entries(
                                  project$instruction_roots, cwd, "project", project_path
    )
    roots <- g$roots
    for (id in names(p$roots)) {
        roots[[id]] <- p$roots[[id]]
    }
    roots[["corteza-data"]] <- list(
                                    id = "corteza-data", path = corteza_data_path("skills"),
                                    origin = "builtin", config_path = ""
    )
    roots[["project"]] <- list(
                               id = "project", path = file.path(cwd, ".corteza", "skills"),
                               origin = "builtin", config_path = ""
    )

    gd <- instruction_config_values(global$instruction_disabled, global_path)
    pd <- instruction_config_values(project$instruction_disabled, project_path)
    disabled <- unique(c(gd$values, pd$values))
    disabled <- sort(disabled[nzchar(disabled)])
    list(
         roots = roots[sort(names(roots))],
         disabled = disabled,
         diagnostics = c(g$diagnostics, p$diagnostics,
                         gd$diagnostics, pd$diagnostics)
    )
}

instruction_path_key <- function(path) {
    key <- gsub("\\\\", "/", path)
    if (.Platform$OS.type == "windows") {
        tolower(key)
    } else {
        key
    }
}

instruction_path_within <- function(path, root) {
    path <- instruction_path_key(path)
    root <- sub("/+$", "", instruction_path_key(root))
    identical(path, root) || startsWith(path, paste0(root, "/"))
}

# Recursively enumerate a root without allowing a symlink to escape it or a
# directory cycle to recurse forever. Only exact SKILL.md names are candidates.
instruction_walk_root <- function(root) {
    diagnostics <- list()
    if (!dir.exists(root)) {
        diagnostics[[1L]] <- instruction_diagnostic("info", "missing_root",
            "Instruction root does not exist.", root)
        return(list(skills = character(), files = character(),
                    diagnostics = diagnostics, canonical_root = NULL))
    }
    root_real <- tryCatch(
                          normalizePath(root, winslash = "/", mustWork = TRUE),
                          error = function(e) NULL
    )
    if (is.null(root_real) || !dir.exists(root_real)) {
        diagnostics[[1L]] <- instruction_diagnostic(
            "error", "unreadable_root", "Instruction root cannot be resolved.", root
        )
        return(list(skills = character(), files = character(),
                    diagnostics = diagnostics, canonical_root = NULL))
    }

    seen <- character()
    files <- character()
    visit <- function(dir) {
        key <- instruction_path_key(dir)
        if (key %in% seen) {
            diagnostics[[length(diagnostics) + 1L]] <<- instruction_diagnostic(
                "warning", "directory_cycle",
                "A repeated instruction directory was skipped.", dir
            )
            return(invisible())
        }
        seen <<- c(seen, key)
        children <- sort(list.files(dir, all.files = TRUE, no.. = TRUE,
                                    full.names = TRUE))
        for (child in children) {
            real <- tryCatch(
                             normalizePath(child, winslash = "/", mustWork = TRUE),
                             error = function(e) NULL
            )
            if (is.null(real)) {
                diagnostics[[length(diagnostics) + 1L]] <<-
                instruction_diagnostic(
                                       "warning", "unresolved_path",
                                       "An instruction path could not be resolved.", child
                )
                next
            }
            if (!instruction_path_within(real, root_real)) {
                diagnostics[[length(diagnostics) + 1L]] <<-
                instruction_diagnostic(
                                       "warning", "symlink_escape",
                                       "A path escaping its instruction root was skipped.", child
                )
                next
            }
            if (dir.exists(real)) {
                visit(real)
            } else
            if (file.exists(real)) {
                files <<- c(files, real)
            }
        }
        invisible()
    }
    visit(root_real)
    files <- sort(unique(files))
    list(
         skills = files[basename(files) == "SKILL.md"],
         files = files,
         diagnostics = diagnostics,
         canonical_root = root_real
    )
}

instruction_read_text <- function(path) {
    size <- file.info(path)$size
    if (is.na(size)) {
        stop("Instruction file cannot be read.", call. = FALSE)
    }
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    bytes <- readBin(con, what = "raw", n = size)
    if (any(bytes == as.raw(0L))) {
        stop("Instruction files must be text; NUL byte found.", call. = FALSE)
    }
    text <- rawToChar(bytes)
    if (!validUTF8(text)) {
        stop("Instruction files must be valid UTF-8.", call. = FALSE)
    }
    Encoding(text) <- "UTF-8"
    text
}

# Narrow frontmatter parser for the two catalog fields. It intentionally does
# not replace parse_skill_md(), whose behavior is part of the legacy API.
instruction_parse_frontmatter <- function(path) {
    text <- instruction_read_text(path)
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    lines <- sub("\r$", "", lines)
    if (length(lines) < 3L || !identical(lines[[1L]], "---")) {
        stop("SKILL.md must start with YAML frontmatter.", call. = FALSE)
    }
    end <- which(lines[-1L] == "---")
    if (!length(end)) {
        stop("SKILL.md frontmatter is not closed.", call. = FALSE)
    }
    end <- end[[1L]] + 1L
    yaml <- lines[seq.int(2L, end - 1L)]
    fields <- list()
    i <- 1L
    while (i <= length(yaml)) {
        line <- yaml[[i]]
        m <- regexec("^([A-Za-z][A-Za-z0-9_-]*):[[:space:]]*(.*)$", line,
                     perl = TRUE)
        hit <- regmatches(line, m)[[1L]]
        if (length(hit)) {
            key <- hit[[2L]]
            value <- hit[[3L]]
            if (key %in% c("name", "description")) {
                if (grepl("^[>|][+-]?$", value)) {
                    block <- character()
                    i <- i + 1L
                    while (i <= length(yaml) &&
                        (grepl("^[[:space:]]+", yaml[[i]]) ||
                            !nzchar(yaml[[i]]))) {
                        block <- c(block, sub("^[[:space:]]+", "", yaml[[i]]))
                        i <- i + 1L
                    }
                    fields[[key]] <- paste(block, collapse = " ")
                    next
                }
                fields[[key]] <- gsub("^[\"']|[\"']$", "", value)
            }
        }
        i <- i + 1L
    }
    name <- trimws(fields$name %||% "")
    description <- trimws(gsub("[[:space:]]+", " ", fields$description %||% ""))
    if (!nzchar(name)) {
        stop("SKILL.md frontmatter needs a non-empty name.", call. = FALSE)
    }
    if (grepl("[[:cntrl:]]", name) || nchar(name) > 120L) {
        stop("SKILL.md name is invalid.", call. = FALSE)
    }
    list(name = name, description = description, text = text)
}

instruction_relative_dir <- function(skill_path, root) {
    dir <- dirname(skill_path)
    if (identical(instruction_path_key(dir), instruction_path_key(root))) {
        return("")
    }
    substring(dir, nchar(sub("/+$", "", root)) + 2L)
}

instruction_entry_id <- function(root_id, relative_dir) {
    relative_dir <- gsub("\\\\", "/", relative_dir)
    if (!nzchar(relative_dir)) {
        root_id
    } else {
        paste(root_id, relative_dir, sep = ":")
    }
}

instruction_snapshot_files <- function(skill_dir,
                                       other_skill_dirs = character()) {
    walk <- instruction_walk_root(skill_dir)
    if (is.null(walk$canonical_root)) {
        return(list(files = list(), diagnostics = walk$diagnostics))
    }
    records <- list()
    nested <- other_skill_dirs[
        instruction_path_key(other_skill_dirs) !=
        instruction_path_key(walk$canonical_root) &
        vapply(other_skill_dirs, instruction_path_within, logical(1L),
               root = walk$canonical_root)
    ]
    for (path in walk$files) {
        if (length(nested) &&
            any(vapply(nested, function(dir) {
            instruction_path_within(path, dir)
        }, logical(1L)))) {
            next
        }
        rel <- if (identical(path, walk$canonical_root)) {
            basename(path)
        } else {
            substring(path, nchar(sub("/+$", "", walk$canonical_root)) + 2L)
        }
        rel <- gsub("\\\\", "/", rel)
        records[[rel]] <- list(
                               relative = rel,
                               path = path,
                               sha256 = digest::digest(file = path, algo = "sha256")
        )
    }
    list(files = records[sort(names(records))], diagnostics = walk$diagnostics)
}

build_instruction_catalog <- function(cwd = getwd()) {
    cfg <- instruction_session_config(cwd)
    diagnostics <- cfg$diagnostics
    candidates <- list()
    for (root_id in names(cfg$roots)) {
        spec <- cfg$roots[[root_id]]
        walked <- instruction_walk_root(spec$path)
        diagnostics <- c(diagnostics, walked$diagnostics)
        if (is.null(walked$canonical_root)) {
            next
        }
        for (path in walked$skills) {
            candidates[[length(candidates) + 1L]] <- list(root_id = root_id,
                root = walked$canonical_root,
                root_specificity = nchar(walked$canonical_root), path = path)
        }
    }

    # An overlapping root sees the same canonical SKILL.md. The most specific
    # configured root owns its stable id; lexical root id breaks exact ties.
    chosen <- list()
    if (length(candidates)) {
        keys <- vapply(candidates, function(x) instruction_path_key(x$path), "")
        for (key in sort(unique(keys))) {
            group <- candidates[keys == key]
            ord <- order(
                         -vapply(group, function(x) x$root_specificity, integer(1L)),
                         vapply(group, function(x) x$root_id, character(1L))
            )
            chosen[[length(chosen) + 1L]] <- group[[ord[[1L]]]]
        }
    }

    entries <- list()
    all_skill_dirs <- unique(vapply(chosen, function(x) dirname(x$path),
                                    character(1L)))
    for (candidate in chosen) {
        rel <- instruction_relative_dir(candidate$path, candidate$root)
        id <- instruction_entry_id(candidate$root_id, rel)
        if (id %in% cfg$disabled) {
            next
        }
        if (!is.null(entries[[id]])) {
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "error", "duplicate_id",
                sprintf("Duplicate instruction id '%s' was excluded.", id)
            )
            entries[[id]] <- NULL
            next
        }
        parsed <- tryCatch(
                           instruction_parse_frontmatter(candidate$path),
                           error = function(e) e
        )
        if (inherits(parsed, "error")) {
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "error", "invalid_frontmatter", conditionMessage(parsed),
                candidate$path
            )
            next
        }
        description <- parsed$description
        if (!nzchar(description)) {
            description <- "(description unavailable)"
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "warning", "missing_description",
                sprintf("Instruction '%s' has no description.", id),
                candidate$path
            )
        }
        inventory <- instruction_snapshot_files(
            dirname(candidate$path), other_skill_dirs = all_skill_dirs
        )
        diagnostics <- c(diagnostics, inventory$diagnostics)
        main <- inventory$files[["SKILL.md"]]
        if (is.null(main)) {
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "error", "missing_main_snapshot",
                sprintf("Instruction '%s' could not be snapshotted.", id),
                candidate$path
            )
            next
        }
        entries[[id]] <- list(
                              id = id, name = parsed$name, description = description,
                              root_id = candidate$root_id, relative_dir = rel,
                              skill_dir = dirname(candidate$path), skill_path = candidate$path,
                              sha256 = main$sha256, resources = inventory$files
        )
    }
    entries <- entries[sort(names(entries))]

    if (length(entries)) {
        names_seen <- vapply(entries, function(x) x$name, character(1L))
        for (name in sort(unique(names_seen[duplicated(names_seen)]))) {
            diagnostics[[length(diagnostics) + 1L]] <- instruction_diagnostic(
                "warning", "duplicate_name",
                sprintf("Instruction name '%s' is shared by multiple ids.", name)
            )
        }
    }
    signature <- paste(vapply(entries, function(x) {
        resources <- paste(vapply(x$resources, function(resource) {
            paste(resource$relative, resource$sha256, sep = "=")
        }, character(1L)), collapse = ";")
        paste(x$id, resources, sep = "=")
    }, character(1L)), collapse = "\n")
    catalog <- list(
                    entries = entries,
                    diagnostics = diagnostics,
                    roots = cfg$roots,
                    disabled = cfg$disabled,
                    hash = digest::digest(signature, algo = "sha256", serialize = FALSE)
    )
    class(catalog) <- "corteza_instruction_catalog"
    catalog
}

format_instruction_catalog <- function(catalog) {
    entries <- catalog$entries %||% list()
    if (!length(entries)) {
        return("")
    }
    lines <- c(
               "## Instruction catalog",
               "",
               paste(
                     "Instruction bodies are loaded on demand. When one applies, call",
                     "skill_instructions with its exact id. Relative supporting files",
                     "may be read with the same tool's resource argument."
        ),
               ""
    )
    for (entry in entries) {
        lines <- c(lines, sprintf("- %s — %s [%s]", entry$name,
                                  entry$description, entry$id))
    }
    paste(lines, collapse = "\n")
}

instruction_reader_selected <- function(filter) {
    if (is.null(filter) || "all" %in% filter) {
        return(TRUE)
    }
    any(filter %in% c("skill_instructions", "instruction", "file", "core"))
}

instruction_validate_resource <- function(resource) {
    if (is.null(resource) || !length(resource)) {
        return("SKILL.md")
    }
    if (!is.character(resource) || length(resource) != 1L || is.na(resource)) {
        stop("resource must be one relative path.", call. = FALSE)
    }
    if (!nzchar(resource)) {
        return("SKILL.md")
    }
    resource <- gsub("\\\\", "/", resource)
    if (startsWith(resource, "/") ||
        grepl("^[A-Za-z]:/", resource) ||
        any(strsplit(resource, "/", fixed = TRUE)[[1L]] == "..")) {
        stop("resource must stay within the selected instruction.",
             call. = FALSE)
    }
    resource <- sub("^\\./", "", resource)
    if (!nzchar(resource)) {
        "SKILL.md"
    } else {
        resource
    }
}

instruction_catalog_read <- function(catalog, id, resource = NULL) {
    if (!inherits(catalog, "corteza_instruction_catalog")) {
        stop("This session has no instruction catalog.", call. = FALSE)
    }
    if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
        stop("id must be one exact instruction id.", call. = FALSE)
    }
    entry <- catalog$entries[[id]]
    if (is.null(entry)) {
        stop(sprintf("Instruction id '%s' is not in this session catalog.", id),
             call. = FALSE)
    }
    relative <- instruction_validate_resource(resource)
    record <- entry$resources[[relative]]
    if (is.null(record)) {
        stop(sprintf(
                     "Resource '%s' is not part of instruction '%s' session snapshot.",
                     relative, id
            ), call. = FALSE)
    }
    if (!file.exists(record$path)) {
        stop(sprintf("Instruction '%s' drifted: '%s' is missing.", id,
                     relative),
             call. = FALSE)
    }
    current <- tryCatch(
                        normalizePath(record$path, winslash = "/", mustWork = TRUE),
                        error = function(e) NULL
    )
    if (is.null(current) ||
        !identical(instruction_path_key(current),
                   instruction_path_key(record$path)) ||
        !instruction_path_within(current, entry$skill_dir)) {
        stop(sprintf("Instruction '%s' drifted: '%s' changed identity.",
                     id, relative), call. = FALSE)
    }
    hash <- digest::digest(file = current, algo = "sha256")
    if (!identical(hash, record$sha256)) {
        stop(sprintf("Instruction '%s' drifted: '%s' changed after session start.",
                     id, relative), call. = FALSE)
    }
    content <- instruction_read_text(current)
    paste0(
           "instruction_id: ", id, "\n",
           "resource: ", relative, "\n",
           "sha256: ", hash, "\n\n",
           content
    )
}

#' Read a session-scoped instruction or supporting resource.
#'
#' Resolves an exact instruction identifier against the immutable catalog
#' captured for the current agent session. Supporting resources are confined to
#' the same snapshotted instruction directory and checked for drift.
#'
#' @param id (character) Exact instruction identifier shown in the session
#'   catalog.
#' @param resource (character) Optional relative supporting-resource path.
#'   Omit to read SKILL.md.
#' @param ctx (list) Host-owned tool execution context containing the session.
#' @return An MCP tool-result list.
#' @keywords internal
tool_skill_instructions <- function(id, resource = NULL, ctx = list()) {
    catalog <- ctx$session$instruction_catalog %||% NULL
    tryCatch(ok(instruction_catalog_read(catalog, id, resource)),
             error = function(e) err(conditionMessage(e)))
}
