# The terminal's "Allow always for this project": the tools a person
# allowed for every later session in one directory.
#
# They are kept in the user's config directory, by directory
# (approval-engine note, decision 5). A repository therefore brings none
# along, trusted or not, and saving one makes no folder in the project.
# The project's own .corteza/approvals.json, where 0.8.0 kept them, is
# not read: the terminal says so when it starts where it finds one, and
# /permissions shows it.
#
# No saved answer answers the credential rule's ask (R/policy.R, and
# cli_approval_cb() in the command).

# Where the answers are kept.
approvals_file <- function() {
    corteza_config_path("approvals.json")
}

# Every saved answer: a list from directory to the tools allowed there.
# A file that cannot be read gives none, and is set aside beside itself
# so that the next answer saved does not overwrite it.
approvals_read <- function() {
    path <- approvals_file()
    if (!file.exists(path)) {
        return(list())
    }
    dirs <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (!is.list(dirs) || (length(dirs) && is.null(names(dirs)))) {
        suppressWarnings(file.rename(path, paste0(path, ".unreadable")))
        return(list())
    }
    dirs
}

# The tools of `dirs` allowed always in the directory `key`.
approvals_in <- function(dirs, key) {
    tools <- unlist(dirs[[key]], use.names = FALSE)
    if (is.character(tools)) {
        tools
    } else {
        character()
    }
}

# The tools allowed always in `cwd`. Directories compare as trust
# compares them (trust_dir()): links resolved, so a link to a directory
# has its answers. A directory inside it has its own.
approvals_saved <- function(cwd) {
    approvals_in(approvals_read(), trust_dir(cwd))
}

# Save `tool` as allowed always in `cwd`. Written beside the file and
# renamed into place, so a reader never sees half of it. Returns
# whether it was saved.
approval_save <- function(cwd, tool) {
    path <- approvals_file()
    dirs <- approvals_read()
    key <- trust_dir(cwd)
    dirs[[key]] <- sort(unique(c(approvals_in(dirs, key), tool)))
    tmp <- paste0(path, ".tmp")
    saved <- tryCatch({
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(dirs, tmp, pretty = TRUE)
        file.rename(tmp, path)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(saved)) {
        unlink(tmp)
    }
    isTRUE(saved)
}

# A line for the terminal at start where the project holds the file
# 0.8.0 kept these answers in, or NULL.
approvals_project_notice <- function(cwd) {
    if (!file.exists(project_approvals_path(cwd))) {
        return(NULL)
    }
    sprintf(paste("This project's .corteza/approvals.json is not read.",
                  "Answers saved with \"Allow always for this project\"",
                  "are kept in %s."), approvals_file())
}

# The saved answers as a display says them.
approvals_label <- function(cwd) {
    tools <- approvals_saved(cwd)
    out <- if (length(tools)) {
        sprintf("%s (kept in %s)", paste(tools, collapse = ", "),
                approvals_file())
    } else {
        "none"
    }
    if (file.exists(project_approvals_path(cwd))) {
        out <- paste0(out, "; the project's .corteza/approvals.json is not read")
    }
    out
}
