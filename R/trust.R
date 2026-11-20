# Project trust: what a session may load from the directory it works in.
#
# A project's own files arrive with the repository: its config, its
# skills, its instruction file, its stored lessons, and the DESCRIPTION
# and commit subjects a briefing is built from. None of them can vouch
# for itself. A session loads them only from a directory the user has
# trusted, and from any other it loads nothing and says so. Approvals a
# project's folder holds are not read at all (R/approvals.R).
#
# Trust decides what corteza loads by itself when a session starts. It
# is not a sandbox: a tool can still read a file in an untrusted
# directory when it is asked to, and what it reads comes back as a tool
# result, under the policy that governs every call.
#
# The list of trusted directories is the user's. It is read from two
# places in the user's config directory and from nowhere a project can
# write: the `trusted_projects` key of config.json, which a person
# edits, and trusted-projects.json, which `/trust` writes. corteza
# never rewrites config.json.

# A directory as trust compares it: links resolved, forward slashes, no
# trailing slash. A path that does not exist stays as written.
trust_dir <- function(dir) {
    dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
    if (nchar(dir) > 1L) {
        dir <- sub("/+$", "", dir)
    }
    dir
}

# Where `/trust` keeps what it was told.
trust_file <- function() {
    corteza_config_path("trusted-projects.json")
}

# The directories `/trust` has saved. A file that cannot be read gives
# none: trust fails closed.
trust_file_read <- function() {
    path <- trust_file()
    if (!file.exists(path)) {
        return(character())
    }
    dirs <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                     error = function(e) NULL)
    if (!is.character(dirs)) {
        return(character())
    }
    dirs
}

# Every directory the user trusts, resolved. An entry has to be a full
# path, or one that starts at the home: a relative one would mean a
# different directory in every session.
trusted_projects <- function() {
    config <- load_config_file(corteza_config_path("config.json"))
    listed <- unlist(config$trusted_projects)
    if (!is.character(listed)) {
        listed <- character()
    }
    dirs <- c(listed, trust_file_read())
    dirs <- path.expand(dirs[!is.na(dirs) & nzchar(dirs)])
    dirs <- dirs[grepl("^(/|[A-Za-z]:[/\\\\])", dirs)]
    unique(vapply(dirs, trust_dir, character(1), USE.NAMES = FALSE))
}

# Is `dir` (as trust_dir() gives it) corteza's scratch folder, or inside
# it? That folder is never a project, whatever the list covers: a room
# with no directory works there and its tools write there, so a file
# found in it was put there by a model and not by the user.
trust_in_scratch <- function(dir) {
    scratch <- trust_dir(bot_scratch_path())
    identical(dir, scratch) || startsWith(dir, paste0(scratch, "/"))
}

# The trusted directory that `cwd` is, or lies inside: the nearest one
# when several do. NULL when none does, and for the scratch folder.
#
# `cwd` has to exist. A path that exists resolves in full, links and
# ".." included, so a link that leads out of a trusted directory is not
# trusted and a directory reached through a link is judged by where it
# is. A path that does not exist would be compared as written, and
# "<trusted>/../elsewhere" would pass.
project_trust_root <- function(cwd = getwd()) {
    if (!is.character(cwd) || length(cwd) != 1L || is.na(cwd) ||
        !nzchar(cwd) || !dir.exists(cwd)) {
        return(NULL)
    }
    dir <- trust_dir(cwd)
    if (trust_in_scratch(dir)) {
        return(NULL)
    }
    roots <- trusted_projects()
    if (!length(roots)) {
        return(NULL)
    }
    inside <- ifelse(endsWith(roots, "/"), roots, paste0(roots, "/"))
    hit <- roots[dir == roots | startsWith(dir, inside)]
    if (!length(hit)) {
        return(NULL)
    }
    hit[[which.max(nchar(hit))]]
}

# Is `cwd` a trusted directory, or inside one?
project_trusted <- function(cwd = getwd()) {
    !is.null(project_trust_root(cwd))
}

# Add `cwd` to the directories `/trust` has saved. Returns the directory
# as saved, invisibly. The file is written beside its final name and
# renamed, so a reader never finds half of it.
project_trust_add <- function(cwd = getwd()) {
    if (!is.character(cwd) || length(cwd) != 1L || is.na(cwd) ||
        !dir.exists(cwd)) {
        stop("Not a directory: ", paste(format(cwd), collapse = " "),
             call. = FALSE)
    }
    dir <- trust_dir(cwd)
    saved <- trust_file_read()
    if (dir %in% saved) {
        return(invisible(dir))
    }
    path <- trust_file()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(paste0(basename(path), "."), tmpdir = dirname(path))
    ok <- tryCatch({
        writeLines(jsonlite::toJSON(c(saved, dir), pretty = TRUE), tmp)
        Sys.chmod(tmp, "600")
        file.rename(tmp, path)
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) {
        unlink(tmp)
        stop("Could not save the trusted directory to ", path, call. = FALSE)
    }
    invisible(dir)
}

# The project's config file. Named here and nowhere else.
project_config_path <- function(cwd) {
    file.path(cwd, ".corteza", "config.json")
}

# The project's config, or an empty one where the directory is not
# trusted. Every reader of the project's config goes through here, so
# the decision is made once and before the file is opened.
project_config <- function(cwd = getwd()) {
    if (!project_trusted(cwd)) {
        return(list())
    }
    load_config_file(project_config_path(cwd))
}

# Which `context_files` entries name a file through the project `cwd`:
# a relative entry, which resolves there, and a full path inside it.
context_file_in_project <- function(cwd, files) {
    if (!length(files)) {
        return(logical())
    }
    if (!is.character(cwd) || length(cwd) != 1L || is.na(cwd) || !nzchar(cwd)) {
        return(rep(TRUE, length(files)))
    }
    full <- path.expand(files)
    full[is.na(full)] <- ""
    relative <- !grepl("^(/|[A-Za-z]:[/\\\\])", full)
    root <- paste0(trust_dir(cwd), "/")
    resolved <- vapply(full, trust_dir, character(1), USE.NAMES = FALSE)
    relative | startsWith(paste0(resolved, "/"), root)
}

# The `context_files` entries a session in `cwd` may load. From a
# trusted directory, all of them. From any other, only a file the user's
# config names by a full path outside it: a relative entry is a name of
# the user's that the project fills in.
project_context_files <- function(cwd, files) {
    files <- as.character(unlist(files))
    if (!length(files) || project_trusted(cwd)) {
        return(files)
    }
    files[!context_file_in_project(cwd, files)]
}

# The project's own skills folder.
project_skills_dir <- function(cwd) {
    file.path(cwd, ".corteza", "skills")
}

# Source the project's skills (`code`) and read their documents
# (`docs`), from a trusted directory only. Sourcing a file runs it, so
# for an untrusted project this is the step that must not happen: the
# decision comes before any file in the folder is opened. The tools go
# to a session's `table` (R/tool-table.R), or to the registry without
# one. Returns whether the folder was loaded.
load_project_skills <- function(cwd = getwd(), code = TRUE, docs = TRUE,
                                table = NULL) {
    if (!project_trusted(cwd)) {
        return(invisible(FALSE))
    }
    if (isTRUE(code)) {
        origin <- if (!is.null(table)) {
            tool_origin("project",
                        normalizePath(cwd, winslash = "/", mustWork = FALSE))
        }
        load_skills(project_skills_dir(cwd), table = table, origin = origin)
    }
    if (isTRUE(docs)) {
        load_skill_docs(project_skills_dir(cwd))
    }
    invisible(TRUE)
}

# Where 0.8.0 kept the terminal's "Allow always for this project". Not
# read: the answers are the user's, kept in the config directory
# (R/approvals.R). Only its presence is reported.
project_approvals_path <- function(cwd) {
    file.path(cwd, ".corteza", "approvals.json")
}

# The project's stored lessons. harness_path() is its one reader, and
# decides first.
project_harness_path <- function(cwd) {
    file.path(cwd, ".corteza", "harness.json")
}

# Is `dir` in a git work tree? Looks for `.git` upward and runs nothing.
trust_in_git <- function(dir) {
    dir <- trust_dir(dir)
    repeat {
        if (file.exists(file.path(dir, ".git"))) {
            return(TRUE)
        }
        up <- dirname(dir)
        if (identical(up, dir)) {
            return(FALSE)
        }
        dir <- up
    }
}

# What `cwd` holds that a session loads from a trusted directory, as
# labels for a person. Looks at names only and opens nothing of the
# project's.
project_offers <- function(cwd = getwd()) {
    at <- function(...) file.path(cwd, ...)
    out <- character()
    if (file.exists(project_config_path(cwd))) {
        out <- c(out, "its config (.corteza/config.json)")
    }
    if (length(list.files(project_skills_dir(cwd), recursive = TRUE))) {
        out <- c(out, "its skills (.corteza/skills)")
    }
    if (file.exists(project_harness_path(cwd))) {
        out <- c(out, "its stored lessons (.corteza/harness.json)")
    }
    for (name in c("AGENTS.md", "CLAUDE.md")) {
        if (file.exists(at(name))) {
            out <- c(out, sprintf("its instructions (%s)", name))
            break
        }
    }
    config <- load_config_file(corteza_config_path("config.json"))
    named <- as.character(unlist(config$context_files))
    named <- named[context_file_in_project(cwd, named)]
    full <- path.expand(named)
    relative <- !grepl("^(/|[A-Za-z]:[/\\\\])", full)
    full[relative] <- file.path(cwd, full[relative])
    named <- named[file.exists(full)]
    if (length(named)) {
        out <- c(out, sprintf("the context files your config names in it (%s)",
                              paste(named, collapse = ", ")))
    }
    from <- character()
    if (file.exists(at("DESCRIPTION"))) {
        from <- "DESCRIPTION"
    }
    if (trust_in_git(cwd)) {
        from <- c(from, "commit subjects")
    }
    if (length(from)) {
        out <- c(out, sprintf("a briefing from its %s",
                              paste(from, collapse = " and ")))
    }
    out
}

# What a session in an untrusted directory reports: what it left alone
# and what changes that. NULL where the directory is trusted or holds
# nothing a session would load. `when` names the moment a new answer
# takes effect, which differs by surface.
project_trust_note <- function(cwd = getwd(),
                               when = "the next start of corteza") {
    if (project_trusted(cwd)) {
        return(NULL)
    }
    offers <- project_offers(cwd)
    if (!length(offers)) {
        return(NULL)
    }
    first <- sprintf("Not trusted: %s. corteza loaded nothing from it. Left alone:",
                     trust_dir(cwd))
    last <- sprintf("/trust yes trusts it and every directory inside it, from %s.",
                    when)
    c(first, paste0("  ", offers), last)
}

# `/trust`, the same on every surface. With no argument it says where
# the directory stands and what it holds. `/trust yes` trusts it: the
# first shows a person what the second agrees to. Returns the lines to
# show.
trust_command <- function(cwd = getwd(), arg = "",
                          when = "the next start of corteza") {
    arg <- tolower(trimws(paste(arg, collapse = " ")))
    if (!nzchar(arg)) {
        return(trust_status(cwd, when))
    }
    if (!identical(arg, "yes")) {
        return("usage: /trust shows where this directory stands, /trust yes trusts it")
    }
    if (project_trusted(cwd)) {
        return(trust_status(cwd, when))
    }
    dir <- tryCatch(project_trust_add(cwd),
                    error = function(e) conditionMessage(e))
    if (!project_trusted(cwd)) {
        return(paste("Not trusted:", dir))
    }
    sprintf(c("Trusted: %s, and every directory inside it.",
              "Its files load from %s.",
              "Saved in %s. Remove the line there to undo it."),
            c(dir, when, trust_file()))
}

# Where a directory stands, in lines for a person.
trust_status <- function(cwd = getwd(), when = "the next start of corteza") {
    root <- project_trust_root(cwd)
    if (is.null(root)) {
        note <- project_trust_note(cwd, when)
        if (is.null(note)) {
            if (is.character(cwd) && length(cwd) == 1L && !is.na(cwd)) {
                where <- trust_dir(cwd)
            } else {
                where <- "(no directory)"
            }
            lines <- c("Not trusted: %s. It holds nothing corteza would load.",
                       "/trust yes trusts it and every directory inside it, from %s.")
            note <- sprintf(lines, c(where, when))
        }
        return(note)
    }
    dir <- trust_dir(cwd)
    if (identical(dir, root)) {
        first <- sprintf("Trusted: %s", dir)
    } else {
        first <- sprintf("Trusted: %s, inside %s", dir, root)
    }
    kept <- "Trusted directories are listed in %s and under trusted_projects in %s."
    c(first, sprintf(kept, trust_file(), corteza_config_path("config.json")))
}

# What `/status` says about trust. A session loads from its directory
# when it starts and not after, so there are 2 facts: how this session
# started (`started`, kept on the session) and how the next one would
# (`now`).
trust_status_word <- function(started, now) {
    if (isTRUE(started)) {
        if (isTRUE(now)) {
            "yes"
        } else {
            "yes for this session, no from the next"
        }
    } else if (isTRUE(now)) {
        "no for this session, yes from the next"
    } else {
        "no (/trust)"
    }
}

# What a session that started untrusted did not load, for `/status`.
# Empty for a session that started trusted, and for a directory that
# holds nothing a session would load.
trust_left_alone <- function(session, cwd = session$cwd) {
    if (isTRUE(session$project_trusted)) {
        return(character())
    }
    offers <- project_offers(cwd)
    if (!length(offers)) {
        return(character())
    }
    c("Not loaded from this directory:", paste0("  ", offers))
}

# Before a session is set up: where the directory is not trusted and
# holds something a session would load, say so, and where a person is
# there to answer, ask once. `ask` takes a prompt and returns what was
# typed; NULL means nobody is there. Returns whether the directory is
# trusted afterwards.
trust_at_start <- function(cwd = getwd(), ask = NULL) {
    if (project_trusted(cwd)) {
        return(invisible(TRUE))
    }
    offers <- project_offers(cwd)
    if (!length(offers)) {
        return(invisible(FALSE))
    }
    cat(sprintf("Not trusted: %s. corteza loads nothing from it. It holds:\n",
                trust_dir(cwd)))
    cat(paste0("  ", offers), sep = "\n")
    if (is.function(ask)) {
        answer <- ask("Trust it and every directory inside it? [y/N] ")
        answer <- tolower(trimws(paste(answer, collapse = " ")))
        if (answer %in% c("y", "yes")) {
            project_trust_add(cwd)
            cat(sprintf("Trusted. Saved in %s.\n\n", trust_file()))
            return(invisible(TRUE))
        }
    }
    cat(sprintf("Left as it is. /trust yes trusts it, from %s.\n\n",
                .repl_trust_when()))
    invisible(FALSE)
}
