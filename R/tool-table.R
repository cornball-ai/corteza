# A session's tools.
#
# The registry (R/registry.R) belongs to the process: it holds corteza's
# own tools and any a host registered by hand. A session takes its tools
# once, when it starts, into a table of its own: those, the user's
# skills, the package functions its config lists, and the skills of its
# project where the user trusts the directory (R/trust.R). turn() offers
# the table and runs the handler the table holds, and the gate reads a
# call's class from the same entry. A tool registered after the session
# started, in the registry or in another session's table, changes
# neither what the session offers nor what it runs. A bot's rooms share
# one process, and each room has a table of its own.
#
# A name is taken once. A later source never replaces an earlier one's
# tool: built in first, then the host's, the user's, the packages' and
# the project's.
#
# Not a sandbox. A skill file's code runs in the process and can reach
# the registry by name; trust decides whether the file runs at all.
#
# A session made by new_session() alone has no table and reads the
# registry.

# An empty table.
tool_table_new <- function() {
    new.env(parent = emptyenv())
}

# Where a tool came from: "builtin", "host", "user", "package" or
# "project", and for a project's tool its directory.
tool_origin <- function(kind, dir = NULL) {
    out <- list(kind = kind)
    if (!is.null(dir)) {
        out$dir <- dir
    }
    out
}

# The source a refused tool came from, and the one that holds its name.
tool_origin_from <- function(origin) {
    switch(origin$kind %||% "", user = "your skills", package = "a package",
           project = sprintf("the project %s", origin$dir %||% "?"),
           host = "this R process", "corteza")
}

tool_origin_holder <- function(origin) {
    switch(origin$kind %||% "", builtin = "a built-in tool",
           host = "a tool registered in this R process",
           user = "one of your skills", package = "a package tool",
           project = "a skill of the same project", "another tool")
}

# An entry's origin as one word. A registry entry carries none: corteza's
# own tools are "builtin", the rest a host's.
tool_origin_kind <- function(entry) {
    kind <- entry$origin$kind
    if (is.null(kind)) {
        kind <- "host"
        if (isTRUE(entry$builtin)) {
            kind <- "builtin"
        }
    }
    kind
}

# Add `skill` to `table` under its name, with its origin. A name the
# table holds keeps its entry: the newcomer is refused, the person is
# told, and the refusal is logged. Returns TRUE when the tool was added.
tool_table_add <- function(table, skill, origin) {
    name <- skill$name
    if (!is.character(name) || length(name) != 1L || is.na(name) ||
        !nzchar(name)) {
        stop("Skill must have a name", call. = FALSE)
    }
    skill$class <- tool_class_check(skill$class, name)
    if (exists(name, envir = table, inherits = FALSE)) {
        held <- table[[name]]$origin
        message(sprintf("Skipping the tool %s from %s: %s has that name.",
                        name, tool_origin_from(origin),
                        tool_origin_holder(held)))
        log_event("tool_name_taken", tool = name, origin = origin$kind,
                  held_by = held$kind %||% "", level = "warn")
        return(invisible(FALSE))
    }
    skill$origin <- origin
    skill$builtin <- identical(origin$kind, "builtin")
    assign(name, skill, envir = table)
    invisible(TRUE)
}

# The entry for `name`: the table's where the session has one, else the
# registry's. NULL when neither holds it.
tool_entry <- function(name, table = NULL) {
    if (!is.character(name) || length(name) != 1L || is.na(name) ||
        !nzchar(name)) {
        return(NULL)
    }
    if (is.null(table)) {
        return(get_skill(name))
    }
    if (exists(name, envir = table, inherits = FALSE)) {
        table[[name]]
    } else {
        NULL
    }
}

# The names a session can call: its table's, or the registry's.
tool_names <- function(table = NULL) {
    if (is.null(table)) {
        list_skills()
    } else {
        ls(table)
    }
}

# Build the table for a session in `cwd`, whose config (load_config())
# lists its package tools. Every surface calls this when it starts a
# session: session_setup(), the terminal, a child process
# (subagent_turn_init()) and serve().
session_tool_table <- function(cwd, config) {
    ensure_skills()
    table <- tool_table_new()
    for (name in list_skills()) {
        entry <- get_skill(name)
        tool_table_add(table, entry, tool_origin(tool_origin_kind(entry)))
    }
    load_skills(corteza_data_path("skills"), table = table,
                origin = tool_origin("user"))
    load_skill_packages(config, table = table)
    load_project_skills(cwd, docs = FALSE, table = table)
    table
}
