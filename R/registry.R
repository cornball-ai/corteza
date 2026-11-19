# Shared tool registry for corteza.
#
# Single source of truth consumed by:
#   - chat()  (in-process calls)
#   - CLI     (in-process calls; same shared run_repl_loop as chat())
#   - serve() (external MCP surface)
#
# Entries are created via skill_spec() in R/skill.R and registered here.
# All three read from .skill_registry directly.

#' Tool registry (package-level environment).
#' @noRd
.skill_registry <- new.env(parent = emptyenv())

# `builtin` is TRUE while register_builtin_skills() runs. A tool
# registered then is corteza's own, and resolve_paths() reads its
# arguments by name. Every string argument of any other tool counts as a
# path (R/policy.R).
.registry_state <- new.env(parent = emptyenv())

registering_builtin <- function() {
    isTRUE(.registry_state$builtin)
}

#' Register a skill in the global registry.
#' @param skill Skill spec from skill_spec().
#' @return Invisible skill name.
#' @noRd
register_skill <- function(skill) {
    if (is.null(skill$name)) {
        stop("Skill must have a name")
    }
    skill$class <- tool_class_check(skill$class, skill$name)
    skill$builtin <- registering_builtin()
    .skill_registry[[skill$name]] <- skill
    invisible(skill$name)
}

#' Get a skill from the registry.
#' @param name Skill name.
#' @return Skill spec or NULL if not found.
#' @noRd
get_skill <- function(name) {
    if (exists(name, envir = .skill_registry, inherits = FALSE)) {
        .skill_registry[[name]]
    } else {
        NULL
    }
}

#' List all registered skills.
#' @return Character vector of skill names.
#' @noRd
list_skills <- function() {
    ls(.skill_registry)
}

#' Clear all skills from the registry.
#' @return Invisible NULL.
#' @noRd
clear_skills <- function() {
    rm(list = ls(.skill_registry), envir = .skill_registry)
    invisible(NULL)
}
