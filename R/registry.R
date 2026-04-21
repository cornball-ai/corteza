# Shared tool registry for corteza.
#
# Single source of truth consumed by:
#   - chat()  (in-process calls)
#   - serve() (external MCP surface)
#   - CLI     (private worker, currently MCP-over-stdio, will swap to
#              callr::r_session in Phase 2)
#
# Entries are created via skill_spec() in R/skill.R and registered here.
# Both chat() and serve() read from .skill_registry directly; the CLI
# will read from here once the transport moves off MCP.

#' Tool registry (package-level environment).
#' @noRd
.skill_registry <- new.env(parent = emptyenv())

#' Register a skill in the global registry.
#' @param skill Skill spec from skill_spec().
#' @return Invisible skill name.
#' @noRd
register_skill <- function(skill) {
    if (is.null(skill$name)) {
        stop("Skill must have a name")
    }
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
