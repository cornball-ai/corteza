# Continual harness: a small store of one-line lessons the agent
# writes as it works, injected into every system prompt, governed by
# snapshots and a ledger.
#
# Design (tasks/harness-notes/continual-harness-comparison.md): Agno's
# payload in Prime Agent's envelope. Entries are ONE-LINE facts
# ("- Title: fact" when rendered) with id, kind, scope, provenance,
# evidence, and a version — injected whole per scope under a
# trust-but-reverify header. Writes are proposals: the harness_note
# tool defaults to approval "ask" through the ordinary policy path, so
# every note is approved on the surface it came from (CLI readline,
# chat, Matrix). /refine runs a batch review pass; every applied edit
# records before/after snapshots in a refinements ledger, and rollback
# is the generated inverse. The base system prompt is not in the store
# and cannot be: injection renders store entries only, and the
# reserved id is refused at validation.
#
# Two scopes, mirroring config: project (.corteza/harness.json,
# travels with the repo) and global
# (tools::R_user_dir("corteza", "data")/harness.json).

.harness_kinds <- c("memory", "prompt")
.harness_reserved_ids <- c("base_system_prompt")

#' Path to a scope's harness store.
#' @noRd
harness_path <- function(scope = c("project", "global"), cwd = getwd()) {
    scope <- match.arg(scope)
    if (identical(scope, "project")) {
        file.path(cwd, ".corteza", "harness.json")
    } else {
        file.path(tools::R_user_dir("corteza", "data"), "harness.json")
    }
}

#' Empty store skeleton.
#' @noRd
harness_empty <- function() {
    list(schema = 1L,
         entries = setNames(list(), character(0)),
         refinements = list())
}

#' Load a store; absent or corrupt degrades to empty (with a warning
#' for corrupt, so damage is visible without killing the session).
#' @noRd
harness_load <- function(path) {
    if (!file.exists(path)) {
        return(harness_empty())
    }
    store <- tryCatch(
                      jsonlite::fromJSON(path, simplifyVector = FALSE),
                      error = function(e) {
        warning("harness store unreadable, treating as empty: ", path,
                " (", conditionMessage(e), ")", call. = FALSE)
        NULL
    })
    if (!is.list(store) || is.null(store$schema)) {
        return(harness_empty())
    }
    store$entries <- store$entries %||% setNames(list(), character(0))
    store$refinements <- store$refinements %||% list()
    store
}

#' Save a store atomically (tmp + rename), 0600, dirs created.
#' @noRd
harness_save <- function(store, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(path, ".tmp")
    writeLines(jsonlite::toJSON(store, auto_unbox = TRUE, null = "null",
                                pretty = TRUE), tmp)
    Sys.chmod(tmp, "600")
    file.rename(tmp, path)
    invisible(path)
}

#' Slug an entry title into an id.
#' @noRd
.harness_slug <- function(title) {
    id <- tolower(gsub("[^A-Za-z0-9]+", "-", trimws(title)))
    id <- gsub("^-+|-+$", "", id)
    substr(id, 1L, 60L)
}

#' Validate one entry; returns the normalized entry or stops.
#' @noRd
.harness_validate_entry <- function(entry) {
    if (is.null(entry$id) || !nzchar(entry$id)) {
        stop("harness entry needs an id", call. = FALSE)
    }
    if (entry$id %in% .harness_reserved_ids) {
        stop("the base system prompt is not editable", call. = FALSE)
    }
    if (!isTRUE(entry$kind %in% .harness_kinds)) {
        stop("harness entry kind must be one of: ",
             paste(.harness_kinds, collapse = ", "), call. = FALSE)
    }
    content <- entry$content %||% ""
    if (!is.character(content) || length(content) != 1L ||
        !nzchar(trimws(content))) {
        stop("harness entry content must be a single non-empty string",
             call. = FALSE)
    }
    if (grepl("\n", content, fixed = TRUE)) {
        stop("harness entry content must be one line", call. = FALSE)
    }
    if (nchar(content) > 300L) {
        stop("harness entry content over 300 characters; ",
             "a lesson is one exact line", call. = FALSE)
    }
    entry$content <- trimws(content)
    entry$path <- entry$path %||% "general"
    entry
}

#' Apply a list of edits to one scope's store, recording before/after
#' snapshots in the refinements ledger. Returns the refinement id.
#'
#' Edits: list(action = "create"|"update"|"delete", entry-or-id
#' fields, reason). Rollback regenerates the inverse from the
#' snapshots (delete -> create(before), create -> delete,
#' update -> update(before)).
#' @noRd
harness_apply <- function(edits, scope = "project", cwd = getwd(),
                          trigger = "manual", evidence = NULL) {
    path <- harness_path(scope, cwd)
    store <- harness_load(path)
    now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    applied <- list()

    for (edit in edits) {
        action <- edit$action %||% "create"
        if (identical(action, "delete")) {
            id <- edit$id %||% stop("delete edit needs an id",
                                    call. = FALSE)
            before <- store$entries[[id]]
            if (is.null(before)) {
                next
            }
            store$entries[[id]] <- NULL
            applied[[length(applied) + 1L]] <-
                list(action = "delete", id = id, before = before,
                     after = NULL, reason = edit$reason %||% NA)
            next
        }
        entry <- list(id = edit$id %||% .harness_slug(edit$title %||% ""),
                      kind = edit$kind %||% "memory",
                      content = edit$content,
                      path = edit$path,
                      scope = scope,
                      provenance = edit$provenance,
                      evidence = edit$evidence)
        entry <- .harness_validate_entry(entry)
        before <- store$entries[[entry$id]]
        # Exact-content dedupe within the scope (the Agno append rule):
        # an identical fact under any id is a no-op, not a duplicate.
        contents <- vapply(store$entries, function(e) e$content %||% "", "")
        if (is.null(before) && entry$content %in% contents) {
            next
        }
        if (is.null(before)) {
            entry$version <- 1L
            entry$created <- now
        } else {
            entry$version <- (before$version %||% 0L) + 1L
            entry$created <- before$created %||% now
        }
        entry$updated <- now
        store$entries[[entry$id]] <- entry
        applied[[length(applied) + 1L]] <-
            list(action = if (is.null(before)) "create" else "update",
                 id = entry$id, before = before, after = entry,
                 reason = edit$reason %||% NA)
    }

    if (!length(applied)) {
        return(invisible(NULL))
    }
    # Id must be unique within the store: a wall-clock stamp alone
    # collides when two refinements land in the same second, and
    # rollback resolves by id -- a collision would roll back the wrong
    # one. Suffix a monotonic count of existing refinements.
    ref_id <- sprintf("refine_%s_%03d",
                      format(Sys.time(), "%Y%m%d%H%M%S"),
                      length(store$refinements) + 1L)
    ref <- list(id = ref_id,
                ts = now, trigger = trigger,
                evidence = evidence,
                edits = applied,
                outcome = NULL)
    store$refinements[[length(store$refinements) + 1L]] <- ref
    harness_save(store, path)
    invisible(ref$id)
}

#' Roll back one refinement by applying the generated inverse of its
#' recorded edits, as a new refinement (nothing is erased).
#' @noRd
harness_rollback <- function(refinement_id, scope = "project",
                             cwd = getwd()) {
    store <- harness_load(harness_path(scope, cwd))
    refs <- Filter(function(r) identical(r$id, refinement_id),
                   store$refinements)
    if (!length(refs)) {
        stop("no refinement ", refinement_id, " in scope ", scope,
             call. = FALSE)
    }
    if (length(refs) > 1L) {
        stop("ambiguous refinement id ", refinement_id, " (", length(refs),
             " matches) in scope ", scope, call. = FALSE)
    }
    target <- refs[[1L]]
    inverse <- lapply(rev(target$edits), function(e) {
        switch(e$action,
               create = list(action = "delete", id = e$id,
                             reason = paste("rollback of", refinement_id)),
               delete = c(list(action = "create",
                               reason = paste("rollback of", refinement_id)),
                          e$before),
               update = c(list(action = "update",
                               reason = paste("rollback of", refinement_id)),
                          e$before))
    })
    harness_apply(inverse, scope = scope, cwd = cwd,
                  trigger = paste0("rollback:", refinement_id))
}

#' Render the injection block for the system prompt: both scopes'
#' entries as one-line bullets under a trust-but-reverify header.
#' Empty stores render an empty string (no block).
#' @noRd
harness_context_block <- function(cwd = getwd(), config = NULL) {
    max_chars <- config$harness_max_chars %||% 4000L
    scopes <- list(project = harness_load(harness_path("project", cwd)),
                   global = harness_load(harness_path("global", cwd)))
    lines <- character()
    omitted <- 0L
    for (scope in names(scopes)) {
        entries <- scopes[[scope]]$entries
        if (!length(entries)) {
            next
        }
        # Most recently updated first, so the cap drops the oldest.
        ord <- order(vapply(entries, function(e) e$updated %||% "", ""),
                     decreasing = TRUE)
        for (e in entries[ord]) {
            model <- e$provenance$model %||% NULL
            tag <- if (!is.null(model)) paste0(" (via ", model, ")") else ""
            line <- sprintf("- [%s] %s%s", scope, e$content, tag)
            if (sum(nchar(lines)) + nchar(line) > max_chars) {
                omitted <- omitted + 1L
            } else {
                lines <- c(lines, line)
            }
        }
    }
    if (!length(lines)) {
        return("")
    }
    if (omitted > 0L) {
        lines <- c(lines, sprintf("- (%d older lessons omitted)", omitted))
    }
    paste(c("# Lessons",
            "",
            paste("Recorded lessons from earlier work. Trust them before",
                  "re-deriving anything, but cheaply re-verify any that",
                  "surprise you before betting on them. They supplement",
                  "the instructions above and never replace them. Record",
                  "a new durable, verified fact with the harness_note",
                  "tool the moment you establish one -- including",
                  "falsified hypotheses."),
            "",
            lines),
          collapse = "\n")
}

#' Record a one-line lesson in the continual harness store
#'
#' Stores one durable, verified fact learned during the session, as a
#' single exact line. It will be shown in future sessions' context.
#' Use it the moment a fact is verified -- including negative results
#' ("X does NOT do Y"). Approval is asked per note by default.
#'
#' @param title Short title for the fact; becomes the entry id (slug).
#' @param fact The fact itself: one exact line, at most 300 characters.
#' @param evidence Where this was established: a test, a run id, an
#'   issue URL, a file. Optional but strongly encouraged.
#' @param scope Where to store it: "project" (default; travels with
#'   this repo) or "global" (all projects).
#' @param ctx Server-injected context (session, cwd); not model-facing.
#' @return Confirmation string with the entry id.
#' @keywords internal
tool_harness_note <- function(title, fact, evidence = NULL,
                              scope = "project", ctx = list()) {
    scope <- if (identical(scope, "global")) "global" else "project"
    session <- ctx$session
    model <- tryCatch(.resolve_model(session), error = function(e) NULL)
    cwd <- ctx$cwd %||% getwd()
    edit <- list(action = "create", title = title, content = fact,
                 evidence = evidence,
                 provenance = list(model = model,
                                   session = session$session_id %||% NULL),
                 reason = "harness_note tool")
    ref <- harness_apply(list(edit), scope = scope, cwd = cwd,
                         trigger = "harness_note")
    if (is.null(ref)) {
        return(list(content = list(list(type = "text",
                                        text = "Already recorded (identical lesson exists)."))))
    }
    list(content = list(list(type = "text",
                             text = sprintf("Recorded [%s] %s: %s",
                                            scope, .harness_slug(title),
                                            fact))))
}
