# /refine: the continual harness's batch review pass, plus list /
# rollback / promote plumbing. Shared by chat() and the CLI through
# run_repl_loop, like every slash command. Interactive approval is
# per-edit readline; nothing applies without a yes.

.REFINE_SYSTEM <- paste(
    "You maintain a small store of one-line lessons for an AI agent.",
    "Given the current entries and a recent conversation transcript,",
    "propose at most 8 edits that make the store more useful for",
    "future sessions. Record only durable, verified facts backed by",
    "the transcript -- reject one-off noise, unsupported hypotheses,",
    "and transient tool output. Falsified hypotheses are worth",
    "recording. Each entry content is ONE exact line under 300",
    "characters. Never propose anything about the base system prompt.",
    "Respond with ONLY a JSON object, no code fences:",
    '{"summary": "...", "edits": [{"action": "create|update|delete",',
    '"kind": "memory|prompt", "id": "existing-id-for-update-or-delete",',
    '"title": "short title (create only)", "content": "the one line",',
    '"path": "grouping, default general", "evidence": "receipt if any",',
    '"reason": "why this edit"}]}',
    "An empty edits array is a valid answer.")

# Flatten session history into readable text for the refine pass.
# Handles both content shapes (string, and anthropic block lists);
# tool traffic is reduced to names so the pass sees decisions, not
# dumps. Returns the LAST max_chars.
.harness_history_text <- function(history, max_chars = 40000L) {
    piece <- function(msg) {
        role <- msg$role %||% "?"
        content <- msg$content
        if (is.character(content)) {
            return(paste0(role, ": ", paste(content, collapse = " ")))
        }
        if (is.list(content)) {
            parts <- vapply(content, function(b) {
                if (identical(b$type, "text")) {
                    b$text %||% ""
                } else if (identical(b$type, "tool_use")) {
                    paste0("[tool: ", b$name %||% "?", "]")
                } else if (identical(b$type, "tool_result")) {
                    "[tool result]"
                } else {
                    ""
                }
            }, "")
            return(paste0(role, ": ",
                          paste(parts[nzchar(parts)], collapse = " ")))
        }
        paste0(role, ":")
    }
    txt <- paste(vapply(history %||% list(), piece, ""), collapse = "\n")
    if (nchar(txt) > max_chars) {
        txt <- substr(txt, nchar(txt) - max_chars + 1L, nchar(txt))
    }
    txt
}

# Render current entries (both scopes) for /refine list and for the
# pass's own input.
.harness_overview <- function(cwd) {
    out <- character()
    for (scope in c("project", "global")) {
        store <- harness_load(harness_path(scope, cwd))
        for (e in store$entries) {
            out <- c(out, sprintf("[%s] %s (v%s, %s): %s", scope,
                                  e$id, e$version %||% 1L,
                                  e$kind %||% "memory", e$content))
        }
    }
    out
}

#' Run /refine and its subcommands. `args` is the tokenized tail of
#' the command line.
#' @noRd
run_refine <- function(ctx, args = character()) {
    cwd <- ctx$cwd %||% getwd()
    sub <- if (length(args)) args[[1L]] else ""

    if (identical(sub, "list")) {
        rows <- .harness_overview(cwd)
        if (!length(rows)) {
            cat("No lessons recorded yet.\n")
        } else {
            cat(rows, sep = "\n")
        }
        return(invisible(NULL))
    }

    if (identical(sub, "rollback")) {
        if (length(args) < 2L) {
            cat("usage: /refine rollback <refinement-id> [--global]\n")
            return(invisible(NULL))
        }
        scope <- if ("--global" %in% args) "global" else "project"
        ref <- tryCatch(harness_rollback(args[[2L]], scope = scope,
                                         cwd = cwd),
                        error = function(e) {
            cat(conditionMessage(e), "\n")
            NULL
        })
        if (!is.null(ref)) {
            cat("Rolled back as", ref, "\n")
        }
        return(invisible(NULL))
    }

    if (identical(sub, "promote")) {
        if (length(args) < 2L) {
            cat("usage: /refine promote <entry-id>\n")
            return(invisible(NULL))
        }
        store <- harness_load(harness_path("project", cwd))
        entry <- store$entries[[args[[2L]]]]
        if (is.null(entry)) {
            cat("No project entry", args[[2L]], "\n")
            return(invisible(NULL))
        }
        edit <- list(action = "create", title = entry$id,
                     content = entry$content, kind = entry$kind,
                     path = entry$path, provenance = entry$provenance,
                     evidence = paste0(entry$evidence %||% "",
                                       " (promoted from project)"),
                     reason = "promotion")
        ref <- harness_apply(list(edit), scope = "global", cwd = cwd,
                             trigger = "promote")
        cat(if (is.null(ref)) "Already in global store.\n" else
            paste("Promoted as", ref, "\n"))
        return(invisible(NULL))
    }

    # Default: the review pass. One completion, no tools.
    session <- ctx$session
    history <- session$history %||% list()
    if (length(history) < 2L) {
        cat("Nothing to refine (no conversation yet).\n")
        return(invisible(NULL))
    }
    overview <- .harness_overview(cwd)
    prompt <- paste(
        "Current store:",
        if (length(overview)) paste(overview, collapse = "\n") else "(empty)",
        "",
        "Recent transcript:",
        .harness_history_text(history),
        sep = "\n")
    cat("Reviewing the conversation for durable lessons...\n")
    resp <- tryCatch(
        llm.api::chat(prompt = prompt, system = .REFINE_SYSTEM,
                      model = .resolve_model(session),
                      provider = session$provider),
        error = function(e) {
            cat("Refine pass failed:", conditionMessage(e), "\n")
            NULL
        })
    if (is.null(resp)) {
        return(invisible(NULL))
    }
    raw <- gsub("^\\s*```(json)?|```\\s*$", "", trimws(resp$content %||% ""))
    plan <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.null(plan) || !length(plan$edits %||% list())) {
        cat("No edits proposed.\n")
        return(invisible(NULL))
    }
    if (nzchar(plan$summary %||% "")) {
        cat(plan$summary, "\n\n")
    }
    approved <- list()
    for (edit in plan$edits) {
        desc <- sprintf("%s %s: %s\n  reason: %s",
                        edit$action %||% "create",
                        edit$id %||% .harness_slug(edit$title %||% ""),
                        edit$content %||% "(delete)",
                        edit$reason %||% "-")
        cat(desc, "\n")
        ans <- tolower(trimws(readline("apply? [y/N] ")))
        if (ans %in% c("y", "yes")) {
            approved[[length(approved) + 1L]] <- edit
        }
    }
    if (!length(approved)) {
        cat("Nothing applied.\n")
        return(invisible(NULL))
    }
    ref <- tryCatch(harness_apply(approved, scope = "project", cwd = cwd,
                                  trigger = "refine"),
                    error = function(e) {
        cat("Apply failed:", conditionMessage(e), "\n")
        NULL
    })
    if (!is.null(ref)) {
        cat("Applied", length(approved), "edit(s) as", ref, "\n")
    }
    invisible(NULL)
}
