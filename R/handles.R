# Handle-based large-result storage.
#
# A printed data.frame can blow thousands of tokens through the LLM's
# context on a single `run_r` call. Instead of returning the printed
# value, we stash the real object in a process-local store and hand the
# LLM back a `summary + handle` pair. Later tool calls can reference
# the handle by its name (a short `.h_NNN` symbol) and the store
# substitutes the real object at eval time.
#
# Handles live for the life of the R process. The store is per-process:
# a subagent's `callr::r_session` child has its own, starting empty.

#' Process-local handle store.
#' @noRd
.handle_store <- new.env(parent = emptyenv())

#' Registry of handle names this package has copied into the user's
#' globalenv. Used by `handle_eval_env()` to remove stale handle
#' bindings (handles that were in globalenv on a previous call but
#' have since been cleared or rebound in the handle store).
#' @noRd
.handle_managed <- new.env(parent = emptyenv())

#' Mint the next handle id for the current session.
#' @noRd
.next_handle_id <- function() {
    existing <- ls(.handle_store, all.names = TRUE)
    n <- length(existing) + 1L
    repeat {
        id <- sprintf(".h_%03d", n)
        if (!exists(id, envir = .handle_store, inherits = FALSE)) {
            return(id)
        }
        n <- n + 1L
    }
}

#' Is a value large enough to stash as a handle?
#'
#' Scalars (length-1 atomics) and NULL pass through. Data frames and
#' matrices are always handled. Anything over 10 KB is handled.
#' @noRd
.is_large_result <- function(x) {
    if (is.null(x)) {
        return(FALSE)
    }
    if (is.atomic(x) && length(x) == 1L) {
        return(FALSE)
    }
    if (is.data.frame(x)) {
        return(TRUE)
    }
    if (is.matrix(x)) {
        return(TRUE)
    }
    if (is.list(x) && length(x) > 10L) {
        return(TRUE)
    }
    if (is.atomic(x) && length(x) > 50L) {
        return(TRUE)
    }
    tryCatch(as.numeric(utils::object.size(x)) > 10000L,
             error = function(e) FALSE)
}

#' Human-readable summary of a stashed value (single-paragraph str()).
#' @noRd
.default_summary <- function(x) {
    out <- tryCatch(
                    utils::capture.output(utils::str(x, max.level = 1L, list.len = 10L)),
                    error = function(e) paste("Error summarising:", conditionMessage(e))
    )
    paste(out, collapse = "\n")
}

#' Stash a value, return `{summary, handle}` for the LLM.
#'
#' @param value The R object to retain.
#' @param summary_fn A function that turns the value into a short
#'   description string. Defaults to [.default_summary()].
#' @return A list with `summary` (character) and `handle` (character).
#' @noRd
with_handle <- function(value, summary_fn = .default_summary) {
    id <- .next_handle_id()
    assign(id, value, envir = .handle_store)
    list(summary = summary_fn(value), handle = id)
}

#' Look up a previously stashed value by handle id.
#'
#' @param handle A handle id, e.g. `.h_001`.
#' @return The stashed value, or NULL if the handle is unknown.
#' @noRd
get_handle <- function(handle) {
    if (!is.character(handle) || length(handle) != 1L) {
        return(NULL)
    }
    if (!exists(handle, envir = .handle_store, inherits = FALSE)) {
        return(NULL)
    }
    get(handle, envir = .handle_store)
}

#' List all active handle ids.
#' @noRd
list_handles <- function() {
    ls(.handle_store, all.names = TRUE)
}

#' Drop all handles from the process-local store (for tests). Also
#' removes any `.h_NNN` bindings the package previously copied into
#' globalenv so a subsequent `tool_run_r()` doesn't see stale
#' handle symbols.
#' @noRd
clear_handles <- function() {
    rm(list = ls(.handle_store, all.names = TRUE), envir = .handle_store)
    ge <- globalenv()
    for (h in ls(.handle_managed, all.names = TRUE)) {
        if (exists(h, envir = ge, inherits = FALSE)) {
            rm(list = h, envir = ge)
        }
    }
    rm(list = ls(.handle_managed, all.names = TRUE), envir = .handle_managed)
    invisible(NULL)
}

#' Return an environment suitable for evaluating user code such that
#' stashed handles are visible as regular R names.
#'
#' The returned env inherits from `parent` (usually `globalenv()`), and
#' every handle in the store is copied in as a top-level binding. This
#' keeps handle lookup scoped to the `run_r` call and avoids leaking
#' handle symbols into the user's globalenv.
#' @noRd
handle_eval_env <- function(parent = globalenv()) {
    # Earlier versions (PR #36) returned a CHILD env of globalenv,
    # which sandboxed handle symbols nicely but silently broke
    # `<-` persistence across tool_run_r() calls -- assignments
    # landed in the child env, not in globalenv, so they
    # disappeared as soon as the call returned. The behavior
    # contradicted the tool's docstring ("Execute R code in the
    # session's global environment") and required users to discover
    # they had to use `<<-` to make anything stick.
    #
    # Restored to: copy handles INTO globalenv (under their hidden
    # `.h_NNN` names, which `ls()` doesn't show by default) and
    # return globalenv. eval(envir = globalenv()) makes `<-` write
    # to the right place. The `parent` argument is now ignored;
    # kept for API parity.
    # R CMD check NOTEs the literal `envir = globalenv()` pattern
    # because most packages shouldn't write to the user's globalenv.
    # For corteza this is intentional: handles need to be visible
    # to the user's eval'd code, and the user's eval'd code runs
    # in globalenv so `<-` persists across run_r() calls (per the
    # tool's docstring). Tried `pos = 1L` to skirt the NOTE but it
    # diverges from globalenv() when there's a sandbox env on the
    # search path (e.g. under tinytest::run_test_file), so back to
    # the explicit form and we live with the NOTE.
    #
    # Codex caught a staleness bug here (2026-05-20): the original
    # version skipped reassignment when the handle name already
    # existed in globalenv. Reusing a handle id (.h_001 rebound in
    # the store to a different value) left the old globalenv copy
    # in place, so tool_run_r('.h_001') returned the previous
    # snapshot. Fix: assign unconditionally so the globalenv copy
    # always reflects the current store, and remove globalenv
    # bindings the package previously created that are no longer
    # in the store.
    ge <- globalenv()
    current <- list_handles()
    previously_managed <- ls(.handle_managed, all.names = TRUE)
    stale <- setdiff(previously_managed, current)
    for (h in stale) {
        # Only remove if the binding still exists; user code might
        # have rm()'d it already, in which case rm() would error.
        if (exists(h, envir = ge, inherits = FALSE)) {
            rm(list = h, envir = ge)
        }
        rm(list = h, envir = .handle_managed)
    }
    for (h in current) {
        assign(h, get(h, envir = .handle_store), envir = ge)
        assign(h, TRUE, envir = .handle_managed)
    }
    ge
}

#' Read / inspect a stashed handle.
#'
#' The LLM's only window onto large stashed objects. Supports a few
#' common ops: `str` (structure), `head` (first six rows / elements),
#' `summary` (R's summary()), `print` (full print of the object).
#'
#' @param handle (character) Handle id, e.g. `.h_001`.
#' @param op (character; one of: str, head, summary, print) Inspection
#'   operation.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_read_handle <- function(handle, op = "str") {
    value <- get_handle(handle)
    if (is.null(value) &&
        !exists(handle, envir = .handle_store, inherits = FALSE)) {
        return(err(sprintf("Unknown handle: %s", handle)))
    }
    text <- tryCatch(switch(op,
                            str = utils::capture.output(utils::str(value)),
                            head = utils::capture.output(utils::head(value)),
                            summary = utils::capture.output(summary(value)),
                            print = utils::capture.output(print(value)),
                            return(err(sprintf("Unknown op: %s", op)))),
                     error = function(e) paste("Error:", conditionMessage(e)))
    ok(paste(text, collapse = "\n"))
}

