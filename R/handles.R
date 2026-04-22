# Handle-based large-result storage.
#
# A printed data.frame can blow thousands of tokens through the LLM's
# context on a single `run_r` call. Instead of returning the printed
# value, we stash the real object in a worker-local store and hand the
# LLM back a `summary + handle` pair. Later tool calls can reference
# the handle by its name (a short `.h_NNN` symbol) — the worker
# substitutes the real object at eval time.
#
# Handles live for the life of the worker session. A fresh
# `callr::r_session` starts with an empty store.

#' Worker-local handle store.
#' @noRd
.handle_store <- new.env(parent = emptyenv())

#' Mint the next handle id for the current session.
#' @noRd
.next_handle_id <- function() {
    existing <- ls(.handle_store, all.names = TRUE)
    n <- length(existing) + 1L
    repeat {
        id <- sprintf(".h_%03d", n)
        if (!exists(id, envir = .handle_store, inherits = FALSE)) return(id)
        n <- n + 1L
    }
}

#' Is a value large enough to stash as a handle?
#'
#' Scalars (length-1 atomics) and NULL pass through. Data frames and
#' matrices are always handled. Anything over 10 KB is handled.
#' @noRd
.is_large_result <- function(x) {
    if (is.null(x)) return(FALSE)
    if (is.atomic(x) && length(x) == 1L) return(FALSE)
    if (is.data.frame(x)) return(TRUE)
    if (is.matrix(x)) return(TRUE)
    if (is.list(x) && length(x) > 10L) return(TRUE)
    if (is.atomic(x) && length(x) > 50L) return(TRUE)
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
    if (!is.character(handle) || length(handle) != 1L) return(NULL)
    if (!exists(handle, envir = .handle_store, inherits = FALSE)) return(NULL)
    get(handle, envir = .handle_store)
}

#' List all active handle ids.
#' @noRd
list_handles <- function() {
    ls(.handle_store, all.names = TRUE)
}

#' Drop all handles from the worker-local store (for tests).
#' @noRd
clear_handles <- function() {
    rm(list = ls(.handle_store, all.names = TRUE), envir = .handle_store)
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
    env <- new.env(parent = parent)
    for (h in list_handles()) {
        assign(h, get(h, envir = .handle_store), envir = env)
    }
    env
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
