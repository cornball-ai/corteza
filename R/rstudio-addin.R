# RStudio addin: route Ctrl+Enter from an .R or .sh script to the
# right corteza::chat() prefix (/r for R, ! for shell) when chat()
# is active. When chat() is not running, behaves like RStudio's
# default "execute line / selection" -- sends the line straight to
# the console.
#
# Setup: bind Ctrl+Enter to "Execute in corteza::chat()" in
#   Tools -> Modify Keyboard Shortcuts -> Addins.

#' Prefix to prepend when sending a line from a script editor to
#' the console. Empty string when no prefix should be added.
#'
#' Pure logic factored out for testability; the RStudio addin
#' wrapper handles the rstudioapi calls.
#' @param ext File extension (lowercase, without dot). Pass
#'   `""` for untitled buffers.
#' @param in_chat Logical. TRUE if `chat()` is currently active
#'   (i.e. `getOption("corteza.chat_active")` is set).
#' @return Character scalar; `""` for no prefix, `"/r "` for R
#'   files, `"! "` for shell files.
#' @noRd
.corteza_prefix_for <- function(ext, in_chat) {
    if (!isTRUE(in_chat)) {
        return("")
    }
    ext <- tolower(as.character(ext))
    if (identical(ext, "r")) {
        return("/r ")
    }
    if (identical(ext, "sh") || identical(ext, "bash")) {
        return("! ")
    }
    ""
}

#' Shared implementation. The two exported addins differ only in
#' whether they advance the cursor after sending -- matching
#' RStudio's pre-assigned Ctrl+Enter (advance) vs Alt+Enter
#' (retain) keybindings.
#' @noRd
.corteza_execute_in_chat <- function(advance_cursor) {
    if (!requireNamespace("rstudioapi", quietly = TRUE) ||
        !rstudioapi::isAvailable()) {
        message("corteza_execute_in_chat() requires RStudio.")
        return(invisible())
    }
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(),
                    error = function(e) NULL)
    if (is.null(ctx)) {
        message("No active editor.")
        return(invisible())
    }

    # Selection text takes priority. Empty selection (cursor only)
    # falls back to the current line, matching RStudio's default
    # Ctrl+Enter behavior.
    sel <- ctx$selection[[1L]]
    had_selection <- nzchar(sel$text)
    if (had_selection) {
        code <- sel$text
        line_num <- sel$range$end[1L]
    } else {
        line_num <- sel$range$start[1L]
        if (line_num < 1L || line_num > length(ctx$contents)) {
            return(invisible())
        }
        code <- ctx$contents[line_num]
    }
    if (!nzchar(trimws(code))) {
        return(invisible())
    }

    in_chat <- isTRUE(getOption("corteza.chat_active", FALSE))
    ext <- tools::file_ext(ctx$path %||% "")
    prefix <- .corteza_prefix_for(ext, in_chat)

    rstudioapi::sendToConsole(paste0(prefix, code), execute = TRUE)

    if (isTRUE(advance_cursor) && line_num < length(ctx$contents)) {
        tryCatch(rstudioapi::setCursorPosition(
                rstudioapi::document_position(line_num + 1L, 1L)
            ),
                 error = function(e) NULL
        )
    }
    invisible()
}

#' Execute current line or selection in `corteza::chat()`
#'
#' RStudio addin. Reads the line or selection under the cursor in
#' the active source editor, prepends `/r` for `.R` files (or
#' `! ` for `.sh` / `.bash` files) when `corteza::chat()` is the
#' active console REPL, and sends the result to the console via
#' `rstudioapi::sendToConsole()`. After sending, the editor cursor
#' advances to the next line (mirroring RStudio's pre-assigned
#' Ctrl+Enter / Cmd+Return behavior).
#'
#' When `chat()` is not running, no prefix is added -- the addin
#' is a superset of RStudio's default "execute line" behavior, so
#' you can bind it to Ctrl+Enter without losing normal R script
#' execution.
#'
#' **Setup:** bind Ctrl+Enter to "Execute in corteza::chat()"
#' under RStudio's Tools -> Modify Keyboard Shortcuts. Choose
#' "Addins" in the dropdown to find the binding.
#'
#' @return Invisible NULL. Side effect: sends a line to the
#'   console.
#' @keywords internal
#' @export
corteza_execute_in_chat <- function() {
    .corteza_execute_in_chat(advance_cursor = TRUE)
}

#' Execute current line or selection in `corteza::chat()` (retain cursor)
#'
#' Same routing logic as [corteza_execute_in_chat()] but the
#' editor cursor stays in place after sending, mirroring RStudio's
#' pre-assigned Alt+Enter / Option+Return behavior.
#'
#' **Setup:** bind Alt+Enter to "Execute in corteza::chat()
#' (retain cursor)" under RStudio's Tools -> Modify Keyboard
#' Shortcuts.
#'
#' @return Invisible NULL.
#' @keywords internal
#' @export
corteza_execute_in_chat_retain <- function() {
    .corteza_execute_in_chat(advance_cursor = FALSE)
}

