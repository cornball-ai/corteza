# RStudio addin: route Ctrl+Enter from an .R or .sh script to the
# right corteza::chat() prefix (/r for R, ! for shell) when chat()
# is active. When chat() is not running, behaves like RStudio's
# default "execute line / selection" -- sends the line straight to
# the console.
#
# Setup: bind Ctrl+Enter to "Execute in corteza::chat()" in
#   Tools -> Modify Keyboard Shortcuts -> Addins.

#' Compute where and how to send one line of source-editor code.
#' Pure logic factored out for testability; the RStudio addin
#' wrapper handles the rstudioapi side.
#'
#' Routing matrix:
#'
#' | ext       | in_chat | target   | text                |
#' |-----------|---------|----------|---------------------|
#' | r / R     | TRUE    | console  | `/r <code>`         |
#' | r / R     | FALSE   | console  | `<code>` (R eval)   |
#' | sh / bash | TRUE    | console  | `! <code>`          |
#' | sh / bash | FALSE   | terminal | `<code>`            |
#' | other     | either  | console  | `<code>`            |
#'
#' Shell scripts outside chat() route to RStudio's Terminal pane
#' (where they actually belong) instead of being wrapped in
#' `system()` and sent to the R console. Inside chat(), the
#' `! <code>` form goes through chat()'s slash-prefix dispatch
#' so the LLM gets the staged output.
#'
#' @param code The raw line / selection text.
#' @param ext File extension (lowercase or mixed; we tolower).
#' @param in_chat Logical. TRUE if `chat()` is the active REPL.
#' @return A list with two elements:
#'   * `target`: `"console"` or `"terminal"`
#'   * `text`: the string to send to that target
#' @noRd
.corteza_route <- function(code, ext, in_chat) {
    ext <- tolower(as.character(ext))
    # Unsaved buffers come back with empty $path so ext is "" --
    # treat them as R, matching RStudio's built-in assumption for
    # untitled documents (codex 2026-05-20: previously the addin
    # routed unsaved buffers as "other", meaning Ctrl+Enter from
    # an unsaved R script inside chat() sent the line as a raw
    # LLM prompt instead of /r ...).
    if (!nzchar(ext)) {
        ext <- "r"
    }
    console <- function(text) list(target = "console", text = text)
    terminal <- function(text) list(target = "terminal", text = text)
    if (isTRUE(in_chat)) {
        if (identical(ext, "r")) {
            return(console(paste0("/r ", code)))
        }
        if (ext %in% c("sh", "bash")) {
            return(console(paste0("! ", code)))
        }
        return(console(code))
    }
    # chat() not active.
    if (ext %in% c("sh", "bash")) {
        return(terminal(code))
    }
    console(code)
}

#' Send `code` to RStudio's Terminal pane. Reuses the currently
#' visible terminal when there is one; otherwise grabs the first
#' terminal in the list, or creates a new one. Returns invisibly.
#' @noRd
.corteza_send_to_terminal <- function(code) {
    if (!requireNamespace("rstudioapi", quietly = TRUE) ||
        !rstudioapi::isAvailable()) {
        message("Sending to RStudio Terminal requires RStudio.")
        return(invisible())
    }
    target_id <- tryCatch(rstudioapi::terminalVisible(),
                          error = function(e) NULL)
    if (is.null(target_id)) {
        terms <- tryCatch(rstudioapi::terminalList(),
                          error = function(e) character(0L))
        target_id <- if (length(terms) > 0L) {
            terms[[1L]]
        } else {
            tryCatch(rstudioapi::terminalCreate(show = TRUE),
                     error = function(e) NULL)
        }
    }
    if (is.null(target_id)) {
        message("Could not open an RStudio Terminal; line not sent.")
        return(invisible())
    }
    # Trailing newline so the terminal executes the line.
    rstudioapi::terminalSend(target_id, paste0(code, "\n"))
    invisible()
}

#' Resolve the R-statement line range that contains `line_num`.
#'
#' Mirrors RStudio's built-in Ctrl+Enter behavior: when the cursor
#' sits inside a multi-line top-level expression (e.g. an `lm()`
#' call wrapped across three lines), the whole expression executes,
#' not just the cursor's line. Implementation parses the buffer
#' with `keep.source = TRUE` and walks the `srcref` attribute for
#' the expression covering `line_num`.
#'
#' Falls back to `c(line_num, line_num)` when:
#'   * the buffer can't be parsed (syntax error elsewhere);
#'   * the cursor is on a blank / comment line outside any
#'     expression.
#'
#' Only used for R buffers -- shell scripts route through the
#' terminal pane line-by-line.
#' @return Integer pair `c(start_line, end_line)`.
#' @noRd
.corteza_statement_range <- function(contents, line_num) {
    if (line_num < 1L || line_num > length(contents)) {
        return(c(line_num, line_num))
    }
    text <- paste(contents, collapse = "\n")
    parsed <- tryCatch(parse(text = text, keep.source = TRUE),
                       error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0L) {
        return(c(line_num, line_num))
    }
    srcrefs <- attr(parsed, "srcref")
    if (is.null(srcrefs)) {
        return(c(line_num, line_num))
    }
    for (sr in srcrefs) {
        l1 <- sr[1L]
        l2 <- sr[3L]
        if (l1 <= line_num && line_num <= l2) {
            return(c(l1, l2))
        }
    }
    c(line_num, line_num)
}

#' Find the next executable line in `contents` at or after `start`.
#' Skips blank lines and full-line comments (after stripping leading
#' whitespace), matching RStudio's built-in Ctrl+Enter behavior.
#' Returns `length(contents) + 1L` if no more code lines exist
#' below.
#' @noRd
.next_code_row <- function(contents, start) {
    n <- length(contents)
    if (start > n) {
        return(n + 1L)
    }
    for (i in seq.int(start, n)) {
        line <- trimws(contents[i])
        if (!nzchar(line)) {
            next
        }
        if (substr(line, 1L, 1L) == "#") {
            next
        }
        return(i)
    }
    n + 1L
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
    # falls back to the current statement, matching RStudio's
    # default Ctrl+Enter behavior of executing the whole multi-line
    # expression when the cursor sits inside one. R-only -- shell
    # / unknown extensions still route line-by-line.
    sel <- ctx$selection[[1L]]
    had_selection <- nzchar(sel$text)
    ext <- tolower(tools::file_ext(ctx$path %||% ""))
    # Unsaved buffers come back with empty $path. Match the
    # ext-fallback in .corteza_route() so unsaved-R-script behavior
    # stays consistent.
    if (!nzchar(ext)) {
        ext <- "r"
    }
    is_r <- identical(ext, "r")
    if (had_selection) {
        code <- sel$text
        line_num <- sel$range$end[1L]
    } else {
        line_num <- sel$range$start[1L]
        if (line_num < 1L || line_num > length(ctx$contents)) {
            return(invisible())
        }
        if (is_r) {
            range <- .corteza_statement_range(ctx$contents, line_num)
            code <- paste(ctx$contents[range[1L]:range[2L]], collapse = "\n")
            # Cursor advance lands after the statement's last line.
            line_num <- range[2L]
        } else {
            code <- ctx$contents[line_num]
        }
    }
    if (!nzchar(trimws(code))) {
        return(invisible())
    }

    in_chat <- isTRUE(getOption("corteza.chat_active", FALSE))
    route <- .corteza_route(code, ext, in_chat)

    if (identical(route$target, "terminal")) {
        .corteza_send_to_terminal(route$text)
    } else {
        # focus = FALSE keeps the cursor in the source editor
        # instead of dragging it to the console after each
        # Ctrl+Enter -- matches RStudio's built-in execute-line
        # behavior.
        rstudioapi::sendToConsole(route$text, execute = TRUE, focus = FALSE)
    }

    if (isTRUE(advance_cursor)) {
        # Skip past blank lines and comments when advancing, so
        # Ctrl+Enter lands on the next executable line -- same as
        # RStudio's built-in. Pass id = ctx$id so the cursor moves
        # in the *source* editor (sendToConsole left focus there
        # via focus = FALSE, but the active-document concept is
        # separate; explicit id is the safe path).
        next_row <- .next_code_row(ctx$contents, line_num + 1L)
        if (next_row <= length(ctx$contents)) {
            tryCatch(rstudioapi::setCursorPosition(
                    rstudioapi::document_position(next_row, 1L),
                    id = ctx$id
                ),
                     error = function(e) NULL
            )
        }
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

