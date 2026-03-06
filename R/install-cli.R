#' Install llamar CLI
#'
#' Install the `llamar` command-line tool to a directory in your PATH.
#'
#' @param path Directory to install to. Defaults to `~/bin`.
#' @param force Overwrite existing installation. Defaults to FALSE.
#'
#' @details
#' This copies the `llamar` script from the package to the specified directory.
#' The script requires:
#'
#' - `r` (littler) for fast R script execution
#' - The `llm.api` package for LLM connectivity
#' - The `llamaR` package itself
#'
#' After installation, you can run `llamar` from any terminal.
#'
#' @return The path to the installed script (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Install to ~/bin (default)
#' install_cli()
#'
#' # Install to custom location
#' install_cli("/usr/local/bin")
#' }
install_cli <- function(path = "~/bin", force = FALSE) {
    path <- path.expand(path)

    # Create directory if needed
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
        message("Created directory: ", path)
    }

    # Source and destination
    src <- system.file("bin", "llamar", package = "llamaR")
    if (!file.exists(src)) {
        stop("CLI script not found in package. This may be a development install.")
    }

    dest <- file.path(path, "llamar")

    # Check if exists
    if (file.exists(dest) && !force) {
        stop("llamar already exists at ", dest,
             ". Use force = TRUE to overwrite.")
    }

    # Copy
    file.copy(src, dest, overwrite = TRUE)

    # Make executable on Unix
    if (.Platform$OS.type != "windows") {
        Sys.chmod(dest, mode = "0755")
    }

    message("Installed llamar to: ", dest)

    # Check if in PATH
    path_dirs <- strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
    if (!path %in% path_dirs) {
        message("\nNote: ", path, " may not be in your PATH.")
        message("Add this to your shell config:")
        message('  export PATH="', path, ':$PATH"')
    }

    invisible(dest)
}

#' Uninstall llamar CLI
#'
#' Remove the `llamar` command-line tool.
#'
#' @param path Directory where llamar is installed. Defaults to `~/bin`.
#'
#' @return TRUE if removed, FALSE if not found (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' uninstall_cli()
#' }
uninstall_cli <- function(path = "~/bin") {
    path <- path.expand(path)
    dest <- file.path(path, "llamar")

    if (file.exists(dest)) {
        file.remove(dest)
        message("Removed: ", dest)
        invisible(TRUE)
    } else {
        message("llamar not found at: ", dest)
        invisible(FALSE)
    }
}

