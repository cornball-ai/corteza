#' Install corteza CLI
#'
#' Install the \code{corteza} command-line tool to a directory in your
#' PATH. On Unix (Linux, macOS) installs the Rscript shebang binary.
#' On Windows installs a \code{.cmd} wrapper alongside the script so
#' \code{corteza} works from cmd.exe / PowerShell.
#'
#' @param path Directory to install to. Default is \code{~/bin} on
#'   Unix, \code{tools::R_user_dir("corteza", "data")/bin} on Windows.
#' @param force Overwrite existing installation.
#'
#' @details
#' Requires:
#' \itemize{
#'   \item \code{r} (littler) for fast R script execution (Unix only —
#'     Windows uses \code{Rscript}).
#'   \item The \code{llm.api} package for LLM connectivity
#'   \item The \code{corteza} package itself
#' }
#'
#' After installation, run \code{corteza} from any terminal (you may
#' need to add the install directory to PATH; the function prints the
#' PATH hint if it isn't already there).
#'
#' @return The installed script path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' install_cli()
#' install_cli("/usr/local/bin")
#' }
install_cli <- function(path = NULL, force = FALSE) {
    is_win <- .Platform$OS.type == "windows"
    if (is.null(path)) {
        path <- if (is_win) {
            file.path(corteza_data_dir(), "bin")
        } else {
            "~/bin"
        }
    }
    path <- path.expand(path)

    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
        message("Created directory: ", path)
    }

    src <- system.file("bin", "corteza", package = "corteza")
    if (!nzchar(src)) {
        stop("CLI script not found in package (development install?)",
             call. = FALSE)
    }

    dest <- file.path(path, "corteza")
    if (file.exists(dest) && !force) {
        stop("corteza already exists at ", dest,
             ". Use force = TRUE to overwrite.", call. = FALSE)
    }

    file.copy(src, dest, overwrite = TRUE)

    if (is_win) {
        # Windows cannot exec an Rscript shebang. Write a .cmd wrapper
        # that invokes Rscript with the script as argument.
        cmd_path <- file.path(path, "corteza.cmd")
        writeLines(c(
                     "@echo off",
                     sprintf("Rscript \"%s\" %%*", dest)
            ), cmd_path)
        message("Installed corteza to: ", dest)
        message("Installed Windows wrapper to: ", cmd_path)
    } else {
        Sys.chmod(dest, mode = "0755")
        message("Installed corteza to: ", dest)
    }

    path_dirs <- strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1L]]
    if (!path %in% path_dirs) {
        message("\nNote: ", path, " is not in your PATH.")
        if (is_win) {
            message("Add it via System Properties -> Environment Variables,")
            message("or temporarily in PowerShell: $env:Path += \";",
                    path, "\"")
        } else {
            message("Add this to your shell config:")
            message('  export PATH="', path, ':$PATH"')
        }
    }

    invisible(dest)
}

#' Uninstall corteza CLI
#'
#' Remove the \code{corteza} command-line tool.
#'
#' @param path Directory where corteza is installed. Default matches
#'   \code{install_cli()}: \code{~/bin} on Unix,
#'   \code{tools::R_user_dir("corteza", "data")/bin} on Windows.
#'
#' @return TRUE if removed, FALSE if not found, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' uninstall_cli()
#' }
uninstall_cli <- function(path = NULL) {
    is_win <- .Platform$OS.type == "windows"
    if (is.null(path)) {
        path <- if (is_win) {
            file.path(corteza_data_dir(), "bin")
        } else {
            "~/bin"
        }
    }
    path <- path.expand(path)

    removed <- FALSE
    dest <- file.path(path, "corteza")
    if (file.exists(dest)) {
        file.remove(dest)
        message("Removed: ", dest)
        removed <- TRUE
    }
    if (is_win) {
        cmd_path <- file.path(path, "corteza.cmd")
        if (file.exists(cmd_path)) {
            file.remove(cmd_path)
            message("Removed: ", cmd_path)
            removed <- TRUE
        }
    }
    if (!removed) {
        message("corteza not found at: ", path)
    }
    invisible(removed)
}

