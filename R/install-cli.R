#' Install corteza CLI
#'
#' Install the \code{corteza} command-line tool. On Unix (Linux, macOS)
#' installs the Rscript shebang binary. On Windows installs a
#' \code{.cmd} wrapper alongside the script so \code{corteza} works from
#' cmd.exe / PowerShell.
#'
#' @param path Directory to install to. Default is
#'   \code{tools::R_user_dir("corteza", "data")/bin}, created when
#'   missing. To put the command in a directory already in your PATH,
#'   name it, as in \code{install_cli("~/bin")}. Up to 0.8.0 the default
#'   on Unix was \code{~/bin}.
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
#' After installation, run \code{corteza} from any terminal. The
#' default directory is not in PATH, and the function prints the line
#' that adds it.
#'
#' An install made by 0.8.0 or earlier stays in \code{~/bin} until you
#' replace it (\code{install_cli("~/bin", force = TRUE)}) or remove it
#' (\code{uninstall_cli("~/bin")}). A shell that finds that copy first
#' keeps running it, so both functions say when one is there.
#'
#' @return The installed script path, invisibly.
#' @export
#'
#' @examples
#' # Into a directory of your choosing, here a throwaway one. With no
#' # argument the command goes to R's user data directory.
#' bin <- file.path(tempdir(), "corteza-bin")
#' install_cli(bin)
#' uninstall_cli(bin)
#' unlink(bin, recursive = TRUE)
install_cli <- function(path = NULL, force = FALSE) {
    is_win <- .Platform$OS.type == "windows"
    default <- is.null(path)
    if (default) {
        path <- cli_default_dir()
    }
    path <- path.expand(path)

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

    # Nothing is made until the call is known to go ahead.
    if (!dir.exists(path)) {
        if (!dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
            stop("Could not create directory: ", path, call. = FALSE)
        }
        message("Created directory: ", path)
    }

    written <- cli_write(src, path, windows = is_win)
    message("Installed corteza to: ", dest)
    if (is_win) {
        message("Installed Windows wrapper to: ", written[["wrapper"]])
    }

    old <- NULL
    if (default) {
        old <- cli_earlier_install()
    }
    if (!is.null(old)) {
        message("\nAn earlier install is at ", old, ". A shell that finds ",
                "it first runs that copy.")
        message("Replace it with install_cli(\"~/bin\", force = TRUE), or ",
                "remove it with uninstall_cli(\"~/bin\").")
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
            if (default && is.null(old)) {
                message("Or name a directory that is already there, as in ",
                        "install_cli(\"~/bin\").")
            }
        }
    }

    invisible(dest)
}

# Where the command goes when the caller names no directory: under R's
# user data directory on every system. Up to 0.8.0 the Unix default was
# ~/bin, made if missing: a directory in the home that the caller had
# not named.
cli_default_dir <- function() {
    file.path(corteza_data_dir(), "bin")
}

# Copy the script into `path`, which exists. Unix gets an executable
# file. Windows cannot exec an Rscript shebang, so it also gets a .cmd
# wrapper that hands the script to Rscript. `windows` is an argument so
# that either form can be written, and tested, on any system.
cli_write <- function(src, path, windows = .Platform$OS.type == "windows") {
    dest <- file.path(path, "corteza")
    if (!file.copy(src, dest, overwrite = TRUE)) {
        stop("Could not write ", dest, call. = FALSE)
    }
    if (!windows) {
        Sys.chmod(dest, mode = "0755")
        return(c(script = dest))
    }
    wrapper <- file.path(path, "corteza.cmd")
    writeLines(c("@echo off", sprintf("Rscript \"%s\" %%*", dest)), wrapper)
    c(script = dest, wrapper = wrapper)
}

# The script 0.8.0 and earlier installed on Unix, when it is still
# there. Looked at and never written. A copy in ~/bin keeps working, and
# a shell that finds it first runs it in place of a newer one elsewhere,
# so a person who installs or removes at the default is told about it.
cli_earlier_install <- function() {
    if (.Platform$OS.type == "windows") {
        return(NULL)
    }
    old <- path.expand(file.path("~", "bin", "corteza"))
    if (!file.exists(old)) {
        return(NULL)
    }
    old
}

#' Uninstall corteza CLI
#'
#' Remove the \code{corteza} command-line tool.
#'
#' @param path Directory where corteza is installed. Default matches
#'   \code{install_cli()}:
#'   \code{tools::R_user_dir("corteza", "data")/bin}. An install made by
#'   0.8.0 or earlier on Unix is in \code{~/bin}; remove it with
#'   \code{uninstall_cli("~/bin")}.
#'
#' @return TRUE if removed, FALSE if not found, invisibly.
#' @export
#'
#' @examples
#' bin <- file.path(tempdir(), "corteza-bin")
#' install_cli(bin)
#' uninstall_cli(bin)
#' unlink(bin, recursive = TRUE)
uninstall_cli <- function(path = NULL) {
    is_win <- .Platform$OS.type == "windows"
    default <- is.null(path)
    if (default) {
        path <- cli_default_dir()
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
    old <- NULL
    if (default) {
        old <- cli_earlier_install()
    }
    if (!is.null(old)) {
        message("An earlier install is at ", old, ". Remove it with ",
                "uninstall_cli(\"~/bin\").")
    }
    invisible(removed)
}
