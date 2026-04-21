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

#' Install Windows dependencies into Rtools
#'
#' Runs \code{pacman -Sy git} inside the Rtools MSYS2 environment so
#' corteza's git tools work without requiring a separate Git for Windows
#' install. Rtools itself must already be installed (Rtools45 for R
#' 4.5.x, Rtools44 for R 4.4.x).
#'
#' This is a convenience helper, not something \code{library(corteza)}
#' runs on your behalf. Packages should not mutate a user's toolchain
#' silently.
#'
#' @param rtools_home Path to Rtools root. Defaults to the
#'   \code{RTOOLS45_HOME} / \code{RTOOLS44_HOME} environment variable,
#'   then \code{C:/rtools45}, then \code{C:/rtools44}.
#' @return TRUE on success, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' install_windows_deps()
#' }
install_windows_deps <- function(rtools_home = NULL) {
    if (.Platform$OS.type != "windows") {
        stop("install_windows_deps() only applies to Windows.",
             call. = FALSE)
    }
    if (is.null(rtools_home) || !nzchar(rtools_home)) {
        rtools_home <- Sys.getenv("RTOOLS45_HOME",
                                  Sys.getenv("RTOOLS44_HOME", ""))
    }
    if (!nzchar(rtools_home)) {
        for (p in c("C:/rtools45", "C:/rtools44")) {
            if (dir.exists(p)) {
                rtools_home <- p
                break
            }
        }
    }
    if (!nzchar(rtools_home) || !dir.exists(rtools_home)) {
        stop("Rtools not found. Install Rtools45 (for R 4.5.x) from ",
             "https://cran.r-project.org/bin/windows/Rtools/ and retry.",
             call. = FALSE)
    }
    pacman <- file.path(rtools_home, "usr", "bin", "pacman.exe")
    if (!file.exists(pacman)) {
        stop("pacman not found at ", pacman, call. = FALSE)
    }
    message("Installing git via pacman in ", rtools_home, " ...")
    status <- system2(pacman, c("-Sy", "--noconfirm", "git"))
    if (status != 0L) {
        stop("pacman failed with exit status ", status, call. = FALSE)
    }
    message("git installed. Restart R (or any shell) so PATH refreshes.")
    invisible(TRUE)
}
