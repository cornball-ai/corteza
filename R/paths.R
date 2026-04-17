# Standard user-writable directories for corteza.
#
# All user-home writes route through these helpers so they land in the
# OS-appropriate location per tools::R_user_dir(). On Linux that follows
# the XDG spec (~/.config/R/corteza, ~/.local/share/R/corteza,
# ~/.cache/R/corteza); on macOS and Windows the OS's native convention
# is used. This is what CRAN requires — packages must not write to
# hardcoded paths like "~/.pkgname/" under any operating system.
#
# None of these helpers create their directory. Call sites call
# dir.create() explicitly when they actually need to write, so package
# load / install / test / examples don't silently create directories as
# a side effect.

corteza_config_dir <- function() {
    tools::R_user_dir("corteza", "config")
}

corteza_data_dir <- function() {
    tools::R_user_dir("corteza", "data")
}

corteza_cache_dir <- function() {
    tools::R_user_dir("corteza", "cache")
}

corteza_config_path <- function(file) {
    file.path(corteza_config_dir(), file)
}

corteza_data_path <- function(...) {
    file.path(corteza_data_dir(), ...)
}

