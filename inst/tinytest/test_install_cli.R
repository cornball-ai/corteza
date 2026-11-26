# install_cli() and uninstall_cli(): where the command goes, what is
# written, and that the default leaves the home directory alone. Up to
# 0.8.0 the Unix default was ~/bin, made if missing.

src <- system.file("bin", "corteza", package = "corteza")
expect_true(nzchar(src))

same_dir <- function(a, b) {
    identical(normalizePath(a, winslash = "/", mustWork = FALSE),
              normalizePath(b, winslash = "/", mustWork = FALSE))
}

# Run `expr`, returning its value beside the messages it sent.
with_messages <- function(expr) {
    msgs <- character()
    value <- withCallingHandlers(expr, message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    list(value = value, msgs = msgs)
}

# --- A directory the caller names ---
local({
    root <- tempfile("cli-")
    bin <- file.path(root, "deep", "bin")
    on.exit(unlink(root, recursive = TRUE))

    got <- with_messages(corteza::install_cli(bin))
    dest <- got$value
    expect_true(same_dir(dest, file.path(bin, "corteza")))
    expect_identical(readLines(dest), readLines(src))
    expect_true(any(grepl("Created directory", got$msgs, fixed = TRUE)))
    expect_true(any(grepl("not in your PATH", got$msgs, fixed = TRUE)))
    # Naming the directory is the choice, so nothing proposes another.
    expect_false(any(grepl("earlier install", got$msgs, fixed = TRUE)))
    expect_false(any(grepl("Or name a directory", got$msgs, fixed = TRUE)))
    if (.Platform$OS.type == "windows") {
        expect_true(file.exists(file.path(bin, "corteza.cmd")))
    } else {
        expect_identical(file.mode(dest), as.octmode("755") & !Sys.umask())
        expect_false(file.exists(file.path(bin, "corteza.cmd")))
    }

    # A second install needs force, and says so before it writes.
    writeLines("mine", dest)
    expect_error(corteza::install_cli(bin), "force = TRUE", fixed = TRUE)
    expect_identical(readLines(dest), "mine")
    again <- with_messages(corteza::install_cli(bin, force = TRUE))
    expect_identical(readLines(dest), readLines(src))
    expect_false(any(grepl("Created directory", again$msgs, fixed = TRUE)))

    # Removal, and a second removal that finds nothing.
    gone <- with_messages(corteza::uninstall_cli(bin))
    expect_true(gone$value)
    expect_false(file.exists(dest))
    expect_false(file.exists(file.path(bin, "corteza.cmd")))
    none <- with_messages(corteza::uninstall_cli(bin))
    expect_false(none$value)
    expect_true(any(grepl("not found", none$msgs, fixed = TRUE)))
})

# --- A directory already in PATH draws no hint ---
local({
    bin <- tempfile("cli-path-")
    dir.create(bin)
    old_path <- Sys.getenv("PATH")
    Sys.setenv(PATH = paste(path.expand(bin), old_path,
                            sep = .Platform$path.sep))
    on.exit({
        Sys.setenv(PATH = old_path)
        unlink(bin, recursive = TRUE)
    })
    got <- with_messages(corteza::install_cli(bin))
    expect_false(any(grepl("not in your PATH", got$msgs, fixed = TRUE)))
})

# --- A directory that cannot be made is an error, not a false "Installed" ---
local({
    # No directory can be made below a file, on any system.
    blocker <- tempfile("cli-file-")
    writeLines("x", blocker)
    on.exit(unlink(blocker))
    got <- tryCatch(with_messages(corteza::install_cli(file.path(blocker,
                                                                 "bin"))),
                    error = function(e) conditionMessage(e))
    expect_true(is.character(got))
    expect_true(grepl("Could not create directory", got, fixed = TRUE))
    expect_identical(readLines(blocker), "x")
})

# --- The Windows form, written on any system ---
local({
    bin <- tempfile("cli-win-")
    dir.create(bin)
    on.exit(unlink(bin, recursive = TRUE))
    got <- corteza:::cli_write(src, bin, windows = TRUE)
    expect_identical(names(got), c("script", "wrapper"))
    expect_identical(basename(got[["wrapper"]]), "corteza.cmd")
    expect_identical(readLines(got[["script"]]), readLines(src))
    expect_identical(readLines(got[["wrapper"]]),
                     c("@echo off",
                       sprintf("Rscript \"%s\" %%*", got[["script"]])))

    # A folder of its own: the Windows form keeps the mode of the installed
    # script, which a read-only library leaves without write permission.
    unix_bin <- tempfile("cli-unix-")
    dir.create(unix_bin)
    on.exit(unlink(unix_bin, recursive = TRUE), add = TRUE)
    unix <- corteza:::cli_write(src, unix_bin, windows = FALSE)
    expect_identical(names(unix), "script")
})

# --- The default: R's user data directory, and nothing in the home ---
# A fresh process, because R resolves "~" once and keeps the answer.
# Last in the file: where the home directory cannot be redirected, the
# rest of the file is skipped.
local({
    home <- tempfile("cli-home-")
    data <- tempfile("cli-data-")
    dir.create(home)
    on.exit(unlink(c(home, data), recursive = TRUE))
    probe <- function() {
        msgs <- character()
        keep <- function(expr) {
            withCallingHandlers(expr, message = function(m) {
                msgs <<- c(msgs, conditionMessage(m))
                invokeRestart("muffleMessage")
            })
        }
        dest <- keep(corteza::install_cli())
        installed <- file.exists(dest)
        install_msgs <- msgs
        msgs <- character()
        removed <- keep(corteza::uninstall_cli())
        list(home = path.expand("~"), dest = dest, installed = installed,
             under = file.path(tools::R_user_dir("corteza", "data"), "bin"),
             install_msgs = install_msgs, removed = removed,
             uninstall_msgs = msgs, left = file.exists(dest))
    }
    env <- c(callr::rcmd_safe_env(), HOME = home, R_USER = home,
             R_USER_DATA_DIR = data)
    got <- callr::r(probe, env = env)
    if (!same_dir(got$home, home)) {
        exit_file("cannot redirect the home directory on this platform")
    }
    expect_true(got$installed)
    expect_true(same_dir(dirname(got$dest), got$under))
    expect_true(startsWith(normalizePath(got$under, winslash = "/",
                                         mustWork = FALSE),
                           normalizePath(data, winslash = "/",
                                         mustWork = FALSE)))
    expect_identical(list.files(home, all.files = TRUE, no.. = TRUE),
                     character())
    expect_true(any(grepl("not in your PATH", got$install_msgs, fixed = TRUE)))
    expect_false(any(grepl("earlier install", got$install_msgs,
                           fixed = TRUE)))
    expect_true(got$removed)
    expect_false(got$left)

    # Windows never had ~/bin as its default, so the rest is Unix's.
    if (.Platform$OS.type != "windows") {
        expect_true(any(grepl("Or name a directory", got$install_msgs,
                              fixed = TRUE)))

        # A copy that 0.8.0 or earlier left in ~/bin is reported and
        # never touched: installing at the default does not update it,
        # and removing at the default does not remove it.
        old <- file.path(home, "bin", "corteza")
        dir.create(dirname(old))
        writeLines("old", old)
        again <- callr::r(probe, env = env)
        expect_true(again$installed)
        expect_true(same_dir(dirname(again$dest), again$under))
        expect_true(any(grepl("earlier install", again$install_msgs,
                              fixed = TRUE)))
        expect_true(any(grepl("install_cli(\"~/bin\", force = TRUE)",
                              again$install_msgs, fixed = TRUE)))
        # One command for ~/bin is enough: the hint that would fail
        # there without `force` is left out.
        expect_false(any(grepl("Or name a directory", again$install_msgs,
                               fixed = TRUE)))
        expect_true(again$removed)
        expect_true(any(grepl("uninstall_cli(\"~/bin\")",
                              again$uninstall_msgs, fixed = TRUE)))
        expect_identical(readLines(old), "old")
        expect_identical(list.files(home, all.files = TRUE, no.. = TRUE),
                         "bin")
        expect_identical(list.files(file.path(home, "bin"), all.files = TRUE,
                                    no.. = TRUE), "corteza")
    }
})
