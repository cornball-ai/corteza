# Where remote control keeps its records: CORTEZA_REMOTE_DIR, else a
# folder in corteza's data folder. An empty value counts as unset. Read
# as a folder, "" put every path at the root of the file system
# ("/bots/<owner>.json"), and the package's own checks got there:
# tinytest leaves a variable that a test file set, and was unset before,
# as "" for the files after it.

local({
    old <- Sys.getenv("CORTEZA_REMOTE_DIR", unset = NA)
    on.exit(if (is.na(old)) {
        Sys.unsetenv("CORTEZA_REMOTE_DIR")
    } else {
        Sys.setenv(CORTEZA_REMOTE_DIR = old)
    }, add = TRUE)
    default <- file.path(tools::R_user_dir("corteza", "data"), "remote-control")

    Sys.unsetenv("CORTEZA_REMOTE_DIR")
    expect_equal(corteza:::.remote_root(), default)

    # Set and empty. R on Windows unsets a variable set to "", so there
    # this is the case above.
    Sys.setenv(CORTEZA_REMOTE_DIR = "")
    expect_equal(corteza:::.remote_root(), default)
    expect_true(startsWith(corteza:::.remote_bot_file("@owner:example"), default))

    here <- file.path(tempdir(), "remote-here")
    Sys.setenv(CORTEZA_REMOTE_DIR = here)
    expect_equal(corteza:::.remote_root(), here)
    expect_true(startsWith(corteza:::.remote_bot_file("@owner:example"), here))
})
