# Two terminals saving "Allow always for this project" at once each keep
# their answer: the save reads, changes and writes the shared file under
# one lock (R/approvals.R). test_trust.R covers the store in one process.

# Starts an R process. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("starts an R process; runs at home and in CI")
}

local({
    ns <- asNamespace("corteza")
    root <- tempfile("approvals-")
    dir.create(file.path(root, "a"), recursive = TRUE)
    dir.create(file.path(root, "b"))
    root <- normalizePath(root, winslash = "/")
    old <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA_character_)
    on.exit({
        if (is.na(old)) {
            Sys.unsetenv("R_USER_CONFIG_DIR")
        } else {
            Sys.setenv(R_USER_CONFIG_DIR = old)
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config"))
    a <- file.path(root, "a")
    b <- file.path(root, "b")

    expect_true(ns$approval_save(a, "bash"))
    store <- ns$approvals_file()
    # One terminal is in the middle of a save: it holds the lock and has
    # read the file, and has not written it yet.
    lock <- paste0(store, ".lock")
    dir.create(lock)
    seen <- ns$approvals_read()
    # Another terminal saves an answer for another directory meanwhile.
    # It inherits the config directory, and waits for the lock.
    other <- callr::r_bg(function(dir) corteza:::approval_save(dir, "run_r"),
                         list(dir = b))
    other$wait(2000)
    expect_true(other$is_alive())
    # The first writes what it read and its own answer, and lets go.
    seen[[ns$trust_dir(a)]] <- c("bash", "git_status")
    jsonlite::write_json(seen, store, pretty = TRUE)
    unlink(lock, recursive = TRUE)
    other$wait(20000)
    expect_true(isTRUE(other$get_result()))
    expect_equal(ns$approvals_saved(a), c("bash", "git_status"))
    expect_equal(ns$approvals_saved(b), "run_r")
})

# R expands "~" at process startup. Give the child its own home so even a
# broken build can only strand locks and pins inside this test's fixture.
local({
    house <- tempfile("lock-home-")
    dir.create(house)
    on.exit(unlink(house, recursive = TRUE), add = TRUE)
    result <- callr::r(function(house) {
        stopifnot(identical(normalizePath("~"), normalizePath(house)))
        # Brackets in the path must stay literal when the lock is removed.
        Sys.setenv(R_USER_CONFIG_DIR = "~/settings[1]", R_USER_DATA_DIR = "~/data",
                   R_USER_CACHE_DIR = "~/cache")
        Sys.unsetenv(c("CORTEZA_STATE_DIR", "CORTEZA_REMOTE_DIR"))
        ns <- asNamespace("corteza")
        roots <- c(ns$corteza_config_dir(), ns$corteza_data_dir(), ns$corteza_cache_dir())
        expected <- vapply(c("config", "data", "cache"), function(type) {
            path.expand(tools::R_user_dir("corteza", type))
        }, character(1), USE.NAMES = FALSE)
        defaults <- c(ns$bot_signal_dir(), ns$.remote_root())
        Sys.setenv(CORTEZA_STATE_DIR = "~/state", CORTEZA_REMOTE_DIR = "~/remote")
        overrides <- c(ns$bot_signal_dir(), ns$.remote_root())
        first <- ns$approval_save(house, "read_file", timeout = 0)
        second <- ns$approval_save(house, "grep_files", timeout = 0)
        store <- "~/payloads/writer"
        dir.create(store, recursive = TRUE)
        raw <- charToRaw("fixture")
        hash <- digest::digest(raw, "sha256", serialize = FALSE)
        path <- file.path(store, hash)
        writeBin(raw, path)
        ref <- list(store = store, hash = hash, status = "stored")
        corteza::audit_payload_pin(ref)
        pinned <- file.exists(paste0(path, ".pin"))
        corteza::audit_payload_pin(ref, FALSE)
        unpinned <- !file.exists(paste0(path, ".pin"))
        later <- Sys.time() + 31 * 86400
        preview <- corteza::audit_payload_expire(store, now = later)
        removed <- corteza::audit_payload_expire(store, now = later, dry_run = FALSE)
        list(saves = c(first, second), saved = ns$approvals_saved(house),
             lock = dir.exists(paste0(ns$approvals_file(), ".lock")),
             pins = c(pinned, unpinned), expired = identical(preview, hash) &&
                 identical(removed, hash) && !file.exists(path),
             roots = identical(roots, expected),
             defaults = identical(defaults, file.path(expected[2], c("state", "remote-control"))),
             overrides = identical(overrides, path.expand(c("~/state", "~/remote"))))
    }, list(house), env = c(HOME = house, R_USER = house), timeout = 20,
    user_profile = FALSE)
    expect_identical(result$saves, c(TRUE, TRUE))
    expect_equal(result$saved, c("grep_files", "read_file"))
    expect_false(result$lock)
    expect_identical(result$pins, c(TRUE, TRUE))
    expect_true(result$expired)
    expect_true(result$roots)
    expect_true(result$defaults)
    expect_true(result$overrides)
})
