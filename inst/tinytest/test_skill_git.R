# Test the git_clone() helper extracted from skill_install().
# Runs entirely offline by cloning a local repository; skips when git
# is unavailable.

if (Sys.which("git") == "") {
    exit_file("git not available")
}

# Success path: clone a freshly-initialised local repo. An empty repo
# (no commits) needs no user.name/user.email and still exercises the
# status == 0 / dest-created path.
src <- tempfile("git_clone_src_")
dir.create(src)
init <- processx::run("git", c("init", "-q", src), error_on_status = FALSE)
if (init$status != 0L) {
    exit_file("git init failed")
}

dest <- tempfile("git_clone_dest_")
expect_silent(corteza:::git_clone(src, dest))
expect_true(dir.exists(dest))

# Failure path: a nonexistent source must raise corteza's clear error
# (processx's default throw is suppressed via error_on_status = FALSE).
missing_src <- tempfile("git_clone_missing_")  # never created
bad_dest <- tempfile("git_clone_bad_")
expect_error(corteza:::git_clone(missing_src, bad_dest),
             "Failed to clone repository")

unlink(c(src, dest), recursive = TRUE)
