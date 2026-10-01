library(tinytest)

# The read-only tools as a boundary. A session confined with
# allowed_paths (the hall monitor, the job reviewer) has nothing else
# between it and the rest of the machine, so each tool is driven through
# its real handler with arguments chosen to get out: shell
# metacharacters, option-looking refs, another repository, a glob that
# climbs, and symlinks.

if (!nzchar(Sys.which("git"))) {
    exit_file("git not available")
}
corteza::ensure_skills()

root <- normalizePath({
    d <- tempfile("confine")
    dir.create(d)
    d
})
inside <- file.path(root, "checkout")
outside <- file.path(root, "outside")
dir.create(file.path(inside, "a"), recursive = TRUE)
dir.create(file.path(inside, "b"))
dir.create(outside)

git <- function(dir, ...) {
    processx::run("git", c("-C", dir, "-c", "user.name=t", "-c",
                           "user.email=t@example.com", ...),
                  error_on_status = FALSE)$stdout
}
for (repo in c(inside, outside)) {
    git(repo, "init", "-q")
}
writeLines("alpha <- 1", file.path(inside, "a", "x.R"))
writeLines("beta <- 1", file.path(inside, "b", "y.R"))
git(inside, "add", ".")
git(inside, "commit", "-q", "-m", "inside first")
writeLines("SECRET <- 'outside'", file.path(outside, "secret.R"))
git(outside, "add", ".")
git(outside, "commit", "-q", "-m", "outside secret commit")
writeLines("alpha <- 2", file.path(inside, "a", "x.R"))
writeLines("beta <- 2", file.path(inside, "b", "y.R"))

call <- function(name, ...) {
    res <- corteza:::call_skill(name, list(...), ctx = list())
    list(error = isTRUE(res$isError), text = res$content[[1L]]$text)
}
confined <- function(to, code) {
    old <- options(corteza.allowed_paths = to)
    on.exit(options(old))
    force(code)
}

# --- A ref cannot run a command, confined or not ---
marker <- file.path(root, "marker")
for (ref in c(sprintf("HEAD; touch %s", marker),
              sprintf("HEAD && touch %s", marker),
              sprintf("$(touch %s)", marker),
              sprintf("`touch %s`", marker),
              sprintf("HEAD | tee %s", marker))) {
    call("git_diff", ref = ref, path = inside)
    call("git_log", ref = ref, path = inside)
    expect_false(file.exists(marker), info = ref)
}
# A path cannot either.
call("git_status", path = sprintf("%s; touch %s", inside, marker))
expect_false(file.exists(marker))

# --- A ref cannot be an option ---
out_file <- file.path(root, "written-by-diff")
r <- call("git_diff", ref = paste0("--output=", out_file), path = inside)
expect_true(r$error)
expect_true(grepl("not an option", r$text))
expect_false(file.exists(out_file))
expect_true(call("git_diff", ref = "--no-index", path = inside,
                 file_path = file.path(outside, "secret.R"))$error)
expect_true(call("git_log", ref = "--all", path = inside)$error)

# --- Ordinary use still works ---
d <- call("git_diff", path = inside)
expect_false(d$error)
expect_true(grepl("+alpha <- 2", d$text, fixed = TRUE))
expect_true(grepl("+beta <- 2", d$text, fixed = TRUE))
expect_true(grepl("inside first", call("git_log", path = inside)$text))
expect_true(grepl("x.R", call("git_status", path = inside)$text))
one <- call("git_diff", path = inside, file_path = "a/x.R")
expect_true(grepl("alpha", one$text))
expect_false(grepl("beta", one$text))
# A range is a ref.
expect_false(call("git_diff", ref = "HEAD..HEAD", path = inside)$error)
expect_true(call("git_status", path = root)$error)  # not a repository

confined(inside, {
    # --- Another repository is out of reach ---
    for (tool in c("git_log", "git_status", "git_diff")) {
        r <- call(tool, path = outside)
        expect_true(r$error, info = tool)
        expect_true(grepl("outside allowed paths", r$text), info = tool)
        expect_false(grepl("secret", r$text), info = tool)
    }
    # --- So is a file filter that climbs out ---
    r <- call("git_diff", path = inside, file_path = "../outside/secret.R")
    expect_true(r$error)
    expect_true(call("git_diff", path = inside,
                     file_path = file.path(outside, "secret.R"))$error)
    # The repository itself still works.
    expect_true(grepl("+alpha <- 2", call("git_diff", path = inside)$text,
                      fixed = TRUE))

    # --- grep_files: a glob that climbs out reads nothing there ---
    g <- call("grep_files", pattern = "SECRET", path = inside,
              file_pattern = "../outside/*.R")
    expect_false(grepl("SECRET", g$text))
    expect_false(grepl("outside", g$text))
    g2 <- call("grep_files", pattern = ".", path = inside,
               file_pattern = "../*/*.R")
    expect_false(grepl("SECRET", g2$text))
    # The directory argument is still checked as before.
    expect_true(call("grep_files", pattern = "SECRET", path = outside)$error)
    # Inside still works.
    expect_true(grepl("alpha", call("grep_files", pattern = "alpha",
                                    path = inside,
                                    file_pattern = "a/*.R")$text))

    if (.Platform$OS.type != "windows") {
        # --- Symlinks are judged by where they lead ---
        file.symlink(file.path(outside, "secret.R"),
                     file.path(inside, "a", "link.R"))
        file.symlink(outside, file.path(inside, "linkdir"))
        g3 <- call("grep_files", pattern = "SECRET", path = inside,
                   file_pattern = "a/*.R")
        expect_false(grepl("SECRET", g3$text))
        g4 <- call("grep_files", pattern = "SECRET", path = inside,
                   file_pattern = "linkdir/*.R")
        expect_false(grepl("SECRET", g4$text))
        expect_true(call("read_file",
                         path = file.path(inside, "a", "link.R"))$error)
        l <- call("list_files", path = inside, recursive = TRUE)
        expect_false(grepl("secret.R", l$text))
        expect_true(grepl("a/x.R", l$text, fixed = TRUE))
        expect_true(call("git_log",
                         path = file.path(inside, "linkdir"))$error)
        unlink(file.path(inside, "a", "link.R"))
        unlink(file.path(inside, "linkdir"))
    }
})

# --- Confined to a subdirectory of a repository ---
# The repository's root is outside what the session may read, so git is
# limited to the directory rather than showing the whole repository.
git(inside, "commit", "-q", "-am", "touch both dirs")
writeLines("only b", file.path(inside, "b", "z.R"))
git(inside, "add", ".")
git(inside, "commit", "-q", "-m", "b only commit")
writeLines("alpha <- 3", file.path(inside, "a", "x.R"))
writeLines("beta <- 3", file.path(inside, "b", "y.R"))
sub <- file.path(inside, "a")
confined(sub, {
    d <- call("git_diff", path = sub)
    expect_false(d$error)
    expect_true(grepl("alpha <- 3", d$text))
    expect_false(grepl("beta", d$text))
    s <- call("git_status", path = sub)
    expect_true(grepl("x.R", s$text))
    expect_false(grepl("y.R", s$text))
    lg <- call("git_log", path = sub)
    expect_true(grepl("touch both dirs", lg$text))
    expect_false(grepl("b only commit", lg$text))
    # The repository root itself is not readable from here.
    expect_true(call("git_diff", path = inside)$error)
    expect_true(call("git_diff", path = sub, file_path = "../b/y.R")$error)
})
# Unconfined, the same directory shows the whole repository, as before.
expect_true(grepl("beta", call("git_diff", path = sub)$text))

unlink(root, recursive = TRUE)
