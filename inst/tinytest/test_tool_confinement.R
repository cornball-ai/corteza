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
blob_y <- trimws(git(inside, "rev-parse", "HEAD:b/y.R"))
tree_b <- trimws(git(inside, "rev-parse", "HEAD:b"))
git(inside, "tag", "on-blob", blob_y)
git(inside, "tag", "on-tree", tree_b)
git(inside, "tag", "-a", "-m", "annotated", "note-on-blob", blob_y)
git(inside, "tag", "on-commit", "HEAD")
git(inside, "tag", "-a", "-m", "annotated", "note-on-commit", "HEAD~1")
not_commits <- c("on-blob", blob_y, substr(blob_y, 1L, 12L), "note-on-blob",
                 "on-tree", tree_b, "HEAD^{tree}",
                 paste0(blob_y, "..", blob_y), paste0("HEAD..", blob_y),
                 paste0("^", blob_y))
commits <- c("HEAD", "on-commit", "note-on-commit", "HEAD~1..HEAD",
             "HEAD~2...HEAD", "HEAD^!", "HEAD@{0}",
             trimws(git(inside, "rev-parse", "HEAD")))
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
    # Pathspec magic is not a way out either. The file filter is checked
    # as a file name, so git has to read it as one: ":(top)b/y.R" is a
    # file of that name in this directory, not b/y.R at the root.
    for (fp in c(":(top)b/y.R", ":/b/y.R", ":(top,glob)b/*.R", ":/",
                 ":(exclude)x.R", ":!x.R")) {
        r <- call("git_diff", path = sub, file_path = fp)
        expect_false(grepl("beta", r$text), info = fp)
        expect_false(grepl("y.R", r$text, fixed = TRUE), info = fp)
    }
    # Nor is a ref that names a file: <rev>:<path> is that file's
    # content, from anywhere in the repository.
    for (ref in c("HEAD:b/y.R", "HEAD:b", ":b/y.R", "HEAD~1:b/y.R..HEAD:b/y.R")) {
        r <- call("git_diff", ref = ref, path = sub, file_path = "x.R")
        expect_true(r$error, info = ref)
        expect_false(grepl("beta", r$text), info = ref)
        expect_true(call("git_log", ref = ref, path = sub)$error, info = ref)
    }
    # The same file reached without a colon: by its object id, or by a
    # tag put on it. What a ref resolves to has to be a commit; how it
    # is spelled says nothing.
    for (ref in not_commits) {
        r <- call("git_diff", ref = ref, path = sub, file_path = "x.R")
        expect_true(r$error, info = ref)
        expect_true(grepl("must name commits", r$text), info = ref)
        expect_false(grepl("beta", r$text), info = ref)
        expect_true(call("git_log", ref = ref, path = sub)$error, info = ref)
    }
    # Refs that do resolve to commits still work, scoped to the directory.
    for (ref in commits) {
        r <- call("git_diff", ref = ref, path = sub)
        expect_false(r$error, info = ref)
        expect_false(grepl("beta", r$text), info = ref)
        expect_false(call("git_log", ref = ref, path = sub)$error, info = ref)
    }
    expect_true(grepl("alpha", call("git_diff", ref = "HEAD~2..HEAD",
                                    path = sub)$text))
})
# Refused whether confined or not: one rule, not two.
for (ref in not_commits) {
    expect_true(call("git_diff", ref = ref, path = inside)$error, info = ref)
}
unknown <- call("git_diff", ref = "no-such-branch", path = inside)
expect_true(unknown$error)
expect_true(grepl("not a revision", unknown$text))
# Unconfined, the same directory shows the whole repository, as before.
expect_true(grepl("beta", call("git_diff", path = sub)$text))
# A literal file filter still filters.
only <- call("git_diff", path = inside, file_path = "b/y.R")
expect_true(grepl("beta", only$text))
expect_false(grepl("alpha", only$text))

# --- Git does not run programs the repository configures ---
# .git/config can be edited by anything with write access to the
# checkout, and git starts what it names during an ordinary status,
# diff, or add. Each program below leaves a marker when it runs.
if (.Platform$OS.type != "windows") {
    ran_dir <- file.path(root, "ran")
    dir.create(ran_dir)
    program <- function(name, body = "cat") {
        p <- file.path(root, paste0(name, ".sh"))
        writeLines(c("#!/bin/sh",
                     sprintf("touch '%s'", file.path(ran_dir, name)), body), p)
        Sys.chmod(p, "755")
        p
    }
    ran <- function() {
        out <- sort(list.files(ran_dir))
        unlink(list.files(ran_dir, full.names = TRUE))
        out
    }
    touch_x <- function(value) {
        writeLines(sprintf("alpha <- %s", value), file.path(inside, "a", "x.R"))
    }
    git(inside, "config", "core.fsmonitor", program("fsmonitor", "exit 1"))
    git(inside, "config", "filter.mark.clean", program("clean"))
    git(inside, "config", "filter.mark.smudge", program("smudge"))
    git(inside, "config", "filter.mark.required", "true")
    git(inside, "config", "diff.mark.textconv", program("textconv", "cat \"$1\""))
    git(inside, "config", "diff.external", program("extdiff", "exit 0"))
    dir.create(file.path(inside, ".git", "hooks"), showWarnings = FALSE)
    hook <- file.path(inside, ".git", "hooks", "post-index-change")
    file.copy(program("hook", "exit 0"), hook)
    Sys.chmod(hook, "755")
    writeLines("*.R filter=mark diff=mark",
               file.path(inside, ".gitattributes"))

    # The setup is live: git itself runs every one of them. (Git only
    # runs a clean filter from `status` for a file whose size did not
    # change, so the edits here keep the length.)
    touch_x(4)
    git(inside, "status")
    git(inside, "diff")
    expect_true(all(c("clean", "extdiff", "fsmonitor", "hook") %in% ran()))
    git(inside, "config", "--unset", "diff.external")
    git(inside, "diff")
    expect_true("textconv" %in% ran())
    git(inside, "config", "diff.external", program("extdiff", "exit 0"))

    # The tools run none of them, and still answer.
    touch_x(5)
    for (tool in c("git_status", "git_diff", "git_log")) {
        r <- call(tool, path = inside)
        expect_false(r$error, info = tool)
        expect_identical(ran(), character(), info = tool)
    }
    d <- call("git_diff", ref = "HEAD", path = inside, file_path = "a/x.R")
    expect_true(grepl("+alpha <- 5", d$text, fixed = TRUE))
    expect_true(grepl("x.R", call("git_status", path = inside)$text))
    expect_identical(ran(), character())
    # Confined to the checkout, as the reviewer is: the same.
    confined(inside, {
        for (tool in c("git_status", "git_diff", "git_log")) {
            call(tool, path = inside)
        }
    })
    expect_identical(ran(), character())

    # The job snapshot adds files and writes trees; it runs none either.
    touch_x(6)
    writeLines("gamma <- 1", file.path(inside, "a", "new.R"))
    first <- corteza:::job_git_snapshot(inside)
    expect_true(corteza:::job_is_sha(first$snapshot))
    touch_x(7)
    second <- corteza:::job_git_snapshot(inside, base = first)
    expect_true(corteza:::job_is_sha(second$snapshot))
    expect_identical(ran(), character())
    # With the filter off the file is snapshotted as it is on disk.
    shown <- call("git_diff",
                  ref = paste0(first$snapshot, "..", second$snapshot),
                  path = inside)
    expect_true(grepl("+alpha <- 7", shown$text, fixed = TRUE))
    expect_identical(ran(), character())

    # A submodule has its own configuration, which a status of the
    # parent would run inside it. The tools do not look inside.
    git(inside, "-c", "protocol.file.allow=always", "submodule", "add", "-q",
        outside, "mod")
    git(inside, "commit", "-q", "-m", "add submodule")
    mod <- file.path(inside, "mod")
    git(mod, "config", "filter.modmark.clean", program("modclean"))
    writeLines("*.R filter=modmark", file.path(mod, ".gitattributes"))
    writeLines("SECRET <- 'changed'", file.path(mod, "secret.R"))
    ran()
    git(inside, "status")
    expect_true("modclean" %in% ran())
    writeLines("SECRET <- 'CHANGED'", file.path(mod, "secret.R"))
    for (tool in c("git_status", "git_diff")) {
        expect_false(call(tool, path = inside)$error, info = tool)
    }
    expect_true(corteza:::job_is_sha(
        corteza:::job_git_snapshot(inside, base = first)$snapshot))
    expect_identical(ran(), character())

    # A driver whose name cannot be overridden: git is not run at all,
    # and the tool says why instead of reporting "no repository".
    git(inside, "config", "filter.a=b.clean", program("eq"))
    writeLines("*.R filter=a=b", file.path(inside, ".gitattributes"))
    touch_x(8)
    for (tool in c("git_status", "git_diff", "git_log")) {
        r <- call(tool, path = inside)
        expect_true(r$error, info = tool)
        expect_true(grepl("could not be turned off", r$text), info = tool)
    }
    expect_null(corteza:::job_git_snapshot(inside))
    expect_identical(ran(), character())
    git(inside, "status")
    expect_true("eq" %in% ran())
    git(inside, "config", "--unset", "filter.a=b.clean")
    writeLines("*.R filter=mark diff=mark",
               file.path(inside, ".gitattributes"))

    # A partial clone fetches the objects it lacks, and a remote can be
    # a command ("ext::"). A read does not start that either: it fails
    # on the missing object.
    git(inside, "config", "remote.origin.url",
        paste0("ext::", program("fetch", "exit 1")))
    git(inside, "config", "remote.origin.promisor", "true")
    git(inside, "config", "remote.origin.partialclonefilter", "blob:none")
    git(inside, "config", "extensions.partialClone", "origin")
    git(inside, "config", "protocol.ext.allow", "always")
    blob <- trimws(git(inside, "rev-parse", "HEAD:b/y.R"))
    unlink(file.path(inside, ".git", "objects", substr(blob, 1L, 2L),
                     substring(blob, 3L)))
    ran()
    git(inside, "diff", "HEAD", "--", "b/y.R")
    expect_true("fetch" %in% ran())
    r <- call("git_diff", ref = "HEAD", path = inside, file_path = "b/y.R")
    expect_true(r$error)
    expect_false(grepl("could not be turned off", r$text))
    call("git_status", path = inside)
    call("git_log", path = inside)
    expect_identical(ran(), character())
    # Both controls are in place: the environment switch newer git reads,
    # and the protocol refused by name for git that does not.
    off <- corteza:::git_programs_off(inside)
    expect_true("protocol.ext.allow=never" %in% off)
    expect_true(all(c("filter.mark.clean=", "filter.mark.smudge=",
                      "filter.mark.process=", "filter.mark.required=false")
                    %in% off))
    expect_true("protocol.allow=never" %in% corteza:::GIT_SAFE_CONFIG)
    # A repository that configures none of this yields nothing. Read with
    # the global and system configuration out of the way: git-lfs, where
    # installed, registers its filter globally (the GitHub runners have
    # it), and those entries are rightly turned off too, but they are
    # the machine's, not this repository's.
    old_cfg <- Sys.getenv(c("GIT_CONFIG_GLOBAL", "GIT_CONFIG_NOSYSTEM"),
                          unset = NA)
    Sys.setenv(GIT_CONFIG_GLOBAL = file.path(root, "empty.gitconfig"),
               GIT_CONFIG_NOSYSTEM = "1")
    file.create(file.path(root, "empty.gitconfig"))
    expect_identical(corteza:::git_programs_off(outside), character())
    for (v in names(old_cfg)) {
        if (is.na(old_cfg[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old_cfg[v]))
    }
}

unlink(root, recursive = TRUE)
