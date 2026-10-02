# What a shell command would touch: the rules applied to each simple
# command R/shell-parse.R finds. See R/exec-scan.R for what the scan is
# for and what it is not.

# ---- Shell: rules --------------------------------------------------------

SHELL_SHELLS <- c("sh", "bash", "zsh", "dash", "ksh", "fish")
SHELL_R <- c("r", "R", "Rscript", "littler")
SHELL_INTERPRETERS <- c("python", "python3", "python2", "perl", "ruby",
                        "node", "nodejs", "php", "lua", "deno", "bun",
                        "pwsh", "powershell")
SHELL_FETCH <- c("curl", "wget")

SHELL_SYSTEM <- c("service", "kill", "pkill", "killall", "shutdown",
                  "reboot", "halt", "poweroff", "at", "batch", "mount",
                  "umount", "useradd", "usermod", "userdel", "groupadd",
                  "passwd", "chown", "chgrp", "visudo", "iptables", "nft",
                  "ufw", "modprobe", "insmod", "rmmod", "chroot",
                  "nsenter", "unshare", "launchctl")
SHELL_NETWORK <- c("ssh", "scp", "sftp", "nc", "ncat", "netcat",
                   "telnet", "ftp", "socat")
SHELL_DESTROY <- c("dd", "shred", "wipefs", "fdisk", "parted", "sgdisk")
SHELL_PACKAGES <- c("apt", "apt-get", "aptitude", "snap", "dnf", "yum",
                    "pacman", "brew", "flatpak")
SHELL_WRITE_ALL <- c("mv", "touch", "mkdir", "tee", "truncate")
SHELL_WRITE_DEST <- c("cp", "install", "ln")

# The first word of `args` that is not an option.
shell_first_arg <- function(args) {
    plain <- args[!startsWith(args, "-")]
    if (length(plain)) {
        plain[[1L]]
    } else {
        ""
    }
}

shell_has_flag <- function(args, short = NULL, long = character()) {
    any(args %in% long) ||
    (!is.null(short) &&
        any(grepl(paste0("^-[A-Za-z]*[", short, "]"), args) &
            !startsWith(args, "--")))
}

# Read one simple command. Returns its verb, so the caller can tell what
# a pipeline's later commands are fed by.
shell_scan_command <- function(sx, cmd, pipeline, depth) {
    v <- shell_verb(cmd$words)
    verb <- v$verb
    args <- v$args
    if (nzchar(v$privileged)) {
        scan_flag(sx, "runs with elevated privileges (", v$privileged, ")")
    }
    for (t in cmd$out) {
        scan_write(sx, t, "redirects output to")
    }
    for (t in cmd$inp) {
        scan_read(sx, t)
    }
    if (!nzchar(verb)) {
        return("")
    }
    # Credentials are flagged whatever the program is.
    for (t in scan_candidates(args)) {
        p <- scan_resolve(sx, t)
        if (!is.null(p) && identical(p$zone, "sensitive")) {
            scan_flag(sx, "touches ", p$text,
                      ", where credentials or secrets are kept")
        }
    }
    interpreter <- verb %in% c(SHELL_SHELLS, SHELL_R, SHELL_INTERPRETERS,
                               "source", ".", "eval")
    if (interpreter && any(pipeline %in% SHELL_FETCH)) {
        scan_flag(sx, "pipes a download into an interpreter (", verb, ")")
    }
    plain <- args[!startsWith(args, "-")]
    if (length(plain)) {
        sub <- plain[[1L]]
    } else {
        sub <- ""
    }

    if (verb %in% SHELL_SYSTEM) {
        scan_flag(sx, "acts on the system or on other processes (", verb, ")")
    } else if (verb %in% c("systemctl", "loginctl")) {
        if (!sub %in% c("status", "show", "is-active", "is-enabled", "is-failed",
                        "list-units", "list-unit-files", "list-timers",
                        "list-sessions", "cat", "")) {
            scan_flag(sx, "controls system services (", verb, " ", sub, ")")
        }
    } else if (verb == "crontab") {
        if (!identical(args, "-l")) {
            scan_flag(sx, "changes scheduled jobs (crontab)")
        }
    } else if (verb == "sysctl") {
        if (any(args == "-w") || any(grepl("=", args, fixed = TRUE))) {
            scan_flag(sx, "changes kernel settings (sysctl)")
        }
    } else if (verb %in% SHELL_PACKAGES) {
        if (!sub %in% c("list", "show", "search", "policy", "depends",
                        "rdepends", "changelog", "info", "find", "")) {
            scan_flag(sx, "installs or removes system packages (", verb, " ",
                      sub, ")")
        }
    } else if (verb == "dpkg") {
        if (!any(args %in% c("-l", "-L", "-s", "-S", "--list", "--status",
                             "--listfiles", "--search"))) {
            scan_flag(sx, "installs or removes system packages (dpkg)")
        }
    } else if (verb %in% c("docker", "podman")) {
        if (sub %in% c("push", "login")) {
            scan_flag(sx, "publishes or pushes (", verb, " ", sub, ")")
        } else if (!sub %in% c("ps", "images", "logs", "inspect", "version",
                               "info", "stats", "top", "port", "diff",
                               "history", "search", "")) {
            scan_flag(sx, "runs or changes containers (", verb, " ", sub, ")")
        }
    } else if (verb %in% SHELL_NETWORK) {
        scan_flag(sx, "connects to another machine (", verb, ")")
    } else if (verb %in% SHELL_FETCH) {
        shell_scan_fetch(sx, verb, args)
    } else if (verb == "rsync") {
        if (any(grepl("^([A-Za-z0-9_.-]+@)?[A-Za-z0-9_.-]+:", plain)) ||
            any(startsWith(plain, "rsync://"))) {
            scan_flag(sx, "connects to another machine (rsync)")
        } else if (length(plain)) {
            scan_write(sx, plain[[length(plain)]], "copies files to")
        }
    } else if (verb == "printenv" || (verb == "env" && !length(args)) ||
        (verb == "set" && !length(args)) ||
        (verb == "export" && identical(args, "-p")) ||
        (verb == "declare" && any(args %in% c("-p", "-x"))) ||
        verb == "compgen") {
        scan_flag(sx, "prints the environment, which can hold API keys (",
                  verb, ")")
    } else if (verb == "git") {
        shell_scan_git(sx, args)
    } else if (verb == "gh") {
        shell_scan_gh(sx, plain, args)
    } else if (verb %in% c("npm", "yarn", "pnpm", "cargo", "gem", "twine",
                           "netlify", "vercel", "hf", "huggingface-cli")) {
        if (sub %in% c("publish", "push", "upload", "deploy", "login",
                       "adduser", "yank", "owner")) {
            scan_flag(sx, "publishes or pushes (", verb, " ", sub, ")")
        } else if (sub %in% c("install", "add", "i") &&
            any(args %in% c("-g", "--global"))) {
            scan_note(sx, "installs packages globally (", verb, ")")
        }
    } else if (verb %in% SHELL_DESTROY || startsWith(verb, "mkfs")) {
        scan_flag(sx, "overwrites or destroys data (", verb, ")")
    } else if (verb %in% c("rm", "rmdir", "unlink")) {
        shell_scan_rm(sx, verb, args)
    } else if (verb == "find") {
        shell_scan_find(sx, args, depth)
    } else if (verb %in% SHELL_WRITE_ALL) {
        for (t in plain) {
            scan_write(sx, t)
        }
    } else if (verb == "chmod") {
        for (t in plain[-1L]) {
            scan_write(sx, t, "changes permissions")
        }
    } else if (verb %in% SHELL_WRITE_DEST) {
        dest <- shell_option_value(args, "-t", "--target-directory")
        if (is.null(dest) && length(plain)) {
            dest <- plain[[length(plain)]]
        }
        if (!is.null(dest)) {
            scan_write(sx, dest, "copies files to")
        }
    } else if (verb == "sed") {
        if (shell_has_flag(args, "i", "--in-place") ||
            any(startsWith(args, "--in-place"))) {
            scripted <- any(args %in% c("-e", "-f", "--expression", "--file"))
            if (scripted) {
                files <- plain
            } else {
                files <- plain[-1L]
            }
            for (t in files) {
                scan_write(sx, t, "edits in place")
            }
        }
    } else if (verb == "tar") {
        shell_scan_tar(sx, args)
    } else if (verb %in% c("unzip", "zip")) {
        dest <- if (verb == "unzip") {
            shell_option_value(args, "-d", NULL) %||% "."
        } else {
            sub
        }
        if (nzchar(dest)) {
            scan_write(sx, dest)
        }
    } else if (verb %in% c("cd", "pushd")) {
        shell_scan_cd(sx, sub)
    } else if (verb %in% SHELL_SHELLS) {
        code <- shell_option_value(args, "-c", NULL, cluster = "c")
        if (!is.null(code)) {
            shell_scan(sx, code, depth + 1L)
        } else if (nzchar(sub)) {
            scan_script(sx, sub, "shell", depth)
        } else if (identical(cmd$sep, "|")) {
            scan_flag(sx, "runs commands piped into a shell (", verb, ")")
        }
    } else if (verb == "eval") {
        shell_scan(sx, paste(args, collapse = " "), depth + 1L)
    } else if (verb %in% c("source", ".")) {
        if (nzchar(sub)) {
            scan_script(sx, sub, "shell", depth)
        }
    } else if (verb %in% SHELL_R) {
        shell_scan_r(sx, verb, args, cmd, depth)
    } else if (verb %in% SHELL_INTERPRETERS) {
        if (identical(utils::head(args, 3L), c("-m", "pip", "install"))) {
            scan_note(sx, "installs Python packages (", verb, " -m pip)")
        } else {
            scan_note(sx, "runs ", verb, " code this check does not read")
        }
    } else if (verb %in% c("pip", "pip3", "pipx", "uv")) {
        if ("install" %in% plain) {
            scan_note(sx, "installs Python packages (", verb, ")")
        }
    } else {
        if (grepl("/", v$word %||% "", fixed = TRUE)) {
            scan_script(sx, v$word, "auto", depth)
        }
        for (t in scan_candidates(args)) {
            scan_read(sx, t)
        }
    }
    verb
}

# The value given to an option: `-d DIR`, `--directory DIR`, or
# `--directory=DIR`. With `cluster`, the short option may end a bundle
# (`-lc CODE`). NULL when the option is absent.
shell_option_value <- function(args, short, long, cluster = NULL) {
    for (k in seq_along(args)) {
        a <- args[[k]]
        hit <- identical(a, short) || (!is.null(long) && identical(a, long)) ||
        (!is.null(cluster) && !startsWith(a, "--") &&
            grepl(paste0("^-[A-Za-z]*", cluster, "$"), a))
        if (hit && k < length(args)) {
            return(args[[k + 1L]])
        }
        if (!is.null(long) && startsWith(a, paste0(long, "="))) {
            return(sub("^[^=]*=", "", a))
        }
    }
    NULL
}

shell_scan_fetch <- function(sx, verb, args) {
    upload <- any(args %in% c("-d", "--data", "--data-raw", "--data-binary",
                              "--data-urlencode", "--data-ascii", "-F",
                              "--form", "--form-string", "-T",
                              "--upload-file", "--json", "--post-data",
                              "--post-file", "--body-data", "--body-file")) ||
    any(grepl("^--(data|form|upload-file|json|post-data|post-file|body-data|body-file)[a-z-]*=",
              args)) ||
    any(grepl("^-[A-Za-z]*[dFT]", args) & !startsWith(args, "--"))
    method <- shell_option_value(args, "-X", "--request") %||%
    shell_option_value(args, "--method", "--method")
    glued <- sub("^-X", "", args[grepl("^-X.", args)])
    method <- toupper(c(method, glued))
    if (upload || any(!method %in% c("GET", "HEAD"))) {
        scan_flag(sx, "sends data to another machine (", verb, ")")
    }
    dest <- if (verb == "curl") {
        shell_option_value(args, "-o", "--output") %||%
        shell_option_value(args, "--output-dir", "--output-dir") %||%
        if (shell_has_flag(args, "O", "--remote-name")) {
            "."
        }
    } else {
        shell_option_value(args, "-O", "--output-document") %||%
        shell_option_value(args, "-P", "--directory-prefix") %||% "."
    }
    if (!is.null(dest) && !identical(dest, "-")) {
        scan_write(sx, dest, "downloads to")
    }
    invisible(NULL)
}

GIT_READ_ONLY <- c("status", "log", "diff", "show", "blame", "grep",
                   "ls-files", "ls-tree", "rev-parse", "rev-list",
                   "describe", "shortlog", "cat-file", "merge-base",
                   "name-rev", "for-each-ref", "show-ref", "count-objects",
                   "fetch", "version", "help", "whatchanged",
                   "check-ignore", "")

shell_scan_git <- function(sx, args) {
    n <- length(args)
    i <- 1L
    dir <- NULL
    while (i <= n && startsWith(args[[i]], "-")) {
        a <- args[[i]]
        if (a %in% c("-C", "--git-dir", "--work-tree")) {
            dir <- if (i < n) args[[i + 1L]]
            i <- i + 2L
        } else if (a %in% c("-c", "--namespace", "--exec-path")) {
            i <- i + 2L
        } else {
            if (grepl("^--(git-dir|work-tree)=", a)) {
                dir <- sub("^[^=]*=", "", a)
            }
            i <- i + 1L
        }
    }
    if (i <= n) {
        sub <- args[[i]]
    } else {
        sub <- ""
    }
    if (i < n) {
        rest <- args[(i + 1L):n]
    } else {
        rest <- character()
    }
    has <- function(...) any(rest %in% c(...))
    first <- shell_first_arg(rest)
    read_only <- sub %in% GIT_READ_ONLY ||
    (sub == "branch" && !length(rest[!rest %in% c("-a", "-r", "-v", "-vv",
                    "--list", "--show-current", "--all", "--remotes")])) ||
    (sub == "remote" && first %in% c("", "show", "get-url")) ||
    (sub == "tag" && (!length(rest) || has("-l", "--list"))) ||
    (sub == "stash" && first %in% c("list", "show")) ||
    (sub == "reflog" && first %in% c("", "show")) ||
    (sub == "worktree" && first == "list") ||
    (sub == "config" && has("--get", "--list", "-l", "--get-all"))
    if (!read_only) {
        where <- if (is.null(dir)) {
            list(zone = scan_zone(sx, sx$cwd), abs = sx$cwd)
        } else {
            scan_resolve(sx, dir)
        }
        if (is.null(where)) {
            scan_note(sx, "runs git ", sub,
                      " in a directory this check could not resolve")
        } else if (!where$zone %in% c("project", "git", "scratch", "write_root")) {
            scan_flag(sx, "changes a git repository outside the project (",
                      where$abs, ")")
        }
    }
    what <- if (sub == "push") {
        "pushes commits (git push)"
    } else if (sub == "reset" && has("--hard")) {
        "discards uncommitted work (git reset --hard)"
    } else if (sub == "clean" && !has("-n", "--dry-run")) {
        "deletes untracked files (git clean)"
    } else if (sub == "checkout" && (has("--", ".", "-f", "--force", "-B"))) {
        "discards uncommitted work or resets a branch (git checkout)"
    } else if (sub == "switch" &&
        has("-C", "--force-create", "--discard-changes", "-f", "--force")) {
        "discards uncommitted work or resets a branch (git switch)"
    } else if (sub == "restore" && (!has("--staged", "-S") ||
            has("--worktree", "-W"))) {
        "discards uncommitted work (git restore)"
    } else if (sub == "branch" &&
        has("-D", "-d", "--delete", "-f", "--force")) {
        "deletes or moves a branch (git branch)"
    } else if (sub %in% c("rebase", "filter-branch", "filter-repo", "update-ref",
                          "prune", "replace")) {
        paste0("rewrites history (git ", sub, ")")
    } else if (sub == "commit" && has("--amend")) {
        "rewrites the last commit (git commit --amend)"
    } else if (sub == "stash" && first %in% c("drop", "clear")) {
        "throws away stashed work (git stash)"
    } else if (sub == "reflog" && first %in% c("expire", "delete")) {
        "deletes recovery history (git reflog)"
    } else if (sub == "gc" && any(startsWith(rest, "--prune"))) {
        "deletes unreachable objects (git gc --prune)"
    } else if (sub == "tag" && has("-d", "--delete")) {
        "deletes a tag (git tag -d)"
    } else if (sub == "worktree" && first %in% c("remove", "prune")) {
        "removes a worktree (git worktree)"
    } else if (sub == "config" && has("--global", "--system")) {
        "changes git settings outside the project (git config)"
    } else if (sub == "remote" && !read_only) {
        "changes the repository's remotes (git remote)"
    }
    if (!is.null(what)) {
        scan_flag(sx, what)
    }
    if (sub == "clone") {
        plain <- rest[!startsWith(rest, "-")]
        scan_write(sx,
            if (length(plain) > 1L) {
                plain[[2L]]
            } else {
                "."
            }, "clones into")
    }
    invisible(NULL)
}

shell_scan_gh <- function(sx, plain, args) {
    if (length(plain)) {
        area <- plain[[1L]]
    } else {
        area <- ""
    }
    if (length(plain) > 1L) {
        act <- plain[[2L]]
    } else {
        act <- ""
    }
    read_only <- area %in% c("search", "status", "help", "version", "") ||
    (area == "auth" && act == "status") ||
    (area == "api" && !any(args %in% c("-X", "--method", "-f", "-F",
                                       "--field", "--raw-field", "--input")) &&
        !any(grepl("^(-X.|--method=|--field=|--raw-field=|--input=)", args))) ||
    (area != "api" && area != "auth" &&
        act %in% c("view", "list", "status", "diff", "checks", "watch"))
    if (!read_only) {
        scan_flag(sx, "acts on GitHub (gh ", trimws(paste(area, act)), ")")
    }
    invisible(NULL)
}

shell_scan_rm <- function(sx, verb, args) {
    recursive <- verb == "rm" && shell_has_flag(args, "rR", "--recursive")
    for (t in args[!startsWith(args, "-")]) {
        p <- scan_resolve(sx, t)
        if (is.null(p)) {
            if (recursive) {
                scan_flag(sx,
                          "deletes recursively at a path chosen at run time (",
                          t, ")")
            } else {
                scan_note(sx, "deletes a path this check could not resolve (",
                          t, ")")
            }
            next
        }
        whole <- !p$glob || p$glob_all
        # "/" is above everything; .path_within() compares by a
        # trailing-slash prefix and does not see that.
        above <- identical(p$abs, "/") || .path_within(sx$root, p$abs)
        if (recursive && whole && above) {
            scan_flag(sx, "deletes the project directory or one above it (",
                      p$abs, ")")
        } else if (identical(p$zone, "sensitive")) {
            scan_flag(sx, "touches ", p$text,
                      ", where credentials or secrets are kept")
        } else if (identical(p$zone, "outside")) {
            scan_flag(sx, "deletes outside the project (", p$abs, ")")
        } else if (identical(p$zone, "git")) {
            scan_flag(sx, "deletes git's own files (", p$abs, ")")
        } else if (identical(p$zone, "control")) {
            scan_flag(sx, "deletes the project's corteza config")
        }
    }
    invisible(NULL)
}

shell_scan_find <- function(sx, args, depth) {
    expr <- which(startsWith(args, "-") | args %in% c("(", "!"))
    if (length(expr)) {
        roots <- args[seq_len(expr[[1L]] - 1L)]
    } else {
        roots <- args
    }
    if (!length(roots)) {
        roots <- "."
    }
    for (t in roots) {
        scan_read(sx, t)
    }
    if ("-delete" %in% args) {
        shell_scan_rm(sx, "rm", c("-r", roots))
    }
    for (k in which(args %in% c("-exec", "-execdir", "-ok", "-okdir"))) {
        tail <- args[-seq_len(k)]
        end <- which(tail %in% c(";", "+"))
        if (length(end)) {
            inner <- tail[seq_len(end[[1L]] - 1L)]
        } else {
            inner <- tail
        }
        # `{}` stands for what find found, which is under its roots.
        inner[inner == "{}"] <- roots[[1L]]
        if (length(inner)) {
            shell_scan_command(sx, list(words = inner, out = character(),
                                        inp = character(), sep = ""),
                               character(), depth + 1L)
        }
    }
    invisible(NULL)
}

shell_scan_tar <- function(sx, args) {
    mode <- args[!startsWith(args, "--")]
    if (length(mode)) {
        mode <- mode[[1L]]
    } else {
        mode <- ""
    }
    if (grepl("x", mode, fixed = TRUE) ||
        any(args %in% c("--extract", "--get"))) {
        scan_write(sx, shell_option_value(args, "-C", "--directory") %||% ".",
                   "extracts into")
    } else if (grepl("c", mode, fixed = TRUE) || "--create" %in% args) {
        archive <- shell_option_value(args, "-f", "--file", cluster = "f")
        if (is.null(archive) && grepl("f$", mode) && length(args) > 1L) {
            archive <- args[[2L]]
        }
        if (!is.null(archive) && !identical(archive, "-")) {
            scan_write(sx, archive, "writes an archive to")
        }
    }
    invisible(NULL)
}

shell_scan_cd <- function(sx, target) {
    if (!nzchar(target) || identical(target, "~")) {
        target <- "~"
    }
    if (identical(target, "-")) {
        sx$cwd_known <- FALSE
        return(invisible(NULL))
    }
    p <- scan_resolve(sx, target)
    if (is.null(p)) {
        sx$cwd_known <- FALSE
        scan_note(sx, "changes directory to a place this check could not ",
                  "resolve (", target, ")")
        return(invisible(NULL))
    }
    if (identical(p$zone, "sensitive")) {
        scan_flag(sx, "touches ", p$text, ", where credentials or secrets are kept")
    } else if (identical(p$zone, "outside")) {
        scan_note(sx, "works in a directory outside the project: ", p$abs)
    }
    sx$cwd <- p$abs
    sx$cwd_known <- TRUE
    invisible(NULL)
}

shell_scan_r <- function(sx, verb, args, cmd, depth) {
    if (verb == "R" && identical(utils::head(args, 1L), "CMD")) {
        if (length(args) > 1L) {
            tool <- args[[2L]]
        } else {
            tool <- ""
        }
        rest <- args[-(1:2)]
        if (tool == "INSTALL") {
            lib <- shell_option_value(rest, "-l", "--library")
            if (is.null(lib)) {
                scan_note(sx, "installs a package into the default R library")
            } else {
                scan_write(sx, lib, "installs a package into")
            }
        } else if (tool == "REMOVE") {
            scan_note(sx, "removes a package from an R library")
        } else if (tool == "BATCH") {
            plain <- rest[!startsWith(rest, "-")]
            if (length(plain)) {
                scan_script(sx, plain[[1L]], "r", depth)
            }
        }
        return(invisible(NULL))
    }
    inline <- FALSE
    for (k in seq_along(args)) {
        if (args[[k]] %in% c("-e", "--eval") && k < length(args)) {
            inline <- TRUE
            r_code_scan(sx, args[[k + 1L]], depth + 1L)
        }
    }
    if (!inline) {
        valued <- c("-l", "--packages", "-d", "--default-packages", "-L",
                    "--libpath")
        skip <- which(args %in% valued) + 1L
        plain <- args[setdiff(seq_along(args), skip)]
        plain <- plain[!startsWith(plain, "-")]
        if (length(plain)) {
            scan_script(sx, plain[[1L]], "r", depth)
        } else if (identical(cmd$sep, "|")) {
            scan_note(sx, "runs R code piped in at run time")
        }
    }
    invisible(NULL)
}

# A script a command runs. One inside the project is read and scanned
# like inline code; anything else is noted, because what it does is not
# in the call.
scan_script <- function(sx, tok, kind, depth) {
    p <- scan_resolve(sx, tok)
    if (is.null(p)) {
        scan_note(sx, "runs a script this check could not find (", tok, ")")
        return(invisible(NULL))
    }
    if (identical(p$zone, "sensitive")) {
        scan_flag(sx, "touches ", p$text,
                  ", where credentials or secrets are kept")
        return(invisible(NULL))
    }
    if (identical(kind, "auto")) {
        kind <- if (grepl("\\.[Rr]$", p$abs)) {
            "r"
        } else if (grepl("\\.(sh|bash)$", p$abs)) {
            "shell"
        } else {
            "other"
        }
    }
    readable <- p$zone %in% c("project", "scratch", "write_root") &&
    kind %in% c("r", "shell") && depth < 3L && file.exists(p$abs) &&
    !dir.exists(p$abs) && isTRUE(file.size(p$abs) < 200000)
    if (!readable) {
        scan_note(sx, "runs a script this check did not read (", p$abs, ")")
        return(invisible(NULL))
    }
    code <- tryCatch(paste(readLines(p$abs, warn = FALSE), collapse = "\n"),
                     error = function(e) NULL)
    if (is.null(code)) {
        scan_note(sx, "runs a script this check could not read (", p$abs, ")")
    } else if (identical(kind, "r")) {
        r_code_scan(sx, code, depth + 1L)
    } else {
        shell_scan(sx, code, depth + 1L)
    }
    invisible(NULL)
}

# Read a shell command line: every simple command in it, the commands
# inside substitutions, and code handed to an interpreter by heredoc.
shell_scan <- function(sx, text, depth = 0L) {
    if (depth > 4L) {
        scan_note(sx, "nests commands deeper than this check follows")
        return(invisible(NULL))
    }
    hd <- shell_heredocs(text)
    subs <- shell_substitutions(hd$text)
    for (inner in subs$inner) {
        # A substitution runs in a subshell: its directory changes stay
        # inside it.
        cwd <- sx$cwd
        known <- sx$cwd_known
        shell_scan(sx, inner, depth + 1L)
        sx$cwd <- cwd
        sx$cwd_known <- known
    }
    pipeline <- character()
    for (cmd in shell_commands(subs$text)) {
        if (!identical(cmd$sep, "|")) {
            pipeline <- character()
        }
        pipeline <- c(pipeline, shell_scan_command(sx, cmd, pipeline, depth))
    }
    for (d in hd$docs) {
        verbs <- vapply(shell_commands(d$opener), function(cmd) {
            shell_verb(cmd$words)$verb
        }, character(1))
        if (any(verbs %in% SHELL_SHELLS)) {
            shell_scan(sx, d$body, depth + 1L)
        } else if (any(verbs %in% SHELL_R)) {
            r_code_scan(sx, d$body, depth + 1L)
        } else if (any(verbs %in% SHELL_INTERPRETERS)) {
            scan_note(sx, "runs code this check does not read (heredoc)")
        }
    }
    invisible(NULL)
}

