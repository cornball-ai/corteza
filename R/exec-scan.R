# What a shell command or a piece of R code would touch, read off its
# text.
#
# policy() resolves the paths a tool names in its arguments and says of
# itself that shell commands and run_r bodies are not parsed. A
# supervisor that only knew what policy knows could rule on write_file
# and on nothing a shell does. This file reads the body.
#
# It returns two lists. `flags` are things a person decides: credentials,
# writes outside the project, privilege, other machines, publishing, work
# thrown away. `notes` are things the reader saw and could not settle,
# handed to whoever judges the call next.
#
# This is a reader, not a sandbox. A command can be written so that
# nothing here recognizes it (a path built at run time, code fetched and
# run by a program this file does not know). What it gives is a floor
# under a judgment made elsewhere: the ordinary ways of doing the listed
# things are caught without asking a model.

# ---- Scan state ----------------------------------------------------------

# `root` is the project. `cwd` moves as the command changes directory.
# `write_roots` are places outside the project that may be written.
scan_new <- function(root, cwd = root, write_roots = character()) {
    sx <- new.env(parent = emptyenv())
    sx$root <- .resolve_real(root, root)
    sx$cwd <- .resolve_real(cwd, sx$root)
    sx$cwd_known <- TRUE
    sx$write_roots <- vapply(as.character(write_roots), function(p) {
        .resolve_real(p, sx$root)
    }, character(1), USE.NAMES = FALSE)
    tmp <- Sys.getenv("TMPDIR")
    scratch <- c("/tmp", "/var/tmp", .resolve_real(tempdir(), "/"),
        if (nzchar(tmp)) .resolve_real(tmp, "/"))
    # Never the filesystem root: an empty or odd TMPDIR must not turn
    # every path into scratch.
    sx$scratch <- unique(scratch[nchar(scratch) > 1L])
    sx$flags <- character()
    sx$notes <- character()
    sx
}

scan_flag <- function(sx, ...) {
    sx$flags <- unique(c(sx$flags, paste0(...)))
    invisible(NULL)
}

scan_note <- function(sx, ...) {
    sx$notes <- unique(c(sx$notes, paste0(...)))
    invisible(NULL)
}

# ---- Paths ---------------------------------------------------------------

# Directories and files where credentials are kept, beyond the ones
# policy() already refuses to decide on its own (.hard_secret_paths()).
# The bot configs are here because they hold a bot's password and token,
# and corteza's config directory because it holds the settings that
# decide who approves what.
scan_sensitive_dirs <- function() {
    c("~/.ssh", "~/.gnupg", "~/.aws", "~/.config/gcloud", "~/.kube",
        "~/.docker", "~/.config/gh", "~/.password-store",
        "~/.local/share/keyrings", "/etc/sudoers.d", corteza_config_dir())
}

scan_sensitive_files <- function() {
    c(.hard_secret_paths(), "~/.netrc", "~/.Renviron", "~/.git-credentials",
        "~/.pgpass", "~/.claude/.credentials.json", "~/.codex/auth.json",
        "/etc/shadow", "/etc/gshadow", "/etc/sudoers",
        Sys.getenv("CORTEZA_MATRIX_CONFIG", ""))
}

scan_sensitive <- function(abs) {
    for (d in scan_sensitive_dirs()) {
        if (.path_within(abs, d)) {
            return(TRUE)
        }
    }
    for (f in scan_sensitive_files()) {
        if (nzchar(f) && .path_within(abs, f)) {
            return(TRUE)
        }
    }
    # A bot's own config: any file directly in ~/.corteza.
    bots <- .normalize_lexical(path.expand("~/.corteza"))
    if (identical(dirname(abs), bots)) {
        return(TRUE)
    }
    base <- basename(abs)
    grepl("^\\.env($|\\.)", base) ||
    grepl("^id_(rsa|dsa|ecdsa|ed25519)", base) ||
    grepl("\\.(pem|p12|pfx|jks|keystore)$", base) ||
    grepl("^\\.?credentials(\\.json)?$", base) ||
    grepl("^/proc/[^/]+/environ$", abs)
}

# Where a resolved path sits: "sensitive", "project", "git" (the
# project's .git), "control" (the project's own corteza config),
# "scratch", "write_root", or "outside".
scan_zone <- function(sx, abs, lex = abs) {
    if (scan_sensitive(abs) || scan_sensitive(lex)) {
        return("sensitive")
    }
    if (.path_within(abs, sx$root)) {
        rel <- substring(abs, nchar(sx$root) + 2L)
        if (identical(rel, ".git") || startsWith(rel, ".git/")) {
            return("git")
        }
        if (identical(rel, ".corteza/config.json")) {
            return("control")
        }
        return("project")
    }
    if (abs %in% c("/dev/null", "/dev/stdout", "/dev/stderr", "/dev/tty") ||
        startsWith(abs, "/dev/fd/")) {
        return("scratch")
    }
    for (s in sx$scratch) {
        if (.path_within(abs, s)) {
            return("scratch")
        }
    }
    for (w in sx$write_roots) {
        if (.path_within(abs, w)) {
            return("write_root")
        }
    }
    "outside"
}

# Resolve a word to a path. NULL when it cannot be resolved from the
# text: a variable, a command substitution, another user's home, or a
# relative path after a directory change that could not be followed.
scan_resolve <- function(sx, tok) {
    text <- tok
    home <- path.expand("~")
    tok <- sub("^\\$\\{?HOME\\}?", home, tok)
    tok <- sub("^\\$\\{?PWD\\}?", sx$cwd, tok)
    tok <- sub("^\\$\\{?TMPDIR\\}?", "/tmp", tok)
    if (!nzchar(tok) || grepl("[$`]", tok) ||
        grepl("__SUB__", tok, fixed = TRUE) || grepl("^~[^/]", tok)) {
        return(NULL)
    }
    # A glob stands for entries of the directory before it.
    glob <- regexpr("[*?\\[{]", tok)
    glob_all <- FALSE
    if (glob > 0L) {
        glob_all <- grepl("(^|/)(\\*|\\.\\*|\\.\\[!\\.\\]\\*)$", tok)
        head <- substring(tok, 1L, glob - 1L)
        tok <- if (!nzchar(head)) {
            "."
        } else if (endsWith(head, "/")) {
            head
        } else {
            dirname(head)
        }
    }
    relative <- !.is_absolute_path(path.expand(tok))
    if (relative && !isTRUE(sx$cwd_known)) {
        return(NULL)
    }
    abs <- .resolve_real(tok, sx$cwd)
    lex <- .resolve_against(tok, sx$cwd)
    list(abs = abs, lex = lex, zone = scan_zone(sx, abs, lex),
         glob = glob > 0L, glob_all = glob_all, text = text)
}

# File names that mark secrets wherever they sit.
SCAN_SECRET_NAME <- paste0("(^|/)(\\.env($|\\.)|id_(rsa|dsa|ecdsa|ed25519))|",
                           "\\.(pem|p12|pfx|jks|keystore)$")

# Does a word name a place outside the current directory's own entries?
scan_path_like <- function(tok) {
    grepl("^(/|~|\\.{1,2}(/|$)|\\$\\{?HOME)", tok)
}

# The path-like parts of a command's words: the word itself, or what
# follows `=` in `--output=/x` and `of=/dev/sda`.
scan_candidates <- function(words) {
    out <- character()
    for (w in words) {
        # Also a secrets file named by a relative path (`cat .env`).
        if (scan_path_like(w) || grepl(SCAN_SECRET_NAME, w)) {
            out <- c(out, w)
        } else if (grepl("=", w, fixed = TRUE)) {
            rhs <- sub("^[^=]*=", "", w)
            if (scan_path_like(rhs)) {
                out <- c(out, rhs)
            }
        }
    }
    out
}

# A path something reads. Credentials are flagged; elsewhere outside the
# project is noted, since policy allows reading there.
scan_read <- function(sx, tok) {
    p <- scan_resolve(sx, tok)
    if (is.null(p)) {
        return(invisible(NULL))
    }
    if (identical(p$zone, "sensitive")) {
        scan_flag(sx, "touches ", p$text,
                  ", where credentials or secrets are kept")
    } else if (identical(p$zone, "outside")) {
        scan_note(sx, "names a path outside the project: ", p$abs)
    }
    invisible(p)
}

# A path something writes, removes, or changes. `what` is the verb
# phrase for the message ("writes", "redirects output to").
scan_write <- function(sx, tok, what = "writes") {
    p <- scan_resolve(sx, tok)
    if (is.null(p)) {
        scan_note(sx, what, " a path this check could not resolve (", tok, ")")
        return(invisible(NULL))
    }
    switch(p$zone,
           sensitive = scan_flag(sx, "touches ", p$text,
                                 ", where credentials or secrets are kept"),
           outside = scan_flag(sx, what, " outside the project (", p$abs, ")"),
           git = scan_flag(sx, what, " git's own files (", p$abs, ")"),
           control = scan_flag(sx, what, " the project's corteza config (",
                               p$abs, "), which decides what needs approval"),
           NULL)
    invisible(p)
}

# ---- Entry ---------------------------------------------------------------

# Names of credential stores, matched in the raw text. This catches a
# path the structured read cannot see: one assembled from pieces, or
# passed inside code for an interpreter this file does not parse.
SCAN_SENSITIVE_TEXT <- paste0(
                              "(^|[^A-Za-z0-9_])(",
                              "\\.ssh(/|\\b)|\\.gnupg\\b|\\.aws(/|\\b)|\\.netrc\\b|\\.Renviron\\b|",
                              "\\.git-credentials\\b|id_(rsa|dsa|ecdsa|ed25519)\\b|authorized_keys\\b|",
                              "/etc/(shadow|sudoers)\\b|\\.credentials\\.json\\b)")

# The longest body read. A longer one goes to a person unread.
SCAN_MAX_CHARS <- 20000L

# Read an exec tool's call. `tool` is "bash", "cmd", "run_r", or
# "run_r_script"; anything else returns nothing found. Returns
# list(flags, notes), both character vectors of plain phrases.
exec_scan <- function(tool, args, root, cwd = root, write_roots = character()) {
    sx <- scan_new(root, cwd, write_roots)
    args <- as.list(args %||% list())
    kind <- switch(tool, bash =, cmd = "shell", run_r =,
                   run_r_script = "r", NULL)
    if (is.null(kind)) {
        return(list(flags = character(), notes = character()))
    }
    text <- args[["command"]] %||% args[["cmd"]] %||% args[["code"]] %||%
    args[["script"]]
    if (!is.character(text)) {
        text <- unlist(Filter(is.character, args), use.names = FALSE)
    }
    text <- paste(text, collapse = "\n")
    if (nchar(text) > SCAN_MAX_CHARS) {
        scan_flag(sx, "is too long to check (", nchar(text), " characters)")
        return(list(flags = sx$flags, notes = sx$notes))
    }
    if (grepl(SCAN_SENSITIVE_TEXT, text, perl = TRUE)) {
        scan_flag(sx, "names a credential store (",
                  trimws(regmatches(text, regexpr(SCAN_SENSITIVE_TEXT, text,
                        perl = TRUE))), ")")
    }
    result <- tryCatch({
        if (identical(kind, "shell")) {
            shell_scan(sx, text)
        } else {
            r_code_scan(sx, text)
        }
        TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(result)) {
        # A reader that failed read nothing. The call is not waved on.
        scan_flag(sx, "could not be checked (", result, ")")
    }
    list(flags = sx$flags, notes = sx$notes)
}
