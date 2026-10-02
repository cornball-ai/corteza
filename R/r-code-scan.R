# What a piece of R code would touch: the functions it calls, where its
# string constants point, and the shell commands it runs. See
# R/exec-scan.R for what the scan is for and what it is not.

# ---- R code --------------------------------------------------------------

# Names as r_call_name() gives them: bare for a name that means one
# thing, `pkg::name` for one that is only this when qualified.
R_SHELL_CALLS <- c("system", "system2", "shell", "shell.exec", "pipe",
                   "processx::run", "sys::exec_wait", "sys::exec_internal",
                   "sys::exec_background")
R_WRITE_CALLS <- c("unlink", "file.remove", "file.rename", "file.copy",
                   "file.create", "file.append", "file.symlink",
                   "file.link", "dir.create", "writeLines", "write",
                   "writeBin", "writeChar", "write.csv", "write.csv2",
                   "write.table", "write.dcf", "saveRDS", "save",
                   "save.image", "sink", "dput", "dump", "download.file",
                   "Sys.chmod", "Sys.setFileTime", "fwrite", "write_json",
                   "write_csv", "write_lines", "write_rds", "zip", "unzip",
                   "untar", "tar", "pdf", "png", "jpeg", "svg",
                   "install.packages", "remove.packages")
R_PUBLISH_CALLS <- c("submit_cran", "devtools::release", "insertPackage",
                     "git_push", "pr_push", "use_github")
R_TAMPER_CALLS <- c("assignInNamespace", "assignInMyNamespace",
                    "unlockBinding", "fixInNamespace", "reassignInPackage",
                    "trace", "untrace")
R_TAMPER_SYMBOLS <- c(".subagent_state", ".job_worker_state")
R_INSTALL_CALLS <- c("install", "install_github", "install_local",
                     "install_version", "install_git", "install_url",
                     "pkg_install", "local_install", "install.packages")

# The names a call is made by: the bare name, and `pkg::name` when it
# was written qualified. Empty when the function is itself computed.
r_call_name <- function(e) {
    f <- e[[1L]]
    if (is.symbol(f)) {
        return(as.character(f))
    }
    if (is.call(f) && length(f) == 3L && is.symbol(f[[1L]]) &&
        as.character(f[[1L]]) %in% c("::", ":::", "$", "@") &&
        (is.symbol(f[[3L]]) || is.character(f[[3L]]))) {
        name <- as.character(f[[3L]])
        if (as.character(f[[1L]]) %in% c("::", ":::") && is.symbol(f[[2L]])) {
            return(c(name, paste0(as.character(f[[2L]]), "::", name)))
        }
        return(name)
    }
    character()
}

# Walk parsed code. Returns the names of the functions called, the
# string constants, the symbols, and for each call that runs a shell
# command the strings inside it and whether anything else is.
r_code_walk <- function(exprs) {
    st <- new.env(parent = emptyenv())
    st$calls <- character()
    st$strings <- character()
    st$symbols <- character()
    st$shell <- list()
    st$sourced <- character()
    st$bare_getenv <- FALSE
    parts <- function(e) {
        strings <- character()
        dynamic <- FALSE
        visit <- function(x) {
            if (is.character(x)) {
                strings <<- c(strings, x)
            } else if (is.call(x)) {
                if (!any(r_call_name(x) %in% c("c", "paste", "paste0",
                            "shQuote", "sprintf", "file.path"))) {
                    dynamic <<- TRUE
                }
                for (k in seq_along(x)[-1L]) {
                    if (!identical(x[[k]], quote(expr =))) {
                        visit(x[[k]])
                    }
                }
            } else if (is.symbol(x)) {
                dynamic <<- TRUE
            }
        }
        for (k in seq_along(e)[-1L]) {
            if (!identical(e[[k]], quote(expr =))) {
                visit(e[[k]])
            }
        }
        list(strings = strings, dynamic = dynamic)
    }
    walk <- function(e) {
        if (is.call(e)) {
            name <- r_call_name(e)
            st$calls <- c(st$calls, name)
            if (any(name %in% R_SHELL_CALLS)) {
                st$shell[[length(st$shell) + 1L]] <- parts(e)
            }
            if ("source" %in% name && length(e) > 1L && is.character(e[[2L]])) {
                st$sourced <- c(st$sourced, e[[2L]])
            }
            if ("Sys.getenv" %in% name && length(e) == 1L) {
                st$bare_getenv <- TRUE
            }
            for (k in seq_along(e)) {
                if (!identical(e[[k]], quote(expr =))) {
                    walk(e[[k]])
                }
            }
        } else if (is.character(e)) {
            st$strings <- c(st$strings, e)
        } else if (is.symbol(e)) {
            st$symbols <- c(st$symbols, as.character(e))
        } else if (is.pairlist(e) || is.expression(e) || is.list(e)) {
            for (k in seq_along(e)) {
                if (!identical(e[[k]], quote(expr =))) {
                    walk(e[[k]])
                }
            }
        }
        invisible(NULL)
    }
    walk(exprs)
    st
}

# Is a string constant written as a path? Absolute, in a home, or
# climbing out of the directory. A lone "/x" that does not exist is more
# often a pattern than a place.
r_path_like <- function(s) {
    if (length(s) != 1L || is.na(s) || !nzchar(s) || nchar(s) > 400L ||
        grepl("[[:space:]]", s) || grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", s)) {
        return(FALSE)
    }
    if (grepl("^~(/|$)", s) || grepl("^\\.\\.(/|$)", s)) {
        return(TRUE)
    }
    if (!startsWith(s, "/")) {
        return(FALSE)
    }
    parts <- strsplit(s, "/", fixed = TRUE)[[1L]]
    parts <- parts[nzchar(parts)]
    length(parts) > 1L || (length(parts) == 1L && file.exists(s))
}

# Read R code: what it calls, where its string constants point, and the
# shell commands it runs.
r_code_scan <- function(sx, code, depth = 0L) {
    if (depth > 4L) {
        scan_note(sx, "nests code deeper than this check follows")
        return(invisible(NULL))
    }
    exprs <- tryCatch(parse(text = code, keep.source = FALSE),
                      error = function(e) NULL)
    if (is.null(exprs)) {
        scan_note(sx, "runs R code that does not parse, so its calls were not read")
        return(invisible(NULL))
    }
    st <- r_code_walk(exprs)
    calls <- unique(st$calls)
    hit <- intersect(calls, R_PUBLISH_CALLS)
    if (length(hit)) {
        scan_flag(sx, "publishes or submits (", hit[[1L]], "())")
    }
    hit <- intersect(calls, R_TAMPER_CALLS)
    if (length(hit)) {
        scan_flag(sx, "changes code inside a loaded package (", hit[[1L]], "())")
    }
    hit <- intersect(st$symbols, R_TAMPER_SYMBOLS)
    if (length(hit)) {
        scan_flag(sx, "reaches into corteza's own runtime state (", hit[[1L]], ")")
    }
    if (any(calls %in% c("eval", "evalq", "eval.parent")) &&
        any(calls %in% c("parse", "str2lang", "str2expression"))) {
        scan_flag(sx, "builds code from text and runs it (eval(parse()))")
    }
    if (isTRUE(st$bare_getenv)) {
        scan_flag(sx, "prints the environment, which can hold API keys ",
                  "(Sys.getenv())")
    }
    for (s in st$sourced) {
        if (grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", s)) {
            scan_flag(sx, "runs code fetched from the network (source())")
        } else {
            scan_script(sx, s, "r", depth)
        }
    }
    for (sh in st$shell) {
        if (isTRUE(sh$dynamic)) {
            scan_note(sx, "runs a shell command built at run time")
        }
        if (length(sh$strings)) {
            shell_scan(sx, paste(c(sh$strings, if (isTRUE(sh$dynamic)) "__SUB__"),
                                 collapse = " "), depth + 1L)
        }
    }
    writes <- any(calls %in% R_WRITE_CALLS)
    for (s in unique(st$strings)) {
        # A relative path stays in the project, so only two are worth
        # resolving: git's own files and the project's corteza config.
        internal <- writes && !grepl("[[:space:]]", s) && nchar(s) < 400L &&
        grepl("(^|/)\\.git(/|$)|(^|/)\\.corteza/config\\.json$", s)
        if (!r_path_like(s) && !internal) {
            next
        }
        p <- scan_resolve(sx, s)
        if (is.null(p)) {
            next
        }
        if (identical(p$zone, "sensitive")) {
            scan_flag(sx, "touches ", p$text, ", where credentials or secrets are kept")
        } else if (identical(p$zone, "outside")) {
            if (writes) {
                scan_flag(sx, "may write outside the project (", p$abs, ")")
            } else {
                scan_note(sx, "names a path outside the project: ", p$abs)
            }
        } else if (writes && p$zone %in% c("git", "control")) {
            scan_flag(sx, "may write ", if (identical(p$zone, "git")) {
                    "git's own files"
                } else {
                    "the project's corteza config"
                }, " (", p$abs, ")")
        }
    }
    if (any(calls %in% R_INSTALL_CALLS)) {
        scan_note(sx, "installs an R package; into the default library ",
                  "unless the call names another")
    }
    if (any(calls %in% c("q", "quit"))) {
        scan_note(sx, "ends the R session it runs in")
    }
    if ("setwd" %in% calls) {
        scan_note(sx, "changes the working directory")
    }
    invisible(NULL)
}

