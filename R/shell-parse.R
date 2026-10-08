# Shell command text to simple commands: quotes, operators, redirects,
# heredocs, and substitutions. Used by the rules in R/shell-scan.R; see
# R/exec-scan.R for what the scan is for.

# ---- Shell: text to commands ---------------------------------------------

# Take heredoc bodies out of a command. Returns the text without them
# and, for each, the line that opened it and its body.
shell_heredocs <- function(text) {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    rx <- "<<-?[[:space:]]*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\\1"
    keep <- character()
    docs <- list()
    i <- 1L
    while (i <= length(lines)) {
        ln <- lines[[i]]
        m <- regmatches(ln, regexec(rx, ln))[[1L]]
        if (length(m) && !grepl("<<<", ln, fixed = TRUE)) {
            delim <- m[[3L]]
            j <- i + 1L
            body <- character()
            while (j <= length(lines) &&
                !identical(trimws(lines[[j]]), delim)) {
                body <- c(body, lines[[j]])
                j <- j + 1L
            }
            opener <- sub(rx, " ", ln)
            docs[[length(docs) + 1L]] <- list(opener = opener,
                body = paste(body, collapse = "\n"))
            keep <- c(keep, opener)
            i <- j + 1L
            next
        }
        keep <- c(keep, ln)
        i <- i + 1L
    }
    list(text = paste(keep, collapse = "\n"), docs = docs)
}

# Take command substitutions out of a command, innermost first. Returns
# the text with each replaced by a placeholder, and what was inside.
# Applied to quoted text too: a substitution inside double quotes runs,
# and treating one inside single quotes as if it did costs only a
# needless question.
shell_substitutions <- function(text) {
    inner <- character()
    for (rx in c("\\$\\(([^()]*)\\)", "`([^`]*)`")) {
        repeat {
            m <- regexpr(rx, text)
            if (m < 0L) {
                break
            }
            s <- regmatches(text, m)
            inner <- c(inner, if (startsWith(s, "$(")) {
                    substring(s, 3L, nchar(s) - 1L)
                } else {
                    substring(s, 2L, nchar(s) - 1L)
                })
            regmatches(text, m) <- "__SUB__"
        }
    }
    list(text = text, inner = inner)
}

# Split a command line into simple commands. Each is a list of `words`
# (quotes removed), `out` and `inp` (redirect targets), and `sep`, the
# operator that came before it ("" for the first, "|", "&&", "||", ";",
# "&"). Parentheses separate commands, so a subshell's commands are read
# like any others.
shell_commands <- function(text) {
    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    n <- length(chars)
    at <- function(k) if (k >= 1L && k <= n) chars[[k]] else ""
    cmds <- list()
    words <- character()
    out <- character()
    inp <- character()
    sep <- ""
    buf <- ""
    has <- FALSE
    pending <- ""
    end_word <- function() {
        if (has) {
            if (identical(pending, "out")) {
                out <<- c(out, buf)
            } else if (identical(pending, "in")) {
                inp <<- c(inp, buf)
            } else if (!identical(pending, "skip")) {
                words <<- c(words, buf)
            }
            pending <<- ""
        }
        buf <<- ""
        has <<- FALSE
    }
    end_cmd <- function(next_sep) {
        end_word()
        if (length(words) || length(out) || length(inp)) {
            cmds[[length(cmds) + 1L]] <<- list(words = words, out = out,
                inp = inp, sep = sep)
        }
        words <<- character()
        out <<- character()
        inp <<- character()
        sep <<- next_sep
        pending <<- ""
    }
    # A redirect operator ends the word before it, unless that word is a
    # file descriptor number, which belongs to the operator.
    before_redirect <- function() {
        if (has && grepl("^[0-9]+$", buf)) {
            buf <<- ""
            has <<- FALSE
        } else {
            end_word()
        }
    }
    i <- 1L
    while (i <= n) {
        ch <- chars[[i]]
        nx <- at(i + 1L)
        if (ch == "\\") {
            if (nx != "\n" && i < n) {
                buf <- paste0(buf, nx)
                has <- TRUE
            }
            i <- i + 2L
        } else if (ch == "'") {
            j <- i + 1L
            while (j <= n && chars[[j]] != "'") {
                j <- j + 1L
            }
            if (j > i + 1L) {
                buf <- paste0(buf, paste(chars[(i + 1L):(j - 1L)], collapse = ""))
            }
            has <- TRUE
            i <- j + 1L
        } else if (ch == "\"") {
            j <- i + 1L
            while (j <= n && chars[[j]] != "\"") {
                if (chars[[j]] == "\\" &&
                    at(j + 1L) %in% c("\"", "\\", "$", "`")) {
                    j <- j + 1L
                }
                buf <- paste0(buf, at(j))
                j <- j + 1L
            }
            has <- TRUE
            i <- j + 1L
        } else if (ch == "#" && !has) {
            while (i <= n && chars[[i]] != "\n") {
                i <- i + 1L
            }
        } else if (ch == " " || ch == "\t") {
            end_word()
            i <- i + 1L
        } else if (ch == "\n" || ch == ";") {
            end_cmd(";")
            i <- i + 1L
        } else if (ch == "&") {
            if (nx == "&") {
                end_cmd("&&")
                i <- i + 2L
            } else if (nx == ">") {
                end_word()
                pending <- "out"
                step <- 2L
                if (at(i + 2L) == ">") {
                    step <- 3L
                }
                i <- i + step
            } else {
                end_cmd("&")
                i <- i + 1L
            }
        } else if (ch == "|") {
            if (nx == "|") {
                end_cmd("||")
                i <- i + 2L
            } else {
                end_cmd("|")
                step <- 1L
                if (nx == "&") {
                    step <- 2L
                }
                i <- i + step
            }
        } else if (ch == "(" || ch == ")") {
            end_cmd(";")
            i <- i + 1L
        } else if (ch == ">") {
            before_redirect()
            i <- i + 1L
            if (at(i) == ">") {
                i <- i + 1L
            }
            if (at(i) == "|") {
                i <- i + 1L
            }
            dup <- FALSE
            if (at(i) == "&") {
                # `>&2` and `2>&1` copy a descriptor and name no file.
                k <- i + 1L
                while (grepl("^[0-9-]$", at(k))) {
                    k <- k + 1L
                }
                if (k > i + 1L) {
                    dup <- TRUE
                    i <- k
                } else {
                    i <- i + 1L
                }
            }
            if (!dup) {
                pending <- "out"
            }
        } else if (ch == "<") {
            before_redirect()
            if (nx == "<") {
                # A here-string, or a heredoc shell_heredocs() left: the
                # word after it is data or a delimiter, not a file.
                i <- i + 2L
                if (at(i) == "<" || at(i) == "-") {
                    i <- i + 1L
                }
                pending <- "skip"
            } else if (nx == "(") {
                i <- i + 1L
            } else {
                pending <- "in"
                i <- i + 1L
            }
        } else {
            buf <- paste0(buf, ch)
            has <- TRUE
            i <- i + 1L
        }
    }
    end_cmd("")
    cmds
}

SHELL_KEYWORDS <- c("{", "}", "!", "then", "do", "else", "elif", "if",
                    "while", "until", "fi", "done", "time")

# Programs that run another command given to them as arguments.
SHELL_PRIVILEGE <- c("sudo", "doas", "pkexec", "su", "runuser")
SHELL_WRAPPERS <- c("nohup", "command", "builtin", "exec", "setsid",
                    "nice", "ionice", "stdbuf", "chrt", "taskset",
                    "timeout", "env", "xargs", "watch")

# The program a simple command runs, past assignments, keywords, and
# wrappers. Returns `verb` (its base name, "" for none), `args`, and
# `privileged` (the privilege wrapper used, or "").
shell_verb <- function(words) {
    n <- length(words)
    i <- 1L
    priv <- ""
    skip_options <- function(valued = character()) {
        while (i <= n && startsWith(words[[i]], "-")) {
            step <- 1L
            if (words[[i]] %in% valued) {
                step <- 2L
            }
            i <<- i + step
        }
    }
    repeat {
        if (i > n) {
            return(list(verb = "", args = character(), privileged = priv))
        }
        w <- words[[i]]
        if (w %in% SHELL_KEYWORDS || grepl("^[A-Za-z_][A-Za-z0-9_]*=", w)) {
            i <- i + 1L
            next
        }
        base <- basename(w)
        if (base %in% SHELL_PRIVILEGE) {
            priv <- base
            i <- i + 1L
            skip_options(c("-u", "-g", "-h", "-p", "-C", "-D", "-R",
                           "-T", "-U"))
            next
        }
        if (!base %in% SHELL_WRAPPERS) {
            break
        }
        i <- i + 1L
        if (base == "env") {
            while (i <= n && (startsWith(words[[i]], "-") ||
                    grepl("^[A-Za-z_][A-Za-z0-9_]*=", words[[i]]))) {
                step <- 1L
                if (words[[i]] == "-u") {
                    step <- 2L
                }
                i <- i + step
            }
            if (i > n) {
                # `env` alone prints the environment.
                return(list(verb = "env", args = character(), privileged = priv))
            }
        } else if (base == "timeout") {
            skip_options(c("-s", "-k"))
            i <- i + 1L
        } else if (base == "xargs") {
            skip_options(c("-I", "-n", "-P", "-d", "-L", "-s", "-a", "-E"))
            # What xargs appends is chosen at run time.
            words <- c(words, "__SUB__")
            n <- length(words)
            if (i >= n) {
                return(list(verb = "echo", args = character(), privileged = priv))
            }
        } else if (base == "watch") {
            skip_options(c("-n", "--interval"))
        } else {
            skip_options(c("-n", "-c", "-p", "-i", "-o", "-e"))
        }
    }
    w <- words[[i]]
    args <- words[-seq_len(i)]
    # A whole command passed as one quoted word (`watch 'a && b'`).
    if (grepl("[[:space:]]", w)) {
        return(list(verb = "sh", args = c("-c", paste(c(w, args), collapse = " ")),
                    privileged = priv))
    }
    list(verb = basename(w), args = args, privileged = priv, word = w)
}

