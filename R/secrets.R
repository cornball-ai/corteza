# Display-only filtering. Native history, tool data and journals remain exact.
.secret_key <- function(key) {
    key <- tolower(gsub("[^[:alnum:]]", "", key))
    !key %in% c("synctoken", "nextbatch", "paginationtoken") &
    grepl("(password|passwd|passphrase|apikey|accesstoken|refreshtoken|clientsecret|authorization|secret|token)$",
          key)
}

.secret_values <- function(x) {
    if (!is.list(x)) {
        return(character())
    }
    out <- character()
    for (i in seq_along(x)) {
        if (is.character(x[[i]]) && isTRUE(.secret_key(names(x)[i] %||% ""))) {
            out <- c(out, x[[i]])
        } else if (is.list(x[[i]])) {
            out <- c(out, .secret_values(x[[i]]))
        }
    }
    unique(out[!is.na(out) & nzchar(out)])
}

.secret_environment <- function(session) {
    env <- Sys.getenv()
    values <- unname(env[.secret_key(names(env))])
    path <- Sys.getenv("CORTEZA_MATRIX_CONFIG")
    if (nzchar(path) && file.exists(path)) {
        cfg <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                        error = function(e) NULL)
        values <- c(values, .secret_values(cfg))
    }
    session$secret_config <- unique(c(values, .secret_values(session$config)))
    session$secret_values <- unique(c(session$secret_config, session$secret_literals %||% character()))
    invisible(NULL)
}

# Only quoted, credential-shaped literals. Calls, identifiers, placeholders
# and shell substitutions are not values. Never learn from arbitrary code.
.secret_literals <- function(text) {
    if (!is.character(text)) {
        return(character())
    }
    pattern <- paste0("(?i)\\b(?!(?:sync|pagination)[_-]?token\\b)(?:[[:alnum:]_]*_)?",
                      "(?:password|passwd|passphrase|api[_-]?key|access[_-]?token|",
                      "refresh[_-]?token|client[_-]?secret|authorization|secret|token)[\"']?",
                      "\\s*[:=]\\s*(?:chr\\s+)?([\"'])([A-Za-z0-9_./+=~-]{12,4096})\\1")
    hits <- unlist(regmatches(text, gregexpr(pattern, text, perl = TRUE)),
                   use.names = FALSE)
    values <- sub(pattern, "\\2", hits, perl = TRUE)
    unique(values[grepl("[A-Za-z]", values) & grepl("[0-9]", values) &
            !grepl("REDACTED", values, fixed = TRUE)])
}

.secret_observe <- function(session, text) {
    session$secret_literals <- tail(unique(c(session$secret_literals %||% character(),
                .secret_literals(text))), 128L)
    session$secret_values <- unique(c(session$secret_config %||% character(),
                                      session$secret_literals))
    invisible(NULL)
}

.redact_text <- function(text, values = character()) {
    if (!is.character(text) || !length(text)) {
        return(text)
    }
    values <- unique(c(values, .secret_literals(text)))
    values <- values[!is.na(values) & nzchar(values)]
    for (value in values[order(nchar(values), decreasing = TRUE)]) {
        text <- gsub(value, "[REDACTED]", text, fixed = TRUE)
        escaped <- as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
        escaped <- substr(escaped, 2L, nchar(escaped) - 1L)
        if (!identical(escaped, value)) {
            text <- gsub(escaped, "[REDACTED]", text, fixed = TRUE)
        }
    }
    text <- gsub("(?s)-----BEGIN (?:[A-Z ]+ )?PRIVATE KEY-----.*?-----END (?:[A-Z ]+ )?PRIVATE KEY-----",
                 "[REDACTED PRIVATE KEY]", text, perl = TRUE)
    gsub("\\b(?:sk-[A-Za-z0-9_-]{16,}|gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,})\\b",
         "[REDACTED]", text, perl = TRUE)
}

.redact_object <- function(x, values = character()) {
    if (is.character(x)) {
        return(.redact_text(x, values))
    }
    if (!is.list(x)) {
        return(x)
    }
    for (i in seq_along(x)) {
        if (is.character(x[[i]]) && isTRUE(.secret_key(names(x)[i] %||% ""))) {
            x[[i]][] <- "[REDACTED]"
        } else {
            x[i] <- list(.redact_object(x[[i]], values))
        }
    }
    x
}

.chat_secrets <- function(chat) {
    if (!is.list(chat) && !is.environment(chat)) {
        return(character())
    }
    .secret_values(chat$env$mx %||% list())
}

# Approval displays are executable input, never redacted previews.
.approval_remote_problem <- function(call, decision = list(),
                                     values = character()) {
    body <- as.character(jsonlite::toJSON(list(args = call$args,
                reason = decision$reason),
            auto_unbox = TRUE, null = "null"))
    labelled <- grepl(paste0("(?i)\\b(?!(?:sync|pagination)[_-]?token\\b)(?:[[:alnum:]_]*_)?",
                             "(?:password|passwd|passphrase|api[_-]?key|access[_-]?token|refresh[_-]?token|client[_-]?secret|authorization|secret|token)[\"']?\\s*[:=]"),
                      paste(unlist(call$args), collapse = "\n"), perl = TRUE)
    if (length(.secret_values(call$args)) || labelled ||
        !identical(body, .redact_text(body, values))) {
        return("Credential-bearing calls must be reviewed in a local CLI; nothing was approved remotely.")
    }
    if (nchar(body, type = "bytes") > 16000L) {
        return("The complete call is too large for a room approval. Review it in a local CLI.")
    }
    NULL
}

# Hold only a suffix that could begin a known credential. Ordinary voice
# deltas are delivered immediately; cancellation remains synchronous.
.redact_stream <- function(send, values = function() character()) {
    pending <- ""
    cancelled <- FALSE
    push <- function(text = "", final = FALSE) {
        if (cancelled) {
            return(FALSE)
        }
        pending <<- paste0(pending, text)
        keys <- unique(values())
        keys <- keys[!is.na(keys) & nzchar(keys)]
        for (key in keys[order(nchar(keys), decreasing = TRUE)]) {
            pending <<- gsub(key, "[REDACTED]", pending, fixed = TRUE)
        }
        keep <- 0L
        if (!final && nzchar(pending)) for (key in keys) {
            n <- min(nchar(pending), nchar(key) - 1L)
            if (n > 0L) for (i in seq.int(n, 1L)) {
                if (endsWith(pending, substr(key, 1L, i))) { keep <- max(keep, i) ; break }
            }
        }
        end <- nchar(pending) - keep
        if (end <= 0L) {
            return(TRUE)
        }
        out <- substr(pending, 1L, end)
        pending <<- substring(pending, end + 1L)
        answer <- send(out)
        cancelled <<- identical(answer, FALSE)
        answer
    }
    list(push = function(text) push(text),
         flush = function() push(final = TRUE))
}
