# Mask credentials at output boundaries, before truncation or serialization.
# This is an accidental-disclosure guard, not a sandbox: arbitrary encoded
# secrets or credentials with no recognizable label cannot be inferred.
.secret_state <- new.env(parent = emptyenv())
.secret_state$values <- character()

.secret_key <- function(key) {
    key <- tolower(gsub("[^[:alnum:]]", "", key))
    grepl("(password|passwd|passphrase|apikey|accesstoken|refreshtoken|clientsecret|authorization|secret|token)$",
          key)
}

.secret_remember <- function(values) {
    values <- values[!is.na(values) & nchar(values) >= 8L &
        !grepl("REDACTED", values, fixed = TRUE)]
    .secret_state$values <- unique(c(.secret_state$values, values))
    invisible(NULL)
}

.secret_register <- function(x) {
    if (!is.list(x)) {
        return(invisible(NULL))
    }
    for (i in seq_along(x)) {
        key <- names(x)[i] %||% ""
        if (is.character(x[[i]]) && isTRUE(.secret_key(key))) {
            .secret_remember(x[[i]])
        } else if (is.list(x[[i]])) {
            .secret_register(x[[i]])
        }
    }
    invisible(NULL)
}

.secret_environment <- function() {
    env <- Sys.getenv()
    .secret_remember(unname(env[.secret_key(names(env))]))
    # Child workers do not inherit the parent's in-memory registry. The
    # bot config named by their environment supplies known credentials for
    # masking even when a tool prints one without a label.
    path <- Sys.getenv("CORTEZA_MATRIX_CONFIG")
    if (nzchar(path) && file.exists(path)) {
        cfg <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                         error = function(e) NULL)
        .secret_register(cfg)
    }
    invisible(NULL)
}

.redact_text <- function(text) {
    if (!is.character(text) || !length(text)) {
        return(text)
    }
    # Learn labelled values before masking, so a later unlabelled echo of
    # the same value is masked too. Includes JSON, shell env, and R str().
    key <- "\\b(?:[[:alnum:]_]*_)?(?:password|passwd|passphrase|api[_-]?key|access[_-]?token|refresh[_-]?token|client[_-]?secret|authorization|secret|token)"
    prefix <- paste0("(?i)(", key, "[\"']?\\s*(?:=|:)\\s*(?:chr\\s+)?)")
    patterns <- c(paste0(prefix, "\"([^\"\\r\\n]+)\""),
                  paste0(prefix, "'([^'\\r\\n]+)'"),
                  paste0(prefix, "([^\\s\"',;{}<>]+)"),
                  "(?i)(Bearer\\s+)([A-Za-z0-9._~+/-]{8,}=*)")
    for (i in seq_along(patterns)) {
        pattern <- patterns[[i]]
        hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE))
        for (hit in unlist(hits, use.names = FALSE)) {
            value <- sub(pattern, "\\2", hit, perl = TRUE)
            if (!grepl("^[0-9]+$", value)) {
                .secret_remember(value)
            }
        }
        replacement <- c('\\1"[REDACTED]"', "\\1'[REDACTED]'",
                         "\\1[REDACTED]", "\\1[REDACTED]")[[i]]
        text <- gsub(pattern, replacement, text, perl = TRUE)
    }
    text <- gsub("(?s)-----BEGIN (?:[A-Z ]+ )?PRIVATE KEY-----.*?-----END (?:[A-Z ]+ )?PRIVATE KEY-----",
                 "[REDACTED PRIVATE KEY]", text, perl = TRUE)
    text <- gsub("\\b(?:sk-[A-Za-z0-9_-]{16,}|gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,})\\b",
                 "[REDACTED]", text, perl = TRUE)
    values <- .secret_state$values
    for (value in values[order(nchar(values), decreasing = TRUE)]) {
        text <- gsub(value, "[REDACTED]", text, fixed = TRUE)
        escaped <- as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
        escaped <- substr(escaped, 2L, nchar(escaped) - 1L)
        text <- gsub(escaped, "[REDACTED]", text, fixed = TRUE)
    }
    text
}

.redact_object <- function(x) {
    if (is.character(x)) {
        return(.redact_text(x))
    }
    if (!is.list(x)) {
        return(x)
    }
    # Provider-signed thinking is opaque protocol state. Rewriting its text
    # invalidates the signature; it is never an output preview. Journals that
    # contain it remain private, just like worker workspace snapshots.
    if (isTRUE(x$type %in% c("thinking", "redacted_thinking")) &&
        (!is.null(x$signature) || !is.null(x$data))) {
        return(x)
    }
    .secret_register(x)
    for (i in seq_along(x)) {
        key <- names(x)[i] %||% ""
        if (is.character(x[[i]]) && isTRUE(.secret_key(key))) {
            x[[i]][] <- "[REDACTED]"
        } else {
            x[i] <- list(.redact_object(x[[i]]))
        }
    }
    x
}

# Buffer incomplete lines so a credential split across provider deltas cannot
# escape the normal output filter. Flush the final line only on completion.
.redact_stream <- function(send) {
    pending <- ""
    push <- function(text = "", final = FALSE) {
        pending <<- paste0(pending, text)
        cuts <- gregexpr("\n", pending, fixed = TRUE)[[1L]]
        if (isTRUE(final)) {
            end <- nchar(pending)
        } else {
            end <- max(0L, cuts)
        }
        if (!isTRUE(final) && grepl("-----BEGIN .*PRIVATE KEY-----", pending) &&
            !grepl("-----END .*PRIVATE KEY-----", pending)) {
            end <- 0L
        }
        if (end == 0L) {
            return(invisible(TRUE))
        }
        out <- substr(pending, 1L, end)
        pending <<- substring(pending, end + 1L)
        send(.redact_text(out))
    }
    list(push = function(text) push(text),
         flush = function() push(final = TRUE))
}
