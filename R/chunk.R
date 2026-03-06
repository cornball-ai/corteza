# Text Chunking for Messaging Platforms
# Splits long messages into platform-appropriate chunks

#' Default chunk limit (characters)
#' @noRd
DEFAULT_CHUNK_LIMIT <- 4000L

#' Chunk text by length
#'
#' Splits text into chunks, preferring to break at newlines or whitespace.
#'
#' @param text Text to chunk
#' @param limit Maximum characters per chunk (default: 4000)
#' @return Character vector of chunks
#' @noRd
chunk_text <- function(text, limit = DEFAULT_CHUNK_LIMIT) {
    if (is.null(text) || nchar(text) == 0) {
        return(character())
    }
    if (limit <= 0) {
        return(text)
    }
    if (nchar(text) <= limit) {
        return(text)
    }

    chunks <- character()
    remaining <- text

    while (nchar(remaining) > limit) {
        window <- substr(remaining, 1, limit)

        # Find best break point: prefer newline, then whitespace
        break_idx <- find_break_point(window)

        # Fallback: hard break at limit
        if (break_idx <= 0) {
            break_idx <- limit
        }

        chunk <- trimws(substr(remaining, 1, break_idx), "right")
        if (nchar(chunk) > 0) {
            chunks <- c(chunks, chunk)
        }

        # Skip the break character if it's whitespace
        next_char <- substr(remaining, break_idx + 1, break_idx + 1)
        if (grepl("\\s", next_char)) {
            skip <- 1L
        } else {
            skip <- 0L
        }
        remaining <- trimws(substr(remaining, break_idx + 1 + skip,
                                   nchar(remaining)),
                            "left")
    }

    if (nchar(remaining) > 0) {
        chunks <- c(chunks, remaining)
    }

    chunks
}

#' Chunk text by paragraph
#'
#' Splits text on paragraph boundaries (blank lines), packing multiple
#' paragraphs into chunks up to the limit.
#'
#' @param text Text to chunk
#' @param limit Maximum characters per chunk (default: 4000)
#' @return Character vector of chunks
#' @noRd
chunk_by_paragraph <- function(text, limit = DEFAULT_CHUNK_LIMIT) {
    if (is.null(text) || nchar(text) == 0) {
        return(character())
    }
    if (limit <= 0) {
        return(text)
    }

    # Normalize line endings
    text <- gsub("\r\n?", "\n", text)

    # Split on paragraph boundaries (blank lines)
    paragraphs <- strsplit(text, "\n\\s*\n+")[[1]]
    paragraphs <- trimws(paragraphs)
    paragraphs <- paragraphs[nchar(paragraphs) > 0]

    if (length(paragraphs) == 0) {
        return(character())
    }

    chunks <- character()
    current <- ""

    for (para in paragraphs) {
        # If paragraph alone exceeds limit, chunk it by length
        if (nchar(para) > limit) {
            # Flush current chunk first
            if (nchar(current) > 0) {
                chunks <- c(chunks, trimws(current))
                current <- ""
            }
            # Chunk the long paragraph
            chunks <- c(chunks, chunk_text(para, limit))
            next
        }

        # Try to add to current chunk
        if (nchar(current) > 0) {
            sep <- "\n\n"
        } else {
            sep <- ""
        }
        candidate <- paste0(current, sep, para)

        if (nchar(candidate) <= limit) {
            current <- candidate
        } else {
            # Flush current and start new
            if (nchar(current) > 0) {
                chunks <- c(chunks, trimws(current))
            }
            current <- para
        }
    }

    # Flush remaining
    if (nchar(current) > 0) {
        chunks <- c(chunks, trimws(current))
    }

    chunks
}

#' Chunk text with mode
#'
#' Unified chunking function that dispatches based on mode.
#'
#' @param text Text to chunk
#' @param limit Maximum characters per chunk
#' @param mode Chunking mode: "length" or "newline" (paragraph)
#' @return Character vector of chunks
#' @noRd
chunk_text_with_mode <- function(text, limit = DEFAULT_CHUNK_LIMIT,
                                 mode = c("length", "newline")) {
    mode <- match.arg(mode)
    if (mode == "newline") {
        chunk_by_paragraph(text, limit)
    } else {
        chunk_text(text, limit)
    }
}

#' Find best break point in text
#'
#' Scans backwards from end to find newline or whitespace.
#'
#' @param text Text window to scan
#' @return Index of best break point, or -1 if none found
#' @noRd
find_break_point <- function(text) {
    n <- nchar(text)
    if (n == 0) {
        return(-1L)
    }

    # Scan backwards for newline first
    for (i in n:1) {
        if (substr(text, i, i) == "\n") {
            return(i)
        }
    }

    # Then whitespace
    for (i in n:1) {
        if (grepl("\\s", substr(text, i, i))) {
            return(i)
        }
    }

    -1L
}

