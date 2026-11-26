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

    # Prefer the last newline, then the last whitespace (tabs included).
    # gregexpr() returns -1 in the first slot when there's no match, so
    # guard on that before taking max().
    newlines <- gregexpr("\n", text, fixed = TRUE)[[1]]
    if (newlines[1L] != -1L) {
        return(max(newlines))
    }

    spaces <- gregexpr("[[:space:]]", text)[[1]]
    if (spaces[1L] != -1L) {
        return(max(spaces))
    }

    -1L
}
