# Deterministic Retrieval for Workspace Objects
# Scores workspace objects by recency, keyword overlap, type affinity,
# and staleness. No embeddings.

#' Score a workspace object for retrieval relevance
#'
#' @param meta Metadata list from ws_meta()
#' @param prompt User prompt string
#' @param current_turn Current turn number
#' @return Numeric score (0-1 range, higher = more relevant)
#' @noRd
ws_score <- function(meta, prompt, current_turn) {
    # Recency (weight 0.4): exponential decay
    turn_distance <- current_turn - (meta$turn %||% 0L)
    recency <- exp(-0.1 * turn_distance)

    # Keyword overlap (weight 0.3): fraction of prompt words in name+class+tool
    prompt_words <- unique(tolower(strsplit(prompt, "[^a-zA-Z0-9_.]+")[[1]]))
    prompt_words <- prompt_words[nchar(prompt_words) > 1]

    if (length(prompt_words) == 0) {
        keyword <- 0
    } else {
        target_text <- tolower(paste(
                                     meta$name,
                                     meta$class,
                                     meta$origin$tool %||% "",
                                     collapse = " "
            ))
        matches <- sum(vapply(prompt_words, function(w) {
            grepl(w, target_text, fixed = TRUE)
        }, logical(1)))
        keyword <- matches / length(prompt_words)
    }

    # Type affinity (weight 0.2)
    type_scores <- c(
                     "data.frame" = 1.0,
                     "matrix" = 0.9,
                     "list" = 0.7,
                     "function" = 0.6,
                     "character" = 0.4,
                     "numeric" = 0.5,
                     "integer" = 0.5,
                     "logical" = 0.3
    )
    type_affinity <- unname(type_scores[meta$class]) %||% 0.5

    # Weighted sum
    score <- 0.4 * recency + 0.3 * keyword + 0.2 * type_affinity

    # Stale penalty
    if (isTRUE(meta$stale)) {
        score <- score * 0.3
    }

    # Pinned bonus
    if (isTRUE(meta$pinned)) {
        score <- score + 0.2
    }

    # Clamp to [0, 1]
    min(1, max(0, score))
}

#' Retrieve relevant workspace objects within a character budget
#'
#' Scores all workspace objects, sorts by relevance, greedily selects
#' within budget_chars.
#'
#' @param prompt User prompt
#' @param budget_chars Max characters for retrieved context (default 8000)
#' @param current_turn Current turn (default: ws_current_turn())
#' @return List of lists, each with name, value, meta, summary, score
#' @noRd
ws_retrieve <- function(prompt, budget_chars = 8000L, current_turn = NULL) {
    nms <- ws_names()
    if (length(nms) == 0) {
        return(list())
    }

    current_turn <- current_turn %||% ws_current_turn()

    # Score all objects
    scored <- lapply(nms, function(nm) {
        meta <- ws_meta(nm)
        value <- ws_get(nm)
        score <- ws_score(meta, prompt, current_turn)
        summary <- ws_summarize(nm, value, meta)
        list(name = nm, value = value, meta = meta,
             summary = summary, score = score,
             chars = nchar(summary))
    })

    # Sort by score descending
    scores <- vapply(scored, function(x) x$score, numeric(1))
    scored <- scored[order(scores, decreasing = TRUE)]

    # Greedily select within budget
    selected <- list()
    used <- 0L
    for (item in scored) {
        if (used + item$chars > budget_chars) {
            next
        }
        selected <- c(selected, list(item))
        used <- used + item$chars
    }

    selected
}

#' Create a compact summary of a workspace object
#'
#' @param name Object name
#' @param value The R value
#' @param meta Provenance metadata
#' @param max_chars Max characters for the summary (default 2000)
#' @return Character string summary
#' @noRd
ws_summarize <- function(name, value, meta, max_chars = 2000L) {
    cls <- meta$class

    body <- if (cls == "data.frame") {
        dims <- sprintf("[%dx%d]", nrow(value), ncol(value))
        col_info <- vapply(names(value), function(cn) {
            col <- value[[cn]]
            cl <- class(col)[1]
            na_count <- sum(is.na(col))
            na_str <- if (na_count > 0) sprintf(" %dNA", na_count) else ""

            extra <- if (is.numeric(col) && length(col) > 0) {
                rng <- range(col, na.rm = TRUE)
                sprintf(" [%.4g,%.4g]", rng[1], rng[2])
            } else if (is.factor(col)) {
                lvls <- levels(col)
                if (length(lvls) <= 5) {
                    sprintf(" {%s}", paste(lvls, collapse = ","))
                } else {
                    sprintf(" {%d levels}", length(lvls))
                }
            } else if (is.character(col) && length(col) > 0) {
                n_unique <- length(unique(col))
                sprintf(" %d unique", n_unique)
            } else {
                ""
            }

            sprintf("%s(%s%s%s)", cn, cl, na_str, extra)
        }, character(1))
        col_str <- paste(col_info, collapse = ", ")
        if (nchar(col_str) > 400) {
            col_str <- paste0(substr(col_str, 1, 397), "...")
        }
        head_str <- tryCatch({
            h <- capture.output(print(head(value, 3)))
            paste(h, collapse = "\n")
        }, error = function(e) "(head unavailable)")
        sprintf("data.frame %s: %s\n%s", dims, col_str, head_str)

    } else if (inherits(value, "matrix")) {
        dims <- sprintf("[%dx%d]", nrow(value), ncol(value))
        tp <- typeof(value)
        rng_str <- if (is.numeric(value) && length(value) > 0) {
            rng <- range(value, na.rm = TRUE)
            sprintf(" range [%.4g, %.4g]", rng[1], rng[2])
        } else {
            ""
        }
        sprintf("matrix %s %s%s", dims, tp, rng_str)

    } else if (cls == "character") {
        if (length(value) > 1) {
            sample_items <- head(value, 3)
            sample_str <- paste(sprintf('"%s"',
                                        substr(sample_items, 1, 50)), collapse = ", ")
            sprintf("character[%d]: %s%s", length(value), sample_str,
                if (length(value) > 3) ", ..." else "")
        } else {
            text <- value
            if (nchar(text) > 500) {
                paste0(substr(text, 1, 497), "...")
            } else {
                text
            }
        }

    } else if (cls == "function") {
        args <- tryCatch(
                         paste(names(formals(value)), collapse = ", "),
                         error = function(e) "..."
        )
        sprintf("function(%s)", args)

    } else {
        # Default: use str() output
        out <- tryCatch({
            s <- capture.output(str(value, max.level = 2, give.attr = FALSE))
            paste(s, collapse = "\n")
        }, error = function(e) as.character(value))
        if (nchar(out) > 300) {
            paste0(substr(out, 1, 297), "...")
        } else {
            out
        }
    }

    if (isTRUE(meta$stale)) {
        stale_marker <- " [STALE]"
    } else {
        stale_marker <- ""
    }
    if (isTRUE(meta$pinned)) {
        pinned_marker <- " [PINNED]"
    } else {
        pinned_marker <- ""
    }
    origin_str <- if (!is.null(meta$origin$tool)) {
        sprintf(" (from %s, turn %d)", meta$origin$tool, meta$turn)
    } else {
        sprintf(" (turn %d)", meta$turn)
    }

    result <- sprintf("**%s**%s%s%s\n%s", name, stale_marker,
                      pinned_marker, origin_str, body)

    # Truncate if over budget
    if (nchar(result) > max_chars) {
        result <- paste0(substr(result, 1, max_chars - 3), "...")
    }
    result
}

#' Compact one-liner digest of entire workspace
#'
#' Always included in context so the LLM knows what exists, even when
#' individual summaries exceed the budget.
#'
#' @return Character string (empty if workspace is empty)
#' @noRd
ws_digest <- function() {
    nms <- ws_names()
    if (length(nms) == 0) {
        return("")
    }

    total_bytes <- ws_size()
    total_mb <- round(total_bytes / 1e6, 1)

    items <- vapply(nms, function(nm) {
        meta <- ws_meta(nm)
        val <- ws_get(nm)
        desc <- if (meta$class == "data.frame") {
            sprintf("%s (data.frame %dx%d)", nm, nrow(val), ncol(val))
        } else if (inherits(val, "matrix")) {
            sprintf("%s (matrix %dx%d)", nm, nrow(val), ncol(val))
        } else if (meta$class == "list") {
            sprintf("%s (list of %d)", nm, length(val))
        } else {
            sprintf("%s (%s)", nm, meta$class)
        }
        if (isTRUE(meta$pinned)) desc <- paste0(desc, "*")
        if (isTRUE(meta$stale)) desc <- paste0(desc, "~")
        desc
    }, character(1))

    sprintf("Workspace: %d objects, %.1fMB. Items: %s",
            length(nms), total_mb, paste(items, collapse = ", "))
}

#' Format retrieved objects as markdown context for system prompt
#'
#' @param retrieved List from ws_retrieve()
#' @return Character string (empty if nothing retrieved)
#' @noRd
ws_format_context <- function(retrieved) {
    digest <- ws_digest()

    if (length(retrieved) == 0 && nchar(digest) == 0) {
        return("")
    }

    parts <- c("## Workspace State", "")
    if (nchar(digest) > 0) {
        parts <- c(parts, digest, "")
    }
    for (item in retrieved) {
        parts <- c(parts, item$summary, "")
    }

    paste(parts, collapse = "\n")
}

