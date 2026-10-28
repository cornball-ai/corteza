# Bounded public progress records shared by direct turns and child workers.
# Keep only rendered previews here; full arguments/results stay in the journal.
rooms_activity_preview <- function(text, lines = 5L, width = 160L) {
    text <- .redact_text(text)
    if (!is.character(text) || !length(text)) {
        return("")
    }
    text <- paste(text, collapse = "\n")
    parts <- strsplit(substr(text, 1L, 12000L), "\n", fixed = TRUE)[[1L]]
    shown <- head(parts, lines)
    shown <- .sanitize_inline(shown, max_chars = width)
    if (length(parts) > lines || nchar(text) > 12000L) {
        shown <- c(shown, "...")
    }
    paste(shown, collapse = "\n")
}

rooms_activity_record <- function(event) {
    event <- event$display %||% .redact_object(event)
    kind <- event$outcome
    if (identical(kind, "progress")) {
        return(list(narration = event$result, batch = event$result))
    }
    if (length(kind) != 1L ||
        !kind %in% c("start", "ran", "deny", "declined")) {
        return(NULL)
    }
    call <- event$call %||% list()
    mc <- call$model_context
    narration <- if (identical(as.integer(mc$call_index %||% 1L), 1L)) {
        rooms_activity_preview(mc$assistant_text, lines = 12L, width = 240L)
    } else ""
    tool <- call$tool %||% event$tool %||% ""
    record <- list(narration = narration,
                   turn_id = call$turn_id, call_id = call$call_id,
                   batch = paste(event$turn_number %||% mc$agent_turn %||% "", narration))
    if (!identical(kind, "start")) {
        record$tool <- list(
                            tool = tool, category = rooms_activity_category(tool),
                            success = isTRUE(event$success), refused = kind %in% c("deny",
                "declined"),
                            added = event$diff$added %||% 0L, removed = event$diff$removed %||% 0L,
                            command = cli_tool_preview(tool, call$args %||% event$args %||% list(), width = 240L),
                            output = rooms_activity_preview(event$result))
    }
    record
}

rooms_activity_add <- function(acc, record) {
    if (is.null(record)) {
        return(invisible(NULL))
    }
    if (identical(acc$detail, FALSE)) {
        record$narration <- ""
        if (!is.null(record$tool)) {
            record$tool$command <- NULL
            record$tool$output <- NULL
        }
    }
    if (nzchar(record$narration %||% "") &&
        !identical(record$batch, acc$narration_key)) {
        acc$items[[length(acc$items) + 1L]] <- list(type = "commentary",
            text = record$narration)
        acc$narration_key <- record$batch
    }
    if (!is.null(record$tool)) {
        acc$events[[length(acc$events) + 1L]] <- record$tool
        acc$items[[length(acc$items) + 1L]] <- list(type = "tool", event = record$tool)
    }
    invisible(NULL)
}

rooms_activity_detail <- function(event) {
    label <- cli_tool_label(event$tool)
    command <- event$command %||% ""
    if (nzchar(command)) {
        label <- paste0(label, "(", command, ")")
    }
    if (isTRUE(event$refused)) {
        label <- paste0(label, " (refused)")
    } else
    if (!isTRUE(event$success)) {
        label <- paste0(label, " (failed)")
    }
    label
}

rooms_activity_group_text <- function(events) {
    text <- rooms_activity_text(events)
    detailed <- any(vapply(events, function(e) {
        nzchar(e$command %||% "") || nzchar(e$output %||% "")
    }, logical(1)))
    if (!detailed) {
        return(text)
    }
    rows <- vapply(events, function(e) {
        output <- e$output %||% ""
        paste0("- ", rooms_activity_detail(e),
            if (nzchar(output)) paste0("\n  ",
                                       gsub("\n", "\n  ", output, fixed = TRUE)))
    }, character(1))
    paste(c(text, rows), collapse = "\n")
}

# Narration is outside <details>; commands and results stay within each group.
# The tail and byte ceilings bound both the UI and Matrix's event size, even
# when escaping expands model-controlled strings or the work runs for hours.
rooms_activity_frame <- function(acc) {
    items <- acc$items %||% lapply(acc$events,
                                   function(e) list(type = "tool", event = e))
    if (!length(items)) {
        return(list(text = "", html = ""))
    }
    total <- length(items)
    items <- tail(items, 30L)
    render <- function(items) {
        text <- html <- character()
        group <- list()
        flush <- function() {
            if (!length(group)) {
                return(invisible(NULL))
            }
            text <<- c(text, rooms_activity_group_text(group))
            html <<- c(html, rooms_activity_html(group))
            group <<- list()
        }
        for (item in items) {
            if (identical(item$type, "commentary")) {
                flush()
                text <- c(text, item$text)
                html <- c(html, paste0("<p>", gsub("\n", "<br>",
                            rooms_escape_html(item$text), fixed = TRUE), "</p>"))
            } else {
                group[[length(group) + 1L]] <- item$event
            }
        }
        flush()
        if (length(items) < total) {
            prefix <- "Earlier activity omitted from this preview."
            text <- c(prefix, text)
            html <- c(paste0("<p>", prefix, "</p>"), html)
        }
        list(text = paste(text, collapse = "\n\n"), html = paste(html, collapse = ""))
    }
    repeat {
        frame <- render(items)
        # Matrix replacements carry the content twice (outer fallback and
        # m.new_content). Count JSON escaping too, leaving room for that copy
        # and the event envelope under the transport's 64 KiB ceiling.
        encoded <- jsonlite::toJSON(list(body = frame$text, formatted_body = frame$html),
                                    auto_unbox = TRUE)
        if (nchar(encoded, type = "bytes") < 24000L || length(items) <= 1L) {
            return(frame)
        }
        items <- items[-1L]
    }
}
