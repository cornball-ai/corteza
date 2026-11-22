# What `corteza acp` sends, built from what a turn reports (R/acp.R).
# Each function here maps without doing any I/O, so its tests need no
# client: a tool event to a tool-call update, reply text to a message
# chunk, a turn's end to a stop reason, a prompt's content to text, and
# an ask to a permission request's options.

# The longest tool result a tool-call update carries; the model saw the
# whole of it.
.ACP_RESULT_CHARS <- 4000L

.acp_text_chunk <- function(text) {
    list(sessionUpdate = "agent_message_chunk",
         content = list(type = "text", text = text))
}

# The text a prompt gives the model: its text blocks, and a link to a
# file as the file's path. corteza acp offers no images, audio or
# embedded resources (`promptCapabilities` all false), so a block of
# another kind is refused rather than dropped.
.acp_prompt_text <- function(blocks) {
    if (!is.list(blocks) || !length(blocks)) {
        .acp_refuse("The prompt has no content.")
    }
    parts <- vapply(blocks, function(block) {
        type <- block$type %||% ""
        if (identical(type, "text")) {
            return(as.character(block$text %||% "")[1L])
        }
        if (identical(type, "resource_link")) {
            uri <- as.character(block$uri %||% "")[1L]
            if (startsWith(uri, "file://")) {
                uri <- utils::URLdecode(sub("^file://", "", uri))
            }
            return(uri)
        }
        .acp_refuse(sprintf(paste("corteza acp takes text and links to files;",
                                  "this prompt has a block of type '%s'."),
                            .sanitize_inline(type, max_chars = 40L)))
    }, character(1))
    paste(parts[nzchar(parts)], collapse = "\n")
}

# ACP's kind for a tool call (ToolKind), from the tool's class.
.acp_tool_kind <- function(call) {
    tool <- call$tool %||% ""
    if (tool %in% c("fetch_url")) {
        return("fetch")
    }
    if (tool %in% c("grep_files", "list_files", "web_search")) {
        return("search")
    }
    klass <- tryCatch(call_class(call), error = function(e) "unknown")
    switch(klass, read = "read", write = "edit", exec = "execute", "other")
}

# The tool and what it acts on: its command, code or path
# (supervisor_brief(), which reads the arguments by their full names). A
# path inside the session's directory is named from there, so a long
# directory does not push the file's name out of the title.
.acp_tool_title <- function(call, cwd = NULL) {
    name <- call$tool %||% "tool"
    brief <- tryCatch(supervisor_brief(call, max_chars = 10000L),
                      error = function(e) "")
    if (!is.character(brief) || length(brief) != 1L || is.na(brief) ||
        !nzchar(trimws(brief))) {
        return(name)
    }
    brief <- trimws(brief)
    if (is.character(cwd) && length(cwd) == 1L && nzchar(cwd)) {
        # The directory as given and as normalized: a call may name either
        # (macOS's /var is /private/var), and on Windows normalizing adds
        # a drive to a path that starts with a slash.
        dirs <- unique(c(cwd, normalizePath(cwd, winslash = "/",
                    mustWork = FALSE)))
        for (inside in paste0(sub("[/\\\\]+$", "", dirs), "/")) {
            if (startsWith(brief, inside)) {
                brief <- substring(brief, nchar(inside) + 1L)
                break
            }
        }
    }
    sprintf("%s: %s", name, .sanitize_inline(brief, max_chars = 120L))
}

# Why a call asks, for a person: what the reviewer or the rules said, or
# the rule a person answers (the credential rule, leaving plan mode).
# The policy's own reason ("default: ...") is corteza's bookkeeping and
# says nothing to the person.
.acp_ask_reason <- function(decision) {
    why <- decision$gate_reason %||% ""
    reason <- decision$reason %||% ""
    if (!nzchar(why) && (.person_rule(decision) ||
            startsWith(reason, "safety:"))) {
        why <- reason
    }
    .sanitize_inline(why, max_chars = 200L)
}

# The files a call names, where they are or would be: a path whose
# directory is not there names nothing a client could open.
.acp_tool_locations <- function(call) {
    paths <- unique(as.character(unlist(call$paths %||% list())))
    paths <- paths[!is.na(paths) & nzchar(paths)]
    paths <- normalizePath(path.expand(paths), winslash = "/", mustWork = FALSE)
    paths <- paths[file.exists(paths) | dir.exists(dirname(paths))]
    lapply(unique(paths), function(p) list(path = p))
}

# The update that announces a call: `tool_call`, with the status a
# permission request gives it ("pending") or a run ("in_progress").
.acp_tool_call <- function(call, id, status, cwd = NULL) {
    update <- list(sessionUpdate = "tool_call", toolCallId = id,
                   title = .acp_tool_title(call, cwd),
                   kind = .acp_tool_kind(call), status = status)
    locations <- .acp_tool_locations(call)
    if (length(locations)) {
        update$locations <- locations
    }
    args <- call$args
    if (is.list(args) && length(args)) {
        update$rawInput <- args
    }
    update
}

# The result a call's last update carries: its text, cut to
# .ACP_RESULT_CHARS, and a diff where the tool made one.
.acp_tool_content <- function(text, diff = NULL) {
    text <- paste(as.character(unlist(text %||% "")), collapse = "\n")
    if (nchar(text) > .ACP_RESULT_CHARS) {
        text <- paste0(substr(text, 1L, .ACP_RESULT_CHARS), "\n[cut here]")
    }
    out <- list(list(type = "content",
                     content = list(type = "text", text = text)))
    lines <- if (is.list(diff)) diff$lines
    if (is.character(lines) && length(lines)) {
        body <- paste(c("```diff", lines, "```"), collapse = "\n")
        out <- c(out, list(list(type = "content",
                                content = list(type = "text", text = body))))
    }
    out
}

# A call's last update: completed, or failed with what it said.
.acp_tool_done <- function(id, success, text, diff = NULL) {
    status <- if (isTRUE(success)) {
        "completed"
    } else {
        "failed"
    }
    list(sessionUpdate = "tool_call_update", toolCallId = id,
         status = status, content = .acp_tool_content(text, diff))
}

# ACP's stop reason for a turn's end (`termination`, R/termination.R).
.acp_stop_reason <- function(termination) {
    if (identical(termination$status, "cancelled")) {
        return("cancelled")
    }
    if (!identical(termination$status, "incomplete")) {
        return("end_turn")
    }
    reason <- termination$reason %||% ""
    if (identical(reason, "max_turns")) {
        return("max_turn_requests")
    }
    if (identical(reason, "refusal")) {
        return("refusal")
    }
    # Any other incomplete reply was cut short by a limit on its length.
    "max_tokens"
}

# The options of a permission request: the terminal's 3 answers. The
# credential rule's ask offers no "always", as at the terminal.
.acp_permission_options <- function(always = TRUE) {
    options <- list(list(optionId = "allow_once", name = "Allow",
                         kind = "allow_once"))
    if (always) {
        options <- c(options, list(list(optionId = "allow_always",
                                        name = "Allow always for this project",
                                        kind = "allow_always")))
    }
    c(options, list(list(optionId = "reject_once", name = "Deny",
                         kind = "reject_once")))
}

# What each mode does: the session's approval_mode (R/policy.R,
# R/review.R) by its name. "allow" and "deny" are offered only where the
# config chose them (.acp_mode_ids()).
.ACP_MODES <- list(
                   ask = list(name = "Ask",
                              description = "Every call that needs approval is asked of you."),
                   review = list(name = "Review",
                                 description = paste("The reviewer rules first on what would be",
            "asked, and passes on what it cannot judge.")),
                   allow = list(name = "Allow",
                                description = paste("Your config's approval_mode: every call",
            "that would ask runs.")),
                   deny = list(name = "Deny",
                               description = paste("Your config's approval_mode: every call",
            "that would ask is refused.")))

.acp_modes <- function(current, ids = c("ask", "review")) {
    list(currentModeId = current,
         availableModes = lapply(ids, function(id) {
        c(list(id = id), .ACP_MODES[[id]])
    }))
}
