# Factual fallback updates do not depend on the model choosing to narrate.
# These run at tool boundaries; they cannot interrupt a blocking tool/request.
.progress_reset <- function(session, now = Sys.time()) {
    session$.progress <- list(started = now, last_at = now, completed = 0L,
                              failed = 0L, last_tool = NULL, commentary = NULL)
}

.progress_record <- function(session, event, now = Sys.time()) {
    if (is.null(session$.progress)) .progress_reset(session, now)
    p <- session$.progress
    lines <- cli_commentary_lines(event)
    if (event$outcome %in% c("start", "task", "deny", "declined") && length(lines)) {
        p$commentary <- paste(lines, collapse = "\n")
        p$last_at <- now
    }
    if (identical(event$outcome, "ran")) {
        p$completed <- p$completed + 1L
        p$failed <- p$failed + as.integer(!isTRUE(event$success))
        p$last_tool <- cli_tool_label(event$call$tool)
    }
    notice <- NULL
    if (identical(event$outcome, "ran") &&
        as.numeric(difftime(now, p$last_at, units = "secs")) >= 60) {
        notice <- sprintf("Still working: %d tool calls completed (%d failed). Latest: %s.",
                          p$completed, p$failed, p$last_tool)
        p$last_at <- now
    }
    session$.progress <- p
    notice
}

.progress_reply <- function(session, reply) {
    if (!grepl("[Max turns reached]", reply, fixed = TRUE)) return(reply)
    p <- session$.progress
    paste(c(reply, "Paused at the tool-turn limit; the task is not marked complete.",
            if (!is.null(p$last_tool)) sprintf("Completed %d tool calls (%d failed). Latest: %s.",
                p$completed, p$failed, p$last_tool),
            if (!is.null(p$commentary)) paste0("Last update:\n", p$commentary),
            "Send `keep going`, or use /auto to continue under supervision."),
          collapse = "\n\n")
}

cli_result_lines <- function(event) {
    text <- rooms_activity_preview(.redact_text(event$result), lines = 5L, width = 120L)
    if (!nzchar(text)) return(character())
    strsplit(text, "\n", fixed = TRUE)[[1L]]
}
