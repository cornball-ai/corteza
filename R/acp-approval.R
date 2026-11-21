# What `corteza acp` does with a call that asks (R/acp.R): the terminal's
# rules, with the client in the person's place. A tool allowed always in
# the session's directory runs without a question, never where the
# credential rule asks. Otherwise the client gets a permission request
# with the terminal's 3 answers: allow, allow always for this project
# (kept as the terminal keeps it, R/approvals.R), deny. A cancel while
# the request is open declines the call, and the turn stops before the
# call (R/turn-stop.R).

.acp_approval_cb <- function(st, sid, cwd) {
    force(st)
    force(sid)
    force(cwd)
    for_session <- character()
    answered <- function(call, decision) {
        name <- call$tool %||% ""
        !.person_rule(decision) &&
        (name %in% for_session || name %in% approvals_saved(cwd))
    }
    # Saved for the directory; held for the session where it cannot be.
    keep_always <- function(name) {
        if (approval_save(cwd, name)) {
            return(TRUE)
        }
        for_session <<- c(for_session, name)
        FALSE
    }
    cb <- function(call, decision) {
        if (answered(call, decision)) {
            return(TRUE)
        }
        live <- st$live
        if (is.null(live) || !identical(live$sid, sid)) {
            return(FALSE)
        }
        shown <- .redact_object(call, live$session$secret_values)
        once <- .person_rule(decision)
        id <- .acp_announce(st, live, shown, "pending")
        title <- .acp_tool_title(shown, live$session$cwd)
        why <- .acp_ask_reason(decision)
        if (nzchar(why)) {
            title <- sprintf("%s (%s)", title, why)
        }
        answer <- .acp_ask(st, "session/request_permission",
                           list(sessionId = sid,
                                toolCall = list(toolCallId = id, title = title,
                    kind = .acp_tool_kind(shown),
                    status = "pending"),
                                options = .acp_permission_options(always = !once)))
        outcome <- if (is.list(answer)) answer$result$outcome
        if (!identical(outcome$outcome, "selected")) {
            # Cancelled, unanswered, or an outcome this agent does not
            # know: never a yes.
            return(FALSE)
        }
        choice <- outcome$optionId %||% ""
        if (identical(choice, "allow_always") && !once) {
            keep_always(call$tool %||% "")
            return(TRUE)
        }
        identical(choice, "allow_once")
    }
    attr(cb, "answered") <- answered
    cb
}
