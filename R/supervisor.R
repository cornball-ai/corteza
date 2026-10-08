# Who answers for a tool call on a surface nobody is watching.
#
# A Matrix room's turns and the jobs it delegates run with no person at
# a prompt. Every call goes through three steps, in this order:
#
#   1. Rules in code (supervisor_route()). Credentials, writes outside
#      the project, privilege, other machines, publishing, work thrown
#      away. Anything they catch goes to a person; no model is asked.
#   2. A monitor: a model with read-only tools and the project's rules,
#      in a process of its own (R/job-monitor.R). It rules on what the
#      rules left open: is this call part of the task, is it what the
#      project allows. It approves, refuses, or passes the call on.
#   3. A person, by reaction in the room, for what steps 1 and 2 sent on.
#
# Step 2 runs only where the bot is configured to approve without asking
# (`auto_approve_asks`). Elsewhere step 3 takes its place, as before.
# Step 1 runs either way, and on every call: policy() decides "allow"
# for reads anywhere and for writes outside the configured code paths,
# and an approval callback is never shown those.
#
# The hall monitor for /auto runs (R/monitor.R) has the same shape and
# another failure rule. An /auto run that meets something outside its
# authority stops for the day. A room has someone to ask, so a call here
# waits for an answer and the work goes on.

# Settings, read from the user's own config file and nowhere else. A
# project's `.corteza/config.json` travels with the repository and is
# inside the directory the supervised agent writes to, so it gets no say
# in who supervises or how far writes may reach.
supervisor_config <- function() {
    cfg <- load_config_file(corteza_config_path("config.json"))$supervisor %||%
    list()
    list(provider = cfg$provider, model = cfg$model,
         reasoning_effort = cfg$reasoning_effort,
         thinking_budget_tokens = cfg$thinking_budget_tokens,
         # `[[`: `$thinking` on a list also matches `thinking_budget_tokens`.
         thinking = cfg[["thinking"]],
         timeout = as.numeric(cfg$timeout_sec %||% 120),
         max_turns = as.integer(cfg$max_turns %||% 6L),
         write_roots = as.character(unlist(cfg$write_roots %||% character())))
}

# Where a call goes, decided without a model. Returns `route`, one of
# "proceed" (run it), "monitor", or "human"; `flags`, what the rules
# caught; and `notes`, what they saw and left to judgment.
#
# `mode` is "monitor" where a monitor answers what the rules leave open,
# "human" where a person does. `cwd` is where the call runs and `root`
# the project it may write in: the checkout that directory belongs to.
supervisor_route <- function(call, decision, cwd = getwd(), mode = "monitor",
                             write_roots = character(),
                             root = job_checkout(cwd)) {
    tool <- call$tool %||% ""
    approval <- decision$approval %||% "ask"
    reason <- decision$reason %||% ""
    flags <- character()
    notes <- character()
    # check_safety()'s verdict is "ask", so an approver decides it. That
    # approver is a person.
    if (startsWith(reason, "safety:")) {
        flags <- c(flags,
                   sprintf("is covered by a hard safety rule (%s)", reason))
    }
    op <- classify_op(tool)
    if (tool %in% .MONITOR_EXEC_TOOLS) {
        found <- exec_scan(tool, call$args, root, cwd, write_roots)
        flags <- c(flags, found$flags)
        notes <- c(notes, found$notes)
    } else {
        sx <- scan_new(root, cwd, write_roots)
        paths <- call$paths %||% resolve_paths(call)
        for (p in paths) {
            # A tool this file does not know may write what it names.
            if (identical(op, "read")) {
                scan_read(sx, p)
            } else {
                scan_write(sx, p)
            }
        }
        if (identical(op, "write") && !length(paths)) {
            scan_flag(sx, "would modify something, but names no path this ",
                      "check could resolve")
        }
        if (identical(op, "unknown") && identical(approval, "ask")) {
            scan_note(sx, tool, " is not a tool whose effect this check knows")
        }
        flags <- c(flags, sx$flags)
        notes <- c(notes, sx$notes)
    }
    route <- if (length(flags)) {
        "human"
    } else if (identical(approval, "allow")) {
        "proceed"
    } else if (identical(mode, "monitor")) {
        "monitor"
    } else {
        "human"
    }
    list(route = route, flags = unique(flags), notes = unique(notes))
}

# A call in a line, for a record or a list of earlier calls.
supervisor_brief <- function(call, max_chars = 160L) {
    args <- as.list(call$args %||% list())
    main <- args[["command"]] %||% args[["code"]] %||% args[["path"]] %||%
    args[["file"]] %||% if (length(args)) {
        args[[1L]]
    } else {
        ""
    }
    .sanitize_inline(paste(as.character(main), collapse = " "),
                     max_chars = max_chars)
}

#' Build the supervising gate for a session nobody is watching.
#'
#' Assigned to \code{session$auto_gate}, which \code{\link{turn}} consults
#' for every call that survived \code{policy()}. Returns one of:
#' \itemize{
#'   \item \code{"proceed"}: run the call.
#'   \item \code{"refuse"}: the monitor said no; the model is told why and
#'     the turn goes on.
#'   \item \code{"declined"}: a person said no, or nobody answered in
#'     time; the model is told and the turn goes on.
#'   \item \code{"deny"}: policy already refused the call.
#' }
#' It never returns \code{"escalate"}: nothing here ends the turn.
#'
#' @param ask \code{function(route, call, decision, notes)} that gets the
#'   call answered and returns \code{list(approved, by, reason)}.
#'   \code{route} is \code{"monitor"} or \code{"human"}; \code{decision}
#'   carries the reason to show; \code{by} names who answered
#'   (\code{"monitor"} for the monitor, anything else for a person or the
#'   lack of one).
#' @param cwd The project directory, or a function returning it.
#' @param mode \code{"monitor"} or \code{"human"}; see
#'   \code{supervisor_route()}.
#' @param write_roots Directories outside the project that may be
#'   written without asking a person.
#' @param on_decision Optional \code{function(record)}, called once per
#'   call that was not simply passed through. Errors in it are ignored.
#' @return A function(call, decision) -> list(action, reason).
#' @noRd
supervisor_gate <- function(ask, cwd = getwd(), mode = "monitor",
                            write_roots = character(), on_decision = NULL) {
    force(ask)
    force(cwd)
    force(mode)
    force(write_roots)
    # One git lookup per directory, not per call.
    roots <- list()
    root_of <- function(dir) {
        if (is.null(roots[[dir]])) {
            roots[[dir]] <<- job_checkout(dir)
        }
        roots[[dir]]
    }
    function(call, decision) {
        if (identical(decision$approval %||% "", "deny")) {
            return(list(action = "deny", reason = decision$reason %||% ""))
        }
        v <- tryCatch({
            dir <- normalizePath(if (is.function(cwd)) cwd() else cwd,
                                 mustWork = FALSE)
            supervisor_route(call, decision, dir, mode, write_roots,
                             root = root_of(dir))
        },
                      # Rules that failed ruled on nothing.
                      error = function(e) {
            list(route = "human", notes = character(),
                 flags = paste0("could not be checked (", conditionMessage(e), ")"))
        })
        if (identical(v$route, "proceed")) {
            return(list(action = "proceed", reason = ""))
        }
        reason <- if (length(v$flags)) {
            paste(v$flags, collapse = "; ")
        } else {
            decision$reason %||% ""
        }
        ans <- tryCatch(
                        ask(v$route, call, list(approval = "ask", reason = reason),
                            v$notes),
                        # Nobody answered: that is never a yes.
                        error = function(e) {
            list(approved = FALSE, by = "error", reason = conditionMessage(e))
        })
        action <- if (isTRUE(ans$approved)) {
            "proceed"
        } else if (identical(ans$by, "monitor")) {
            "refuse"
        } else {
            "declined"
        }
        result <- list(action = action, reason = ans$reason %||% reason)
        if (is.function(on_decision)) {
            tryCatch(on_decision(list(
                                      tool = call$tool %||% "",
                                      brief = supervisor_brief(call),
                                      route = v$route,
                                      action = action,
                                      by = ans$by %||% "unknown",
                                      reason = result$reason,
                                      flags = v$flags,
                                      notes = v$notes)),
                     error = function(e) NULL)
        }
        result
    }
}

# ---- Decision record -----------------------------------------------------

# One line per supervised call, kept with the job it belongs to. The
# monitor is shown the tail of it, and the job's result reports the
# counts.
supervisor_log_path <- function(job_id) {
    file.path(job_dir(job_id), "supervisor.jsonl")
}

supervisor_log_append <- function(job_id, record) {
    record$at <- job_now()
    line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null")
    cat(line, "\n", sep = "", file = supervisor_log_path(job_id), append = TRUE)
    invisible(TRUE)
}

supervisor_log_read <- function(job_id) {
    path <- supervisor_log_path(job_id)
    if (!file.exists(path)) {
        return(list())
    }
    lines <- readLines(path, warn = FALSE)
    Filter(Negate(is.null), lapply(lines[nzchar(lines)], function(ln) {
        tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE),
                 error = function(e) NULL)
    }))
}

# How a job's supervised calls were answered: counts by who answered and
# how. NULL when none were.
supervisor_log_summary <- function(job_id) {
    log <- supervisor_log_read(job_id)
    if (!length(log)) {
        return(NULL)
    }
    by <- vapply(log, function(r) {
        if (identical(r$by, "monitor")) "monitor" else "person"
    }, character(1))
    ok <- vapply(log, function(r) identical(r$action, "proceed"), logical(1))
    list(monitor_approved = sum(by == "monitor" & ok),
         monitor_refused = sum(by == "monitor" & !ok),
         person_approved = sum(by == "person" & ok),
         person_declined = sum(by == "person" & !ok))
}

# The summary as a sentence for the room.
supervisor_summary_text <- function(s) {
    if (is.null(s)) {
        return(NULL)
    }
    parts <- c(
        if (s$monitor_approved) sprintf("%d approved by the monitor",
                                        s$monitor_approved),
        if (s$monitor_refused) sprintf("%d refused by the monitor",
                                       s$monitor_refused),
        if (s$person_approved) sprintf("%d approved in the room",
                                       s$person_approved),
        if (s$person_declined) sprintf("%d sent to the room and not approved",
                                       s$person_declined))
    if (!length(parts)) {
        return(NULL)
    }
    sprintf("Tool calls that needed approval: %s.", paste(parts, collapse = ", "))
}
