# May an /auto run call `name`? Asked at the call (turn()'s handler):
# the tool list stays the session's while the run goes (WP14). `tool`
# is the definition the session sends, which says whether a write names
# its target.
.auto_tool_allowed <- function(name, config, table = NULL, tool = NULL) {
    auto <- get_auto_config(config)
    if (name %in% auto$never_broker) {
        return(FALSE)
    }
    if (name %in% c("edit_context", "task_create", "task_update",
                    "exit_plan_mode")) {
        return(TRUE)
    }
    op <- classify_op(name, table)
    identical(op, "read") ||
    (identical(op, "write") && .auto_names_target(name, tool, table)) ||
    (identical(op, "exec") && isTRUE(auto$allow_exec))
}

# The note the next prompt opens with when an /auto run starts or ends
# (R/request-prefix.R). Nobody answers during the run, so asking first
# or waiting for a plan's approval would hang it.
.auto_note <- function(on) {
    if (isTRUE(on)) {
        return(paste("[An unattended /auto run has started: nobody answers",
                     "until it ends. Do not stop to ask clarifying questions",
                     "or to wait for plan approval: make reasonable",
                     "assumptions and proceed. The plan you propose is",
                     "accepted automatically. A call outside the run's",
                     "scope is refused, and the refusal says why.]"))
    }
    paste("[The /auto run has ended. A person answers again: ask before you",
          "assume, and wait for a plan's approval.]")
}

# Whether a write tool has an argument the envelope reads as its target.
# monitor_in_envelope() refuses a write whose target it cannot resolve,
# so a write that can name none (harness_note) is not offered to a run
# only the envelope approves: each call would end the run.
.auto_names_target <- function(name, tool, table = NULL) {
    props <- tool$input_schema$properties %||%
    tool_entry(name, table)$inputSchema$properties %||% list()
    if (.args_named_by_corteza(name, table)) {
        return(any(names(props) %in% .path_arg_keys))
    }
    any(vapply(props, function(p) identical(p$type, "string"), logical(1)))
}

.auto_status <- function(ctx) {
    last <- ctx$last_auto_state
    if (is.null(last)) {
        return("Auto is off. /auto continues the current task; /auto <goal> starts a new run.")
    }
    paste0("Auto is off. Last run: ", last$stop_reason, "\n",
           "Use /auto resume to continue. /auto --exec resume also permits monitored shell/R execution.")
}

# An /auto run works with the session's own tools, under its hall
# monitor (R/monitor.R). A talker's tools only read and hand work on,
# and a doer's calls are answered by its conversation, not by the run:
# the run would do nothing. So a talker session does not start one.
.auto_talker_text <- function() {
    paste("/auto runs the session's own tools unattended, and a talker",
          "session's tools only read and hand work to the doer. For /auto,",
          "turn talker mode off: \"talker\": {\"enabled\": false} in the",
          "corteza config.")
}

.auto_command <- function(ctx, text) {
    spec <- parse_auto_flags(text)
    # What the person typed after /auto, flags aside: the run's goal, or
    # nothing, or "resume". The goal stood in below for nothing is
    # corteza's, and the transcript keeps the person's words apart from
    # it (run_auto_loop()).
    typed <- spec$goal
    if (identical(spec$goal, "status")) {
        .remote_emit(ctx, .auto_status(ctx))
        cat(.auto_status(ctx), "\n")
        return(invisible(NULL))
    }
    if (isTRUE(ctx$session$talker)) {
        .remote_emit(ctx, .auto_talker_text())
        cat(.auto_talker_text(), "\n")
        return(invisible(NULL))
    }
    if (identical(spec$goal, "resume")) {
        previous <- ctx$last_auto_request
        if (is.null(previous)) {
            cat("No previous auto run in this process. Use /auto to continue the current conversation.\n")
            return(invisible(NULL))
        }
        spec$goal <- previous$goal
        spec$loops <- spec$loops %||% previous$loops
        spec$allow_exec <- spec$allow_exec %||% previous$allow_exec
    }
    if (!nzchar(spec$goal)) {
        if (!length(ctx$session$history %||% list()) &&
            !nzchar(ctx$last_assistant_response %||% "")) {
            cat("There is no current task yet. Use /auto <goal> to start one.\n")
            return(invisible(NULL))
        }
        spec$goal <- paste("Continue the current task in this conversation,",
                           "following its established requirements and approved plan.")
    }
    ctx$last_auto_request <- spec
    run_auto_loop(ctx, spec$goal, max_loops = spec$loops,
                  allow_exec = spec$allow_exec,
                  continuous = is.null(spec$loops), typed = typed)
    invisible(NULL)
}
