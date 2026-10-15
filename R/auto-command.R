.auto_filter_tools <- function(tools, config = NULL) {
    if (is.null(config)) {
        return(tools)
    }
    auto <- get_auto_config(config)
    Filter(function(tool) {
        name <- unsanitize_tool_name(tool$name %||% "")
        if (name %in% auto$never_broker) return(FALSE)
        if (name %in% c("edit_context", "task_create", "task_update",
                        "exit_plan_mode")) return(TRUE)
        op <- classify_op(name)
        op %in% c("read", "write") ||
        (identical(op, "exec") && isTRUE(auto$allow_exec))
    }, tools)
}

.auto_status <- function(ctx) {
    last <- ctx$last_auto_state
    if (is.null(last)) {
        return("Auto is off. /auto continues the current task; /auto <goal> starts a new run.")
    }
    paste0("Auto is off. Last run: ", last$stop_reason, "\n",
           "Use /auto resume to continue. /auto --exec resume also permits monitored shell/R execution.")
}

.auto_command <- function(ctx, text) {
    spec <- parse_auto_flags(text)
    if (identical(spec$goal, "status")) {
        cat(.auto_status(ctx), "\n")
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
                  continuous = is.null(spec$loops))
    invisible(NULL)
}
