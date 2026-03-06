# Scheduler
# Runs scheduled tasks and handles notifications

#' Run the scheduler once
#'
#' Checks for due tasks and executes them.
#'
#' @param agent_id Agent identifier
#' @param config Config list
#' @param verbose Print status messages
#' @return Number of tasks executed
#' @export
scheduler_run_once <- function(agent_id = "main", config = NULL,
                               verbose = TRUE) {
    if (is.null(config)) {
        config <- load_config(getwd())
    }

    due_tasks <- task_get_due(agent_id)

    if (nrow(due_tasks) == 0) {
        if (verbose) {
            message("No tasks due")
        }
        return(0L)
    }

    if (verbose) {
        message(sprintf("Found %d tasks due", nrow(due_tasks)))
    }

    executed <- 0L

    for (i in seq_len(nrow(due_tasks))) {
        task <- as.list(due_tasks[i,])

        if (verbose) {
            message(sprintf("Running task: %s", task$name))
        }

        result <- tryCatch({
            scheduler_execute_task(task, config, verbose)
        }, error = function(e) {
            list(success = FALSE, error = e$message)
        })

        # Record run
        if (result$success) {
            task_record_run(task$id, "success",
                            result = result$result,
                            tokens_used = result$tokens_used,
                            agent_id = agent_id)
            executed <- executed + 1L
        } else {
            task_record_run(task$id, "error",
                            error = result$error,
                            agent_id = agent_id)
        }

        # Send notification
        scheduler_notify(task, result, config)
    }

    executed
}

#' Execute a single task
#'
#' @param task Task list from task_get()
#' @param config Config list
#' @param verbose Print status
#' @return List with success, result/error, tokens_used
#' @noRd
scheduler_execute_task <- function(task, config, verbose = TRUE) {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        return(list(success = FALSE, error = "llm.api package not installed"))
    }

    provider <- config$provider %||% "anthropic"
    model <- config$model

    # Build system prompt
    system_prompt <- sprintf(
                             "You are running a scheduled task.\n\nTask: %s\nDescription: %s\n\n%s",
                             task$name,
                             task$description %||% "No description",
                             "Complete the task and respond with results."
    )

    result <- tryCatch({
        resp <- llm.api::chat(
                              prompt = task$prompt,
                              provider = provider,
                              model = model,
                              system = system_prompt,
                              temperature = 0.3
        )
        list(
             success = TRUE,
             result = resp$content,
             tokens_used = resp$usage$total_tokens %||% 0L
        )
    }, error = function(e) {
        list(success = FALSE, error = e$message)
    })

    result
}

#' Send notification for task result
#'
#' @param task Task list
#' @param result Execution result
#' @param config Config list
#' @return Invisible NULL
#' @noRd
scheduler_notify <- function(task, result, config) {
    sink <- task$notification_sink %||% "console"

    message_text <- if (result$success) {
        sprintf("[Task: %s] Completed successfully\n\n%s",
                task$name, result$result %||% "")
    } else {
        sprintf("[Task: %s] Failed: %s", task$name,
                result$error %||% "Unknown error")
    }

    switch(sink,
           console = {
        message(message_text)
    },
           file = {
        path <- config$notifications$file$path %||%
        file.path(get_workspace_dir(), "notifications.log")
        cat(sprintf("[%s] %s\n", format(Sys.time()), message_text),
            file = path, append = TRUE)
    },
           signal = {
        # Use Signal transport if configured
        tryCatch({
            sig_config <- config$channels$signal
            if (!is.null(sig_config) && isTRUE(sig_config$enabled)) {
                transport <- transport_signal(sig_config)
                recipient <- config$notifications$signal$recipient %||%
                sig_config$account
                msg <- message_normalize(
                    text = message_text,
                    sender = "scheduler",
                    channel = "signal",
                    metadata = list(reply_to = recipient)
                )
                transport$send(msg)
            }
        }, error = function(e) {
            message(sprintf("Failed to send Signal notification: %s",
                            e$message))
        })
    }
    )

    invisible(NULL)
}

#' Run the scheduler daemon
#'
#' Continuously checks for and runs due tasks.
#'
#' @param agent_id Agent identifier
#' @param config Config list
#' @param check_interval_secs Seconds between checks (default: 60)
#' @param verbose Print status messages
#' @export
scheduler_daemon <- function(agent_id = "main", config = NULL,
                             check_interval_secs = 60L, verbose = TRUE) {
    if (is.null(config)) {
        config <- load_config(getwd())
    }

    if (verbose) {
        message("Scheduler daemon starting...")
    }

    tryCatch({
        while (TRUE) {
            scheduler_run_once(agent_id, config, verbose)
            Sys.sleep(check_interval_secs)
        }
    }, interrupt = function(e) {
        if (verbose) message("\nScheduler daemon stopped")
    })

    invisible(NULL)
}

