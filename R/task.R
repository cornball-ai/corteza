# Task Store
# SQLite-based task persistence for scheduled and recurring tasks

#' Get path to task database
#'
#' @param agent_id Agent identifier (default: "main")
#' @return Path to SQLite file
#' @noRd
task_db_path <- function(agent_id = "main") {
    dir <- file.path(get_workspace_dir(), "tasks")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.path(dir, sprintf("%s.sqlite", agent_id))
}

#' Initialize task database schema
#'
#' @param con SQLite connection
#' @return Invisible NULL
#' @noRd
task_db_init <- function(con) {
    sql_tasks <- paste0(
                        "CREATE TABLE IF NOT EXISTS tasks (",
                        "id INTEGER PRIMARY KEY AUTOINCREMENT,",
                        "name TEXT NOT NULL,",
                        "description TEXT,",
                        "schedule TEXT,",
                        "prompt TEXT NOT NULL,",
                        "status TEXT NOT NULL DEFAULT 'active',",
                        "created_at INTEGER NOT NULL,",
                        "updated_at INTEGER NOT NULL,",
                        "last_run INTEGER,",
                        "next_run INTEGER,",
                        "run_count INTEGER DEFAULT 0,",
                        "last_result TEXT,",
                        "last_error TEXT,",
                        "notification_sink TEXT DEFAULT 'console')"
    )
    DBI::dbExecute(con, sql_tasks)

    sql_runs <- paste0(
                       "CREATE TABLE IF NOT EXISTS task_runs (",
                       "id INTEGER PRIMARY KEY AUTOINCREMENT,",
                       "task_id INTEGER NOT NULL,",
                       "started_at INTEGER NOT NULL,",
                       "finished_at INTEGER,",
                       "status TEXT NOT NULL,",
                       "result TEXT,",
                       "error TEXT,",
                       "tokens_used INTEGER,",
                       "FOREIGN KEY (task_id) REFERENCES tasks(id))"
    )
    DBI::dbExecute(con, sql_runs)

    invisible(NULL)
}

#' Open task database connection
#'
#' @param agent_id Agent identifier
#' @return SQLite connection
#' @noRd
task_db_open <- function(agent_id = "main") {
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("RSQLite package required for task scheduling", call. = FALSE)
    }

    path <- task_db_path(agent_id)
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
    task_db_init(con)
    con
}

#' Close task database connection
#'
#' @param con SQLite connection
#' @noRd
task_db_close <- function(con) {
    DBI::dbDisconnect(con)
}

#' Parse cron expression to get next run time
#'
#' Simple cron parser supporting: minute hour day-of-month month day-of-week
#' Special strings: @hourly, @daily, @weekly, @monthly
#'
#' @param schedule Cron expression or special string
#' @param from Starting time (default: now)
#' @return POSIXct of next run, or NULL if invalid
#' @noRd
parse_cron_next <- function(schedule, from = Sys.time()) {
    schedule <- trimws(schedule)

    # Handle special strings
    if (schedule == "@hourly") {
        schedule <- "0 * * * *"
    } else if (schedule == "@daily") {
        schedule <- "0 8 * * *"
    } else if (schedule == "@weekly") {
        schedule <- "0 8 * * 1"
    } else if (schedule == "@monthly") {
        schedule <- "0 8 1 * *"
    }

    # Parse cron fields
    parts <- strsplit(schedule, "\\s+")[[1]]
    if (length(parts) != 5) {
        return(NULL)
    }

    minute <- parts[1]
    hour <- parts[2]

    # Simple implementation: find next matching minute/hour
    now <- as.POSIXlt(from)

    if (minute == "*") {
        target_minute <- now$min
    } else {
        target_minute <- as.integer(minute)
    }
    if (hour == "*") {
        target_hour <- now$hour
    } else {
        target_hour <- as.integer(hour)
    }

    # Build target time
    target <- now
    target$min <- target_minute
    target$sec <- 0

    if (hour != "*") {
        target$hour <- target_hour
    }

    # If target is in the past, advance appropriately
    if (as.POSIXct(target) <= from) {
        if (hour == "*") {
            target$hour <- target$hour + 1
        } else {
            target$mday <- target$mday + 1
        }
    }

    as.POSIXct(target)
}

#' Create a new scheduled task
#'
#' @param name Task name
#' @param prompt Prompt to send to agent
#' @param schedule Cron expression or special string
#' @param description Optional description
#' @param notification_sink Where to send output: console, file, signal
#' @param agent_id Agent identifier
#' @return Task ID
#' @export
task_create <- function(name, prompt, schedule = NULL, description = NULL,
                        notification_sink = "console", agent_id = "main") {
    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    now <- as.integer(Sys.time())
    next_run <- if (!is.null(schedule)) {
        as.integer(parse_cron_next(schedule))
    } else {
        NULL
    }

    sql <- paste0(
                  "INSERT INTO tasks (name, description, schedule, prompt, status,",
                  " created_at, updated_at, next_run, notification_sink)",
                  " VALUES (?, ?, ?, ?, 'active', ?, ?, ?, ?)"
    )
    DBI::dbExecute(con, sql, params = list(name, description, schedule, prompt,
            now, now, next_run, notification_sink))

    DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
}

#' List tasks
#'
#' @param status Filter by status: active, paused, completed, all (default: active)
#' @param agent_id Agent identifier
#' @return Data frame of tasks
#' @export
task_list <- function(status = "active", agent_id = "main") {
    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    if (status == "all") {
        DBI::dbGetQuery(con, "SELECT * FROM tasks ORDER BY next_run ASC")
    } else {
        DBI::dbGetQuery(con, "SELECT * FROM tasks WHERE status = ? ORDER BY next_run ASC",
                        params = list(status))
    }
}

#' Get a task by ID
#'
#' @param task_id Task ID
#' @param agent_id Agent identifier
#' @return Task list, or NULL if not found
#' @export
task_get <- function(task_id, agent_id = "main") {
    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    result <- DBI::dbGetQuery(con, "SELECT * FROM tasks WHERE id = ?",
                              params = list(task_id))

    if (nrow(result) == 0) {
        return(NULL)
    }

    as.list(result[1,])
}

#' Update a task
#'
#' @param task_id Task ID
#' @param ... Fields to update (name, description, schedule, prompt, status, notification_sink)
#' @param agent_id Agent identifier
#' @return Invisible TRUE on success
#' @export
task_update <- function(task_id, ..., agent_id = "main") {
    updates <- list(...)
    if (length(updates) == 0) {
        return(invisible(TRUE))
    }

    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    # Build update SQL
    set_clauses <- character()
    params <- list()

    for (field in names(updates)) {
        if (field %in% c("name", "description", "schedule", "prompt",
                         "status", "notification_sink")) {
            set_clauses <- c(set_clauses, sprintf("%s = ?", field))
            params <- c(params, list(updates[[field]]))

            # Recalculate next_run if schedule changed
            if (field == "schedule" && !is.null(updates[[field]])) {
                next_run <- as.integer(parse_cron_next(updates[[field]]))
                set_clauses <- c(set_clauses, "next_run = ?")
                params <- c(params, list(next_run))
            }
        }
    }

    set_clauses <- c(set_clauses, "updated_at = ?")
    params <- c(params, list(as.integer(Sys.time())))
    params <- c(params, list(task_id))

    sql <- sprintf("UPDATE tasks SET %s WHERE id = ?",
                   paste(set_clauses, collapse = ", "))
    DBI::dbExecute(con, sql, params = params)

    invisible(TRUE)
}

#' Delete a task
#'
#' @param task_id Task ID
#' @param agent_id Agent identifier
#' @return Invisible TRUE on success
#' @export
task_delete <- function(task_id, agent_id = "main") {
    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    DBI::dbExecute(con, "DELETE FROM task_runs WHERE task_id = ?",
                   params = list(task_id))
    DBI::dbExecute(con, "DELETE FROM tasks WHERE id = ?",
                   params = list(task_id))

    invisible(TRUE)
}

#' Pause a task
#'
#' @param task_id Task ID
#' @param agent_id Agent identifier
#' @return Invisible TRUE
#' @export
task_pause <- function(task_id, agent_id = "main") {
    task_update(task_id, status = "paused", agent_id = agent_id)
}

#' Resume a paused task
#'
#' @param task_id Task ID
#' @param agent_id Agent identifier
#' @return Invisible TRUE
#' @export
task_resume <- function(task_id, agent_id = "main") {
    task_update(task_id, status = "active", agent_id = agent_id)
}

#' Record a task run
#'
#' @param task_id Task ID
#' @param status Run status: success, error
#' @param result Result text (for success)
#' @param error Error message (for error)
#' @param tokens_used Tokens used (optional)
#' @param agent_id Agent identifier
#' @return Run ID
#' @noRd
task_record_run <- function(task_id, status, result = NULL, error = NULL,
                            tokens_used = NULL, agent_id = "main") {
    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    now <- as.integer(Sys.time())

    sql_insert <- paste0(
                         "INSERT INTO task_runs (task_id, started_at, finished_at, status,",
                         " result, error, tokens_used) VALUES (?, ?, ?, ?, ?, ?, ?)"
    )
    DBI::dbExecute(con, sql_insert,
                   params = list(task_id, now, now, status, result, error, tokens_used))

    # Update task metadata
    task <- task_get(task_id, agent_id)
    next_run <- if (!is.null(task$schedule)) {
        as.integer(parse_cron_next(task$schedule))
    } else {
        NULL
    }

    sql_update <- paste0(
                         "UPDATE tasks SET last_run = ?, next_run = ?,",
                         " run_count = run_count + 1, last_result = ?,",
                         " last_error = ?, updated_at = ? WHERE id = ?"
    )
    DBI::dbExecute(con, sql_update,
                   params = list(now, next_run, result, error, now, task_id))

    DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
}

#' Get runs for a task
#'
#' @param task_id Task ID
#' @param limit Max runs to return (default: 10)
#' @param agent_id Agent identifier
#' @return Data frame of runs
#' @export
task_runs <- function(task_id, limit = 10L, agent_id = "main") {
    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    sql <- paste0(
                  "SELECT * FROM task_runs WHERE task_id = ?",
                  " ORDER BY started_at DESC LIMIT ?"
    )
    DBI::dbGetQuery(con, sql, params = list(task_id, limit))
}

#' Get tasks due for execution
#'
#' @param agent_id Agent identifier
#' @return Data frame of due tasks
#' @noRd
task_get_due <- function(agent_id = "main") {
    con <- task_db_open(agent_id)
    on.exit(task_db_close(con))

    now <- as.integer(Sys.time())

    sql <- paste0(
                  "SELECT * FROM tasks WHERE status = 'active'",
                  " AND next_run IS NOT NULL AND next_run <= ?",
                  " ORDER BY next_run ASC"
    )
    DBI::dbGetQuery(con, sql, params = list(now))
}

#' Format task list for display
#'
#' @param tasks Data frame from task_list()
#' @return Character string for display
#' @noRd
format_task_list <- function(tasks) {
    if (nrow(tasks) == 0) {
        return("No scheduled tasks.")
    }

    format_time <- function(ts) {
        if (is.na(ts) || is.null(ts)) {
            return("--")
        }
        format(as.POSIXct(ts, origin = "1970-01-01"), "%Y-%m-%d %H:%M")
    }

    lines <- c("Scheduled tasks:")

    for (i in seq_len(nrow(tasks))) {
        t <- tasks[i,]
        status_marker <- switch(t$status,
                                active = "",
                                paused = " [PAUSED]",
                                completed = " [DONE]",
                                ""
        )
        next_str <- if (!is.na(t$next_run)) {
            sprintf(" -> next: %s", format_time(t$next_run))
        } else {
            ""
        }
        lines <- c(lines, sprintf("  [%d] %s%s%s",
                                  t$id, t$name, status_marker, next_str))
        if (!is.null(t$schedule) && !is.na(t$schedule)) {
            lines <- c(lines, sprintf("      schedule: %s", t$schedule))
        }
    }

    paste(lines, collapse = "\n")
}

