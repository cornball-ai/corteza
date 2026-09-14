# Supervised persistent R execution.
#
# The historical tool_run_r() path intentionally evaluates in the caller's
# process and remains available for embedded sessions and direct callers. This
# file adds a session-owned callr worker for unattended runtimes that need a
# real wall-clock bound without throwing a delayed setTimeLimit interrupt into
# unrelated host code.

# Process-local child state. Each callr worker loads its own namespace, so this
# environment cannot be shared across sessions or leak into the host process.
.run_r_worker_state <- new.env(parent = emptyenv())
.run_r_worker_rehome_attr <- "corteza_worker_rehome"

#' Resolve the configured run_r execution mode.
#' @noRd
.run_r_mode <- function(ctx = list()) {
    session <- ctx$session
    config <- session$config %||% ctx$config %||% list()
    mode <- config$run_r_mode %||% "in_process"
    if (!is.character(mode) || length(mode) != 1L || is.na(mode) ||
        !mode %in% c("in_process", "worker")) {
        stop("run_r_mode must be 'in_process' or 'worker'", call. = FALSE)
    }
    mode
}

#' Validate one positive finite timeout value.
#' @noRd
.run_r_timeout_value <- function(x, name) {
    valid <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x > 0
    if (!isTRUE(valid)) {
        stop(name, " must be a single positive finite number of seconds",
             call. = FALSE)
    }
    if (x > floor(.Machine$integer.max / 1000)) {
        stop(name, " exceeds the supported wall-clock limit", call. = FALSE)
    }
    as.numeric(x)
}

#' Resolve a model request against host-owned timeout bounds.
#' @noRd
.run_r_timeout <- function(requested = NULL, ctx = list()) {
    session <- ctx$session
    config <- session$config %||% ctx$config %||% list()
    default <- .run_r_timeout_value(config$skill_timeout %||% 30,
                                    "skill_timeout")
    maximum <- .run_r_timeout_value(config$skill_timeout_max %||% 1800,
                                    "skill_timeout_max")
    if (default > maximum) {
        stop("skill_timeout must not exceed skill_timeout_max", call. = FALSE)
    }
    value <- if (is.null(requested)) {
        default
    } else {
        .run_r_timeout_value(requested, "timeout")
    }
    if (value > maximum) {
        stop(sprintf("timeout exceeds the host maximum of %s seconds",
                     format(maximum)), call. = FALSE)
    }
    # A durable host such as ARC can provide a tighter, per-call deadline
    # derived from an authoritative external lease. It can only narrow.
    session_cap <- session$run_r_timeout_cap
    if (is.function(session_cap)) {
        session_cap <- session_cap()
    }
    for (cap in list(ctx$timeout_cap, session_cap)) {
        if (!is.null(cap)) {
            if (is.numeric(cap) && length(cap) == 1L && !is.na(cap) && cap == 0) {
                stop("No run_r execution time remains under the host deadline",
                     call. = FALSE)
            }
            cap <- .run_r_timeout_value(cap, "timeout_cap")
            value <- min(value, cap)
        }
    }
    value
}

#' Child-side initialization for a supervised run_r worker.
#' @noRd
.run_r_worker_child_init <- function(cwd) {
    worker_init(cwd)
    .run_r_worker_state$workspace <- new.env(parent = globalenv())
    invisible(TRUE)
}

#' Child-side evaluation. Interrupts become ordinary structured results so the
#' worker remains alive and any assignments completed before interruption stay
#' available for inspection.
#' @noRd
.run_r_worker_child_eval <- function(code, bindings = list()) {
    env <- .run_r_worker_state$workspace
    if (length(bindings)) {
        for (name in names(bindings)) {
            value <- bindings[[name]]
            if (is.function(value) &&
                isTRUE(attr(value, .run_r_worker_rehome_attr, exact = TRUE))) {
                attr(value, .run_r_worker_rehome_attr) <- NULL
                environment(value) <- env
            }
            assign(name, value, envir = env)
        }
    }
    before <- ls(env, all.names = TRUE)
    started <- Sys.time()
    interrupted <- FALSE
    result <- tryCatch(
                       tool_run_r(code, envir = env),
                       interrupt = function(e) {
        interrupted <<- TRUE
        err("run_r was interrupted by its host deadline")
    }
    )
    after <- ls(env, all.names = TRUE)
    list(
         result = result,
         interrupted = interrupted,
         elapsed_seconds = as.numeric(difftime(Sys.time(), started,
                units = "secs")),
         workspace = list(
                          added = setdiff(after, before),
                          removed = setdiff(before, after)
        )
    )
}

#' Child-side handle reader for a supervised workspace.
#' @noRd
.run_r_worker_child_read_handle <- function(handle, op) {
    env <- .run_r_worker_state$workspace
    store <- handle_store_for(env)
    value <- get_handle(handle, store = store)
    if (is.null(value) && !exists(handle, envir = store, inherits = FALSE)) {
        return(err(sprintf("Unknown handle: %s", handle)))
    }
    text <- tryCatch(
                     switch(op, str = utils::capture.output(utils::str(value)),
                            head = utils::capture.output(utils::head(value)),
                            summary = utils::capture.output(summary(value)),
                            print = utils::capture.output(print(value)),
                            return(err(sprintf("Unknown op: %s", op)))),
                     error = function(e) paste("Error:", conditionMessage(e))
    )
    ok(paste(text, collapse = "\n"))
}

#' Resolve a requested subagent artifact in its supervised workspace.
#' @noRd
.run_r_worker_return_value <- function(session, name) {
    if (!.run_r_worker_is_alive(session)) {
        return(list(found = FALSE, value = NULL))
    }
    session$.run_r_worker$run(function(id) {
        ns <- asNamespace("corteza")
        state <- get(".run_r_worker_state", envir = ns)
        env <- state$workspace
        store <- get("handle_store_for", envir = ns)(env)
        if (exists(id, envir = store, inherits = FALSE)) {
            return(list(found = TRUE,
                        value = get("get_handle", envir = ns)(id, store = store)))
        }
        if (exists(id, envir = env, inherits = FALSE)) {
            return(list(found = TRUE, value = get(id, envir = env, inherits = FALSE)))
        }
        list(found = FALSE, value = NULL)
    }, list(id = name))
}

#' Child-side atomic checkpoint for a supervised workspace.
#' @noRd
.run_r_worker_child_save <- function(path, exclude = character()) {
    env <- .run_r_worker_state$workspace
    objects <- setdiff(ls(env, all.names = TRUE), exclude)
    objects <- objects[!grepl("^\\.h_[0-9]+$", objects)]
    snapshot <- new.env(parent = emptyenv())
    for (name in objects) {
        value <- get(name, envir = env, inherits = FALSE)
        if (is.function(value) && identical(environment(value), env)) {
            environment(value) <- emptyenv()
            attr(value, .run_r_worker_rehome_attr) <- TRUE
        }
        assign(name, value, envir = snapshot)
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(path, ".tmp-", Sys.getpid())
    on.exit(unlink(tmp), add = TRUE)
    save(list = objects, envir = snapshot, file = tmp)
    if (!isTRUE(file.rename(tmp, path))) {
        stop("could not atomically replace run_r workspace checkpoint",
             call. = FALSE)
    }
    objects
}

#' Is a session's supervised worker alive?
#' @noRd
.run_r_worker_is_alive <- function(session) {
    if (!is.environment(session) || is.null(session$.run_r_worker)) {
        return(FALSE)
    }
    isTRUE(tryCatch(session$.run_r_worker$is_alive(),
                    error = function(e) FALSE))
}

#' Atomically checkpoint a live supervised workspace.
#'
#' This is a host-side durability primitive, not a model tool. It writes from
#' inside the worker, avoiding serialization of large R objects through the
#' callr control channel. Returns the saved object names, or NULL when the
#' session has no live worker. Callers must not replace a prior checkpoint with
#' a stale parent copy when this function errors.
#' @noRd
.run_r_worker_save <- function(session, path, exclude = character()) {
    if (!.run_r_worker_is_alive(session)) {
        return(NULL)
    }
    if (!is.character(path) || length(path) != 1L || is.na(path) ||
        !nzchar(path)) {
        stop("worker checkpoint path must be one non-empty string",
             call. = FALSE)
    }
    if (!is.character(exclude) || anyNA(exclude)) {
        stop("worker checkpoint exclusions must be character", call. = FALSE)
    }
    session$.run_r_worker$run(
                              function(target, skipped) {
        save_worker <- get(".run_r_worker_child_save",
                           envir = asNamespace("corteza"),
                           inherits = FALSE)
        save_worker(target, skipped)
    },
                              list(target = path, skipped = exclude)
    )
}

#' Close a session's supervised worker, if one exists.
#' @noRd
.run_r_worker_close <- function(session) {
    if (!is.environment(session)) {
        return(invisible(FALSE))
    }
    worker <- session$.run_r_worker
    if (!is.null(worker)) {
        tryCatch(worker$close(), error = function(e) NULL)
        session$.run_r_worker <- NULL
        return(invisible(TRUE))
    }
    invisible(FALSE)
}

#' Start or return a live session-owned worker.
#' @noRd
.run_r_worker <- function(session, cwd = getwd()) {
    if (!is.environment(session)) {
        stop("worker run_r requires a session environment", call. = FALSE)
    }
    worker <- session$.run_r_worker
    if (!is.null(worker) && isTRUE(tryCatch(worker$is_alive(),
                error = function(e) FALSE))) {
        return(worker)
    }
    if (!is.null(worker)) {
        tryCatch(worker$close(), error = function(e) NULL)
    }
    worker <- callr::r_session$new(wait = TRUE)
    initialized <- tryCatch({
        worker$run(
                   function(path) {
            library(corteza)
            init <- get(".run_r_worker_child_init",
                        envir = asNamespace("corteza"), inherits = FALSE)
            init(path)
        },
                   list(path = cwd)
        )
        TRUE
    }, error = function(e) {
        tryCatch(worker$close(), error = function(close_error) NULL)
        stop("Failed to initialize run_r worker: ", conditionMessage(e),
             call. = FALSE)
    })
    if (!isTRUE(initialized)) {
        stop("Failed to initialize run_r worker", call. = FALSE)
    }
    session$.run_r_worker <- worker
    session$.run_r_worker_generation <-
    as.integer(session$.run_r_worker_generation %||% 0L) + 1L
    worker
}

#' Attach structured execution metadata without changing the MCP text contract.
#' @noRd
.run_r_execution_result <- function(result, status, timeout, generation,
                                    state_retained, details = list()) {
    result$execution <- c(list(status = status, timeout_seconds = timeout,
                               worker_generation = generation,
                               state_retained = isTRUE(state_retained)), details)
    result
}

#' Execute one expression through the session-owned worker.
#' @noRd
.run_r_worker_execute <- function(code, timeout, ctx = list()) {
    session <- ctx$session
    cwd <- ctx$cwd %||% session$cwd %||% getwd()
    worker <- .run_r_worker(session, cwd)
    generation <- session$.run_r_worker_generation
    bindings <- ctx$run_r_bindings %||% list()
    if (!is.list(bindings) || (length(bindings) &&
            (is.null(names(bindings)) || any(!nzchar(names(bindings)))))) {
        return(err("run_r_bindings must be a fully named list"))
    }

    call_error <- tryCatch({
        worker$call(
                    function(src, values) {
            evaluate <- get(".run_r_worker_child_eval",
                            envir = asNamespace("corteza"), inherits = FALSE)
            evaluate(src, values)
        },
                    list(src = code, values = bindings)
        )
        NULL
    }, error = function(e) e)
    if (inherits(call_error, "error")) {
        alive <- isTRUE(tryCatch(worker$is_alive(), error = function(e) FALSE))
        if (!alive) {
            .run_r_worker_close(session)
        }
        return(.run_r_execution_result(
                                       err(paste("run_r worker failed:", conditionMessage(call_error))),
                                       "error", timeout, generation, alive
            ))
    }
    state <- worker$poll_process(as.integer(ceiling(timeout * 1000)))
    timed_out <- !identical(state, "ready")
    if (timed_out) {
        tryCatch(worker$interrupt(), error = function(e) NULL)
        state <- worker$poll_process(2000L)
    }

    if (!identical(state, "ready")) {
        .run_r_worker_close(session)
        text <- sprintf(paste(
                              "run_r timed out after %s seconds and did not stop cleanly.",
                              "The worker was terminated, so its in-memory workspace was lost.",
                              "Before retrying, reduce or split the computation, reuse a helper,",
                              "parallelize only independent work, or use an approximation."
            ), format(timeout))
        return(.run_r_execution_result(err(text), "killed", timeout,
                                       generation, FALSE))
    }

    msg <- worker$read()
    if (!is.null(msg$error)) {
        return(.run_r_execution_result(
                                       err(paste("run_r worker failed:", conditionMessage(msg$error))),
                                       "error", timeout, generation, TRUE
            ))
    }
    payload <- msg$result
    if (timed_out || isTRUE(payload$interrupted)) {
        text <- sprintf(paste(
                              "run_r timed out after %s seconds.",
                              "The worker remains available, but assignments completed before",
                              "the interrupt may persist. Inspect state before retrying. Optimize",
                              "or reuse helpers, reduce or split the work, parallelize only safe",
                              "independent work, or approximate when exact work is unnecessary."
            ), format(timeout))
        return(.run_r_execution_result(
                                       err(text), "timeout", timeout, generation, TRUE,
                                       list(elapsed_seconds = payload$elapsed_seconds,
                    workspace = payload$workspace)
            ))
    }
    .run_r_execution_result(
                            payload$result, "ok", timeout, generation, TRUE,
                            list(elapsed_seconds = payload$elapsed_seconds,
                                 workspace = payload$workspace)
    )
}

#' Model-facing run_r dispatcher. Direct tool_run_r() callers retain the
#' historical in-process contract; sessions may opt into worker mode.
#' @noRd
.tool_run_r_session <- function(code, timeout = NULL, ctx = list()) {
    if (!identical(.run_r_mode(ctx), "worker")) {
        if (!is.null(timeout)) {
            return(err(paste("A run_r timeout requires run_r_mode = 'worker';",
                             "the in-process compatibility mode cannot enforce it safely.")))
        }
        return(tool_run_r(code))
    }
    effective <- tryCatch(.run_r_timeout(timeout, ctx), error = function(e) e)
    if (inherits(effective, "error")) {
        return(err(conditionMessage(effective)))
    }
    .run_r_worker_execute(code, effective, ctx)
}

#' Model-facing handle reader that follows worker-owned handles.
#' @noRd
.tool_read_handle_session <- function(handle, op = "str", ctx = list()) {
    if (!identical(.run_r_mode(ctx), "worker")) {
        return(tool_read_handle(handle, op))
    }
    session <- ctx$session
    worker <- session$.run_r_worker
    if (is.null(worker) || !isTRUE(tryCatch(worker$is_alive(),
                error = function(e) FALSE))) {
        return(err(sprintf("Unknown handle: %s", handle)))
    }
    result <- tryCatch(
                       worker$run(
                                  function(id, action) {
        read_handle <- get(".run_r_worker_child_read_handle",
                           envir = asNamespace("corteza"), inherits = FALSE)
        read_handle(id, action)
    },
                                  list(id = handle, action = op)
        ),
                       error = function(e) err(paste("read_handle worker failed:",
                conditionMessage(e)))
    )
    result
}
