# Immutable, per-attempt state. The shared warm-worker checkpoint is never a
# resume source: later work can replace it. A directory rename publishes all
# files together; hashes catch missing or corrupted files before restoration.
job_resume_path <- function(id) {
    job_check_id(id)
    file.path(job_dir(id), "resume")
}

.job_resume_publish <- function(stage, path) file.rename(stage, path)

.job_resume_unsafe <- function(x, root, seen = list(), depth = 0L) {
    if (depth > 100L || inherits(x, "connection") ||
        typeof(x) %in% c("externalptr", "weakref")) {
        return(TRUE)
    }
    if (is.function(x)) {
        return(.job_resume_unsafe(environment(x), root, seen, depth + 1L))
    }
    if (is.environment(x)) {
        if (identical(x, root) || identical(x, emptyenv()) ||
            identical(x, baseenv()) ||
            isNamespace(x) || startsWith(environmentName(x), "package:") ||
            any(vapply(seen, identical, TRUE, x))) {
            return(FALSE)
        }
        seen <- c(seen, list(x))
        keys <- ls(x, all.names = TRUE)
        if (any(vapply(keys, bindingIsActive, TRUE, env = x))) {
            return(TRUE)
        }
        return(any(vapply(mget(keys, x, inherits = FALSE), .job_resume_unsafe,
                          TRUE, root = root, seen = seen, depth = depth + 1L)) ||
               .job_resume_unsafe(parent.env(x), root, seen, depth + 1L))
    }
    if (isS4(x)) {
        return(.job_resume_unsafe(attributes(x), root, seen, depth + 1L))
    }
    if (is.list(x) ||
        is.pairlist(x)) return(any(vapply(x, .job_resume_unsafe, TRUE,
                root = root, seen = seen, depth = depth + 1L)))
    if (length(attributes(x))) {
        return(.job_resume_unsafe(attributes(x), root, seen, depth + 1L))
    }
    FALSE
}

.job_resume_save <- function(job_id, identity, session, env = globalenv()) {
    if (!is.environment(session) || !length(session$history)) {
        stop("No completed conversation history is available", call. = FALSE)
    }
    path <- job_resume_path(job_id)
    if (dir.exists(path)) {
        stop("A resume snapshot already exists", call. = FALSE)
    }
    stage <- tempfile(".resume-", tmpdir = job_dir(job_id))
    dir.create(stage, mode = "0700")
    on.exit(unlink(stage, recursive = TRUE), add = TRUE)
    keys <- ls(env, all.names = TRUE)
    omitted <- keys[vapply(keys, function(n) {
        bindingIsActive(n, env) || grepl("^\\.[ho]_[0-9]+$", n) ||
        .job_resume_unsafe(get(n, env, inherits = FALSE), env)
    }, TRUE)]
    objects <- .workspace_checkpoint_write(env,
        file.path(stage, "workspace.RData"), omitted)
    saveRDS(session$history, file.path(stage, "history.rds"))
    files <- c("workspace.RData", "history.rds")
    hashes <- as.list(vapply(file.path(stage, files), digest::digest, "",
                             algo = "sha256", file = TRUE))
    names(hashes) <- files
    info <- list(format = 1L, job = job_id, identity = identity, at = job_now(),
                 provider = session$last_route$provider %||% session$provider,
                 model = session$last_route$model %||% .resolve_model(session),
                 objects = as.list(objects), omitted = as.list(omitted),
                 packages = as.list(sub("^package:", "", grep("^package:", search(), value = TRUE))),
                 hashes = hashes)
    job_write_file(file.path(stage, "ready.json"), info)
    Sys.chmod(file.path(stage, c(files, "ready.json")), "0600")
    if (!isTRUE(.job_resume_publish(stage, path))) {
        stop("Could not publish resume snapshot", call. = FALSE)
    }
    list(ready = TRUE, omitted = as.list(omitted), at = info$at)
}

job_resume_snapshot <- function(id, identity) {
    path <- job_resume_path(id)
    info <- job_read_file(file.path(path, "ready.json"))
    if (is.null(info) || !identical(info$format, 1L) ||
        !identical(info$job, id) ||
        !identical(job_identity_text(info$identity),
                   job_identity_text(identity))) {
        stop("Resume snapshot is missing or belongs to a different worker", call. = FALSE)
    }
    for (name in c("history.rds", "workspace.RData")) {
        file <- file.path(path, name)
        if (!file.exists(file) || !identical(info$hashes[[name]],
                digest::digest(file, algo = "sha256", file = TRUE))) {
            stop("Resume snapshot failed its integrity check", call. = FALSE)
        }
    }
    info
}

.job_resume_restore <- function(id, spec, session) {
    info <- job_resume_snapshot(id, spec$identity)
    if (!identical(info$provider, session$provider) ||
        !identical(info$model, .resolve_model(session))) {
        stop("Resume requires the snapshot's provider and model", call. = FALSE)
    }
    path <- job_resume_path(id)
    history <- readRDS(file.path(path, "history.rds"))
    if (!is.list(history) || !length(history)) {
        stop("Invalid resume history", call. = FALSE)
    }
    env <- new.env(parent = emptyenv())
    load(file.path(path, "workspace.RData"), env)
    values <- .workspace_checkpoint_values(get(.run_r_worker_checkpoint_key,
            env),
        globalenv())
    for (pkg in unlist(info$packages, use.names = FALSE)) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }
    # Journal failure blocks replacement. Fresh workers never restore the
    # shared checkpoint for resume, so unrelated job objects cannot leak in.
    journal_rewrite(session, session$history %||% list(), history, reason = "job_resume")
    session$history <- history
    for (name in names(values)) {
        assign(name, values[[name]], globalenv())
    }
    paste0("Continue the incomplete job from the completed history above. ",
           "Do not repeat completed tools merely to reconstruct the conversation. ",
           "The checkout files were NOT restored and may have changed since ", info$at,
           "; inspect relevant current files and changes before editing. ",
           "Tool output handles are unavailable; reread needed data. ",
           "Workspace objects omitted as unrestorable: ",
           paste(c(unlist(info$omitted), if (!length(info$omitted)) "none"), collapse = ", "), ".")
}
