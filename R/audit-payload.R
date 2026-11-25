.audit_limits <- function(config) {
    defaults <- list(result_bytes = 16 * 1024 ^ 2,
                     turn_bytes = 256 * 1024 ^ 2, quota_bytes = 2 * 1024 ^ 3)
    settings <- config$audit$limits
    for (name in names(defaults)) {
        value <- settings[[name]]
        if (is.numeric(value) && length(value) == 1L && !is.na(value) &&
            value >= 0) {
            defaults[[name]] <- value
        }
    }
    defaults
}

.audit_payload_path <- function(ref) {
    if (!is.list(ref) || !is.character(ref$hash) || length(ref$hash) != 1L ||
        is.na(ref$hash) || !grepl("^[a-f0-9]{64}$", ref$hash) ||
        !is.character(ref$store) || length(ref$store) != 1L ||
        is.na(ref$store)) {
        stop("Invalid audit payload reference", call. = FALSE)
    }
    .windows_io_path(file.path(path.expand(ref$store), ref$hash))
}

.audit_payload_put <- function(audit, text, kind, format = "text") {
    if (is.null(audit)) {
        return(NULL)
    }
    bytes <- charToRaw(enc2utf8(text))
    size <- length(bytes)
    hash <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    store <- file.path(audit$state$root, "payloads", audit$state$id)
    ref <- list(store = store, hash = hash, bytes = size, format = format,
                status = "stored")
    omit <- function(reason) {
        if (identical(reason, "quota")) .audit_notice(
            "Corteza audit payload quota reached; work continues with incomplete payload records.", kind = "quota")
        ref$status <- "omitted"
        ref$reason <- reason
        .audit_append(audit$state, "payload_omitted", turn_id = audit$id,
                      kind = kind, payload = ref, reason = reason)
        ref
    }
    limits <- audit$limits %||% .audit_limits(NULL)
    if (identical(kind, "tool_full") && size > limits$result_bytes) {
        return(omit("per_result"))
    }
    already <- hash %in% (audit$payload_hashes %||% character())
    if (!already && (audit$payload_bytes %||% 0) + size > limits$turn_bytes) {
        return(omit("per_turn"))
    }
    path <- .audit_payload_path(ref)
    mask <- Sys.umask("0077")
    on.exit(Sys.umask(mask), add = TRUE)
    dir.create(store, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    tryCatch(harness_with_lock(path, {
        if (!file.exists(path)) {
            # Stores belong to one writer incarnation; no cross-process quota
            # race or shared mutable index is needed. Pins count toward quota.
            usage <- function() {
                files <- list.files(store, "^[a-f0-9]{64}$", full.names = TRUE)
                sum(file.info(.windows_io_path(files))$size, na.rm = TRUE)
            }
            used <- audit$state$payload_used %||% usage()
            # Explicit expiry can free space between turns. Refresh at the
            # quota boundary, rather than stat every prior blob on each write.
            if (used + size > limits$quota_bytes) used <- usage()
            if (used + size > limits$quota_bytes) return(omit("quota"))
            tmp <- paste0(path, ".tmp")
            on.exit(unlink(tmp, expand = FALSE), add = TRUE)
            con <- file(tmp, "wb")
            tryCatch(writeBin(bytes, con), finally = close(con))
            if (!file.rename(tmp, path)) stop("Could not commit audit payload")
            audit$state$payload_used <- used + size
        } else if (!already &&
             !identical(digest::digest(file = path, algo = "sha256"), hash)) {
            stop("Existing audit payload failed its content hash")
        }
        if (!already) Sys.setFileTime(path, Sys.time())
        if (!already) {
            audit$payload_hashes <- c(audit$payload_hashes %||% character(), hash)
            audit$payload_bytes <- (audit$payload_bytes %||% 0) + size
        }
        ref
    }), error = function(e) {
        .audit_notice()
        omit("capture_failure")
    })
}

#' Read an immutable audit payload
#'
#' Reads only a payload whose bytes still match its recorded SHA-256. Redaction
#' affects the returned view, never the stored record. An omitted, expired,
#' missing or damaged payload is an error, not an empty successful read.
#' @param ref Payload reference from an audit event or request manifest.
#' @param redact Apply display redaction. Defaults to TRUE.
#' @param secrets Additional known credential values to hide in the view.
#' @return The decoded JSON object or UTF-8 text, as recorded.
#' @export
#' @examples
#' # A payload as an audit event records it: bytes named by their SHA-256
#' # in a writer's store, under a folder called "payloads".
#' store <- file.path(tempfile("audit"), "payloads", "example")
#' dir.create(store, recursive = TRUE)
#' text <- "the full output of a tool call"
#' hash <- digest::digest(charToRaw(text), algo = "sha256", serialize = FALSE)
#' writeBin(charToRaw(text), file.path(store, hash))
#' ref <- list(store = store, hash = hash, format = "text", status = "stored")
#' audit_payload_read(ref)
#' audit_payload_pin(ref)
#' audit_payload_pin(ref, pin = FALSE)
#' # A dry run lists what is older than 30 days and unpinned: nothing here.
#' audit_payload_expire(store)
#' unlink(dirname(dirname(store)), recursive = TRUE)
audit_payload_read <- function(ref, redact = TRUE, secrets = character()) {
    if (!identical(ref$status, "stored")) {
        stop("Audit payload omitted: ", ref$reason %||% "unknown",
             call. = FALSE)
    }
    path <- .audit_payload_path(ref)
    if (!file.exists(path)) {
        stop("Audit payload missing or expired", call. = FALSE)
    }
    if (!identical(digest::digest(file = path, algo = "sha256"), ref$hash)) {
        stop("Audit payload content hash mismatch", call. = FALSE)
    }
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    if (identical(ref$format, "json")) {
        value <- jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)
    } else {
        value <- rawToChar(bytes)
    }

    if (isTRUE(redact)) {
        .redact_object(value, secrets)
    } else {
        value
    }
}

#' Pin or unpin an audit payload
#'
#' Pins keep incident/evaluation evidence through explicit expiry. Pinned bytes
#' still count toward the store quota; capture reports quota omissions when full.
#' @param ref Payload reference from an audit event.
#' @param pin TRUE to pin, FALSE to remove the pin.
#' @return The payload reference, invisibly.
#' @export
#' @examples
#' # A payload as an audit event records it: bytes named by their SHA-256
#' # in a writer's store, under a folder called "payloads".
#' store <- file.path(tempfile("audit"), "payloads", "example")
#' dir.create(store, recursive = TRUE)
#' text <- "the full output of a tool call"
#' hash <- digest::digest(charToRaw(text), algo = "sha256", serialize = FALSE)
#' writeBin(charToRaw(text), file.path(store, hash))
#' ref <- list(store = store, hash = hash, format = "text", status = "stored")
#' audit_payload_read(ref)
#' audit_payload_pin(ref)
#' audit_payload_pin(ref, pin = FALSE)
#' # A dry run lists what is older than 30 days and unpinned: nothing here.
#' audit_payload_expire(store)
#' unlink(dirname(dirname(store)), recursive = TRUE)
audit_payload_pin <- function(ref, pin = TRUE) {
    path <- .audit_payload_path(ref)
    if (!dir.exists(ref$store)) stop("Cannot pin a missing payload", call. = FALSE)
    harness_with_lock(path, {
        if (!identical(ref$status, "stored") || !file.exists(path)) {
            stop("Cannot pin a missing payload", call. = FALSE)
        }
        if (isTRUE(pin)) {
            .private_record_write(paste0(path, ".pin"), .audit_now(), append = FALSE)
        } else {
            unlink(paste0(path, ".pin"), expand = FALSE)
        }
    })
    invisible(ref)
}

#' Expire unpinned audit payloads explicitly
#'
#' Automatic expiry requires a positive \code{retention.audit_payload_days}
#' in the user's configuration. Only files in the named writer's payload store are
#' eligible; event metadata and other writers' stores are left intact. Expiry
#' decisions append to a separate maintenance stream before deletion. Event
#' retention is a planning value until an explicit event-retention tool is used.
#' @param store A writer's payload store path from a payload reference.
#' @param days Retain this many days, default 30.
#' @param now Comparison time, default Sys.time().
#' @param dry_run Report eligible hashes without deleting, default TRUE.
#' @return Character vector of eligible (or deleted) hashes.
#' @export
#' @examples
#' # A payload as an audit event records it: bytes named by their SHA-256
#' # in a writer's store, under a folder called "payloads".
#' store <- file.path(tempfile("audit"), "payloads", "example")
#' dir.create(store, recursive = TRUE)
#' text <- "the full output of a tool call"
#' hash <- digest::digest(charToRaw(text), algo = "sha256", serialize = FALSE)
#' writeBin(charToRaw(text), file.path(store, hash))
#' ref <- list(store = store, hash = hash, format = "text", status = "stored")
#' audit_payload_read(ref)
#' audit_payload_pin(ref)
#' audit_payload_pin(ref, pin = FALSE)
#' # A dry run lists what is older than 30 days and unpinned: nothing here.
#' audit_payload_expire(store)
#' unlink(dirname(dirname(store)), recursive = TRUE)
audit_payload_expire <- function(store, days = 30, now = Sys.time(),
                                 dry_run = TRUE) {
    stopifnot(is.character(store), length(store) == 1L, dir.exists(store),
              identical(basename(dirname(store)), "payloads"),
              is.numeric(days), length(days) == 1L, !is.na(days), days >= 0)
    if (!isTRUE(dry_run) && !isFALSE(dry_run)) stop("dry_run must be TRUE or FALSE")
    # A caller or an older payload reference may name the store with "~".
    store <- path.expand(store)
    root <- dirname(dirname(store))
    if (!retention_within(store, root)) return(character())
    eligible <- function(path) {
        io <- .windows_io_path(path)
        pin <- paste0(io, ".pin")
        link <- Sys.readlink(pin)
        utils::file_test("-f", io) && retention_within(path, root) &&
            !file.exists(pin) && (is.na(link) || !nzchar(link)) &&
            isTRUE(retention_idle(now, file.mtime(io)) > days)
    }
    paths <- Filter(eligible, list.files(store, "^[a-f0-9]{64}$", full.names = TRUE))
    if (isTRUE(dry_run) || !length(paths)) return(basename(paths))
    session <- new.env(parent = emptyenv())
    session$audit_dir <- file.path(dirname(dirname(store)), "maintenance")
    if ("maintenance" %in% list.files(root, all.files = TRUE) &&
        (!dir.exists(session$audit_dir) || !retention_within(session$audit_dir, root))) return(character())
    state <- .audit_state(session)
    removed <- character()
    for (path in paths) {
        lock <- paste0(path, ".lock")
        io <- .windows_io_path(path)
        lock_io <- paste0(io, ".lock")
        link <- Sys.readlink(lock_io)
        if ((!is.na(link) && nzchar(link)) ||
            (file.exists(lock_io) && !retention_within(lock, root))) next
        success <- tryCatch(harness_with_lock(io, {
            if (eligible(path) && .audit_append(state, "payload_expiry", store = store,
                    hash = basename(path), reason = "expiry", status = "intent")) {
                ok <- retention_unlink(io)
                .audit_append(state, "payload_expiry", store = store, hash = basename(path),
                              reason = "expiry", status = if (ok) "removed" else "failed")
                ok
            } else FALSE
        }, timeout = 0, stale = Inf), error = function(e) FALSE)
        if (success) removed <- c(removed, basename(path))
    }
    removed
}
