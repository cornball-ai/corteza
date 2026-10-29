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
    file.path(ref$store, ref$hash)
}

.audit_payload_put <- function(audit, text, kind, format = "text") {
    if (is.null(audit)) return(NULL)
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
    if (identical(kind, "tool_full") &&
        size > limits$result_bytes) return(omit("per_result"))
    already <- hash %in% (audit$payload_hashes %||% character())
    if (!already &&
        (audit$payload_bytes %||% 0) + size > limits$turn_bytes) return(omit("per_turn"))
    path <- .audit_payload_path(ref)
    tryCatch({
        if (!file.exists(path)) {
            # Stores belong to one writer incarnation; no cross-process quota
            # race or shared mutable index is needed. Pins count toward quota.
            usage <- function() {
                files <- list.files(store, "^[a-f0-9]{64}$", full.names = TRUE)
                sum(file.info(files)$size, na.rm = TRUE)
            }
            used <- audit$state$payload_used %||% usage()
            # Explicit expiry can free space between turns. Refresh at the
            # quota boundary, rather than stat every prior blob on each write.
            if (used + size > limits$quota_bytes) used <- usage()
            if (used + size > limits$quota_bytes) return(omit("quota"))
            mask <- Sys.umask("0077")
            on.exit(Sys.umask(mask), add = TRUE)
            dir.create(store, recursive = TRUE, showWarnings = FALSE, mode = "0700")
            tmp <- paste0(path, ".tmp")
            on.exit(unlink(tmp), add = TRUE)
            con <- file(tmp, "wb")
            tryCatch(writeBin(bytes, con), finally = close(con))
            if (!file.rename(tmp, path)) stop("Could not commit audit payload")
            audit$state$payload_used <- used + size
        } else if (!already && !identical(digest::digest(file = path, algo = "sha256"), hash)) {
            stop("Existing audit payload failed its content hash")
        }
        if (!already) Sys.setFileTime(path, Sys.time())
        if (!already) {
            audit$payload_hashes <- c(audit$payload_hashes %||% character(), hash)
            audit$payload_bytes <- (audit$payload_bytes %||% 0) + size
        }
        ref
    }, error = function(e) {
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
#' \dontrun{
#' audit_payload_read(ref)
#' }
audit_payload_read <- function(ref, redact = TRUE, secrets = character()) {
    if (!identical(ref$status, "stored")) {
        stop("Audit payload omitted: ", ref$reason %||% "unknown",
             call. = FALSE)
    }
    path <- .audit_payload_path(ref)
    if (!file.exists(path)) stop("Audit payload missing or expired", call. = FALSE)
    if (!identical(digest::digest(file = path, algo = "sha256"), ref$hash)) {
        stop("Audit payload content hash mismatch", call. = FALSE)
    }
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    value <- if (identical(ref$format, "json")) jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE) else
    rawToChar(bytes)
    if (isTRUE(redact)) .redact_object(value, secrets) else value
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
#' \dontrun{
#' audit_payload_pin(ref)
#' }
audit_payload_pin <- function(ref, pin = TRUE) {
    path <- .audit_payload_path(ref)
    if (!identical(ref$status, "stored") ||
        !file.exists(path)) stop("Cannot pin a missing payload", call. = FALSE)
    if (isTRUE(pin)) {
        .private_record_write(paste0(path, ".pin"), .audit_now(),
                              append = FALSE)
    } else unlink(paste0(path, ".pin"))
    invisible(ref)
}

#' Expire unpinned audit payloads explicitly
#'
#' Never runs automatically. Only files in the named writer's payload store are
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
#' \dontrun{
#' audit_payload_expire(ref$store)
#' }
audit_payload_expire <- function(store, days = 30, now = Sys.time(),
                                 dry_run = TRUE) {
    stopifnot(is.character(store), length(store) == 1L, dir.exists(store),
              identical(basename(dirname(store)), "payloads"),
              is.numeric(days), length(days) == 1L, !is.na(days), days >= 0)
    paths <- list.files(store, "^[a-f0-9]{64}$", full.names = TRUE)
    age <- as.numeric(difftime(now, file.info(paths)$mtime, units = "days"))
    paths <- paths[!is.na(age) & age > days &
        !file.exists(paste0(paths, ".pin"))]
    if (isTRUE(dry_run) || !length(paths)) return(basename(paths))
    session <- new.env(parent = emptyenv())
    session$audit_dir <- file.path(dirname(dirname(store)), "maintenance")
    state <- .audit_state(session)
    removed <- character()
    for (path in paths) {
        if (!.audit_append(state, "payload_expiry", store = store,
                           hash = basename(path), reason = "expiry", status = "intent")) break
        success <- unlink(path) == 0L
        .audit_append(state, "payload_expiry", store = store,
                      hash = basename(path), reason = "expiry", status = if (success) {
                "removed"
            } else {
                "failed"
            })
        if (success) removed <- c(removed, basename(path))
    }
    removed
}
