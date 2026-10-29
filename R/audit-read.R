#' Read an audit event stream
#'
#' Uses the auto-log decoder, retaining malformed-line locations and separating
#' a torn final line from interior damage. A missing terminal turn record means
#' unresolved completion. Redaction changes this returned view only; callers
#' can supply additional known credential values. It cannot detect all secrets.
#' @param path Path to a writer's JSONL stream.
#' @param redact Apply display redaction, default TRUE.
#' @param secrets Additional known credential values to hide.
#' @return List with records, auto-run start/end compatibility fields and
#'   malformed-record diagnostics. Turn endings remain in records.
#' @export
#' @examples
#' audit_read(tempfile())
audit_read <- function(path, redact = TRUE, secrets = character()) {
    result <- auto_log_read(path)
    if (isTRUE(redact)) .redact_object(result, secrets) else result
}
