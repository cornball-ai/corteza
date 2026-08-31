# The auto-run log: one append-only JSON Lines file per auto run,
# under agents/<agent>/auto/<run_id>.jsonl.
#
# This is the record of *authority*, not of conversation. The
# conversation already persists (session transcript), and so does tool
# execution (the _trace.jsonl) -- but neither says who decided a call
# could run, which budget check fired, or why a run stopped. Before
# this file existed, a call refused by the envelope left no trace
# anywhere at all.
#
# Shape mirrors the transcript conventions on purpose: a header record
# first, one record per decision as it happens, a terminal record last.
# A file with no run_end is a run whose process died, readable from the
# file alone. Records carry the run_id so they join against the session
# transcript and trace (which carry auto_run_id when written during a
# run) and against the monitor's own subagent transcript (whose id the
# "monitor" record names).

AUTO_LOG_VERSION <- 1L

#' Directory for auto-run logs.
#' @noRd
auto_log_dir <- function(agent_id = DEFAULT_AGENT_ID) {
    corteza_data_path("agents", agent_id, "auto")
}

#' Path for one run's log.
#' @noRd
auto_log_path <- function(run_id, agent_id = DEFAULT_AGENT_ID) {
    file.path(auto_log_dir(agent_id), paste0(run_id, ".jsonl"))
}

#' New run id: start-time sortable, with entropy against collisions.
#'
#' Generated before anything is validated, so a refused start still has
#' an id to be logged under.
#' @noRd
auto_new_run_id <- function() {
    paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-",
           paste(format(as.hexmode(sample.int(256L, 4L) - 1L), width = 2L),
                 collapse = ""))
}

#' Append one record to a run log. Never throws.
#'
#' Recording must not be able to stop a run (contrast the gate's
#' executed-call accounting, where a failure must halt the call). But a
#' silent recording failure would mean flying blind while believing
#' the run is on the record, so the FALSE return is there for the
#' caller to notice and say so -- once.
#'
#' @param path Log file path.
#' @param type Record type string.
#' @param ... Named fields for the record.
#' @return TRUE if the record was written, FALSE otherwise.
#' @noRd
auto_log_append <- function(path, type, ...) {
    # Warnings suppressed as well as errors caught: a run whose log path
    # is unwritable would otherwise emit two R warnings per decision,
    # and the FALSE return is already the signal.
    tryCatch(suppressWarnings({
        dir <- dirname(path)
        if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE, mode = "0700")
        }
        rec <- c(list(type = type,
                      ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")),
                 list(...))
        rec <- rec[!vapply(rec, is.null, logical(1))]
        json <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null",
                                 na = "null", digits = NA)
        cat(json, "\n", file = path, append = TRUE, sep = "")
        TRUE
    }), error = function(e) FALSE)
}

#' Read a run log back.
#'
#' @param path Log file path.
#' @return List with `start` (the run_start record or NULL), `records`
#'   (every record, in order), and `end` (the run_end record, or NULL
#'   for a run whose process died before writing one).
#' @noRd
auto_log_read <- function(path) {
    if (!file.exists(path)) {
        return(list(start = NULL, records = list(), end = NULL))
    }
    lines <- readLines(path, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    recs <- lapply(lines, function(l) {
        tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE),
                 error = function(e) NULL)
    })
    recs <- recs[!vapply(recs, is.null, logical(1))]
    types <- vapply(recs, function(r) r$type %||% "", character(1))
    list(start = if (any(types == "run_start")) {
            recs[[which(types == "run_start")[[1L]]]]
        } else {
            NULL
        },
         records = recs,
         end = if (any(types == "run_end")) {
            recs[[utils::tail(which(types == "run_end"), 1L)]]
        } else {
            NULL
        })
}
