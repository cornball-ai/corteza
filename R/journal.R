# Session journal: the durable, provider-native record of a session's
# history.
#
# The openclaw transcript (<id>.jsonl) records what people exchanged, as
# text. The trace (<id>_trace.jsonl) records tool calls cut to a
# preview. Neither holds the tool traffic the model actually saw, so a
# rewrite of the live history (compaction, archival) used to destroy
# the only full copy of it. The journal closes that gap the way the
# ARC-AGI-3 driver did in September 2026: a snapshot of the live
# history after every change, and before every rewrite a segment
# holding the exact history being replaced, written before the live
# history is touched so a failed write blocks the rewrite.
#
# Layout, under the owning agent's sessions directory:
#
#   <id>.journal/live.json           the live history as last mirrored
#   <id>.journal/segment-NNNN.json   one per rewrite, in order
#   <id>.journal/entries/<hash>.json shared immutable entries for context edits
#
# Context-edit segments store the operations, a reverse delta of changed
# entries, and hashes of the replacement's shared entries. Those entries are
# written before the segment; unused entries after a failed commit are harmless.
# journal_load() expands these segments into the same history/after envelopes
# as full compaction segments, independently of later live-snapshot changes.
#
# A rewrite replaces `before` with `after`. Each segment records
# `inherited`, the number of head entries of `before` that the previous
# rewrite's `after` had already recorded. The full record of a session
# is each segment's history past its inherited head, in order, then the
# same slice of the live snapshot. Synthetic entries a rewrite adds (a
# compaction summary, an [archived turn] marker) are in `after` and so
# never enter the record: it stays what the model and the tools
# actually produced, with nothing counted twice.
#
# A rewrite commits in two writes. The segment goes first, under the
# next number, and counts for nothing yet. The live snapshot goes
# second, carrying that number as `seq`: a segment is part of the record
# only once a live snapshot acknowledges it. A failure or a crash
# between the two leaves the previous snapshot in charge, the
# unacknowledged segment is ignored by every reader and overwritten by
# the next rewrite, and the in-memory state advances only after both
# writes, so a retry starts from where the record actually is. That is
# what keeps "every entry once" true across a failed rewrite.
#
# A session without a resolvable directory has no journal, and every
# function here is a no-op for it, so sessions built bare in tests and
# direct turn() callers with their own persistence see no change.

JOURNAL_LIVE <- "live.json"
JOURNAL_SEGMENT_PATTERN <- "^segment-[0-9]{4}[.]json$"

#' Install a journal on a session.
#'
#' Sets where the journal lives and initializes its state, which
#' includes rolling an existing live snapshot (a previous process, a
#' resumed session) into a segment so it is not overwritten.
#'
#' @param session A session environment from new_session().
#' @param dir Explicit journal directory. NULL derives one from
#'   `agent_id` and `session$sessionId` at the point of use.
#' @param agent_id Owning agent for the derived directory; ignored when
#'   `dir` is given.
#' @return The journal directory, invisibly; NULL when none resolves or
#'   initialization failed. A failure is logged, not raised: a session
#'   can run without its journal, and the first rewrite raises it again
#'   where it blocks the rewrite.
#' @noRd
journal_install <- function(session, dir = NULL, agent_id = NULL) {
    # Only an environment can carry journal state; anything else (a
    # test's stand-in session) simply has no journal.
    if (!is.environment(session)) {
        return(invisible(NULL))
    }
    if (!is.null(dir)) {
        session$journal_dir <- dir
    } else if (!is.null(agent_id)) {
        session$journal_agent_id <- agent_id
    }
    state <- tryCatch(.journal_state(session), error = function(e) {
        log_event("journal_init_failed", error = conditionMessage(e),
                  level = "warn")
        NULL
    })
    invisible(if (is.null(state)) NULL else state$dir)
}

#' Resolve a session's journal directory, or NULL for no journal.
#' @noRd
journal_dir <- function(session) {
    if (!is.environment(session) || !journal_enabled(session)) {
        return(NULL)
    }
    if (!is.null(session$journal_dir)) {
        return(session$journal_dir)
    }
    id <- session$sessionId
    if (is.null(id) || !is.character(id) || length(id) != 1L || !nzchar(id)) {
        return(NULL)
    }
    agent_id <- session$journal_agent_id
    if (is.null(agent_id)) {
        return(NULL)
    }
    file.path(sessions_dir(agent_id), paste0(id, ".journal"))
}

#' Is journaling on for this session? Config `journal_enabled` (default
#' TRUE) and a per-session override, either set to FALSE turns it off.
#' @noRd
journal_enabled <- function(session) {
    !isFALSE(session$journal_enabled) &&
    !isFALSE(session$config$journal_enabled)
}

#' Write the live snapshot. Never throws: this runs inside llm.api's
#' history_callback and the interrupt handlers, where an error would
#' either vanish or mask the condition being handled. Returns TRUE on
#' success, FALSE on failure (logged) or when the session has no journal.
#' @noRd
journal_snapshot <- function(session) {
    state <- tryCatch(.journal_state(session), error = function(e) {
        log_event("journal_init_failed", error = conditionMessage(e),
                  level = "warn")
        NULL
    })
    if (is.null(state)) {
        return(invisible(FALSE))
    }
    ok <- tryCatch({
        .journal_write_live(state, session$history %||% list())
        TRUE
    }, error = function(e) {
        log_event("journal_snapshot_failed", dir = state$dir,
                  error = conditionMessage(e), level = "warn")
        FALSE
    })
    invisible(ok)
}

#' Record a rewrite of the live history, before it happens.
#'
#' Writes the segment holding `before` and `after`, then the live
#' snapshot of `after` that acknowledges it, then advances the state.
#' Errors propagate: the caller must not replace `session$history` when
#' this throws, so a write failure costs the rewrite, never the record.
#'
#' @param before,after The history being replaced and its replacement.
#' @param reason Short lifecycle marker (`"threshold"`, `"checkpoint"`,
#'   `"context_overflow"`, `"archival"`, `"scope"`, `"rollover"`, ...).
#' @param summary Optional summary text the rewrite introduced.
#' @param edits NULL for full segments; an operation list (possibly empty)
#'   selects compact edit segments with shared immutable entries.
#' @return The segment path, invisibly; NULL when the session has no
#'   journal.
#' @noRd
journal_rewrite <- function(session, before, after, reason, summary = NULL,
                            edits = NULL) {
    state <- .journal_state(session)
    if (is.null(state)) {
        return(invisible(NULL))
    }
    invisible(.journal_commit(state, before %||% list(), after %||% list(),
                              reason, summary = summary, edits = edits))
}

#' Has the live history been rewritten since `mark`? FALSE without a
#' journal or a mark, so a caller keeps its old behavior.
#' `ignore` names rewrites that preserve message positions, which callers
#' such as archival can allow without invalidating their pre-turn offset.
#' @noRd
journal_rewritten_since <- function(session, mark, ignore = character()) {
    if (is.null(mark)) {
        return(FALSE)
    }
    state <- tryCatch(.journal_state(session), error = function(e) NULL)
    if (is.null(state) || identical(as.integer(state$seq), as.integer(mark$seq))) {
        return(FALSE)
    }
    if (!length(ignore)) {
        return(TRUE)
    }
    seqs <- seq_len(state$seq)
    # Missing or corrupt metadata cannot prove that message positions stayed
    # stable. Conservatively skip archival while keeping this check no-throw.
    tryCatch(any(vapply(seqs[seqs > as.integer(mark$seq)], function(seq) {
        seg <- .journal_read_json(.journal_segment_path(state$dir, seq))
        is.null(seg$reason) || !seg$reason %in% ignore
    }, logical(1))), error = function(e) TRUE)
}

#' A position in the journal, taken before a turn, so the entries the
#' turn added can be recovered afterwards whatever rewrites ran in
#' between. NULL when the session has no journal.
#' @noRd
journal_mark <- function(session) {
    state <- tryCatch(.journal_state(session), error = function(e) NULL)
    if (is.null(state)) {
        return(NULL)
    }
    list(seq = state$seq, live_len = length(session$history %||% list()))
}

#' The entries added to a session's history since `mark`.
#'
#' Segments committed since the mark contribute their history past the
#' mark (for the first) or past their inherited head (for later ones);
#' the live history contributes what follows its inherited head. With
#' no rewrite since the mark this is the tail of the live history.
#' @noRd
journal_entries_since <- function(session, mark) {
    state <- .journal_state(session)
    history <- session$history %||% list()
    if (is.null(state) || is.null(mark)) {
        return(.journal_tail(history, mark$live_len %||% 0L))
    }
    out <- list()
    seqs <- seq_len(state$seq)
    seqs <- seqs[seqs > as.integer(mark$seq)]
    first <- TRUE
    segments <- if (length(seqs)) journal_load(state$dir)$segments else list()
    for (s in seqs) {
        seg <- segments[[s]]
        skip <- if (first) {
            as.integer(mark$live_len)
        } else {
            as.integer(seg$inherited %||% 0L)
        }
        out <- c(out, .journal_tail(seg$history %||% list(), skip))
        first <- FALSE
    }
    if (first) {
        skip <- as.integer(mark$live_len)
    } else {
        skip <- state$inherited
    }
    c(out, .journal_tail(history, skip))
}

#' The slice a turn added, with a fallback to length slicing for a
#' session without a journal (or a caller that took no mark).
#' @noRd
journal_turn_slice <- function(session, mark, pre_len) {
    if (is.null(mark) || is.null(journal_dir(session))) {
        return(.journal_tail(session$history %||% list(), pre_len))
    }
    journal_entries_since(session, mark)
}

#' Read a journal directory: the live envelope and the committed
#' segments in order. A segment the live snapshot has not acknowledged
#' (a rewrite that failed or was cut off between its two writes) is
#' left out. Missing pieces are NULL / empty.
#' @noRd
journal_load <- function(dir) {
    live <- .journal_read_live(dir)
    if (is.null(live)) {
        committed <- 0L
    } else {
        committed <- as.integer(live$seq %||% 0L)
    }
    seqs <- .journal_segment_seqs(dir)
    seqs <- seqs[seqs <= committed]
    segments <- lapply(seqs, function(s) {
        .journal_read_json(.journal_segment_path(dir, s))
    })
    # Edit segments retain replaced entries and references to immutable shared
    # survivors. They never depend on a later live snapshot staying unchanged
    # (a host may bridge providers or restore a checkpoint).
    entries <- new.env(parent = emptyenv())
    for (i in seq_along(segments)) {
        seg <- segments[[i]]
        if (!is.null(seg$reverse_delta)) {
            seg$after <- lapply(seg$after_refs, function(id) {
                if (!exists(id, entries, inherits = FALSE)) {
                    assign(id, .journal_read_json(file.path(dir, "entries", paste0(id, ".json"))), entries)
                }
                get(id, entries, inherits = FALSE)
            })
            seg$history <- lapply(seg$reverse_delta, function(item) {
                if (!is.null(item$from)) seg$after[[item$from]] else item$entry
            })
            segments[[i]] <- seg
        }
    }
    list(live = live, segments = segments)
}

#' The full record of a session: every entry the model and tools
#' produced, once, in order, with the synthetic entries rewrites
#' introduced left out.
#' @noRd
journal_history <- function(dir) {
    j <- journal_load(dir)
    out <- list()
    for (seg in j$segments) {
        out <- c(out,
                 .journal_tail(seg$history %||% list(), seg$inherited %||% 0L))
    }
    if (!is.null(j$live)) {
        out <- c(out, .journal_tail(j$live$history %||% list(),
                                    j$live$inherited %||% 0L))
    }
    out
}

# ---- State ----

# The journal's state rides on the session as a small environment:
# dir, seq (the last segment the live snapshot acknowledges), inherited
# (head entries of the live history already recorded). It is rebuilt
# whenever the resolved directory changes, which is how /clear (a new
# sessionId) moves to a new journal with fresh state.
.journal_state <- function(session) {
    dir <- journal_dir(session)
    if (is.null(dir)) {
        return(NULL)
    }
    state <- session$.journal
    if (is.environment(state) && identical(state$dir, dir)) {
        return(state)
    }
    state <- new.env(parent = emptyenv())
    state$dir <- dir
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    }
    # The committed count comes from the live snapshot, never from the
    # files on disk: a segment past it is a rewrite that never finished.
    previous <- .journal_read_live(dir, repair = TRUE)
    state$seq <- as.integer(previous$seq %||% 0L)
    state$inherited <- 0L
    # A live snapshot already here belongs to an earlier process or an
    # earlier life of a resumed session. Keep it as a segment rather
    # than overwrite it; the history this session starts with is then
    # the rollover's `after`, so it is recorded there and inherited.
    if (!is.null(previous)) {
        state$inherited <- as.integer(previous$inherited %||% 0L)
        .journal_commit(state, previous$history %||% list(),
                        session$history %||% list(), "rollover")
    }
    session$.journal <- state
    state
}

# The two-write commit described at the top of the file. The state
# advances only once the live snapshot acknowledging the segment is on
# disk; until then the segment is a stray the next rewrite overwrites.
.journal_commit <- function(state, before, after, reason, summary = NULL,
                             edits = NULL) {
    seq <- state$seq + 1L
    path <- .journal_write_segment(state, seq, before, after, reason,
                                   summary = summary, edits = edits)
    .journal_write_live(state, after, inherited = length(after), seq = seq)
    state$seq <- seq
    state$inherited <- length(after)
    path
}

# ---- Files ----

.journal_segment_path <- function(dir, seq) {
    file.path(dir, sprintf("segment-%04d.json", as.integer(seq)))
}

.journal_segment_seqs <- function(dir) {
    if (!dir.exists(dir)) {
        return(integer())
    }
    files <- list.files(dir, pattern = JOURNAL_SEGMENT_PATTERN)
    sort(as.integer(sub("^segment-([0-9]{4})[.]json$", "\\1", files)))
}

.journal_write_segment <- function(state, seq, before, after, reason,
                                   summary = NULL, edits = NULL) {
    envelope <- list(
                     seq = as.integer(seq),
                     reason = reason,
                     timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                     inherited = as.integer(state$inherited),
                     kept = length(after),
                     summary = summary,
                     history = before,
                     after = after
    )
    if (!is.null(edits)) {
        hashes <- vapply(after, digest::digest, character(1))
        entry_dir <- file.path(state$dir, "entries")
        dir.create(entry_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
        envelope$after_refs <- lapply(after, function(entry) {
            id <- digest::digest(entry, algo = "sha256")
            path <- file.path(entry_dir, paste0(id, ".json"))
            if (!file.exists(path)) .journal_write_json(entry, path)
            id
        })
        envelope$reverse_delta <- lapply(before, function(entry) {
            candidates <- which(hashes == digest::digest(entry))
            same <- candidates[vapply(after[candidates], identical, logical(1), entry)]
            if (length(same)) list(from = same[[1L]]) else list(entry = entry)
        })
        envelope$edits <- edits
        envelope$history <- NULL
        envelope$after <- NULL
    }
    .journal_write_json(envelope, .journal_segment_path(state$dir, seq))
}

.journal_write_live <- function(state, history, inherited = state$inherited,
                                seq = state$seq) {
    envelope <- list(
                     inherited = as.integer(inherited),
                     seq = as.integer(seq),
                     updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                     history = history
    )
    .journal_write_json(envelope, file.path(state$dir, JOURNAL_LIVE))
}

# The live snapshot, allowing for a replacement cut off between parking
# the old file and landing the new one (see .journal_write_json): the
# parked copy stands in, and with `repair` it is moved back into place.
# A parked copy beside a live file is left over from a replacement that
# did land; `repair` removes it. Readers that must not write (journal_load)
# leave both as they are.
.journal_read_live <- function(dir, repair = FALSE) {
    live_path <- file.path(dir, JOURNAL_LIVE)
    prev_path <- paste0(live_path, ".prev")
    if (repair && !file.exists(live_path) && file.exists(prev_path)) {
        .journal_rename(prev_path, live_path)
    }
    path <- if (file.exists(live_path)) {
        live_path
    } else if (file.exists(prev_path)) {
        prev_path
    }
    if (is.null(path)) {
        return(NULL)
    }
    if (repair && identical(path, live_path) && file.exists(prev_path)) {
        unlink(prev_path)
    }
    .journal_read_json(path)
}

# Atomic write: serialize to a sibling temp file, then rename over the
# target. digits = NA keeps numbers exact; auto_unbox = TRUE and
# null = "null" give the shape fromJSON(simplifyVector = FALSE) reads
# back identically for the provider-native histories llm.api builds.
#
# Where rename does not replace an existing target (Windows), the
# current file is parked as <path>.prev before the rename is retried,
# never deleted: it may be the only copy of the commit marker, and a
# retry that fails or a process that stops here leaves it for
# .journal_read_live to fall back to. A landed replacement removes it.
.journal_write_json <- function(x, path) {
    json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null",
                             na = "null", digits = NA)
    tmp <- paste0(path, ".tmp")
    written <- tryCatch({
        suppressWarnings(writeLines(json, tmp, useBytes = TRUE))
        TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(written)) {
        stop("journal: could not write ", path, ": ", written, call. = FALSE)
    }
    ok <- .journal_rename(tmp, path)
    if (!ok && file.exists(path)) {
        prev <- paste0(path, ".prev")
        unlink(prev)
        if (.journal_rename(path, prev)) {
            ok <- .journal_rename(tmp, path)
            if (ok) {
                unlink(prev)
            }
        }
    }
    if (!ok) {
        unlink(tmp)
        stop("journal: could not write ", path, call. = FALSE)
    }
    invisible(path)
}

# One place for the rename, so a test can make it fail the way a
# platform does.
.journal_rename <- function(from, to) {
    isTRUE(suppressWarnings(file.rename(from, to)))
}

.journal_read_json <- function(path) {
    jsonlite::fromJSON(path, simplifyVector = FALSE)
}

.journal_tail <- function(history, skip) {
    skip <- as.integer(skip %||% 0L)
    n <- length(history)
    if (skip >= n) {
        return(list())
    }
    history[(skip + 1L):n]
}
