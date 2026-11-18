# The archive: what becomes of a room's conversation.
#
# corteza keeps a ledger of what each room or thread session exchanged
# (session$transcript: the Matrix event, the role, the text; see
# bot_transcript_add()). Filing it is the host's. The on_archive hook
# (R/hooks.R) gets each new range of the ledger and decides what to keep
# (cerebro writes a summary and a pointer to the room's events). With
# no hook nothing is filed.
#
# corteza owns the bookkeeping. A range the host took is recorded by its
# event ids (the archive keys, bot_archive_state_*() in R/rooms.R), so a
# restart's backfill does not hand it over again. A range the host did
# not take waits on disk, so neither /clear nor a restart loses it, and
# goes again at the next /clear or flush of its session, or at any
# flush. A range from a flush stays open and takes the conversation's
# later entries; one from /clear is closed and goes on its own.

# Where a session's ranges wait for the host: beside the archive keys,
# named by the session's id, which tells 2 bots in 1 room apart.
bot_archive_pending_path <- function(session_id) {
    file.path(bot_signal_dir(), "archive", paste0(session_id, ".pending.json"))
}

# The registry key and the ranges kept at `path`. A file that cannot be
# read is set aside, not replaced: what it holds is the host's to file.
bot_archive_pending_read <- function(path) {
    if (!file.exists(path)) {
        return(NULL)
    }
    kept <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (!is.list(kept) || !is.character(kept$key) || !is.list(kept$ranges)) {
        aside <- paste0(path, ".unreadable")
        message("on_archive: unreadable pending archive set aside as ", aside)
        suppressWarnings(file.rename(path, aside))
        return(NULL)
    }
    kept
}

# Keep `ranges` for the session at `path`, or remove the file when none
# is left. Written beside and renamed, so a process killed mid-write
# leaves the previous file whole. FALSE, reported, when it cannot be
# written: the caller then keeps the entries in memory instead.
bot_archive_pending_write <- function(path, key, ranges) {
    if (!length(ranges)) {
        unlink(path)
        return(invisible(TRUE))
    }
    tmp <- paste0(path, ".tmp")
    ok <- tryCatch({
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(list(key = key, ranges = ranges), tmp,
                             auto_unbox = TRUE, null = "null", digits = NA)
        file.rename(tmp, path)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(ok)) {
        unlink(tmp)
        message("on_archive: could not keep a pending archive at ", path)
    }
    invisible(isTRUE(ok))
}

# A range of a session's ledger, as the host receives it. The id is the
# session's and the range's first event's, so it stays the same however
# often the range is offered and however much an open one grows.
bot_archive_range <- function(cfg, key, room_id, entries, reason) {
    parts <- strsplit(key, "\r", fixed = TRUE)[[1L]]
    thread <- NULL
    if (length(parts) > 1L) {
        thread <- parts[[2L]]
    }
    session_id <- bot_session_id(cfg, key)
    ids <- bot_transcript_ids(entries)
    list(id = digest::digest(paste(session_id, ids[[1L]], sep = "\n"),
                             algo = "sha256", serialize = FALSE),
         bot = bot_job_owner(cfg), room_id = room_id, thread = thread,
         session = session_id, first_event = ids[[1L]],
         last_event = ids[[length(ids)]], entries = entries, reason = reason)
}

# The ledger's entries in the form the host receives, each event once
# and none the host already took.
bot_archive_unfiled <- function(entries, filed) {
    entries <- lapply(entries %||% list(), function(e) {
        list(event_id = as.character(e$event_id %||% ""),
             role = as.character(e$role %||% ""),
             content = as.character(e$content %||% ""))
    })
    ids <- bot_transcript_ids(entries)
    entries[nzchar(ids) & !duplicated(ids) & !(ids %in% filed)]
}

# Offer one range to the host. Its answer when it took the range,
# list(archived = TRUE, ref), with a ref that is not one string dropped;
# NULL, reported, when it did not.
bot_archive_offer <- function(on_archive, chat, cfg, range) {
    why <- "not archived"
    answer <- tryCatch(on_archive(chat, cfg, range$room_id, range),
                       error = function(e) {
        why <<- conditionMessage(e)
        NULL
    })
    if (!is.list(answer) || !isTRUE(answer$archived)) {
        message("on_archive hook: ", why, " (", range$room_id,
                ", kept to retry)")
        return(NULL)
    }
    ref <- answer$ref
    if (!is.character(ref) || length(ref) != 1L || is.na(ref) || !nzchar(ref)) {
        ref <- NULL
    }
    list(archived = TRUE, ref = ref)
}

# Hand one session's unfiled conversation to the host: the ranges kept
# for it on disk, and what its ledger holds since. `session` may be
# NULL, for ranges whose session this process no longer holds.
#
# The ranges are written down before the host is asked, and the ledger
# is emptied once they are, so a process that dies while the host works
# loses nothing. Returns list(answer, filed): the host's answer for the
# range this call made or extended (NULL when there was none, or the
# host did not take it), and how many ranges the host took.
bot_archive_session <- function(session, key, chat = NULL, on_archive = NULL,
                                cfg = NULL, reason = "flush") {
    out <- list(answer = NULL, filed = 0L)
    if (!is.function(on_archive)) {
        return(invisible(out))
    }
    room_id <- strsplit(key, "\r", fixed = TRUE)[[1L]][[1L]]
    if (!is.null(session)) {
        room_id <- session$room_id %||% room_id
    }
    path <- bot_archive_pending_path(bot_session_id(cfg, key))
    filed <- bot_archive_state_read(room_id)
    ranges <- list()
    for (r in bot_archive_pending_read(path)$ranges) {
        r$entries <- bot_archive_unfiled(r$entries, filed)
        if (length(r$entries)) {
            ranges <- c(ranges, list(r))
        }
    }
    held <- unlist(lapply(ranges, function(r) bot_transcript_ids(r$entries)))
    new <- bot_archive_unfiled(session$transcript, c(filed, held))
    # An open range, kept from a flush, takes what the session said since.
    # One /clear closed stays as it was.
    entries <- new
    n <- length(ranges)
    if (n && identical(ranges[[n]]$reason, "flush")) {
        entries <- c(ranges[[n]]$entries, new)
        ranges <- ranges[-n]
    }
    current <- NULL
    if (length(entries)) {
        current <- bot_archive_range(cfg, key, room_id, entries, reason)
    }
    offered <- c(ranges, if (!is.null(current)) list(current))
    if (!length(offered)) {
        # Everything queued was taken before: a restart's backfill
        # replaying known events, or a range recorded just before the
        # process that offered it stopped.
        bot_archive_pending_write(path, key, list())
        if (!is.null(session)) {
            session$transcript <- list()
            .context_room_snapshot(session)
        }
        return(invisible(out))
    }
    kept <- bot_archive_pending_write(path, key, offered)
    if (kept && !is.null(session)) {
        session$transcript <- list()
        .context_room_snapshot(session)
    }
    left <- list()
    took <- character()
    for (i in seq_along(offered)) {
        answer <- bot_archive_offer(on_archive, chat, cfg, offered[[i]])
        if (is.null(answer)) {
            left <- c(left, offered[i])
            next
        }
        took <- c(took, bot_transcript_ids(offered[[i]]$entries))
        out$filed <- out$filed + 1L
        if (!is.null(current) && i == length(offered)) {
            out$answer <- answer
        }
    }
    if (length(took)) {
        bot_archive_state_write(room_id, c(filed, took))
    }
    if (kept) {
        bot_archive_pending_write(path, key, left)
    } else if (!is.null(session)) {
        # Nowhere to keep it on disk: the session holds what the host
        # did not take, as it did before.
        session$transcript <- list()
        if (!is.null(current) && is.null(out$answer)) {
            session$transcript <- current$entries
        }
        .context_room_snapshot(session)
    }
    invisible(out)
}

#' Hand each room's new conversation to the host
#'
#' Offers what every room and thread session said since its last
#' archive to the host's \code{on_archive} hook (see
#' \code{\link{bot_run_init}}), and again whatever the hook did not take
#' before. A range the host took is consumed from the session's ledger
#' and its Matrix event ids are kept per room under
#' \code{CORTEZA_STATE_DIR}, so a restart's backfill is recognized
#' rather than offered again. A range it did not take waits there for
#' the next flush. With no hook nothing is filed.
#'
#' @param sessions A registry environment built by
#'   \code{bot_run}/\code{bot_poll}. Keys are room IDs, values
#'   are session environments carrying \code{$transcript}, the
#'   Matrix-visible event ledger.
#' @param chat Optional chat.api client, handed to the hook.
#' @param on_archive The host's \code{on_archive} hook, or NULL.
#' @param cfg The bot config, which names the bot. NULL reads the saved
#'   one when there is a hook.
#'
#' @return Integer count of ranges the host took, invisibly.
#' @examples
#' \dontrun{
#' # Requires a running Matrix session registry and a host's hook.
#' reg <- new.env(parent = emptyenv())
#' bot_archive_all(reg, on_archive = function(chat, cfg, room_id, archive) {
#'     list(archived = TRUE, ref = NULL)
#' })
#' }
#' @export
bot_archive_all <- function(sessions, chat = NULL, on_archive = NULL,
                            cfg = NULL) {
    if (!is.environment(sessions)) {
        stop("sessions must be an environment registry", call. = FALSE)
    }
    if (!is.function(on_archive)) {
        return(invisible(0L))
    }
    cfg <- cfg %||% bot_load_config()
    n <- 0L
    done <- character()
    for (key in ls(envir = sessions, all.names = TRUE)) {
        s <- get(key, envir = sessions, inherits = FALSE)
        n <- n + bot_archive_session(s, key, chat, on_archive, cfg)$filed
        done <- c(done, bot_session_id(cfg, key))
    }
    # Ranges whose session this process no longer holds: cleared, or
    # held by the process before a restart. Another bot's are left alone.
    dir <- file.path(bot_signal_dir(), "archive")
    for (path in list.files(dir, pattern = "[.]pending[.]json$",
                            full.names = TRUE)) {
        id <- sub("[.]pending[.]json$", "", basename(path))
        key <- bot_archive_pending_read(path)$key
        if (id %in% done || is.null(key) ||
            !identical(bot_session_id(cfg, key), id)) {
            next
        }
        n <- n + bot_archive_session(NULL, key, chat, on_archive, cfg)$filed
    }
    invisible(n)
}
