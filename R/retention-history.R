# Age limits for individual records apply only when the user enables them.
# Enumerate writer-owned layouts, including jobs under an external state root.
retention_history <- function(config, dry_run, now = Sys.time(), idle = NULL) {
    rows <- list()
    result <- function() {
        if (length(rows)) return(do.call(rbind, rows))
        empty <- retention_rows()
        empty$removed <- logical()
        empty
    }
    if (!any(unlist(config[c("journal_segment_days", "audit_payload_days",
                             "workspace_snapshot_days")]) > 0)) return(result())
    agent <- NA_character_
    record <- function(paths, kind, id, remove) {
        for (p in paths) {
            info <- file.info(.windows_io_path(p), extra_cols = FALSE)
            if (is.na(info$mtime) || info$isdir) next
            row <- retention_row(agent, id, kind, p, info$mtime,
                                 retention_idle(now, info$mtime), info$size)
            row$removed <- !dry_run && isTRUE(remove(p))
            rows[[length(rows) + 1L]] <<- row
        }
    }
    audit <- function(dir, root, id) {
        days <- config$audit_payload_days
        if (days <= 0 || !dir.exists(dir) || !retention_within(dir, root)) return()
        for (base in c(dir, file.path(dir, "tool-workers"))) {
            for (store in retention_store_dirs(file.path(base, "payloads"), root)) {
                if (!grepl("^[0-9]{8}T[0-9]{6}-[[:xdigit:]]+-[0-9]+$", store$name)) next
                stream <- file.path(base, paste0(store$name, ".jsonl"))
                if (!utils::file_test("-f", stream) || !retention_within(stream, root)) next
                hashes <- audit_payload_expire(store$dir, days, now)
                if (!length(hashes)) next
                if (!dry_run) {
                    # Capture size and time before the maintenance commit.
                    before <- length(rows)
                    record(file.path(store$dir, hashes), "payload", id, function(p) FALSE)
                    gone <- audit_payload_expire(store$dir, days, now, dry_run = FALSE)
                    for (i in before + seq_len(length(rows) - before)) {
                        rows[[i]]$removed <<- basename(rows[[i]]$path) %in% gone
                    }
                } else record(file.path(store$dir, hashes), "payload", id, function(p) FALSE)
            }
        }
    }
    whole <- idle$path[idle$kind == "journal"]
    workers <- idle$path[idle$kind %in% c("worker", "checkpoint")]
    if (length(workers)) whole <- c(whole, file.path(workers, "journal"))
    journal <- function(dir, root, id) {
        if (!dir.exists(dir) || !retention_within(dir, root)) return()
        audit(file.path(dir, "audit"), root, id)
        if (config$journal_segment_days > 0 && !dir %in% whole) {
            retention_segments(dir, root, id, config$journal_segment_days, now, record, dry_run)
        }
    }
    data <- corteza_data_dir()
    audit(file.path(data, "audit"), data, "global")
    for (a in retention_store_dirs(file.path(data, "agents"), data)) {
        agent <- a$name
        sessions <- file.path(a$dir, "sessions")
        if (!dir.exists(sessions) || !retention_within(sessions, data)) next
        for (dir in list.files(sessions, "[.]journal$", full.names = TRUE)) {
            journal(dir, data, sub("[.]journal$", "", basename(dir)))
        }
        days <- config$workspace_snapshot_days
        if (days > 0) {
            for (metadata in list.files(sessions, "_workspace[.]json$", full.names = TRUE)) {
                id <- sub("_workspace[.]json$", "", basename(metadata))
                transcript <- file.path(sessions, paste0(id, ".jsonl"))
                if (!retention_within(metadata, data) || !utils::file_test("-f", metadata) ||
                    !retention_within(transcript, data) || !utils::file_test("-f", transcript)) next
                header <- tryCatch(jsonlite::fromJSON(metadata, simplifyVector = FALSE),
                                   error = function(e) NULL)
                if (!is.list(header) || !is.numeric(header$turn) || !is.list(header$objects)) next
                values <- file.path(sessions, paste0(id, "_workspace.rds"))
                paths <- c(values[file.exists(values)], metadata)
                eligible <- function(p) {
                    retention_within(p, data) && utils::file_test("-f", p) &&
                        isTRUE(retention_idle(now, file.mtime(p)) > days)
                }
                if (all(vapply(paths, eligible, logical(1)))) {
                    record(paths, "workspace snapshot", id, function(p) {
                        if (identical(p, metadata) && file.exists(values)) return(FALSE)
                        eligible(p) && retention_unlink(p)
                    })
                }
            }
        }
    }
    agent <- NA_character_
    state <- bot_signal_dir()
    for (d in retention_store_dirs(file.path(state, "workers"), state)) {
        if (grepl("^[0-9a-f]{24}$", d$name)) journal(file.path(d$dir, "journal"), state, d$name)
    }
    for (d in retention_store_dirs(file.path(state, "jobs"), state)) {
        if (retention_is_id(d$name)) audit(file.path(d$dir, "audit"), state, d$name)
    }
    result()
}

retention_segments <- function(dir, root, id, days, now, record, dry_run) {
    regular <- function(p) utils::file_test("-f", p) && retention_within(p, root)
    live <- file.path(dir, JOURNAL_LIVE)
    if (!regular(live)) return()
    read <- function(p) tryCatch(.journal_read_json(p), error = function(e) NULL)
    committed <- read(live)$seq
    if (!is.numeric(committed) || length(committed) != 1L || !is.finite(committed)) return()
    seqs <- .journal_segment_seqs(dir)
    seqs <- seqs[seqs <= committed]
    if (length(seqs) < 2L || tail(seqs, 1L) != committed || any(diff(seqs) != 1L)) return()
    paths <- vapply(seqs, function(i) .journal_segment_path(dir, i), "")
    if (!all(vapply(paths, regular, logical(1)))) return()
    aged <- function(p) regular(p) && isTRUE(retention_idle(now, file.mtime(p)) > days)
    end <- which(!vapply(paths, aged, logical(1)) | seqs == committed)[[1L]] - 1L
    if (end == 0L) return()
    read_group <- function(i) {
        seg <- read(paths[[i]])
        if (!is.list(seg) || !identical(seg$seq, seqs[[i]]) ||
            !is.character(seg$reason) || length(seg$reason) != 1L) return(NA_real_)
        group <- seg$entry_group %||% 0L
        if (!is.numeric(group) || length(group) != 1L || !is.finite(group) ||
            group != trunc(group) || group < 0 || group > seqs[[i]]) return(NA_real_)
        group
    }
    groups <- vapply(seq_along(seqs), read_group, numeric(1))
    if (anyNA(groups)) return()
    # Remove only an old prefix. Keep the most recent acknowledged rewrite.
    old <- seq_len(end)
    record(paths[old], "segment", id, function(p) aged(p) && retention_unlink(p))
    # Keep every group referenced by a remaining segment, including the
    # last group the writer used. A future edit can still use that group.
    used <- groups[groups > 0]
    if (!length(used)) return()
    retired <- setdiff(groups[old], c(0, groups[-old], tail(used, 1L)))
    if (!dry_run && any(file.exists(paths[old]))) return()
    for (group in retired) {
        here <- file.path(dir, "entries", .journal_group_name(group))
        if (!dir.exists(here) || !retention_within(here, root)) next
        entries <- list.files(here, "^[[:xdigit:]]{32}[.]json$", full.names = TRUE)
        record(Filter(regular, entries), "entry", id, function(p) regular(p) && retention_unlink(p))
        if (!dry_run) retention_rmdir(here)
    }
}
