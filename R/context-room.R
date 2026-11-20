# A conversation's edited window and the event IDs it has already consumed must
# restart together. Both travel in the journal's atomic live envelope.
# `talker`: the room's talker settings (bot_talker_config()), whose
# `context_edit` and `context_vault` stand over the account's.
.context_room_config <- function(cfg, talker = bot_talker_config(cfg)) {
    tryCatch(list(editing = utils::modifyList(cfg$context_edit %||% list(),
                talker$context_edit %||% list()),
                  vault = talker$context_vault %||% cfg$context_vault),
             error = function(e) {
        warning("Conversation context editing disabled: ", conditionMessage(e),
                call. = FALSE)
        list(editing = list(enabled = FALSE, cadence = "pressure", strategy = "hybrid"),
             vault = NULL)
    })
}

.context_room_record <- function(session) {
    if (!.context_edit_archives(session)) {
        return(invisible(NULL))
    }
    session$context_checkpoint <- list(
                                       room_context = TRUE,
                                       seen_event_ids = as.list(session$seen_event_ids %||% character()),
                                       transcript = session$transcript %||% list(),
                                       rehydrate_checked = isTRUE(session$rehydrate_checked))
    invisible(NULL)
}

.context_room_snapshot <- function(session) {
    if (!.context_edit_archives(session)) {
        return(invisible(NULL))
    }
    .context_room_record(session)
    journal_snapshot(session)
}

.context_room_restore <- function(session, dir, cfg,
                                  talker = bot_talker_config(cfg)) {
    if (is.null(.context_room_config(cfg, talker)$vault)) {
        return(invisible(FALSE))
    }
    saved <- .journal_read_live(dir)
    meta <- saved$context
    if (!isTRUE(meta$room_context)) {
        return(invisible(FALSE))
    }
    # A restored history goes into a process that built its system text
    # anew, so its thinking blocks come out (R/history-replace.R). The
    # journal is installed after this, and its rollover segment then
    # holds the saved history as it was.
    history_replace(session, saved$history %||% list(), "restore")
    session$seen_event_ids <- as.character(unlist(meta$seen_event_ids,
            use.names = FALSE))
    session$transcript <- meta$transcript %||% list()
    session$rehydrate_checked <- isTRUE(meta$rehydrate_checked)
    session$context_checkpoint <- meta
    invisible(TRUE)
}

.context_room_clear <- function(session) {
    if (!.context_edit_archives(session)) {
        return(invisible(NULL))
    }
    .context_room_record(session)
    history_replace(session, list(), "clear")
    invisible(NULL)
}
