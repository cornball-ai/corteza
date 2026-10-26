# Host hooks: policy the bot loop asks about but does not own.
#
# corteza runs a conversation. What happens to a conversation once it
# ends, and where a thread's earlier context comes from, are the host's
# decisions (cerebro files segments and folds them into topic rooms).
# The loop calls out through named hooks and carries on without them:
# with none supplied, /clear archives and resets, and a thread reply
# starts from the thread's own history.

BOT_HOOK_NAMES <- c("on_clear", "thread_context")

# Validate the host's hook list once, at init, so a misspelled slot is
# an error at startup rather than a hook that silently never fires.
bot_hooks <- function(hooks) {
    if (is.null(hooks)) {
        return(list())
    }
    if (!is.list(hooks) || (length(hooks) && is.null(names(hooks)))) {
        stop("hooks must be NULL or a named list of functions", call. = FALSE)
    }
    unknown <- setdiff(names(hooks), BOT_HOOK_NAMES)
    if (length(unknown)) {
        stop("unknown hook(s): ", paste(unknown, collapse = ", "),
             "; known: ", paste(BOT_HOOK_NAMES, collapse = ", "),
             call. = FALSE)
    }
    bad <- names(hooks)[!vapply(hooks, is.function, logical(1))]
    if (length(bad)) {
        stop("hook(s) must be functions: ", paste(bad, collapse = ", "),
             call. = FALSE)
    }
    hooks
}

# What on_clear handed back, or NULL when it is not a segment the
# acknowledgement can name. The hook's contract is list(id, name) or
# NULL; anything else is treated as "no segment" rather than letting a
# malformed return break the /clear acknowledgement.
bot_hook_segment <- function(seg) {
    if (!is.list(seg) || !is.character(seg$name) || length(seg$name) != 1L ||
        is.na(seg$name) || !nzchar(seg$name)) {
        return(NULL)
    }
    seg
}
