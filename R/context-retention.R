# Conversation owners may keep a recent window intact while editing older
# context. Workers never acquire this protection through policy inheritance.
.context_retention_turns <- function(session) {
    if (!isTRUE(session$context_conversation)) return(0)
    .context_edit_config(session$config, session$context_edit)$retain_turns %||% 0
}

.context_retention_boundary <- function(session, history) {
    keep <- .context_retention_turns(session)
    if (keep == 0) return(Inf)
    humans <- which(vapply(history, .context_edit_human, logical(1)))
    # The newest human entry starts the current turn. Tool-result-only user
    # messages do not start turns. Before a new prompt is appended this keeps
    # one extra completed turn, conservatively delaying labels until a later
    # checkpoint. The same boundary is rechecked when queued edits commit.
    if (length(humans) <= keep + 1) return(1L)
    humans[[length(humans) - keep]]
}
