# A local guide can show the settings before a user's first model request.
start_guide <- function(ctx, topic) {
    topics <- list(
        models = c("Models and subscriptions",
            "/model lists available routes. /model <name> <provider> switches the work model.",
            "anthropic_claude uses a Claude subscription. openai_codex uses a Codex subscription.",
            "anthropic and openai use API credentials. /doctor checks the current route."),
        approvals = c("Who approves tools",
            "/permissions shows your mode. Choose review to let a model rule on eligible prompts.",
            "/plan allows inspection. A person approves leaving plan mode.",
            "/auto <goal> starts a separate supervised run with its own monitor and limits."),
        work = c("Who runs the tools",
            "In the terminal and chat(), the current model runs tools by default.",
            'Set "talker": {"enabled": true} to delegate work to a doer. /jobs shows progress.',
            'Set "task_list_enabled": true for task tools and /tasks.'),
        context = c("History and prompt caching",
            "/context shows the token budget. /context-edit status shows how history is managed.",
            'For model-managed history set "context_edit": {"enabled": true, "cadence": "always", "strategy": "clm"}.',
            'Anthropic defaults to a 5-minute prompt cache. Set "cache" to "1h" for longer pauses',
            'or "none" to turn it off. The cache is separate from the journal on disk.'),
        storage = c("What stays on disk",
            "Idle session journals and traces default to 90 days. Transcripts are kept.",
            "Audit payloads, active journal segments and old R snapshots each have an opt-in age.",
            "Set a positive number of days under retention in your personal config using",
            "audit_payload_days, journal_segment_days or workspace_snapshot_days. Zero keeps them.",
            "Preview eligible files in R with corteza::sessions_prune(dry_run = TRUE)."))
    if (!length(topic)) {
        cat("Getting to know corteza\n")
        if (!is.null(ctx)) cat("Current route: ", ctx$provider, " / ", ctx$model, "\n", sep = "")
        command <- if (is.null(ctx)) "corteza guide" else "/guide"
        for (key in names(topics)) {
            cat(sprintf("  %s %-10s %s\n", command, key, topics[[key]][1L]))
        }
    } else if (length(topic) == 1L && topic %in% names(topics)) {
        cat(topics[[topic]], sep = "\n")
    } else {
        cat("Choose one of: ", paste(names(topics), collapse = ", "), "\n", sep = "")
    }
    cat('/config shows effective settings. In R, vignette("configuration", package = "corteza")',
        " lists the full set.\n", sep = "")
    invisible(NULL)
}
