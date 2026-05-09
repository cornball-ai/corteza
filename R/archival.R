# Retroactive-extraction runtime.
#
# Opt-in path activated by config$archival$enabled = TRUE. After a turn
# completes, the parent's history slice for that turn is moved into a
# fresh subagent (the "holder") that holds the full transcript. The
# parent keeps {summary, subagent_id} in its history so the LLM can see
# what happened and decide whether to query_subagent for detail.
#
# All logic in this file is offline-safe except archival_summarize,
# which makes a single LLM call to llm.api::agent. Default off so CRAN
# users see no behavior change.

# ---- Trigger evaluation ----

#' Decide if a finished turn qualifies for archival.
#' @param arc_cfg The `config$archival` block.
#' @param history_slice List of message entries for the just-finished turn.
#' @param depth Current archival depth (0 at the parent, increments
#'   inside subagents).
#' @param max_turns_hit Logical: did this turn end with [Max turns reached]?
#' @return Single logical.
#' @noRd
archival_should_trigger <- function(arc_cfg, history_slice, depth = 0L,
                                    max_turns_hit = FALSE) {
    cap <- arc_cfg$trigger$depth_cap %||% 3L
    if (depth >= cap) {
        return(FALSE)
    }
    if (isTRUE(arc_cfg$trigger$on_max_turns) && isTRUE(max_turns_hit)) {
        return(TRUE)
    }
    tc_threshold <- arc_cfg$trigger$tool_call_threshold %||% 10L
    if (archival_count_tool_calls(history_slice) >= tc_threshold) {
        return(TRUE)
    }
    tk_threshold <- arc_cfg$trigger$token_threshold %||% 8000L
    if (archival_estimate_tokens(history_slice) >= tk_threshold) {
        return(TRUE)
    }
    FALSE
}

#' Count tool-use/tool-result pairs in a history slice.
#'
#' Walks each entry's content. List-of-blocks form contributes any
#' entries with type tool_use or tool_result; we divide by 2 to count
#' pairs. Flat-string content contributes nothing (the token threshold
#' will catch that case).
#' @noRd
archival_count_tool_calls <- function(history_slice) {
    if (length(history_slice) == 0L) {
        return(0L)
    }
    total <- 0L
    for (entry in history_slice) {
        cnt <- entry$content
        if (is.list(cnt)) {
            for (block in cnt) {
                btype <- block$type %||% ""
                if (btype %in% c("tool_use", "tool_result")) {
                    total <- total + 1L
                }
            }
        }
    }
    as.integer(total %/% 2L)
}

#' Cheap token estimate for a history slice.
#'
#' Same `ceiling(nchar / 4)` heuristic the CLI already uses
#' (inst/bin/corteza:406-415). Walks all string content, including
#' content-block text fields.
#' @noRd
archival_estimate_tokens <- function(history_slice) {
    if (length(history_slice) == 0L) {
        return(0L)
    }
    chars <- 0L
    for (entry in history_slice) {
        cnt <- entry$content
        if (is.character(cnt)) {
            chars <- chars + sum(nchar(cnt, type = "chars"))
        } else if (is.list(cnt)) {
            for (block in cnt) {
                txt <- block$text %||% block$content %||% ""
                if (is.character(txt)) {
                    chars <- chars + sum(nchar(txt, type = "chars"))
                }
                # tool_use input gets serialized to JSON before the API
                # sees it; estimate via deparse length as a stand-in.
                if (!is.null(block$input)) {
                    chars <- chars +
                        sum(nchar(paste(deparse(block$input), collapse = " "),
                                  type = "chars"))
                }
            }
        }
    }
    as.integer(ceiling(chars / 4))
}

#' Is the slice's last entry an unfinished assistant tool_use?
#'
#' Used by maybe_archive_turn to refuse archival when the model emitted
#' a tool_use but the corresponding tool_result isn't present yet. That
#' state means the turn isn't really finished; archiving would lose the
#' tool call's context entirely.
#' @noRd
archival_slice_has_unfinished_tool_use <- function(history_slice) {
    n <- length(history_slice)
    if (n == 0L) {
        return(FALSE)
    }
    last <- history_slice[[n]]
    if (!identical(last$role, "assistant")) {
        return(FALSE)
    }
    cnt <- last$content
    if (!is.list(cnt)) {
        return(FALSE)
    }
    tool_use_ids <- character(0)
    for (block in cnt) {
        if (identical(block$type, "tool_use")) {
            tool_use_ids <- c(tool_use_ids, block$id %||% "")
        }
    }
    if (length(tool_use_ids) == 0L) {
        return(FALSE)
    }
    # Look for matching tool_result blocks anywhere in the slice. If
    # there's no matching pair for any tool_use, the turn is mid-flight.
    result_ids <- character(0)
    for (entry in history_slice) {
        cnt2 <- entry$content
        if (is.list(cnt2)) {
            for (block in cnt2) {
                if (identical(block$type, "tool_result")) {
                    result_ids <- c(result_ids,
                                    block$tool_use_id %||% block$id %||% "")
                }
            }
        }
    }
    !all(tool_use_ids %in% result_ids)
}

# ---- Transcript rendering for the summarizer ----

#' Render a history slice as plain text for the summarization prompt.
#'
#' Format: `## role\n<content>\n` repeated. Tool blocks rendered as
#' `[tool_use: name(input)]` and `[tool_result: <text>]`.
#' @noRd
archival_render_transcript <- function(history_slice) {
    if (length(history_slice) == 0L) {
        return("")
    }
    parts <- character(0)
    for (entry in history_slice) {
        role <- entry$role %||% "user"
        cnt <- entry$content
        body <- if (is.character(cnt)) {
            paste(cnt, collapse = "\n")
        } else if (is.list(cnt)) {
            block_strs <- vapply(cnt, archival_render_block, character(1))
            paste(block_strs, collapse = "\n")
        } else {
            ""
        }
        parts <- c(parts, sprintf("## %s\n%s", role, body))
    }
    paste(parts, collapse = "\n\n")
}

#' Render a single content block for the transcript.
#' @noRd
archival_render_block <- function(block) {
    btype <- block$type %||% "text"
    if (identical(btype, "text")) {
        return(block$text %||% "")
    }
    if (identical(btype, "tool_use")) {
        name <- block$name %||% "?"
        input_str <- if (!is.null(block$input)) {
            paste(deparse(block$input), collapse = " ")
        } else {
            ""
        }
        return(sprintf("[tool_use: %s(%s)]", name, input_str))
    }
    if (identical(btype, "tool_result")) {
        result_txt <- if (is.list(block$content)) {
            inner <- vapply(block$content, function(b) {
                b$text %||% ""
            }, character(1))
            paste(inner, collapse = " ")
        } else {
            as.character(block$content %||% "")
        }
        return(sprintf("[tool_result: %s]", result_txt))
    }
    sprintf("[%s]", btype)
}

#' Convert a history entry to plain text for transcript_append.
#'
#' transcript_append wants a flat string; we preserve role-tagged
#' formatting so the on-disk JSONL stays readable.
#' @noRd
archival_history_entry_to_text <- function(entry) {
    cnt <- entry$content
    if (is.character(cnt)) {
        return(paste(cnt, collapse = "\n"))
    }
    if (is.list(cnt)) {
        block_strs <- vapply(cnt, archival_render_block, character(1))
        return(paste(block_strs, collapse = "\n"))
    }
    ""
}

# ---- Summary parsing ----

#' Validate a structured summary string.
#'
#' Runs jsonlite::fromJSON in tryCatch. On parse failure returns the raw
#' text with `[unparsed]` prefix so the parent slot still has something
#' the LLM can read.
#' @noRd
archival_validate_structured <- function(text) {
    parsed <- tryCatch(
        jsonlite::fromJSON(text, simplifyVector = FALSE),
        error = function(e) NULL
    )
    if (is.null(parsed)) {
        return(paste0("[unparsed] ", text))
    }
    text
}

# ---- Summary prompt templates ----

ARCHIVAL_PROMPT_STRUCTURED <- paste(
    "You compress a completed agent turn into a JSON object so the parent",
    "agent can keep a compact record while a held subagent retains the full",
    "transcript.",
    "",
    "Produce a JSON object with keys:",
    "  \"outcome\": one short sentence describing what was accomplished.",
    "  \"key_findings\": array of strings, max 5.",
    "  \"files_touched\": array of file paths.",
    "  \"tools_used\": array of tool names.",
    "  \"open_questions\": array of strings, may be empty.",
    "",
    "Output ONLY the JSON object, no surrounding prose, no code fences.",
    sep = "\n"
)

ARCHIVAL_PROMPT_PARAGRAPH <- paste(
    "You compress a completed agent turn into one paragraph so the parent",
    "agent can keep a compact record while a held subagent retains the full",
    "transcript.",
    "",
    "Write 3-5 sentences covering the user's request, the work the agent",
    "performed, the outcome, and any unresolved threads. No bullet points,",
    "no headings.",
    sep = "\n"
)

#' Pick the system prompt for the configured summary style.
#' @noRd
archival_summary_system_prompt <- function(style) {
    if (identical(style, "structured")) {
        ARCHIVAL_PROMPT_STRUCTURED
    } else {
        ARCHIVAL_PROMPT_PARAGRAPH
    }
}

# ---- Summarization (one llm.api::agent call) ----

#' Generate a summary for a history slice via a single LLM call.
#'
#' Uses llm.api::agent with no tools and max_turns = 1. Provider/model
#' default to the parent's; archival_archive_turn applies the
#' summary$model override if the user set one.
#' @noRd
archival_summarize <- function(history_slice, style = "structured",
                               provider = "anthropic", model = NULL) {
    # Ollama JSON-mode reliability is wildly variable, so force
    # paragraph style for ollama no matter what config says.
    if (identical(provider, "ollama") && identical(style, "structured")) {
        warning("archival: ollama provider is unreliable for structured ",
                "summary; falling back to paragraph style for this call.",
                call. = FALSE)
        style <- "paragraph"
    }
    sys <- archival_summary_system_prompt(style)
    user <- sprintf("Summarize the following completed agent turn:\n\n%s",
                    archival_render_transcript(history_slice))
    resp <- tryCatch(
        llm.api::agent(prompt = user, system = sys, tools = list(),
                       model = model, provider = provider,
                       max_turns = 1L, history = list(), verbose = FALSE),
        error = function(e) {
            log_event("archival_summary_failed", error = conditionMessage(e),
                      level = "warn")
            NULL
        }
    )
    if (is.null(resp)) {
        return("[summary unavailable]")
    }
    summary <- as.character(resp$content %||% "")
    if (!nzchar(summary)) {
        return("[summary unavailable]")
    }
    if (identical(style, "structured")) {
        archival_validate_structured(summary)
    } else {
        summary
    }
}

# ---- Persistence (reuse transcript_append) ----

#' Write an archived subagent's transcript to disk.
#'
#' Reuses transcript_append with a per-subagent agent_id so each holder
#' lives in its own bucket: agents/subagent-<id>/sessions/<id>.jsonl.
#' @noRd
archival_persist_subagent <- function(subagent_id, history_slice, summary,
                                      parent_session_id, provider = "anthropic",
                                      model = NULL) {
    agent_id <- paste0("subagent-", subagent_id)
    sess <- list(sessionId = subagent_id, cwd = getwd(),
                 provider = provider, model = model)
    transcript_write_header(subagent_id, sess$cwd, agent_id)
    for (entry in history_slice) {
        role <- entry$role %||% "user"
        body <- archival_history_entry_to_text(entry)
        transcript_append(sess, role, body, provider = provider,
                          model = model, agent_id = agent_id)
    }
    transcript_append(sess, "assistant",
                      paste0("[archival summary]\n\n", summary),
                      provider = "corteza", model = "archival",
                      agent_id = agent_id)
    invisible(NULL)
}

# ---- Archive orchestrator ----

#' Spawn a holder subagent, seed it with the slice, summarize, persist.
#'
#' Returns list(summary, subagent_id) on success, NULL on any failure.
#' Failure paths log via log_event so the caller leaves history alone.
#' @noRd
archival_archive_turn <- function(turn_session, prompt, history_slice,
                                  arc_cfg, depth = 0L,
                                  parent_session_id = NULL,
                                  parent_provider = "anthropic",
                                  parent_model = NULL,
                                  config = NULL) {
    if (is.null(config)) {
        config <- load_config(getwd())
    }
    task_label <- paste0("Archive: ", archival_first_line(prompt))

    # Spawn the holder. tools = character(0) means the holder has no
    # active tools; it's a transcript repository, not an agent that
    # might fan out further on its own. Stamp archival_depth on the
    # caller session so the spawned holder records depth = caller + 1.
    turn_session$archival_depth <- as.integer(depth)
    spawn_attempt <- tryCatch({
        subagent_spawn(task = task_label, tools = character(0),
                       parent_session = turn_session, config = config)
    }, error = function(e) {
        log_event("archival_failed", phase = "spawn",
                  error = conditionMessage(e), level = "warn")
        NULL
    })
    if (is.null(spawn_attempt)) {
        return(NULL)
    }
    subagent_id <- spawn_attempt
    info <- .subagent_registry[[subagent_id]]
    if (is.null(info)) {
        log_event("archival_failed", phase = "registry_lookup",
                  level = "warn")
        return(NULL)
    }

    # Seed the child's history.
    seed_attempt <- tryCatch({
        info$session$run(
            function(h) corteza::subagent_seed_history(h),
            list(h = history_slice)
        )
        TRUE
    }, error = function(e) {
        log_event("archival_failed", phase = "seed",
                  error = conditionMessage(e), level = "warn")
        FALSE
    })
    if (!isTRUE(seed_attempt)) {
        try(subagent_kill(subagent_id), silent = TRUE)
        return(NULL)
    }

    # Generate the summary in the parent process. Provider stays with
    # the parent; model can be overridden via summary$model.
    summary_model <- arc_cfg$summary$model %||% parent_model
    summary_style <- arc_cfg$summary$style %||% "structured"
    summary <- archival_summarize(history_slice,
                                  style = summary_style,
                                  provider = parent_provider,
                                  model = summary_model)

    # Persist on disk via the existing transcript layer.
    persist_attempt <- tryCatch({
        archival_persist_subagent(subagent_id, history_slice, summary,
                                  parent_session_id = parent_session_id,
                                  provider = parent_provider,
                                  model = parent_model)
        TRUE
    }, error = function(e) {
        log_event("archival_failed", phase = "persist",
                  error = conditionMessage(e), level = "warn")
        FALSE
    })
    if (!isTRUE(persist_attempt)) {
        try(subagent_kill(subagent_id), silent = TRUE)
        return(NULL)
    }

    log_event("archival_succeeded", subagent_id = subagent_id,
              depth = depth, slice_len = length(history_slice))
    list(summary = summary, subagent_id = subagent_id)
}

#' Take the first line of a string (trimmed, capped at 80 chars).
#' @noRd
archival_first_line <- function(text) {
    if (!is.character(text) || length(text) == 0L || !nzchar(text[1])) {
        return("(no prompt)")
    }
    parts <- strsplit(text[1], "\n", fixed = TRUE)[[1]]
    line <- if (length(parts) >= 1L) parts[1] else ""
    if (is.na(line)) line <- ""
    line <- trimws(line)
    if (!nzchar(line)) {
        return("(no prompt)")
    }
    if (nchar(line) > 80L) {
        line <- paste0(substr(line, 1L, 77L), "...")
    }
    line
}

# ---- Top-level helper for call sites ----

#' Maybe archive the just-finished turn.
#'
#' Called from chat() and inst/bin/corteza after turn() returns. Reads
#' config, evaluates triggers, runs archival_archive_turn, mutates the
#' turn_session history slice in place. Defensive: any failure leaves
#' the turn untouched and logs.
#' @param turn_session The session env returned by new_session().
#' @param prompt User prompt that drove this turn.
#' @param pre_turn_len length(turn_session$history) captured BEFORE turn().
#' @param result Return value from turn() (unused for now; reserved for
#'   future trigger inputs like usage tokens).
#' @param config Loaded config list.
#' @param parent_session_id The on-disk session id (from disk_session).
#' @param max_turns_hit Did this turn end with [Max turns reached]?
#' @param depth Archival depth (0 at the parent).
#' @noRd
maybe_archive_turn <- function(turn_session, prompt, pre_turn_len, result,
                               config, parent_session_id,
                               max_turns_hit = FALSE, depth = 0L) {
    arc_cfg <- config$archival %||% list()
    if (!isTRUE(arc_cfg$enabled)) {
        return(invisible())
    }

    history <- turn_session$history %||% list()
    post_turn_len <- length(history)
    if (post_turn_len <= pre_turn_len) {
        return(invisible())
    }
    slice <- history[(pre_turn_len + 1L):post_turn_len]

    if (!archival_should_trigger(arc_cfg, slice, depth = depth,
                                 max_turns_hit = max_turns_hit)) {
        return(invisible())
    }

    if (archival_slice_has_unfinished_tool_use(slice)) {
        log_event("archival_skipped", reason = "unfinished_tool_use",
                  level = "info")
        return(invisible())
    }

    archived <- archival_archive_turn(
        turn_session = turn_session, prompt = prompt,
        history_slice = slice, arc_cfg = arc_cfg, depth = depth,
        parent_session_id = parent_session_id,
        parent_provider = turn_session$provider %||% "anthropic",
        parent_model = turn_session$model_map$cloud,
        config = config
    )
    if (is.null(archived)) {
        return(invisible())
    }

    # Replace the turn slice with one synthetic assistant message that
    # carries {summary, id}. The user prompt that drove this turn lives
    # at index pre_turn_len in the slice (first entry, role=user); we
    # preserve it. The compressed assistant block replaces everything
    # after.
    keep <- history[seq_len(pre_turn_len)]
    user_msg <- slice[[1]]
    if (!identical(user_msg$role %||% "", "user")) {
        # Defensive: if the slice doesn't start with the user prompt,
        # synthesize one so the conversation stays valid.
        user_msg <- list(role = "user", content = prompt)
    }
    archived_assistant <- list(
        role = "assistant",
        content = sprintf("[archived turn]\nsubagent_id: %s\n\n%s",
                          archived$subagent_id, archived$summary)
    )
    turn_session$history <- c(keep, list(user_msg), list(archived_assistant))

    # Refresh system prompt so the new subagent shows up in the live
    # listing on the next turn. load_context reads the registry fresh.
    new_system <- tryCatch(
        load_context(turn_session$cwd %||% getwd()),
        error = function(e) NULL
    )
    if (!is.null(new_system)) {
        turn_session$system <- new_system
    }
    invisible()
}
