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
    length(archival_history_tool_calls(history_slice))
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
    # Only flag mid-flight when the slice ends with an assistant
    # message: a tool result message after the call closes the loop
    # for that round.
    last <- history_slice[[n]]
    if (!identical(last$role %||% "", "assistant")) {
        return(FALSE)
    }
    records <- archival_history_tool_calls(history_slice)
    if (length(records) == 0L) {
        return(FALSE)
    }
    any(vapply(records, function(r) !isTRUE(r$completed), logical(1)))
}

# ---- Transcript rendering for the summarizer ----

#' Render a history slice as plain text for the summarization prompt.
#'
#' Format: `## role\n<content>\n` repeated. Tool calls render as
#' `[tool_use: name(args)]` followed by `[tool_result: <text>]`,
#' regardless of whether the underlying history is Anthropic-style
#' (typed content blocks) or OpenAI-style (`tool_calls` field plus
#' `role = "tool"` result messages). Built off the canonical record
#' walk so summaries on moonshot/kimi/ollama actually include the
#' tool calls.
#' @noRd
archival_render_transcript <- function(history_slice) {
    if (length(history_slice) == 0L) {
        return("")
    }
    records <- archival_history_tool_calls(history_slice)
    # Index records by call_message_index for cheap lookup. Each
    # assistant message can carry multiple tool calls.
    by_call_idx <- list()
    for (r in records) {
        key <- as.character(r$call_message_index)
        by_call_idx[[key]] <- c(by_call_idx[[key]], list(r))
    }
    # Skip OpenAI-shape result messages here; they get rendered inline
    # alongside the call that produced them.
    parts <- character(0)
    for (i in seq_along(history_slice)) {
        entry <- history_slice[[i]]
        role <- entry$role %||% "user"
        if (identical(role, "tool")) {
            next
        }
        text <- archival_entry_plain_text(entry)
        call_lines <- character(0)
        calls_here <- by_call_idx[[as.character(i)]]
        if (length(calls_here) > 0L) {
            for (r in calls_here) {
                call_lines <- c(call_lines, archival_record_render(r))
            }
        }
        body <- paste(c(text, call_lines), collapse = "\n")
        if (!nzchar(body)) {
            next
        }
        parts <- c(parts, sprintf("## %s\n%s", role, body))
    }
    paste(parts, collapse = "\n\n")
}

#' Render a record as `[tool_use: name(args)]\n[tool_result: text]`.
#'
#' Also handles the unfinished case (no result yet).
#' @noRd
archival_record_render <- function(r) {
    args_str <- if (!is.null(r$arguments)) {
        paste(deparse(r$arguments), collapse = " ")
    } else {
        ""
    }
    call <- sprintf("[tool_use: %s(%s)]", r$name %||% "?", args_str)
    if (isTRUE(r$completed)) {
        sprintf("%s\n[tool_result: %s]", call, r$result %||% "")
    } else {
        sprintf("%s\n[tool_result: (pending)]", call)
    }
}

#' Plain-text content for a history entry, ignoring tool blocks.
#'
#' Tool calls render via the record walk; this strips them out so
#' they're not double-counted.
#' @noRd
archival_entry_plain_text <- function(entry) {
    cnt <- entry$content
    if (is.character(cnt)) {
        return(paste(cnt, collapse = "\n"))
    }
    if (is.list(cnt)) {
        parts <- vapply(cnt, function(block) {
            if (identical(block$type %||% "text", "text")) {
                return(block$text %||% "")
            }
            ""
        }, character(1))
        return(paste(parts[nzchar(parts)], collapse = "\n"))
    }
    ""
}

#' Convert a history entry to plain text for transcript_append.
#'
#' transcript_append wants a flat string; we preserve role-tagged
#' formatting so the on-disk JSONL stays readable. Handles both the
#' Anthropic content-block shape and the OpenAI tool_calls-field shape
#' so dumps from moonshot/kimi/ollama sessions are not just empty
#' strings.
#' @noRd
archival_history_entry_to_text <- function(entry) {
    role <- entry$role %||% ""
    cnt <- entry$content
    parts <- character(0)

    # OpenAI-shape role=="tool" message: render once as
    # `[tool_result: ...]`. Skip the generic character branch below to
    # avoid duplicating the body.
    if (identical(role, "tool")) {
        parts <- c(parts,
                   sprintf("[tool_result: %s]", as.character(cnt %||% "")))
    } else if (is.character(cnt)) {
        # Collapse first; nzchar(vec) returns a vector and would error
        # the if-condition for multi-element character content.
        flat <- paste(cnt, collapse = "\n")
        if (nzchar(flat)) {
            parts <- c(parts, flat)
        }
    } else if (is.list(cnt)) {
        for (block in cnt) {
            btype <- block$type %||% "text"
            if (identical(btype, "text")) {
                txt <- paste(block$text %||% "", collapse = "\n")
                if (nzchar(txt)) {
                    parts <- c(parts, txt)
                }
            } else if (identical(btype, "tool_use")) {
                args_str <- if (!is.null(block$input)) {
                    paste(deparse(block$input), collapse = " ")
                } else ""
                parts <- c(parts, sprintf("[tool_use: %s(%s)]",
                        block$name %||% "?", args_str))
            } else if (identical(btype, "tool_result")) {
                parts <- c(parts, sprintf(
                        "[tool_result: %s]",
                        .archival_block_result_text(block)
                    ))
            }
        }
    }

    # OpenAI-shape assistant entries carry tool calls in a side field.
    if (!is.null(entry$tool_calls)) {
        for (tc in entry$tool_calls) {
            fn <- tc$`function` %||% list()
            args_raw <- fn$arguments %||% ""
            args_str <- if (is.character(args_raw)) {
                paste(args_raw, collapse = " ")
            } else {
                paste(deparse(args_raw), collapse = " ")
            }
            parts <- c(parts, sprintf("[tool_use: %s(%s)]",
                                      fn$name %||% "?", args_str))
        }
    }

    paste(parts, collapse = "\n")
}

# ---- Summary parsing ----

#' Validate a structured summary string.
#'
#' Runs jsonlite::fromJSON in tryCatch. On parse failure returns the raw
#' text with `[unparsed]` prefix so the parent slot still has something
#' the LLM can read.
#' @noRd
archival_validate_structured <- function(text) {
    parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                       error = function(e) NULL)
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
#' Uses `llm.api::agent` with no tools and `max_turns = 1`. The whole
#' call is run inside `callr::r(timeout = ...)` so a hung provider can't
#' wedge the parent CLI: we kill the child process and return a
#' placeholder string instead.
#' @noRd
archival_summarize <- function(history_slice, style = "structured",
                               provider = "anthropic", model = NULL,
                               timeout_seconds = 30L) {
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
    summary <- tryCatch(
                        callr::r(
                                 function(prompt, system, model, provider) {
        resp <- llm.api::agent(
                               prompt = prompt, system = system, tools = list(),
                               model = model, provider = provider,
                               max_turns = 1L, history = list(), verbose = FALSE
        )
        as.character(resp$content %||% "")
    },
                                 args = list(prompt = user, system = sys, model = model,
                provider = provider),
                                 timeout = timeout_seconds
        ),
                        error = function(e) {
        log_event("archival_summary_failed",
                  error = conditionMessage(e), level = "warn")
        NULL
    }
    )
    if (is.null(summary) || !nzchar(summary)) {
        return("[summary unavailable]")
    }
    if (identical(style, "structured")) {
        archival_validate_structured(summary)
    } else {
        summary
    }
}

#' Generate the summary in a background process; do not wait.
#'
#' Used by `archival_archive_turn` when `archival$async` is enabled
#' (the default). The bg child runs the same summary call as
#' `archival_summarize` and appends the result to the holder's
#' on-disk transcript when finished. The parent doesn't poll or join;
#' if the bg child crashes, the holder still has the full slice for
#' `query_subagent` to draw on.
#' @noRd
archival_summarize_bg <- function(subagent_id, history_slice, style,
                                  provider, model, cwd = getwd(),
                                  timeout_seconds = 60L) {
    callr::r_bg(
                function(subagent_id, history_slice, style, provider, model,
                         cwd, timeout_seconds) {
        library(corteza)
        setwd(cwd)
        archival_summarize <- utils::getFromNamespace("archival_summarize",
            "corteza")
        transcript_append <- utils::getFromNamespace(
            "transcript_append", "corteza")
        summary <- tryCatch(
                            archival_summarize(history_slice, style = style,
                provider = provider, model = model,
                timeout_seconds = timeout_seconds),
                            error = function(e) "[summary unavailable]"
        )
        agent_id <- paste0("subagent-", subagent_id)
        sess <- list(sessionId = subagent_id, cwd = cwd,
                     provider = provider, model = model)
        tryCatch(
                 transcript_append(
                                   sess, "assistant",
                                   paste0("[archival summary]\n\n", summary),
                                   provider = "corteza", model = "archival",
                                   agent_id = agent_id
            ),
                 error = function(e) NULL
        )
        invisible(TRUE)
    },
                args = list(subagent_id = subagent_id,
                            history_slice = history_slice,
                            style = style, provider = provider, model = model,
                            cwd = cwd, timeout_seconds = timeout_seconds),
                supervise = FALSE
    )
    invisible(NULL)
}

# ---- Persistence (reuse transcript_append) ----

#' Write an archived subagent's transcript to disk.
#'
#' Reuses transcript_append with a per-subagent agent_id so each holder
#' lives in its own bucket: agents/subagent-<id>/sessions/<id>.jsonl.
#' @noRd
archival_persist_subagent <- function(subagent_id, history_slice, summary,
                                      parent_session_id,
                                      provider = "anthropic", model = NULL) {
    agent_id <- paste0("subagent-", subagent_id)
    sess <- list(sessionId = subagent_id, cwd = getwd(), provider = provider,
                 model = model)
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
                                  parent_model = NULL, config = NULL) {
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

    # From here on the holder is registered in .subagent_registry but
    # the caller (maybe_archive_turn) hasn't yet rewired parent
    # history to reference it. An interrupt anywhere in the seed /
    # persist / summarize plumbing below would unwind before
    # maybe_archive_turn's own on.exit guard ever installed, leaving
    # the holder live with no owner -- a silent memory + disk leak.
    # Guard it here too; release only at the explicit success
    # returns (transferred <- TRUE before each).
    transferred <- FALSE
    on.exit({
        if (!isTRUE(transferred)) {
            try(subagent_kill(subagent_id), silent = TRUE)
            log_event("archival_orphan_cleaned",
                      subagent_id = subagent_id,
                      phase = "archive_internal",
                      level = "warn")
        }
    }, add = TRUE)

    info <- .subagent_registry[[subagent_id]]
    if (is.null(info)) {
        log_event("archival_failed", phase = "registry_lookup", level = "warn")
        return(NULL)
    }
    # Holders aren't doing active work -- they hold state for later
    # query_subagent calls. The default 30-minute subagent timeout was
    # designed for tool-using subagents where a stuck child shouldn't
    # linger. Override it here so holders live as long as the parent
    # process. (One year is "effectively unlimited" given subagents
    # die with the CLI anyway.)
    .subagent_registry[[subagent_id]]$timeout <-
    Sys.time() + 365 * 24 * 60 * 60

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

    summary_model <- arc_cfg$summary$model %||% parent_model
    summary_style <- arc_cfg$summary$style %||% "structured"
    is_async <- isTRUE(arc_cfg$async %||% TRUE)

    if (is_async) {
        # Persist the slice now with a placeholder summary so the
        # holder's transcript has the body content immediately. The
        # real summary is appended later by the bg process. The
        # placeholder is what shows up in the parent's history; the
        # holder still owns the full slice for query_subagent.
        placeholder <- sprintf(
                               "[archived turn pending summary] | task=%s | %d entries",
                               archival_first_line(prompt), length(history_slice)
        )
        persist_attempt <- tryCatch({
            archival_persist_subagent(subagent_id, history_slice,
                                      placeholder,
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
        # Fire-and-forget bg summary. supervise = FALSE so it survives
        # parent exit; if it fails, the placeholder stays.
        timeout_secs <- as.integer(arc_cfg$summary$timeout_seconds %||% 60L)
        archival_summarize_bg(subagent_id, history_slice,
                              style = summary_style,
                              provider = parent_provider,
                              model = summary_model,
                              cwd = getwd(),
                              timeout_seconds = timeout_secs)
        log_event("archival_succeeded_async", subagent_id = subagent_id,
                  depth = depth, slice_len = length(history_slice))
        # Hand the holder back to maybe_archive_turn intact; its
        # own on.exit guard owns the parent-history-collapse window.
        transferred <- TRUE
        return(list(summary = placeholder, subagent_id = subagent_id))
    }

    # Synchronous path. Used when archival$async is set to FALSE.
    timeout_secs <- as.integer(arc_cfg$summary$timeout_seconds %||% 30L)
    summary <- archival_summarize(history_slice, style = summary_style,
                                  provider = parent_provider,
                                  model = summary_model,
                                  timeout_seconds = timeout_secs)
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
    # Hand the holder back to maybe_archive_turn intact; its own
    # on.exit guard owns the parent-history-collapse window.
    transferred <- TRUE
    list(summary = summary, subagent_id = subagent_id)
}

#' Take the first line of a string (trimmed, capped at 80 chars).
#' @noRd
archival_first_line <- function(text) {
    if (!is.character(text) || length(text) == 0L || !nzchar(text[1])) {
        return("(no prompt)")
    }
    parts <- strsplit(text[1], "\n", fixed = TRUE)[[1]]
    if (length(parts) >= 1L) {
        line <- parts[1]
    } else {
        line <- ""
    }
    if (is.na(line)) {
        line <- ""
    }
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

    # Surface the wait. With async = TRUE (default) the spawn + seed +
    # persist round-trip is ~250-500ms; the summary runs in r_bg and
    # the prompt returns immediately. With async = FALSE the
    # summarization LLM call is sync (capped by summary.timeout_seconds)
    # and can be tens of seconds.
    if (depth == 0L && interactive()) {
        if (isTRUE(arc_cfg$async %||% TRUE)) {
            cat("\u25CF Archiving (summary in background)...\n")
        } else {
            cat("\u25CF Archiving turn (this may take a few seconds)...\n")
        }
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
        if (depth == 0L && interactive()) {
            cat("  Archive failed; turn left intact (see log for details).\n")
        }
        return(invisible())
    }

    # archival_archive_turn returned a holder, but the parent doesn't
    # reference it yet (the synthetic assistant block hasn't replaced
    # the slice in turn_session$history). If anything interrupts
    # between here and the history collapse below, the holder is left
    # in .subagent_registry with no parent reference -- a silent leak.
    # Track the transfer and kill the orphan on any non-completion
    # exit path.
    transferred <- FALSE
    on.exit({
        if (!isTRUE(transferred) && !is.null(archived$subagent_id)) {
            try(subagent_kill(archived$subagent_id), silent = TRUE)
            log_event("archival_orphan_cleaned",
                      subagent_id = archived$subagent_id,
                      level = "warn")
        }
    }, add = TRUE)

    if (depth == 0L && interactive()) {
        info <- .subagent_registry[[archived$subagent_id]]
        handle <- if (!is.null(info$seq)) {
            as.character(info$seq)
        } else {
            substr(archived$subagent_id, 1L, 8L)
        }
        cat(sprintf("  Archived to subagent [%s]. /ask %s ...\n",
                    handle, handle))
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
    # Ownership transferred: parent history now references the holder.
    # The on.exit guard above sees this flag set and lets the holder
    # live.
    transferred <- TRUE

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

