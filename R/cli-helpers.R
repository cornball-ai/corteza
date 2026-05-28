# Display helpers and small shell/git utilities lifted out of
# `inst/bin/corteza` so the same code can back the chat() slash
# commands that need them (/status, /doctor, /config, /diff, /review,
# /compact). The CLI script still drives the I/O (printing colors,
# reading prompts); the package owns the formatting and data
# collection.
#
# These are intentionally @noRd — internal to corteza. None of the
# external MCP surfaces or downstream packages need them. Promote to
# @export only if a real downstream caller appears.

# ---- Model name normalization -----------------------------------------

#' Resolve a per-call model name against the provider's known synonyms.
#'
#' If `model` is supplied and provider-known, return it (sometimes
#' rewritten — e.g. moonshot's `kimi-k2` is the legacy name for the
#' currently-deployed `kimi-k2.6`). If `model` is NULL/empty, fall
#' back to the provider's default from `default_provider_model()`.
#' @noRd
resolve_provider_model <- function(provider, model = NULL) {
    if (!is.null(model) && nzchar(model)) {
        if (identical(provider, "moonshot") && identical(model, "kimi-k2")) {
            return("kimi-k2.6")
        }
        return(model)
    }
    default_provider_model(provider)
}

#' Pick the temperature to use for a one-shot llm.api::chat call.
#'
#' Moonshot's hosted Kimi rejects temperatures < 1; for them we always
#' send 1. Other providers honor whatever the caller passed.
#' @noRd
preferred_chat_temperature <- function(provider, temperature) {
    if (identical(provider, "moonshot")) {
        return(1)
    }
    temperature
}

# ---- Process / git helpers --------------------------------------------

#' Run a system command and capture stdout+stderr together with the
#' exit status. Returns a list with `lines`, joined `text`, and
#' `status`. Used by the git helpers below and (formerly) the CLI's
#' tool buffer trace, but the layout suits any "run X, capture
#' result" path so we keep it generic.
#' @noRd
capture_command <- function(command, args = character()) {
    output <- tryCatch(
                       system2(command, args, stdout = TRUE, stderr = TRUE),
                       error = function(e) {
        structure(paste("Error:", e$message), status = 1L)
    }
    )
    list(lines = output, text = paste(output, collapse = "\n"),
         status = attr(output, "status") %||% 0L)
}

#' Cap an arbitrary text blob at `max_lines` lines and `max_chars`
#' characters, appending a `[truncated ...]` marker when either limit
#' fires. Used by /diff and /review so unbounded git output doesn't
#' flood the terminal or the LLM context.
#' @noRd
truncate_output <- function(text, max_lines = 300L, max_chars = 60000L) {
    if (is.null(text) || nchar(text) == 0L) {
        return("")
    }
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    if (length(lines) > max_lines) {
        lines <- c(lines[seq_len(max_lines)],
                   sprintf("[truncated at %d lines]", max_lines))
    }
    text <- paste(lines, collapse = "\n")
    if (nchar(text) > max_chars) {
        text <- paste0(substr(text, 1L, max_chars),
                       "\n[truncated by character limit]")
    }
    text
}

#' Are we inside a git working tree?
#' @noRd
in_git_repo <- function() {
    result <- capture_command("git", c("rev-parse", "--is-inside-work-tree"))
    isTRUE(result$status == 0L) && identical(trimws(result$text), "true")
}

#' Collect the git diff against a ref (default `HEAD`), with status
#' shown when the worktree is clean against the ref. Used by /diff
#' and /review.
#' @noRd
collect_git_diff <- function(ref = NULL) {
    if (!in_git_repo()) {
        return(list(ok = FALSE, text = "Not inside a git repository."))
    }

    target <- trimws(ref %||% "")
    if (nchar(target) > 0L) {
        target <- target
    } else {
        target <- "HEAD"
    }

    status <- capture_command("git", c("status", "--short"))
    diff <- capture_command("git",
                            c("diff", "--no-ext-diff", "--find-renames", "--unified=3", target))

    if (diff$status != 0L) {
        return(list(ok = FALSE, text = diff$text))
    }
    if (nchar(trimws(diff$text)) == 0L) {
        if (nchar(trimws(status$text)) > 0L) {
            return(list(
                        ok = FALSE,
                        text = paste0(
                                      "No tracked diff against ", target,
                                      ". Untracked or ignored files may still be present.\n\n",
                                      truncate_output(status$text, max_lines = 50L,
                            max_chars = 4000L)
                    )
                ))
        }
        return(list(ok = FALSE, text = paste("No git diff against",
                    target, ".")))
    }

    list(ok = TRUE,
         target = target,
         status = truncate_output(status$text, max_lines = 80L,
                                  max_chars = 6000L),
         diff = truncate_output(diff$text, max_lines = 500L, max_chars = 70000L))
}

# ---- Provider / model availability checks -----------------------------

#' Translate a provider name to its expected API-key environment
#' variable. Used by /doctor and /status to flag missing keys.
#' @noRd
provider_env_var <- function(provider) {
    switch(provider, anthropic = "ANTHROPIC_API_KEY",
           openai = "OPENAI_API_KEY", paste0(toupper(provider), "_API_KEY"))
}

#' Quick reachability check for the configured provider.
#'
#' For `ollama`, calls `validate_model()` (which talks to the local
#' Ollama HTTP API). For everything else, just checks that the
#' provider's API-key env var is set — we don't ping the cloud
#' provider just to render /status.
#'
#' Returns `list(ok = <logical>, message = <character>)`.
#' @noRd
provider_status <- function(provider, model = NULL) {
    if (identical(provider, "ollama")) {
        err <- tryCatch({
            validate_model(provider, model)
            NULL
        }, error = function(e) conditionMessage(e))

        if (is.null(err)) {
            return(list(ok = TRUE, message = "ollama reachable"))
        }
        return(list(ok = FALSE, message = err))
    }

    env_var <- provider_env_var(provider)
    if (nchar(Sys.getenv(env_var, "")) == 0L) {
        return(list(ok = FALSE, message = paste("missing", env_var)))
    }

    list(ok = TRUE, message = paste(env_var, "set"))
}

# ---- /status, /config, /doctor formatters -----------------------------

#' One-line-per-field snapshot for the /status slash command. Pure
#' formatter — caller passes the already-resolved values (live token
#' total, context-limit ceiling, etc.) so this function is trivially
#' unit-testable without hitting any state.
#' @noRd
format_status_summary <- function(session, provider, display_model, tools,
                                  opts, config, session_tokens,
                                  context_limit, context_files, skill_docs) {
    memory_mode <- if (isTRUE(config$context_include_memory_logs) ||
        isTRUE(config$memory_flush_enabled)) {
        "legacy corteza memory enabled"
    } else {
        "legacy corteza memory disabled"
    }
    paste(
          c(
            sprintf("Session: %s", session$sessionKey %||% "(unnamed)"),
            sprintf("Model: %s @ %s", display_model %||% "(default)",
                    provider %||% "(default)"),
            sprintf("Tools: %d | Dry-run: %s",
                    length(tools %||% list()),
                if (isTRUE(opts$dry_run)) "on" else "off"),
            sprintf("Context: %d project file(s) | %d skill doc(s) | %s",
                    length(context_files %||% character()),
                    length(skill_docs %||% character()), memory_mode),
            sprintf("Legacy memory tools: %s",
                if (isTRUE(config$legacy_memory_tools_enabled)) {
                    "visible"
                } else {
                    "hidden"
                }),
            sprintf("Live context: %s / %s tokens",
                    format_tokens(session_tokens %||% 0L),
                    format_tokens(context_limit %||% 0L)),
            sprintf("Approval mode: %s", config$approval_mode %||% "ask")
        ),
          collapse = "\n"
    )
}

#' Multi-line printable snapshot of the runtime config the
#' user/agent should care about. Pure formatter; doesn't read disk.
#' @noRd
format_config_summary <- function(config, provider, display_model, opts) {
    paste(
          c(
            "Runtime config",
            sprintf("provider: %s", provider %||% "(default)"),
            sprintf("model: %s", display_model %||% "(default)"),
            sprintf("port: %s",
                if (is.null(opts$port)) "(n/a)" else as.character(opts$port)),
            sprintf("tools: %s",
                if (is.null(opts$tools)) {
                    "all"
                } else {
                    paste(opts$tools, collapse = ", ")
                }),
            sprintf("context files: %s",
                if (length(config$context_files) > 0L) {
                    paste(config$context_files, collapse = ", ")
                } else {
                    "(none)"
                }),
            sprintf("daily memory logs: %s",
                if (isTRUE(config$context_include_memory_logs)) {
                    "enabled"
                } else {
                    "disabled"
                }),
            sprintf("compaction memory flush: %s",
                if (isTRUE(config$memory_flush_enabled)) {
                    "enabled"
                } else {
                    "disabled"
                }),
            sprintf("legacy memory tools: %s",
                if (isTRUE(config$legacy_memory_tools_enabled)) {
                    "visible"
                } else {
                    "hidden"
                }),
            sprintf("approval mode: %s", config$approval_mode %||% "ask"),
            sprintf("dangerous tools: %s",
                    paste(config$dangerous_tools %||% character(), collapse = ", "))
        ),
          collapse = "\n"
    )
}

#' Diagnostic snapshot for /doctor. Pulls a few live checks
#' (provider reachability, git status, approval file presence) so the
#' user can spot a misconfigured environment quickly.
#'
#' For tests, the `provider_check_fn` / `git_check_fn` args let
#' callers swap in stubs that don't hit the network or shell out.
#' @noRd
format_doctor_report <- function(cwd, session, provider, display_model,
                                 tools, config, context_files, skill_docs,
                                 provider_check_fn = provider_status,
                                 git_check_fn = in_git_repo) {
    provider_check <- provider_check_fn(
                                        provider,
                                        model = if (!is.null(session$model)) session$model else NULL
    )
    approvals_path <- file.path(cwd, ".corteza", "approvals.json")

    paste(
          c(
            "corteza doctor",
            sprintf("cwd: %s", cwd),
            sprintf("session: %s", session$sessionKey %||% "(unnamed)"),
            sprintf("provider: %s (%s)",
                    provider %||% "(default)",
                if (isTRUE(provider_check$ok)) {
                    provider_check$message
                } else {
                    paste("check failed:", provider_check$message)
                }),
            sprintf("model: %s", display_model %||% "(default)"),
            sprintf("tools: %d available (in-process)",
                    length(tools %||% list())),
            sprintf("git: %s",
                if (isTRUE(git_check_fn())) {
                    "repository detected"
                } else {
                    "not a git repository"
                }),
            sprintf("context files: %d",
                    length(context_files %||% character())),
            sprintf("skill docs: %d", length(skill_docs %||% character())),
            sprintf("daily memory logs: %s",
                if (isTRUE(config$context_include_memory_logs)) {
                    "enabled"
                } else {
                    "disabled"
                }),
            sprintf("compaction memory flush: %s",
                if (isTRUE(config$memory_flush_enabled)) {
                    "enabled"
                } else {
                    "disabled"
                }),
            sprintf("legacy memory tools: %s",
                if (isTRUE(config$legacy_memory_tools_enabled)) {
                    "visible"
                } else {
                    "hidden"
                }),
            sprintf("approval mode: %s", config$approval_mode %||% "ask"),
            sprintf("project approvals: %s",
                if (file.exists(approvals_path)) {
                    approvals_path
                } else {
                    "none"
                })
        ),
          collapse = "\n"
    )
}

# ---- /review -----------------------------------------------------------

#' Send a code-review request to the configured provider with the
#' working tree diff as the body. Caller is responsible for
#' collecting the diff (typically via `collect_git_diff()`); this
#' helper only handles the prompt assembly and the chat call.
#'
#' Returns the llm.api::chat result list or an error object so the
#' caller can render the failure case.
#'
#' For tests, pass `chat_fn = function(...) list(content = "stub")`
#' to bypass the network round-trip.
#' @noRd
run_review <- function(provider, model, diff_target, diff_status, diff_text,
                       chat_fn = llm.api::chat) {
    review_prompt <- paste(
                           "Review the current git changes.",
                           "Focus on bugs, behavioral regressions, risky assumptions, and missing tests.",
                           "List concrete findings first with file paths and line references when the diff makes them available.",
                           "If there are no material issues, reply with exactly: No findings.",
                           "",
                           sprintf("Git diff target: %s", diff_target),
                           "",
                           "Git status:",
                           diff_status %||% "(clean)",
                           "",
                           "Diff:",
                           diff_text,
                           sep = "\n"
    )

    tryCatch({
        chat_fn(
                prompt = review_prompt,
                provider = provider,
                model = resolve_provider_model(provider, model),
                system = paste(
                               "You are performing code review on local changes.",
                               "Findings must come first and should prioritize correctness over style."
            ),
                temperature = preferred_chat_temperature(provider, 0.1)
        )
    }, error = function(e) e)
}

# ---- /compact ----------------------------------------------------------

#' The summarization prompt for whole-session compaction. Kept as
#' package data so /compact (chat() and CLI) and any future surface
#' that wants the same compaction shape can share one canonical
#' phrasing.
#' @noRd
.compact_prompt <- "
Summarize this conversation concisely, preserving:
1. What was accomplished (completed tasks, files modified)
2. Current work in progress
3. Key decisions and constraints mentioned
4. Pending tasks or next steps
5. Any errors encountered and their resolution

Be specific about file names, function names, and technical details.
Format as a structured summary the assistant can use to continue the work.
"

#' Whole-session compaction: asks the configured provider for a
#' single-paragraph summary of the conversation so far. Returns
#' `list(summary, tokens)` on success and NULL on failure (the chat
#' call already printed an error in the failure branch). Callers are
#' responsible for replacing the session's history with the summary
#' and persisting.
#'
#' For tests, `chat_fn` can be stubbed to avoid the network round-trip.
#' @noRd
do_compact <- function(session, provider, model, chat_fn = llm.api::chat,
                       emit = function(...) cat(...)) {
    emit("Auto-compacting conversation...\n")

    # Elide oversized message bodies (e.g. a huge tool result already in
    # history) so the summarization prompt itself can't blow the model
    # limit -- this is what lets /compact recover an already-wedged
    # session. compact_message_text() caps each body; .compact_trim_total()
    # drops the oldest messages if the aggregate still overflows.
    rendered <- vapply(session$messages %||% list(), function(m) {
        text <- if (is.list(m$content) && length(m$content) > 0L &&
                       !is.null(m$content[[1L]]$text)) {
            m$content[[1L]]$text
        } else {
            m$content
        }
        sprintf("[%s]: %s", m$role, compact_message_text(text))
    }, character(1L))
    conv_text <- paste(.compact_trim_total(rendered), collapse = "\n\n")

    summary_prompt <- sprintf("%s\n\n---\nConversation to summarize:\n%s",
                              .compact_prompt, conv_text)

    result <- tryCatch({
        chat_fn(
                prompt = summary_prompt,
                provider = provider,
                model = resolve_provider_model(provider, model),
                system = "You are a helpful assistant that creates concise conversation summaries.",
                temperature = preferred_chat_temperature(provider, 0.3)
        )
    }, error = function(e) {
        emit(sprintf("Compaction failed: %s\n", conditionMessage(e)))
        NULL
    })

    if (is.null(result)) {
        return(NULL)
    }

    summary <- result$content %||% ""
    new_tokens <- estimate_text_tokens(summary)

    emit(sprintf("Compacted to ~%s tokens\n", format_tokens(new_tokens)))

    list(summary = summary, tokens = new_tokens)
}

