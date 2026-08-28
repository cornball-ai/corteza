# Bounded auto mode: the agent driving itself, supervised.
#
# The loop is the easy half. What makes it runnable unattended is in
# R/monitor.R -- the authority gate that stands in for the human at the
# approval prompt. This file is the budget, the progress measurement,
# and the stop conditions around it.
#
# Order of checks per iteration is deliberate: everything mechanical
# runs first, because it is free, deterministic, and cannot be talked
# out of its answer. The monitor is only asked when nothing cheaper has
# already decided to stop.

# ---- Progress measurement ----

# Directories never worth hashing. .git churns on every command and
# would report progress for a loop that did nothing but run git status.
.AUTO_SKIP_DIRS <- c(".git", ".Rproj.user", ".Rcheck", "node_modules",
                     "__pycache__", ".venv", "renv/library")

#' Files whose content defines "did anything change".
#'
#' Inside a git repo, ask git: `ls-files -co --exclude-standard` is the
#' tracked-or-untracked set minus everything .gitignore'd, which is
#' exactly the right scope and costs one process. Outside one, walk the
#' tree and skip the usual noise by hand.
#'
#' @param dir Directory to scan.
#' @return Character vector of paths relative to `dir`.
#' @noRd
auto_tracked_files <- function(dir = getwd()) {
    # suppressWarnings: git_run uses system2(stdout = TRUE), which warns on
    # a nonzero exit. Outside a repo `rev-parse` exits 128 by design and
    # the fallback below is the correct answer, so the warning is noise on
    # a supported path rather than a signal.
    in_repo <- identical(
                         suppressWarnings(tryCatch(
                git_run(c("rev-parse", "--is-inside-work-tree"), path = dir)$text,
                error = function(e) "")),
                         "true")
    if (in_repo) {
        res <- suppressWarnings(git_run(c("ls-files", "-co",
                    "--exclude-standard"), path = dir))
        if (identical(res$status, 0L) && nzchar(res$text)) {
            return(unlist(strsplit(res$text, "\n", fixed = TRUE)))
        }
    }
    all <- list.files(dir, recursive = TRUE, all.files = TRUE,
                      no.. = TRUE, full.names = FALSE)
    keep <- !vapply(all, function(p) {
        any(vapply(.AUTO_SKIP_DIRS,
                   function(d) startsWith(p, paste0(d, "/")) || identical(p, d),
                   logical(1)))
    }, logical(1), USE.NAMES = FALSE)
    all[keep]
}

#' Snapshot the workspace by content.
#'
#' Content hash, not size + mtime. A rewrite that changes nothing but
#' the timestamp would otherwise read as progress, and an edit that
#' happens to preserve file size would vanish -- so a loop could look
#' busy while thrashing, or look stalled while working.
#'
#' The snapshot is the *baseline*, taken before the run starts, so a
#' delta is always measured against the state the user handed us. Their
#' own uncommitted and untracked work is in the baseline and therefore
#' never counted as something the run did.
#'
#' @param dir Directory to snapshot.
#' @return Named character vector: relative path -> content hash.
#' @noRd
worktree_digest <- function(dir = getwd()) {
    files <- auto_tracked_files(dir)
    if (length(files) == 0L) {
        return(structure(character(), names = character()))
    }
    full <- file.path(dir, files)
    hashes <- vapply(full, function(p) {
        if (!file.exists(p) || dir.exists(p)) {
            return(NA_character_)
        }
        tryCatch(digest::digest(file = p, algo = "xxhash64"),
                 error = function(e) NA_character_)
    }, character(1), USE.NAMES = FALSE)
    keep <- !is.na(hashes)
    structure(hashes[keep], names = files[keep])
}

#' Compare a later snapshot against the baseline.
#'
#' @param baseline Named character vector from `worktree_digest()`.
#' @param current Named character vector from `worktree_digest()`.
#' @return List with `added`, `removed`, `modified` (character vectors)
#'   and `changed` (logical: did anything at all move).
#' @noRd
worktree_delta <- function(baseline, current) {
    base_names <- names(baseline) %||% character()
    cur_names <- names(current) %||% character()
    added <- setdiff(cur_names, base_names)
    removed <- setdiff(base_names, cur_names)
    shared <- intersect(base_names, cur_names)
    modified <- shared[baseline[shared] != current[shared]]
    list(added = added, removed = removed, modified = modified,
         changed = length(added) > 0L || length(removed) > 0L ||
         length(modified) > 0L)
}

#' Render a delta for the monitor's progress query.
#'
#' Names what moved rather than shipping the whole diff: the monitor
#' also has `git_diff` and can read the parts it cares about.
#' @noRd
format_worktree_delta <- function(delta) {
    if (!isTRUE(delta$changed)) {
        return("(nothing changed on disk since the run started)")
    }
    line <- function(label, paths) {
        if (length(paths) == 0L) {
            return(character())
        }
        shown <- utils::head(paths, 40L)
        extra <- if (length(paths) > 40L) {
            sprintf(" ... and %d more", length(paths) - 40L)
        } else {
            ""
        }
        sprintf("%s (%d): %s%s", label, length(paths),
                paste(shown, collapse = ", "), extra)
    }
    paste(c(line("modified", delta$modified),
            line("added", delta$added),
            line("removed", delta$removed)), collapse = "\n")
}

# ---- Budget ----

#' Total spend for the run so far, main agent plus monitor.
#'
#' Both halves or the number is a fiction. `session_accumulate_spend()`
#' tallies main-agent turns on the session; `subagent_spend_total()`
#' tallies subagents process-wide, which is where the monitor's own
#' queries land. Summing only the first bills the supervisor to nobody
#' and lets a run drift past its cap by however much the monitor cost.
#'
#' Measured as a delta from the run's start so a long-lived `chat()`
#' session that already spent money before `/auto` doesn't start the
#' run already over budget.
#'
#' @param session Session environment.
#' @param baseline List from `auto_spend_baseline()`.
#' @return List with `cost`, `tokens`, and `cost_known` (FALSE when any
#'   contributing segment had no price).
#' @noRd
auto_spend_since <- function(session, baseline) {
    segs <- (session$spend %||% list())$segments %||% list()
    main_cost <- sum(vapply(segs, function(s) s$cost %||% 0, numeric(1)))
    main_tok <- sum(vapply(segs,
                           function(s) as.numeric(s$total_tokens %||% 0),
                           numeric(1)))
    main_missing <- any(vapply(segs, function(s) isTRUE(s$cost_missing),
                               logical(1)))
    sub <- tryCatch(subagent_spend_total(), error = function(e) list())

    list(
         cost = (main_cost - (baseline$main_cost %||% 0)) +
         ((sub$cost %||% 0) - (baseline$sub_cost %||% 0)),
         tokens = (main_tok - (baseline$main_tokens %||% 0)) +
         ((sub$total_tokens %||% 0) - (baseline$sub_tokens %||% 0)),
         # A missing price makes the total a floor, not a number. Treating
         # an unknown cost as zero is how a capped run quietly isn't.
         cost_known = !main_missing && !isTRUE(sub$cost_missing)
    )
}

#' Capture spend counters at the start of an auto run.
#' @noRd
auto_spend_baseline <- function(session) {
    segs <- (session$spend %||% list())$segments %||% list()
    sub <- tryCatch(subagent_spend_total(), error = function(e) list())
    list(
         main_cost = sum(vapply(segs, function(s) s$cost %||% 0, numeric(1))),
         main_tokens = sum(vapply(segs,
                                  function(s) as.numeric(s$total_tokens %||% 0),
                                  numeric(1))),
         sub_cost = sub$cost %||% 0,
         sub_tokens = sub$total_tokens %||% 0
    )
}

# ---- Stop conditions ----

#' Decide whether the loop should stop before spending another turn.
#'
#' Mechanical only: loop count, wall clock, spend, tokens, tool-call
#' volume, and consecutive no-change iterations. Every one of these is
#' checkable without asking a model anything, so they run first and the
#' monitor is only consulted when they all pass.
#'
#' `max_loops` alone is not a bound on work: one iteration can run
#' `session$max_turns` model turns, each with a batch of tool calls, so
#' ten loops can mean hundreds of agent turns. `max_tool_calls` is the
#' bound that actually holds.
#'
#' @param state Loop state list (see `auto_state()`).
#' @param auto Resolved auto config from `get_auto_config()`.
#' @return List with `stop` (logical) and `reason` (character).
#' @noRd
auto_check_limits <- function(state, auto) {
    halt <- function(why) list(stop = TRUE, reason = why)

    if (state$loop > auto$max_loops) {
        return(halt(sprintf("reached max_loops (%d)", auto$max_loops)))
    }
    elapsed <- as.numeric(difftime(Sys.time(), state$started, units = "mins"))
    if (is.finite(auto$max_minutes) && elapsed >= auto$max_minutes) {
        return(halt(sprintf("hit the time cap (%.1f of %g minutes)", elapsed,
                            auto$max_minutes)))
    }
    if (state$tool_calls >= auto$max_tool_calls) {
        return(halt(sprintf("hit the tool-call cap (%d)", auto$max_tool_calls)))
    }
    spend <- state$spend %||% list(cost = 0, tokens = 0, cost_known = TRUE)
    if (!isTRUE(spend$cost_known)) {
        return(halt(paste("stopping: a model in this run has no price in",
                          "llm.api's table, so the spend cap can't be",
                          "enforced")))
    }
    if (is.finite(auto$max_cost) && (spend$cost %||% 0) >= auto$max_cost) {
        return(halt(sprintf("hit the spend cap ($%.4f of $%g)",
                            spend$cost, auto$max_cost)))
    }
    if (is.finite(auto$max_tokens) &&
        (spend$tokens %||% 0) >= auto$max_tokens) {
        return(halt(sprintf("hit the token cap (%.0f of %g)",
                            spend$tokens, auto$max_tokens)))
    }
    if (state$stalled >= auto$stall_loops) {
        return(halt(sprintf(
                            "nothing changed on disk for %d consecutive iterations",
                            state$stalled)))
    }
    list(stop = FALSE, reason = "")
}

#' Fresh loop state.
#' @noRd
auto_state <- function(session, dir = getwd()) {
    list(loop = 1L, started = Sys.time(), baseline = worktree_digest(dir),
         spend_baseline = auto_spend_baseline(session),
         spend = list(cost = 0, tokens = 0, cost_known = TRUE),
         tool_calls = 0L, stalled = 0L, last_reply = "")
}

# ---- The worker's continuation prompt ----

#' Prompt handed to the worker on each iteration.
#'
#' Short on purpose. The worker's own session already holds its previous
#' reply and can reconstruct the diff with the tools it has, so repeating
#' either here buys nothing and costs context on every single iteration.
#'
#' The goal is restated every time regardless, because that is the one
#' thing the session might lose: compaction summarizes old turns, and the
#' original instruction is the oldest turn in the run.
#'
#' `AUTO_STATUS` is evidence, not authority. A worker saying "done" is an
#' input to the monitor's progress query, which checks it against what
#' actually changed on disk. The loop never stops just because the worker
#' says so.
#'
#' @param goal The run's goal.
#' @param loop Current iteration.
#' @param max_loops Iteration budget.
#' @return Character scalar.
#' @noRd
auto_continuation_prompt <- function(goal, loop, max_loops) {
    paste0(sprintf("Auto iteration %d of %d.\n\n", loop, max_loops),
           "Original goal: ", goal, "\n\n",
           "Continue from the current session and workspace state. Take the\n",
           "next concrete step toward the goal.\n\n",
           "If the acceptance criteria are fully satisfied, report\n",
           "AUTO_STATUS: done with concise evidence. Otherwise report\n",
           "AUTO_STATUS: continue and the next unresolved step.")
}

#' Parse `/auto [--loops N] [--exec|--no-exec] <goal>`.
#'
#' `--no-exec` is the tighter setting and is always honored. `--exec` is
#' a call-site grant that project config can still veto; see
#' `auto_envelope_config()`.
#'
#' @param text Argument tail after the `/auto` token.
#' @return List with `goal`, `loops` (integer or NULL), `allow_exec`
#'   (logical or NULL).
#' @noRd
parse_auto_flags <- function(text) {
    text <- as.character(text %||% "")[1]
    if (is.na(text)) {
        text <- ""
    }
    loops <- NULL
    m <- regexpr("--loops[[:space:]]+([0-9]+)", text)
    if (m != -1L) {
        loops <- as.integer(sub(".*--loops[[:space:]]+([0-9]+).*", "\\1",
                                regmatches(text, m)))
        text <- sub("--loops[[:space:]]+[0-9]+", "", text)
    }
    allow_exec <- NULL
    if (grepl("--no-exec", text, fixed = TRUE)) {
        allow_exec <- FALSE
        text <- sub("--no-exec", "", text, fixed = TRUE)
    } else if (grepl("--exec", text, fixed = TRUE)) {
        allow_exec <- TRUE
        text <- sub("--exec", "", text, fixed = TRUE)
    }
    list(goal = trimws(gsub("[[:space:]]+", " ", text)), loops = loops,
         allow_exec = allow_exec)
}

#' Drive an unattended run through the ordinary REPL loop.
#'
#' Auto mode does not get its own turn loop. `run_repl_loop()` already
#' takes `read_input` as an injected hook, so the whole of auto mode is
#' a `read_input` that answers with the next prompt instead of blocking
#' on the user, and with EOF (`character(0)`) when the run should end.
#'
#' That is deliberately not an extraction of the loop body. Everything
#' the loop does between turns -- transcript, task sync, archival, spend
#' accounting, the context meter, auto-compaction -- is behaviour an
#' unattended run needs at least as much as an attended one, and a
#' second copy of it would be a second thing to keep in step. Here there
#' is no second copy: auto runs are the same code path with a different
#' source of prompts.
#'
#' The checks run between turns, in cost order. Mechanical limits first
#' because they are free and cannot be argued with; the monitor only
#' when they all pass.
#'
#' @param ctx REPL context env, as built by `chat()`.
#' @param goal Character. What the run is for.
#' @param max_loops Integer or NULL. Overrides the configured cap.
#' @param allow_exec Logical or NULL. Call-site exec grant; project
#'   config can still veto it (see `auto_envelope_config()`).
#' @return Invisibly, the final loop state.
#' @noRd
run_auto_loop <- function(ctx, goal, max_loops = NULL, allow_exec = NULL) {
    cwd <- ctx$cwd %||% getwd()
    config <- auto_envelope_config(cwd, allow_exec = allow_exec)
    auto <- get_auto_config(config)
    if (!is.null(max_loops)) {
        auto$max_loops <- as.integer(max_loops)
    }
    palette <- ctx$palette %||% list()
    say <- function(fmt, ...) {
        cat(sprintf("%s[auto] %s%s\n", palette$dim %||% "",
                    sprintf(fmt, ...), palette$reset %||% ""))
    }

    say("goal: %s", goal)
    say("caps: %d loops, %g min, $%g, %s tool calls",
        auto$max_loops, auto$max_minutes, auto$max_cost,
        format(auto$max_tool_calls))

    monitor_id <- monitor_spawn(goal, ctx$session, config)
    say("monitor %s watching (read-only, no network)", substr(monitor_id, 1L, 8L))

    state <- auto_state(ctx$session, cwd)
    state$last_snapshot <- state$baseline
    state$tool_baseline <- ctx$session$turn_number %||% 0L
    stop_reason <- "ended"

    # Restore the session to its attended shape no matter how the run
    # ends -- interrupt, error, escalation. A session left carrying a
    # dead monitor's gate would refuse or escalate every subsequent tool
    # call, which looks like corteza being broken rather than a run
    # having finished.
    prev_gate <- ctx$session$auto_gate
    prev_input <- ctx$read_input
    prev_eof <- ctx$eof_message
    ctx$eof_message <- ""
    on.exit({
        ctx$session$auto_gate <- prev_gate
        ctx$read_input <- prev_input
        ctx$eof_message <- prev_eof
        ctx$auto_halt <- NULL
        tryCatch(subagent_kill(monitor_id), error = function(e) NULL)
    }, add = TRUE)

    ctx$session$auto_gate <- monitor_auto_gate(
        monitor_id, config, cwd,
        on_verdict = function(call, action, reason) {
        if (!identical(action, "proceed")) {
            say("monitor %s %s: %s", action, call$tool %||% "?", reason)
        }
    })

    ctx$read_input <- function(prompt_str) {
        # First call: hand over the goal and let the loop run turn 1.
        if (state$loop == 1L) {
            state$loop <<- 2L
            return(goal)
        }

        # An escalation during the turn we just ran ends things here.
        # The gate already refused the call and the loop already printed
        # why; this is the part that stops the run rather than feeding
        # another prompt into it.
        if (!is.null(ctx$auto_halt)) {
            stop_reason <<- paste("escalated:", ctx$auto_halt)
            return(character(0))
        }

        state$spend <<- auto_spend_since(ctx$session, state$spend_baseline)
        state$tool_calls <<- (ctx$session$turn_number %||% 0L) -
        state$tool_baseline

        current <- worktree_digest(cwd)
        # Stall is measured against the previous iteration, not the run
        # baseline: a run that made one edit on turn 1 and nothing since
        # is stalled, even though it still differs from where it started.
        step <- worktree_delta(state$last_snapshot, current)
        if (isTRUE(step$changed)) {
            state$stalled <<- 0L
        } else {
            state$stalled <<- state$stalled + 1L
        }
        state$last_snapshot <<- current

        limits <- auto_check_limits(state, auto)
        if (isTRUE(limits$stop)) {
            stop_reason <<- limits$reason
            return(character(0))
        }

        reply <- ctx$last_assistant_response %||% ""
        # The worker's own claim is evidence for the monitor, never the
        # stop authority: "done" gets checked against what moved on disk.
        claimed <- auto_parse_status(reply)
        run_delta <- worktree_delta(state$baseline, current)
        verdict <- monitor_ask_progress(
                                        monitor_id, goal, reply,
                                        paste0(format_worktree_delta(run_delta),
                "\n\nWorker self-report: AUTO_STATUS: ", claimed),
                                        loop = state$loop - 1L, max_loops = auto$max_loops,
                                        timeout = auto$monitor_timeout)

        if (!identical(verdict$verdict, "continue")) {
            stop_reason <<- sprintf("monitor said %s: %s", verdict$verdict,
                                    verdict$reason)
            return(character(0))
        }
        if (identical(claimed, "done")) {
            say("worker reported done; monitor says keep going: %s",
                verdict$reason)
        }

        loop_now <- state$loop
        state$loop <<- state$loop + 1L
        say("iteration %d/%d  $%.4f  %d tool calls",
            loop_now, auto$max_loops, state$spend$cost %||% 0,
            state$tool_calls)
        auto_continuation_prompt(goal, loop_now, auto$max_loops)
    }

    run_repl_loop(ctx)
    say("stopped: %s", stop_reason)
    say("spent $%.4f over %d tool calls in %.1f min",
        state$spend$cost %||% 0, state$tool_calls,
        as.numeric(difftime(Sys.time(), state$started, units = "mins")))
    state$stop_reason <- stop_reason
    invisible(state)
}

#' Read the worker's self-reported status out of its reply.
#'
#' Line-anchored for the same reason the monitor's verdict is: a
#' mid-sentence mention of the token is the model narrating, not
#' reporting. Absent or ambiguous reads as "continue", which is the safe
#' default here -- the cost of a spurious extra iteration is one turn,
#' and the monitor and the mechanical caps both still apply.
#'
#' @param reply Assistant text.
#' @return "done" or "continue".
#' @noRd
auto_parse_status <- function(reply) {
    if (is.null(reply) || !is.character(reply) || length(reply) != 1L ||
        is.na(reply)) {
        return("continue")
    }
    lines <- unlist(strsplit(reply, "\r?\n", perl = TRUE))
    hits <- grepl("^[[:space:]>*_`-]*AUTO_STATUS[[:space:]]*:", lines,
                  ignore.case = TRUE)
    if (!any(hits)) {
        return("continue")
    }
    tails <- tolower(sub("^[[:space:]>*_`-]*AUTO_STATUS[[:space:]]*:", "",
                         lines[hits], ignore.case = TRUE))
    found <- unique(c(
            if (any(grepl("\\bdone\\b", tails))) "done",
            if (any(grepl("\\bcontinue\\b", tails))) "continue"
        ))
    if (length(found) != 1L) {
        return("continue")
    }
    found
}
