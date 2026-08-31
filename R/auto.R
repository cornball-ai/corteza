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
    seg_tok <- vapply(segs, function(s) as.numeric(s$total_tokens %||% 0),
                      numeric(1))
    seg_missing <- vapply(segs, function(s) isTRUE(s$cost_missing), logical(1))
    main_tok <- sum(seg_tok)
    sub <- tryCatch(subagent_spend_total(), error = function(e) list())

    # Unpriced spend is measured by differencing a counter, not by
    # reading a flag.
    #
    # cost_missing is sticky per segment and, once aggregated across
    # subagents, process-wide and permanent. Reading it outright means
    # one unpriced model earlier in the session poisons every auto run
    # afterwards. An earlier attempt at this compared "did the segment
    # grow" against the flag, which is no better: a segment already
    # flagged from earlier unpriced usage reports unknown the moment it
    # takes any *priced* usage, and since a run's own monitor always adds
    # subagent tokens, one historical unpriced subagent made every future
    # run unknown forever.
    #
    # missing_tokens accumulates only the tokens that actually came back
    # without a price (R/spend.R, R/subagent.R), so the difference across
    # this run answers the question that was being asked all along: was
    # the spend *this run created* priced.
    missing_now <- sum(vapply(segs,
                              function(s) as.numeric(s$missing_tokens %||% 0),
                              numeric(1))) + (sub$missing_tokens %||% 0)
    missing_base <- (baseline$missing_tokens %||% 0)

    list(
         cost = (main_cost - (baseline$main_cost %||% 0)) +
         ((sub$cost %||% 0) - (baseline$sub_cost %||% 0)),
         tokens = (main_tok - (baseline$main_tokens %||% 0)) +
         ((sub$total_tokens %||% 0) - (baseline$sub_tokens %||% 0)),
         # A missing price makes the total a floor, not a number. Treating
         # an unknown cost as zero is how a capped run quietly isn't.
         cost_known = (missing_now - missing_base) <= 0
    )
}

#' Capture spend counters at the start of an auto run.
#' @noRd
auto_spend_baseline <- function(session) {
    segs <- (session$spend %||% list())$segments %||% list()
    sub <- tryCatch(subagent_spend_total(), error = function(e) list())
    seg_tok <- vapply(segs, function(s) as.numeric(s$total_tokens %||% 0),
                      numeric(1))
    list(
         main_cost = sum(vapply(segs, function(s) s$cost %||% 0, numeric(1))),
         main_tokens = sum(seg_tok),
         # Unpriced tokens across both halves, so auto_spend_since() can
         # difference them rather than reading a sticky flag.
         missing_tokens = sum(vapply(segs,
                                     function(s) as.numeric(s$missing_tokens %||%
                    0), numeric(1))) + (sub$missing_tokens %||% 0),
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
    # `kind` is the machine-readable half of the answer. The reason
    # string is for the operator; tooling reading the run log gets the
    # kind, so "which cap fired" never requires parsing prose.
    halt <- function(why, kind) list(stop = TRUE, reason = why, kind = kind)

    if (state$loop > auto$max_loops) {
        return(halt(sprintf("reached max_loops (%d)", auto$max_loops), "loops"))
    }
    elapsed <- as.numeric(difftime(Sys.time(), state$started, units = "mins"))
    if (is.finite(auto$max_minutes) && elapsed >= auto$max_minutes) {
        return(halt(sprintf("hit the time cap (%.1f of %g minutes)", elapsed,
                            auto$max_minutes), "minutes"))
    }
    if (state$tool_calls >= auto$max_tool_calls) {
        return(halt(sprintf("hit the tool-call cap (%d)", auto$max_tool_calls),
                    "tool_calls"))
    }
    spend <- state$spend %||% list(cost = 0, tokens = 0, cost_known = TRUE)
    note <- ""
    if (isTRUE(spend$cost_known)) {
        if (is.finite(auto$max_cost) && (spend$cost %||% 0) >= auto$max_cost) {
            return(halt(sprintf("hit the spend cap ($%.4f of $%g)",
                                spend$cost, auto$max_cost), "cost"))
        }
    } else {
        # No price available. This is NOT automatically a failure, and
        # treating it as one made auto mode unusable on the provider it
        # was most likely to run under: an OAuth / subscription plan has
        # no per-token price to report, so `cost_missing` is permanently
        # true and every run refused to start.
        #
        # The point of the spend cap is to bound the run, and tokens
        # bound it just as well when dollars are unavailable -- they are
        # reported either way. So the dollar check is skipped, the token
        # check below still applies, and the caller is told once that
        # the money cap is not the thing holding the line. Only if there
        # is no token cap to fall back on is there genuinely nothing
        # enforceable, and then it stops.
        if (!is.finite(auto$max_tokens)) {
            return(halt(paste("no model price available and no token cap to",
                              "fall back on, so nothing bounds this run"),
                        "unbounded"))
        }
        note <- sprintf(paste("no model price available (subscription or",
                              "cost-blind provider); bounding on the token cap",
                              "instead (%g)"), auto$max_tokens)
    }
    if (is.finite(auto$max_tokens) &&
        (spend$tokens %||% 0) >= auto$max_tokens) {
        return(halt(sprintf("hit the token cap (%.0f of %g)",
                            spend$tokens, auto$max_tokens), "tokens"))
    }
    if (state$stalled >= auto$stall_loops) {
        return(halt(sprintf(
                            "nothing changed on disk for %d consecutive iterations",
                            state$stalled), "stall"))
    }
    list(stop = FALSE, reason = "", note = note)
}

#' Fresh loop state.
#' @noRd
auto_state <- function(session, dir = getwd()) {
    list(loop = 1L, started = Sys.time(), baseline = worktree_digest(dir),
         spend_baseline = auto_spend_baseline(session),
         spend = list(cost = 0, tokens = 0, cost_known = TRUE),
         # gate_calls is counted by the gate itself, so the cap is
         # enforced against calls as they are brokered rather than
         # against the session counter's view between turns.
         tool_calls = 0L, gate_calls = 0L, stalled = 0L, last_reply = "",
         noted_budget = FALSE)
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

#' Check that every configured bound is a usable positive number.
#'
#' A cap of 0, NA, or a negative is not a tighter setting, it is a
#' broken one -- and the failure is quiet, because the first iteration
#' has nothing to compare against and runs before any limit check can
#' bite. So the run refuses to start instead.
#'
#' @param auto Resolved auto config.
#' @return Character vector of problems; empty when all bounds are sane.
#' @noRd
auto_validate_bounds <- function(auto) {
    checks <- list(max_loops = auto$max_loops, max_minutes = auto$max_minutes,
                   max_cost = auto$max_cost, max_tokens = auto$max_tokens,
                   max_tool_calls = auto$max_tool_calls,
                   stall_loops = auto$stall_loops)
    bad <- character()
    for (nm in names(checks)) {
        v <- suppressWarnings(as.numeric(checks[[nm]]))
        # Finite as well as positive. Inf passes a `<= 0` test and then
        # disables the bound entirely in auto_check_limits() -- an
        # infinite cap on a mode whose entire premise is being bounded.
        # Rejecting is the consistent choice: someone who wants no time
        # limit should not get it by writing one that silently isn't.
        if (length(v) != 1L || is.na(v) || !is.finite(v) || v <= 0) {
            bad <- c(bad, sprintf(
                                  "%s must be a positive finite number (got %s)", nm,
                                  paste(format(checks[[nm]]), collapse = ",")))
        }
    }
    bad
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
    palette <- ctx$palette %||% list()
    say <- function(fmt, ...) {
        cat(sprintf("%s[auto] %s%s\n", palette$dim %||% "",
                    sprintf(fmt, ...), palette$reset %||% ""))
    }

    # The run gets its identity before anything else -- before config
    # resolution, which reads files and can itself fail. A start the
    # bounds (or the config) refuse still appears on the record, as
    # run_start + run_end in the same file. record() never throws
    # (auto_log_append), but a failing log is said once -- silently
    # unrecorded is the one state this layer exists to rule out.
    run_id <- auto_new_run_id()
    log_path <- auto_log_path(run_id)
    log_broken <- FALSE
    record <- function(type, ...) {
        ok <- auto_log_append(log_path, type, run_id = run_id, ...)
        if (!ok && !log_broken) {
            log_broken <<- TRUE
            say("run log writes failing (%s); run continues unrecorded",
                log_path)
        }
        invisible(ok)
    }
    record("run_start", version = AUTO_LOG_VERSION, goal = goal,
           cwd = cwd,
           session_id = ctx$disk_session$sessionId %||% NA_character_,
           provider = ctx$provider %||% NA_character_,
           model = ctx$model %||% NA_character_)

    # Every unwind that skips the normal ending still closes the file:
    # an error escaping the loop, an interrupt outside a turn, an
    # escalation nothing caught. Only hard termination -- the process
    # dying -- leaves a file with no run_end, which is exactly what a
    # missing run_end is documented to mean. Ordinary errors are
    # captured with their message by the tryCatch sites below; anything
    # else ends as unexpected_exit.
    ended <- FALSE
    unexpected <- NULL
    on.exit({
        if (!ended) {
            record("run_end",
                   stop_category = if (is.null(unexpected)) {
                    "unexpected_exit"
                } else {
                    "unexpected_error"
                },
                   stop_reason = unexpected %||%
                   paste("non-local exit before run_end (interrupt, or a",
                         "condition outside the loop)"))
        }
    }, add = TRUE)

    # Config resolution can throw (unreadable config files); a run that
    # dies here must still close its record.
    config <- tryCatch(auto_envelope_config(cwd, allow_exec = allow_exec),
                       error = function(e) {
        unexpected <<- paste("config resolution failed:", conditionMessage(e))
        stop(e)
    })
    auto <- get_auto_config(config)
    if (!is.null(max_loops)) {
        auto$max_loops <- as.integer(max_loops)
    }
    record("config",
           caps = list(max_loops = auto$max_loops,
                       max_minutes = auto$max_minutes,
                       max_cost = auto$max_cost,
                       max_tokens = auto$max_tokens,
                       max_tool_calls = auto$max_tool_calls,
                       stall_loops = auto$stall_loops),
           allow_exec = isTRUE(auto$allow_exec),
           allow_exec_source = if (!is.null(allow_exec)) {
            "call_site"
        } else {
            auto$allow_exec_source
        },
           allow_exec_vetoed = isTRUE(auto$allow_exec_vetoed),
           never_broker = auto$never_broker)

    # Refuse a nonsense budget rather than interpreting it. A bound of 0
    # or NA would otherwise still permit the first worker turn, since the
    # first iteration is the one with nothing yet to compare against.
    bad <- auto_validate_bounds(auto)
    if (length(bad) > 0L) {
        say("refusing to start: %s", paste(bad, collapse = "; "))
        record("run_end", stop_category = "refused_start",
               stop_reason = paste(bad, collapse = "; "),
               loops = 0L, tool_calls = 0L)
        ended <- TRUE
        return(invisible(NULL))
    }

    say("goal: %s", goal)
    say("run %s", run_id)
    say("caps: %d loops, %g min, $%g, %s tool calls",
        auto$max_loops, auto$max_minutes, auto$max_cost,
        format(auto$max_tool_calls))

    # Stamped before the monitor spawns, not after: the spawn writes the
    # child's transcript header, and the header picks the run id up from
    # the parent session at that moment. Restored on exit no matter how
    # the run ends, so attended trace entries never carry a stale run.
    prev_run_id <- ctx$session$auto_run_id
    ctx$session$auto_run_id <- run_id
    # Per-run call ids: c1 is this run's first gated call. The run_id on
    # every record keeps ids from colliding across runs.
    ctx$session$auto_call_seq <- 0L
    on.exit(ctx$session$auto_run_id <- prev_run_id, add = TRUE)

    # A spawn failure is an ordinary error the operator will see, but
    # the run record must say it too -- this is the named case where "no
    # run_end means the process died" used to be false.
    monitor_id <- tryCatch(monitor_spawn(goal, ctx$session, config),
                           error = function(e) {
        unexpected <<- paste("monitor spawn failed:", conditionMessage(e))
        stop(e)
    })
    # Full id: session_id() returns a readable slug ("bold_lagrange"),
    # not a UUID, so truncating to 8 characters just mangles it.
    say("monitor %s watching (read-only, no network)", monitor_id)
    record("monitor", monitor_id = monitor_id)

    state <- auto_state(ctx$session, cwd)
    state$last_snapshot <- state$baseline
    state$tool_baseline <- ctx$session$turn_number %||% 0L
    stop_reason <- "ended"
    # Category set at the same site as the reason, never parsed back out
    # of the reason string: free-form text is for the operator, the
    # category is for tooling, and deriving one from the other is how
    # they drift.
    stop_cat <- "ended"
    stop_kind <- NULL
    stop_it <- function(category, reason, kind = NULL) {
        stop_cat <<- category
        stop_reason <<- reason
        stop_kind <<- kind
    }

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

    # Enforced at the gate, immediately before each brokered call and
    # again after each approval query. Between-turn checks are not
    # enough: one worker turn makes many tool calls, so a cap checked
    # only between turns can be overshot by a whole turn's worth of
    # work, and the monitor query that approves a call costs tokens of
    # its own.
    refresh_spend <- function() {
        state$spend <<- auto_spend_since(ctx$session, state$spend_baseline)
        # gate_calls alone, deliberately not session$turn_number.
        #
        # turn_number is incremented in .make_tool_handler() *before* the
        # gate is consulted, so it counts the call being decided as
        # already made. Taking the max of the two put the cap one call
        # early in the real handler -- a cap of 5 permitted 4 -- while a
        # test calling auto_gate() directly saw only gate_calls and
        # looked correct. The two also measure different things:
        # turn_number counts every dispatch, including task intercepts,
        # dry runs, and policy denials, none of which execute work.
        # gate_calls, read before its increment, is exactly the number of
        # calls this run has approved and executed.
        state$tool_calls <<- state$gate_calls
    }
    budget_check <- function(event) {
        refresh_spend()
        # The loop counter is a between-turn concern and is already
        # incremented for the turn now running, so checking it here would
        # abort the final permitted iteration partway through. Every
        # other bound is a real-time quantity and applies mid-turn.
        probe <- state
        probe$loop <- 1L
        # The tool-call cap counts calls *executed*. Checked before the
        # counter moves, so a cap of N permits exactly N calls and
        # refuses the N+1th. The post-approval recheck skips this bound
        # entirely: whether this call fits was settled a moment ago, and
        # re-applying it against the now-incremented counter would refuse
        # the very call just approved. That recheck exists because the
        # monitor query cost tokens, not to re-litigate the count.
        if (identical(event, "monitor")) {
            probe$tool_calls <- 0L
        }
        lim <- auto_check_limits(probe, auto)
        # Said once per run, not once per brokered call: the operator
        # needs to know the dollar cap is not the thing holding the line,
        # and needs to know it exactly one time.
        if (nzchar(lim$note %||% "") && !isTRUE(state$noted_budget)) {
            state$noted_budget <<- TRUE
            say("%s", lim$note)
        }
        if (isTRUE(lim$stop)) {
            stop_it("limit", lim$reason, lim$kind)
        }
        lim
    }
    # The counter moves here and nowhere else: the gate calls this only
    # after a call has cleared the envelope, the monitor, and the
    # post-query budget recheck. Counting at check time instead would
    # charge a refused call against a cap documented as counting calls
    # executed.
    on_approved <- function() {
        state$gate_calls <<- state$gate_calls + 1L
    }

    ctx$session$auto_gate <- monitor_auto_gate(
        monitor_id, config, cwd,
        budget_check = budget_check,
        on_approved = on_approved,
        on_verdict = function(call, action, reason) {
        if (!identical(action, "proceed")) {
            say("monitor %s %s: %s", action, call$tool %||% "?", reason)
        }
    },
        on_decision = function(d) {
        # One record per gate consultation, envelope refusals included
        # -- the calls that previously left no trace anywhere. The loop
        # counter is pre-incremented when the prompt is handed out, so
        # the turn now executing is state$loop - 1. The spend fields are
        # the budget snapshot the decision was made against (state was
        # refreshed by budget_check just before the gate decided).
        record("gate", seq = d$seq, call_id = d$call_id, tool = d$tool,
               action = d$action, authority = d$authority,
               reason = d$reason,
               policy_approval = d$policy_approval,
               policy_reason = d$policy_reason,
               envelope_ok = d$envelope_ok,
               budget_event = d$budget_event,
               request_id = d$request_id, verdict = d$verdict,
               loop = max(state$loop - 1L, 1L),
               spend_cost = state$spend$cost %||% 0,
               spend_tokens = state$spend$tokens %||% 0,
               tool_calls = state$tool_calls)
    })

    ctx$read_input <- function(prompt_str) {
        # First call: hand over the goal and let the loop run turn 1.
        if (state$loop == 1L) {
            state$loop <<- 2L
            return(goal)
        }

        # Ahead of the terminal-status check below, not after it: a run
        # ending on an error or an interrupt still spent whatever the
        # turn spent, and the closing report should say so rather than
        # quoting figures from before the last approved call.
        refresh_spend()

        # Anything other than a clean turn ends the run.
        #
        # An attended session can print an error and let the user decide
        # what to do next; an unattended one has nobody to decide, and
        # carrying on is actively wrong here. ctx$last_assistant_response
        # still holds the *previous* successful turn's reply after a
        # failure, so without this the monitor would be shown a stale
        # reply, plausibly say continue, and the run would spin on a
        # turn that keeps failing the same way. An interrupt matters
        # doubly: Ctrl+C is the operator stopping the run, and it must
        # stop the run rather than just the turn inside it.
        status <- ctx$last_turn_status %||% "ok"
        if (!identical(status, "ok")) {
            stop_it(switch(status,
                           escalate = "escalate",
                           interrupt = "interrupt",
                           denied = "denied",
                           error = "error",
                           "turn_ended"),
                    switch(status,
                           escalate = paste("escalated:", ctx$auto_halt %||% "?"),
                           interrupt = "interrupted",
                           denied = "a tool call was denied",
                           error = paste("turn errored:", ctx$last_turn_error %||% "?"),
                           paste("turn ended:", status)))
            return(character(0))
        }

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
            stop_it("limit", limits$reason, limits$kind)
            return(character(0))
        }

        reply <- ctx$last_assistant_response %||% ""
        # The worker's own claim is evidence for the monitor, never the
        # stop authority: "done" gets checked against what moved on disk.
        claimed <- auto_parse_status(reply)
        run_delta <- worktree_delta(state$baseline, current)
        # Explicit request id, so the progress record and the monitor's
        # transcript name the same exchange.
        progress_req <- sprintf("p%d", state$loop - 1L)
        verdict <- monitor_ask_progress(
                                        monitor_id, goal, reply,
                                        paste0(format_worktree_delta(run_delta),
                "\n\nWorker self-report: AUTO_STATUS: ", claimed),
                                        loop = state$loop - 1L, max_loops = auto$max_loops,
                                        request_id = progress_req,
                                        timeout = auto$monitor_timeout)

        # The progress query itself cost tokens, so re-read the budget
        # before its answer is acted on. Refreshed ahead of both exits
        # below so the closing spend report includes the cost of the
        # query, even when that query is what ended the run.
        refresh_spend()

        record("progress", loop = state$loop - 1L,
               request_id = progress_req,
               verdict = verdict$verdict, reason = verdict$reason,
               claimed = claimed,
               changed = isTRUE(run_delta$changed),
               files_changed = length(run_delta$added) +
               length(run_delta$removed) + length(run_delta$modified))

        if (!identical(verdict$verdict, "continue")) {
            stop_it("monitor", sprintf("monitor said %s: %s",
                                       verdict$verdict, verdict$reason))
            return(character(0))
        }

        # Only "continue" needs authorizing. Stopping needs no budget,
        # and checking the cap first would relabel a monitor's "goal met"
        # as "hit the spend cap" whenever both happened to be true.
        # Without this, the query could cross a cost, token, or time cap
        # and still buy a whole further worker turn.
        post <- auto_check_limits(state, auto)
        if (isTRUE(post$stop)) {
            stop_it("limit", post$reason, post$kind)
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
    state$stop_category <- stop_cat
    state$run_id <- run_id

    final <- worktree_digest(cwd)
    final_delta <- worktree_delta(state$baseline, final)
    record("run_end", stop_category = stop_cat, stop_reason = stop_reason,
           stop_kind = stop_kind,
           loops = max(state$loop - 1L, 0L),
           tool_calls = state$tool_calls,
           spend = list(cost = state$spend$cost %||% 0,
                        tokens = state$spend$tokens %||% 0,
                        cost_known = isTRUE(state$spend$cost_known %||% TRUE)),
           changed = isTRUE(final_delta$changed),
           files_changed = length(final_delta$added) +
           length(final_delta$removed) + length(final_delta$modified),
           delta = auto_delta_evidence(state$baseline, final, final_delta),
           duration_min = round(as.numeric(difftime(Sys.time(),
                    state$started,
                    units = "mins")), 2))
    ended <- TRUE
    invisible(state)
}

#' Durable evidence of what a run changed.
#'
#' Paths plus content hashes, per changed file, so the run's actual
#' result stays reconstructable after the checkout moves on -- a count
#' and a boolean stop meaning anything the moment anyone touches the
#' tree again. Capped per category: a run that somehow rewrites
#' thousands of files still gets a bounded record, with the truncation
#' stated rather than silent.
#'
#' @param baseline,final Named hash vectors from `worktree_digest()`.
#' @param delta List from `worktree_delta(baseline, final)`.
#' @param cap Max entries per category.
#' @return List with `added`, `removed`, `modified` (each a list of
#'   path+hash records) and `truncated`.
#' @noRd
auto_delta_evidence <- function(baseline, final, delta, cap = 1000L) {
    take <- function(paths) utils::head(paths, cap)
    truncated <- length(delta$added) > cap || length(delta$removed) > cap ||
    length(delta$modified) > cap
    list(
         added = lapply(take(delta$added), function(p) {
        list(path = p, hash = unname(final[[p]]))
    }),
         removed = lapply(take(delta$removed), function(p) {
        list(path = p, hash = unname(baseline[[p]]))
    }),
         modified = lapply(take(delta$modified), function(p) {
        list(path = p, base = unname(baseline[[p]]), final = unname(final[[p]]))
    }),
         truncated = truncated)
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
