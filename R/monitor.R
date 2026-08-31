# Hall monitor: a read-only supervisor subagent for unattended runs.
#
# turn() routes every "ask" verdict from policy() to session$approval_cb.
# With a human in the seat that is a readline() prompt. Unattended it has
# two bad answers: new_session()'s default denies everything (auto mode
# does nothing the moment it touches a write), or it auto-approves (an
# unsupervised agent with write_file and bash in your repo).
#
# The monitor is the third answer. It is a subagent with read-only tools
# and its own context, spawned once per auto run, asked two questions:
#
#   1. Should this specific tool call proceed?   monitor_ask_approval()
#   2. Is this loop still making progress?       monitor_ask_progress()
#
# It is not the worker grading itself: separate process, separate tool
# grants, and it sees the policy decision, which the worker does not get
# to author.
#
# WHAT THIS IS AND IS NOT. The monitor is a guardrail against drift and
# mistakes -- an agent that has lost the plot, is thrashing, or is about
# to clobber something it shouldn't. It is NOT a security boundary
# against an adversarial model. A read-only LLM reading shell strings
# cannot be one, and building it as though it were would just move the
# trust somewhere less visible. What actually bounds an auto run is the
# envelope below (hard, mechanical, no model involved), the policy
# engine's own safety rules, and running it in a repo you can git reset.

# ---- Turn-aborting escalation ----

#' Build a condition meaning "an unattended run needs a human".
#'
#' Same shape and same reasoning as [user_deny_condition()] in R/deny.R:
#' the class deliberately excludes `"error"` so the defensive
#' `tryCatch(error = function(e) FALSE)` wrappers inside
#' [.make_tool_handler()] cannot swallow it, and `"interrupt"` lets the
#' existing chat()/CLI interrupt-marker machinery unwind cleanly for a
#' surface that has not registered a handler of its own.
#'
#' Escalation aborts the whole turn rather than declining one call. A
#' supervisor that says "a human needs to look at this" and then watches
#' the model try the next thing has not stopped anything.
#'
#' @param reason Character. Why a human is needed.
#' @param tool Character. Tool that triggered it.
#' @return A condition of class
#'   `c("corteza_auto_escalate", "interrupt", "condition")`.
#' @noRd
auto_escalate_condition <- function(reason = "", tool = "?") {
    tool_str <- if (length(tool) && nzchar(tool)) {
        as.character(tool)[1]
    } else {
        "?"
    }
    reason_str <- if (length(reason) && nzchar(reason)) {
        as.character(reason)[1]
    } else {
        "unspecified"
    }
    structure(
              class = c("corteza_auto_escalate", "interrupt", "condition"),
              list(message = sprintf("Auto run escalated on %s: %s", tool_str,
                                     reason_str),
                   tool = tool_str, reason = reason_str, call = NULL)
    )
}

# ---- Verdict contract ----

# Two vocabularies, deliberately disjoint. The monitor is one long-lived
# subagent answering two different questions, so a reply meant for one
# must not be readable as an answer to the other: an approval reply of
# "continue" would otherwise be indistinguishable from a progress reply,
# and a stale or duplicated reply could authorize a call it never saw.
# Sharing "escalate" is safe because it means the same thing in both and
# is the fail-closed direction anyway.
.MONITOR_VERDICTS_PROGRESS <- c("continue", "stop", "escalate")
.MONITOR_VERDICTS_APPROVAL <- c("approve", "refuse", "escalate")
.MONITOR_VERDICTS <- unique(c(.MONITOR_VERDICTS_PROGRESS,
                              .MONITOR_VERDICTS_APPROVAL))

#' Parse a monitor reply into a verdict.
#'
#' The monitor is instructed to answer with exactly two lines:
#'
#' \preformatted{
#' VERDICT: continue | stop | escalate
#' REASON: <one line>
#' }
#'
#' Anything that does not parse cleanly comes back \code{"escalate"},
#' never \code{"continue"}. A supervisor whose garbled output reads as
#' approval is the failure mode this whole file exists to avoid, so the
#' unparseable case is a real branch with real tests rather than a
#' hoped-for one.
#'
#' Ambiguity is also escalation: if the reply carries more than one
#' distinct VERDICT value (the model argued with itself, or quoted the
#' contract and then contradicted it) there is no defensible way to pick
#' one, so we don't.
#'
#' @param text Character scalar, the monitor's reply.
#' @param allowed Character vector of verdict words valid for the question
#'   that was asked. Defaults to the progress vocabulary. A word outside
#'   this set escalates even when it is a valid verdict for the *other*
#'   question -- that is the point of keeping the vocabularies disjoint.
#' @param request_id Character or NULL. When supplied, the reply must
#'   echo it on a \code{REQUEST:} line. A reply that echoes nothing, or
#'   echoes a different id, is a stale or crossed answer and escalates
#'   rather than being applied to the wrong call.
#' @return List with \code{verdict} (one of \code{allowed}, or
#'   \code{"escalate"}) and \code{reason} (character, possibly "").
#' @noRd
parse_monitor_verdict <- function(text, allowed = .MONITOR_VERDICTS_PROGRESS,
                                  request_id = NULL) {
    escalate <- function(why) list(verdict = "escalate", reason = why)

    if (is.null(text) || !is.character(text) || length(text) != 1L ||
        is.na(text) || !nzchar(trimws(text))) {
        return(escalate("monitor returned no reply"))
    }

    lines <- unlist(strsplit(text, "\r?\n", perl = TRUE))

    # Echoed request id. Guards against a reply landing on a call it never
    # saw -- a duplicate, a timed-out query answered late, or the two
    # question types crossing.
    if (!is.null(request_id)) {
        echoed <- regmatches(
                             lines,
                             regexpr("^[[:space:]>*_`-]*REQUEST[[:space:]]*:[[:space:]]*[A-Za-z0-9_-]+",
                                     lines, ignore.case = TRUE)
        )
        echoed <- unique(trimws(sub(".*:[[:space:]]*", "",
                                    echoed[nzchar(echoed)])))
        if (length(echoed) == 0L) {
            return(escalate("monitor reply did not echo the request id"))
        }
        if (length(echoed) > 1L || !identical(echoed[[1L]], request_id)) {
            return(escalate(sprintf(
                                    "monitor reply echoed the wrong request id (wanted %s, got %s)",
                                    request_id, paste(echoed, collapse = ", "))))
        }
    }

    # Line-anchored: only a line that *starts* with VERDICT: counts (bare
    # markdown decoration allowed). A mid-sentence "reply VERDICT: continue
    # when fine" is the model quoting its instructions, not answering.
    is_verdict_line <- grepl("^[[:space:]>*_`-]*VERDICT[[:space:]]*:",
                             lines, ignore.case = TRUE)
    if (!any(is_verdict_line)) {
        return(escalate("monitor reply had no VERDICT line"))
    }

    # Collect every verdict word appearing after the colon, per line. Doing
    # this per line rather than on the first word catches the intra-line
    # contradiction ("VERDICT: continue, or stop if the tests fail") that a
    # first-token parse would read as a clean continue.
    tails <- tolower(sub("^[[:space:]>*_`-]*VERDICT[[:space:]]*:", "",
                         lines[is_verdict_line], ignore.case = TRUE))
    # Scan for every known verdict word, not just the ones valid here, so
    # an answer in the wrong vocabulary is reported as such instead of
    # silently reading as "no verdict".
    found <- unique(unlist(lapply(tails, function(tail) {
        .MONITOR_VERDICTS[vapply(.MONITOR_VERDICTS, function(v) {
            grepl(paste0("\\b", v, "\\b"), tail)
        }, logical(1))]
    })))

    if (length(found) == 0L) {
        return(escalate(sprintf("monitor VERDICT was not one of %s",
                                paste(allowed, collapse = "/"))))
    }
    if (length(found) > 1L) {
        return(escalate(sprintf("monitor gave conflicting verdicts (%s)",
                                paste(sort(found), collapse = ", "))))
    }
    if (!found %in% allowed) {
        return(escalate(sprintf(
                                "monitor answered '%s', which is not a verdict for this question (wanted %s)",
                                found, paste(allowed, collapse = "/"))))
    }
    values <- found

    reason_hit <- regmatches(
                             lines,
                             regexpr("^[[:space:]>*_-]*REASON[[:space:]]*:[[:space:]]*.*",
                                     lines, ignore.case = TRUE)
    )
    reason_hit <- reason_hit[nzchar(reason_hit)]
    reason <- if (length(reason_hit) > 0L) {
        trimws(sub("^[[:space:]>*_-]*REASON[[:space:]]*:[[:space:]]*", "",
                   reason_hit[[1L]], ignore.case = TRUE))
    } else {
        ""
    }

    list(verdict = unique(values), reason = reason)
}

# ---- Path helpers ----

#' TRUE when a path is absolute on this platform.
#' @noRd
.is_absolute_path <- function(p) {
    startsWith(p, "/") || startsWith(p, "\\") ||
    grepl("^[A-Za-z]:[/\\\\]", p)
}

#' Lexically normalize a path: resolve "." and ".." without touching disk.
#'
#' normalizePath() cannot do this for a path that does not exist yet, and
#' a write to a new file is exactly the case the envelope has to rule on.
#' So we resolve textually. A leading ".." that walks above the root is
#' kept as-is, which makes the containment check below fail closed.
#' @noRd
.normalize_lexical <- function(p) {
    p <- gsub("\\\\", "/", p)
    absolute <- .is_absolute_path(p)
    parts <- unlist(strsplit(p, "/", fixed = TRUE))
    out <- character()
    for (part in parts) {
        if (!nzchar(part) || identical(part, ".")) {
            next
        }
        if (identical(part, "..")) {
            if (length(out) > 0L && !identical(out[[length(out)]], "..")) {
                out <- out[-length(out)]
            } else if (!absolute) {
                out <- c(out, "..")
            }
            next
        }
        out <- c(out, part)
    }
    joined <- paste(out, collapse = "/")
    if (absolute) {
        paste0("/", joined)
    } else {
        joined
    }
}

#' Resolve a tool-supplied path to an absolute, lexically normal form.
#' @noRd
.resolve_against <- function(p, cwd) {
    p <- path.expand(p)
    if (!.is_absolute_path(p)) {
        p <- file.path(cwd, p)
    }
    .normalize_lexical(p)
}

#' TRUE when `path` sits inside `root` (or is `root` itself).
#' @noRd
.path_within <- function(path, root) {
    root <- .normalize_lexical(path.expand(root))
    identical(path, root) || startsWith(path, paste0(root, "/"))
}

#' Resolve a path through symlinks as far as the filesystem allows.
#'
#' Lexical normalization alone is not containment: a symlink inside the
#' project pointing at /etc reads as an in-project path, and a write
#' through it lands outside. normalizePath() resolves links but only for
#' paths that exist, and a write to a *new* file is the common case. So
#' walk up to the deepest existing ancestor, resolve that for real, and
#' re-attach the not-yet-existing tail.
#' @noRd
.resolve_real <- function(p, root) {
    lex <- .resolve_against(p, root)
    tail <- character()
    cur <- lex
    # Bounded: dirname() reaches "/" (or a drive root) and then fixpoints.
    while (nzchar(cur) && !file.exists(cur)) {
        tail <- c(basename(cur), tail)
        parent <- dirname(cur)
        if (identical(parent, cur)) {
            break
        }
        cur <- parent
    }
    if (!nzchar(cur) || !file.exists(cur)) {
        return(lex)
    }
    real <- normalizePath(cur, winslash = "/", mustWork = FALSE)
    if (length(tail) > 0L) {
        real <- paste(c(real, tail), collapse = "/")
    }
    .normalize_lexical(real)
}

# ---- Authority envelope ----

# Tools whose effect on the filesystem cannot be resolved from their
# arguments. resolve_paths() says so itself (R/policy.R): shell commands
# and run_r code bodies are not parsed. The monitor can still rule on
# these -- it reads the literal command -- but it is reading text, not a
# resolved path set, and the config key that enables it is named so that
# whoever turns it on knows which of those two they are getting.
.MONITOR_EXEC_TOOLS <- c("bash", "cmd", "run_r", "run_r_script")

#' Resolve auto settings so project config can tighten but never widen.
#'
#' \code{load_config()} merges project over global, which is right for
#' ordinary settings and wrong for the two keys that decide how much
#' authority a supervisor has. A project's \code{.corteza/config.json}
#' travels with the repo; a cloned repo must not be able to hand itself
#' exec brokering or clear the never-broker list.
#'
#' So the two envelope keys are resolved across layers by their safe
#' direction rather than by precedence:
#'
#' \itemize{
#'   \item \code{allow_exec}: on only if global (or the call site) says
#'     so AND the project does not say otherwise. Project can veto,
#'     never grant.
#'   \item \code{never_broker}: the union of both layers. Either can
#'     add; neither can remove.
#' }
#'
#' Every other auto key (budgets, timeouts) merges normally -- a project
#' lowering its own loop cap is fine.
#'
#' @param cwd Project directory.
#' @param allow_exec Logical or NULL. Call-site grant, treated as the
#'   global layer. NULL defers to config.
#' @return Config list with a reconciled \code{auto} block.
#' @noRd
auto_envelope_config <- function(cwd = getwd(), allow_exec = NULL) {
    config <- load_config(cwd)
    global <- load_config_file(corteza_config_path("config.json"))$auto %||%
    list()
    project <- load_config_file(
                                file.path(cwd, ".corteza", "config.json"))$auto %||% list()

    # Default off. Shell and run_r bodies cannot be path-resolved
    # (resolve_paths() says so itself in R/policy.R), so an auto run
    # cannot bound what they touch. Execution is therefore something the
    # operator turns on for a specific run (--exec) or in their own
    # global config -- never something a run assumes. A project config
    # can still veto, and still cannot grant.
    granted <- allow_exec %||% global$allow_exec %||% FALSE
    vetoed <- identical(project$allow_exec, FALSE)
    config$auto <- config$auto %||% list()
    config$auto$allow_exec <- isTRUE(granted) && !vetoed
    # Provenance for the run record: which layer produced the grant (or
    # the default), and whether a project veto then cut it off. Together
    # with the effective value these reconstruct the whole resolution.
    config$auto$allow_exec_source <- if (!is.null(allow_exec)) {
        "call_site"
    } else if (!is.null(global$allow_exec)) {
        "global_config"
    } else {
        "default"
    }
    config$auto$allow_exec_vetoed <- vetoed
    config$auto$never_broker <- unique(c(
            as.character(global$never_broker %||% character()),
            as.character(project$never_broker %||% character())
        ))
    config
}

#' Auto-run settings with defaults applied.
#'
#' Call \code{auto_envelope_config()} first when the config came off disk;
#' this function trusts the \code{auto} block it is handed.
#' @param config Config list from load_config().
#' @return List of auto-mode settings.
#' @noRd
get_auto_config <- function(config = list()) {
    cfg <- config$auto %||% list()
    list(
         max_loops = as.integer(cfg$max_loops %||% 10L),
         max_minutes = as.numeric(cfg$max_minutes %||% 30),
         max_cost = as.numeric(cfg$max_cost %||% 5),
         max_tokens = as.numeric(cfg$max_tokens %||% 2e6),
         # The bound that actually holds. max_loops caps iterations, but
         # one iteration runs up to session$max_turns model turns, each
         # with a batch of tool calls -- so ten loops can mean hundreds
         # of agent turns and thousands of calls.
         max_tool_calls = as.integer(cfg$max_tool_calls %||% 300L),
         stall_loops = as.integer(cfg$stall_loops %||% 2L),
         # Off unless explicitly granted; see auto_envelope_config().
         allow_exec = isTRUE(cfg$allow_exec %||% FALSE),
         # Provenance from auto_envelope_config(), carried through for
         # the run record. "unresolved" marks a config that never went
         # through the resolution (tests building auto blocks by hand).
         allow_exec_source = cfg$allow_exec_source %||% "unresolved",
         allow_exec_vetoed = isTRUE(cfg$allow_exec_vetoed),
         # Tools the monitor may never broker, whatever it thinks.
         # Empty by default: the envelope is about what a call touches,
         # not which tool it is. This is here for a user who wants a
         # specific tool to always stop for them.
         never_broker = as.character(cfg$never_broker %||% character()),
         monitor_model = cfg$monitor_model %||% NULL,
         monitor_timeout = as.integer(cfg$monitor_timeout %||% 120L)
    )
}

#' Decide whether a tool call is inside the monitor's authority.
#'
#' Runs BEFORE the monitor is consulted. Outside the envelope the monitor
#' is never asked at all -- the run stops for a human. The envelope is
#' mechanical on purpose: no model judgment is involved in deciding what a
#' model is allowed to judge.
#'
#' Note what is deliberately NOT here: \code{config$dangerous_tools}.
#' That list is \code{bash}, \code{run_r}, \code{write_file},
#' \code{replace_in_file} -- precisely the calls the monitor exists to
#' broker. Excluding them would make auto mode inert. The envelope is
#' about what a call *touches*, not which tool it is: \code{write_file}
#' inside the project is the monitor's job; \code{write_file} to
#' \code{~/.bashrc} is not.
#'
#' @param call The call list handed to \code{policy()}.
#' @param decision The decision \code{policy()} returned for it.
#' @param config Config list from \code{load_config()}.
#' @param cwd Session working directory; the containment root.
#' @return List with \code{ok} (logical) and \code{reason} (character).
#' @noRd
monitor_in_envelope <- function(call, decision, config = list(),
                                cwd = getwd()) {
    outside <- function(why) list(ok = FALSE, reason = why)
    auto <- get_auto_config(config)
    tool <- call$tool %||% ""

    # 1. Hard safety verdicts from check_safety() (credential paths).
    #    Un-overridable by user config today; stays un-overridable by a
    #    supervisor.
    reason <- decision$reason %||% ""
    if (startsWith(reason, "safety:")) {
        return(outside(sprintf("hard safety rule (%s)", reason)))
    }

    # 2. Tools the user pinned as never-broker.
    if (tool %in% auto$never_broker) {
        return(outside(sprintf("%s is in auto.never_broker", tool)))
    }

    # 3. Unresolvable effect: exec tools when exec brokering is off.
    op <- classify_op(tool)
    if (tool %in% .MONITOR_EXEC_TOOLS && !isTRUE(auto$allow_exec)) {
        return(outside(sprintf(
                               "%s executes code whose effect can't be resolved from its arguments, and auto.allow_exec is off",
                               tool)))
    }

    # 4. Unknown tools are unbrokerable. classify_op() knows the built-in
    #    read/write/exec sets; everything else -- every package tool
    #    registered through skill_packages, base::file.remove among them
    #    -- lands here. An unrecognized tool may well mutate, and we
    #    cannot tell what it touches, so a human decides.
    if (identical(op, "unknown")) {
        return(outside(sprintf(
                               "%s is not a recognized read/write/exec tool, so its effect can't be bounded",
                               tool)))
    }

    # 5. Containment: every resolved path must sit inside the project and
    #    must not be a configured denied path. Resolution goes through
    #    symlinks (.resolve_real), because a link inside the project
    #    pointing outside it is exactly how this check gets walked past.
    root_real <- .resolve_real(cwd, cwd)
    denied <- vapply(config$denied_paths %||% character(),
                     function(d) .resolve_real(d, root_real), character(1),
                     USE.NAMES = FALSE)
    paths <- call$paths %||% resolve_paths(call)

    # 6. A mutating call whose target we could not resolve is refused,
    #    not waved through. resolve_paths() is best-effort over known
    #    argument names; a write tool that names its target something
    #    else would otherwise pass this loop trivially by having nothing
    #    to check.
    if (identical(op, "write") && length(paths) == 0L) {
        return(outside(sprintf(
                               "%s would modify something, but no target path could be resolved from its arguments",
                               tool)))
    }

    for (raw in paths) {
        abs <- .resolve_real(raw, root_real)
        for (d in denied) {
            if (.path_within(abs, d)) {
                return(outside(sprintf("%s is under a denied path (%s)",
                                       raw, d)))
            }
        }
        if (!.path_within(abs, root_real)) {
            return(outside(sprintf("%s resolves outside the project (%s)",
                                   raw, abs)))
        }
    }

    list(ok = TRUE, reason = "")
}

# ---- Monitor lifecycle ----

#' System prompt for the hall monitor.
#'
#' Deliberately short. The monitor's job is narrow and the contract is
#' the only thing it must get right, so the prompt spends its words on
#' the output format and on the one bias we want (when unsure, escalate).
#' @param goal The auto run's goal statement.
#' @return Character scalar.
#' @noRd
monitor_system_prompt <- function(goal) {
    paste0(
           "You are the hall monitor for an unattended agent run. Another agent\n",
           "is working toward a goal with no human watching. You supervise it.\n",
           "You have read-only tools. You cannot write, execute, or fetch.\n\n",
           "## The goal\n\n", goal, "\n\n",
           "## How to answer\n\n",
           "Reply with exactly two lines and nothing else:\n\n",
           "VERDICT: continue\n",
           "REASON: one line, plain, concrete\n\n",
           "VERDICT must be one of:\n",
           "  continue  the work is on track; proceed\n",
           "  stop      the goal is met, or further work is pointless\n",
           "  escalate  a human needs to look at this\n\n",
           "## What to watch for\n\n",
           "- Work that has drifted off the goal.\n",
           "- Claims of completion that the diff does not support. Check the\n",
           "  diff before believing \"done\".\n",
           "- Thrashing: the same edit applied and reverted, or tests failing\n",
           "  the same way with nothing new tried.\n",
           "- Changes far outside what the goal implies.\n",
           "- Tests or checks deleted or weakened rather than fixed.\n\n",
           "When you are unsure, escalate. A run stopped early is cheap; a run\n",
           "that kept going when it should not have is not."
    )
}

#' Spawn the hall monitor for an auto run.
#'
#' Read-only tool grant, and web search explicitly off. The tool list
#' alone does not achieve the latter: subagent_turn_init() builds its
#' session without a web_search argument, so provider-native server-side
#' search is on by default for every subagent regardless of preset. The
#' monitor reads the worker's transcript, which is attacker-influenceable
#' text, so it gets no outbound channel at all.
#'
#' @param goal Character, the auto run's goal.
#' @param session Parent session environment.
#' @param config Config list.
#' @return Subagent id (character).
#' @noRd
monitor_spawn <- function(goal, session = NULL, config = list()) {
    auto <- get_auto_config(config)
    subagent_spawn(task = monitor_system_prompt(goal),
                   model = auto$monitor_model, preset = "monitor",
                   parent_session = session, config = config)
}

#' Ask the monitor to rule on a single pending tool call.
#'
#' @param id Monitor subagent id.
#' @param call The call list.
#' @param decision The policy decision for it.
#' @param request_id Token the reply must echo back.
#' @param timeout Seconds to wait.
#' @return Parsed verdict list, in the approval vocabulary.
#' @noRd
monitor_ask_approval <- function(id, call, decision, request_id = NULL,
                                 timeout = 120L) {
    prompt <- paste0(
                     "The worker wants to make this tool call. Rule on it.\n\n",
                     "REQUEST-ID: ", request_id %||% "(none)", "\n",
                     "TOOL: ", call$tool %||% "?", "\n",
                     "POLICY SAID: ", decision$approval %||% "?", " -- ",
                     decision$reason %||% "", "\n",
                     "ARGUMENTS:\n", .monitor_render_args(call$args), "\n\n",
                     "Answer with exactly these three lines:\n\n",
                     "REQUEST: ", request_id %||% "(none)", "\n",
                     "VERDICT: approve\n",
                     "REASON: one line\n\n",
                     "VERDICT must be one of:\n",
                     "  approve   the call runs\n",
                     "  refuse    the call is blocked; the worker is told and continues\n",
                     "  escalate  the whole run halts for a human\n\n",
                     "Do not answer with continue or stop; those words belong to the\n",
                     "other question you get asked."
    )
    .monitor_query(id, prompt, timeout,
                   allowed = .MONITOR_VERDICTS_APPROVAL,
                   request_id = request_id)
}

#' Ask the monitor whether the loop should keep going.
#'
#' The diff is passed in rather than left for the monitor to fetch, so
#' "I'm done" is checked against what actually changed on disk rather
#' than taken on the worker's word.
#'
#' @param id Monitor subagent id.
#' @param goal The goal statement.
#' @param reply The worker's last assistant reply.
#' @param diff Diff text since the run started.
#' @param loop Current loop number.
#' @param max_loops Loop budget.
#' @param timeout Seconds to wait.
#' @return Parsed verdict list.
#' @noRd
monitor_ask_progress <- function(id, goal, reply, diff, loop = 1L,
                                 max_loops = 10L, request_id = NULL,
                                 timeout = 120L) {
    req <- request_id %||% sprintf("p%d", loop)
    prompt <- paste0(
                     sprintf("Loop %d of %d just finished.\n\n", loop, max_loops),
                     "REQUEST-ID: ", req, "\n\n",
                     "WHAT THE WORKER SAID:\n",
                     .monitor_truncate(reply, 4000L), "\n\n",
                     "WHAT ACTUALLY CHANGED ON DISK SINCE THE RUN STARTED:\n",
                     .monitor_truncate(diff, 8000L), "\n\n",
                     "Should the run continue? Answer with exactly these three lines:\n\n",
                     "REQUEST: ", req, "\n",
                     "VERDICT: continue\n",
                     "REASON: one line\n\n",
                     "VERDICT must be one of continue, stop, or escalate. Do not answer\n",
                     "with approve or refuse; those words belong to the other question."
    )
    .monitor_query(id, prompt, timeout,
                   allowed = .MONITOR_VERDICTS_PROGRESS, request_id = req)
}

#' Query the monitor, converting any failure into an escalation.
#'
#' A monitor that timed out, died, or errored is a monitor that did not
#' approve anything. Same reasoning as the unparseable reply: the absence
#' of a verdict is never a yes.
#' @noRd
.monitor_query <- function(id, prompt, timeout = 120L,
                           allowed = .MONITOR_VERDICTS_PROGRESS,
                           request_id = NULL) {
    result <- tryCatch(
                       subagent_query(id, prompt, wait = TRUE, timeout = timeout),
                       error = function(e) e
    )
    if (inherits(result, "condition")) {
        return(list(verdict = "escalate",
                    reason = paste("monitor unreachable:",
                                   conditionMessage(result))))
    }
    # subagent_query() returns the reply as a character scalar -- see
    # .format_subagent_reply() -- not a list with a $reply field.
    # Reading $reply off it raised "$ operator is invalid for atomic
    # vectors" on every approval query, so the monitor could never
    # return a verdict and every auto run escalated on its first tool
    # call. Nothing caught it because the tests stubbed this function's
    # callers rather than exercising the contract underneath it.
    #
    # The list branch is kept because a `return_name` query does return
    # a richer object; taking whichever shape arrives costs nothing and
    # means a change on either side degrades to an escalation rather
    # than an error.
    text <- if (is.character(result)) {
        paste(result, collapse = "\n")
    } else if (is.list(result)) {
        paste(as.character(result$reply %||% ""), collapse = "\n")
    } else {
        ""
    }
    parse_monitor_verdict(text, allowed = allowed, request_id = request_id)
}

#' Render tool arguments for the monitor prompt.
#' @noRd
.monitor_render_args <- function(args) {
    args <- as.list(args %||% list())
    if (length(args) == 0L) {
        return("  (none)")
    }
    parts <- vapply(names(args), function(nm) {
        v <- args[[nm]]
        txt <- if (is.character(v) || is.numeric(v) || is.logical(v)) {
            paste(as.character(v), collapse = " ")
        } else {
            paste(utils::capture.output(utils::str(v)), collapse = " ")
        }
        sprintf("  %s: %s", nm, .monitor_truncate(txt, 2000L))
    }, character(1), USE.NAMES = FALSE)
    paste(parts, collapse = "\n")
}

#' Truncate with an explicit marker so the monitor knows it saw a slice.
#' @noRd
.monitor_truncate <- function(text, max_chars) {
    text <- paste(as.character(text %||% ""), collapse = "\n")
    if (!nzchar(text)) {
        return("(nothing)")
    }
    if (nchar(text) <= max_chars) {
        return(text)
    }
    paste0(substr(text, 1L, max_chars),
           sprintf("\n... [truncated, %d chars total]", nchar(text)))
}

# ---- Approval callback ----

#' Build the auto-authority gate backed by the hall monitor.
#'
#' Assigned to \code{session$auto_gate}, which \code{\link{turn}} consults
#' for every tool call that survived \code{policy()} -- whatever verdict
#' policy reached. It is deliberately not an \code{approval_cb}: that hook
#' only ever sees \code{"ask"}, and the default tensor resolves ordinary
#' project writes to \code{"allow"} outside \code{~/projects}, so a
#' supervisor wired there would never be consulted for the edits that
#' matter. See the comment at the gate call site in \code{R/turn.R}.
#'
#' Returns one of three actions:
#' \itemize{
#'   \item \code{"proceed"} — run the call; also stands in for approval,
#'     so the human prompt is skipped.
#'   \item \code{"refuse"} — the model is told no and the turn continues.
#'   \item \code{"escalate"} — the turn aborts for a human.
#' }
#'
#' @param monitor_id Subagent id from \code{monitor_spawn()}.
#' @param config Config list, already through
#'   \code{auto_envelope_config()}.
#' @param cwd Containment root.
#' @param budget_check Optional \code{function(event)} returning
#'   \code{list(stop, reason)}. Called with \code{"call"} before each
#'   brokered call and \code{"monitor"} after each approval query.
#'   Without it, a cap is only enforced between worker turns, and one
#'   turn can make hundreds of calls -- so the advertised bound would be
#'   exceeded by however much a single turn managed. Asking the monitor
#'   also costs money, so the budget has to be re-read after its reply
#'   and before its verdict is acted on.
#' @param on_approved Optional \code{function()} called only when a call
#'   is actually cleared to run, after both the envelope and the monitor
#'   have agreed and after the post-query budget recheck. This is where a
#'   caller counts executed calls: incrementing at check time instead
#'   would charge a refused call, or one halted by the recheck, against a
#'   budget it never spent.
#' @param on_verdict Optional function(call, action, reason) for display.
#' @param on_decision Optional function(record) receiving one structured
#'   record per gate consultation: which authority decided (budget,
#'   envelope, monitor, accounting), the action, the reason, and -- when
#'   the monitor was consulted -- the request id and raw verdict. This
#'   is observability, not authority: errors are swallowed like
#'   \code{on_verdict}'s, because a broken recorder must not block work.
#'   The caller is expected to notice its own write failures (see
#'   \code{auto_log_append()}).
#' @return A function(call, decision) -> list(action, reason).
#' @noRd
monitor_auto_gate <- function(monitor_id, config = list(), cwd = getwd(),
                              budget_check = NULL, on_approved = NULL,
                              on_verdict = NULL, on_decision = NULL) {
    auto <- get_auto_config(config)
    force(monitor_id)
    counter <- 0L
    decisions <- 0L

    over_budget <- function(event) {
        if (!is.function(budget_check)) {
            return(NULL)
        }
        res <- tryCatch(budget_check(event),
                        error = function(e) {
            list(stop = TRUE,
                 reason = paste("budget check failed:", conditionMessage(e)))
        })
        if (isTRUE(res$stop)) {
            list(action = "escalate", reason = res$reason %||% "over budget")
        } else {
            NULL
        }
    }

    function(call, decision) {
        # Attribution travels with the result: every branch that decides
        # also names itself, so the record cannot drift from the logic
        # the way a post-hoc classification of reason strings would.
        authority <- NULL
        request_id <- NULL
        raw_verdict <- NULL
        decisions <<- decisions + 1L
        envelope_ok <- NULL
        budget_event <- NULL

        # Before anything else, including the envelope: a run that has
        # spent its budget stops here rather than one turn later.
        result <- over_budget("call")
        if (!is.null(result)) {
            authority <- "budget"
            budget_event <- "call"
        }

        if (is.null(result)) {
            env <- monitor_in_envelope(call, decision, config, cwd)
            envelope_ok <- isTRUE(env$ok)
            if (!isTRUE(env$ok)) {
                # Outside the envelope the monitor is not consulted at
                # all. No model judgment decides what a model is allowed
                # to judge.
                result <- list(action = "escalate", reason = env$reason)
                authority <- "envelope"
            } else {
                counter <<- counter + 1L
                req <- sprintf("a%d", counter)
                v <- monitor_ask_approval(monitor_id, call, decision,
                    request_id = req,
                    timeout = auto$monitor_timeout)
                request_id <- req
                raw_verdict <- v$verdict
                # The query itself cost tokens. Re-read the budget before
                # acting on the answer, or an approval bought with the
                # last of the budget still authorizes the call.
                post <- over_budget("monitor")
                if (!is.null(post)) {
                    result <- post
                    authority <- "budget"
                    budget_event <- "monitor"
                } else {
                    result <- list(action = switch(v$verdict,
                            approve = "proceed",
                            refuse = "refuse",
                            "escalate"),
                                   reason = v$reason)
                    authority <- "monitor"
                }
            }
        }

        # Counted only once the call is genuinely cleared to run: past
        # the envelope, past the monitor, and past the post-query budget
        # recheck. A refusal did no work and must not consume the
        # executed-call budget.
        #
        # A failure to count is NOT swallowed. on_approved is the only
        # thing advancing the executed-call counter, so a throwing
        # callback would run the call uncounted and, repeated, disable
        # max_tool_calls entirely -- a cap that silently stops counting
        # is worse than no cap. If the call cannot be accounted for, it
        # does not run. (Contrast on_verdict below, which is display:
        # there, swallowing is right, because a broken progress line
        # must not block work.)
        if (identical(result$action, "proceed") && is.function(on_approved)) {
            counted <- tryCatch({
                on_approved()
                TRUE
            }, error = function(e) conditionMessage(e))
            if (!isTRUE(counted)) {
                result <- list(
                               action = "escalate",
                               reason = paste("executed-call accounting failed, refusing to",
                        "run uncounted:", counted))
                authority <- "accounting"
            }
        }
        if (is.function(on_decision)) {
            # The whole chain, not just the outcome: what policy said
            # coming in, whether the envelope was evaluated and how it
            # ruled, which budget phase fired (pre-envelope "call" or
            # post-query "monitor"), and the monitor's raw verdict --
            # even when a budget stop overrode it. Nothing here has to
            # be recovered from a reason string later.
            tryCatch(on_decision(list(
                                      seq = decisions,
                                      call_id = call$call_id,
                                      tool = call$tool %||% NA_character_,
                                      action = result$action,
                                      authority = authority,
                                      reason = result$reason,
                                      policy_approval = decision$approval,
                                      policy_reason = decision$reason,
                                      envelope_ok = envelope_ok,
                                      budget_event = budget_event,
                                      request_id = request_id,
                                      verdict = raw_verdict)),
                     error = function(e) NULL)
        }
        if (is.function(on_verdict)) {
            tryCatch(on_verdict(call, result$action, result$reason),
                     error = function(e) NULL)
        }
        result
    }
}
