# Policy engine: one decision per tool call.
#
# policy(call) -> list(model, approval, reason)
#
# Every tool has a class, declared where it is registered (skill_spec(),
# register_skill_from_fn(), a skill_packages entry): read, write, exec,
# control or continue. A tool that declares none is "unknown".
#
# Order (approval-engine note, WP18):
#   1. The session's mode. A room with no directory (WP10) refuses all
#      but reads and the control tools it names; plan mode refuses
#      write, exec, continue and unknown. Nothing after this lifts a
#      mode's deny.
#   2. The rules a person answers. A path under a credential store asks
#      (rule = "credential"), and so does leaving plan mode, the call to
#      exit_plan_mode (rule = "plan_exit"): the person set the mode, and
#      the plan is theirs to approve. No saved "always" answers either,
#      and neither the user's policy function nor the config lifts them.
#   3. The user's policy function, if options(corteza.policy = ...) is
#      set. Its verdict is final.
#   4. The table: the tool's class, and personal data refused for a write
#      or a command in a room.
#   5. The config: a permissions entry replaces the verdict for its tool;
#      for the rest, dangerous_tools asks and approval_mode says what
#      "ask" means. Neither lifts a deny.
#
# A "call" is a list with fields:
#   tool      - character, the tool name (e.g. "read_file")
#   args      - list of arguments the LLM supplied
#   paths     - character, filesystem paths touched (resolved from args if NULL)
#   urls      - character, URLs touched (resolved from args if NULL)
#   channel   - character, one of "cli", "console", "matrix"
#   context   - list, optional; context$recent_classes carries sticky
#               classifications from earlier calls in the same turn,
#               context$plan_mode the session's mode,
#               context$plan_mode_locked whether the session may end it
#               (not a job's worker, R/job-authority.R), and
#               context$unbound whether it is a room with no directory
#   class     - character, optional: the tool's class, as the session's
#               table holds it (turn() sets it, R/tool-table.R). Without
#               it the class is read from the registry.
#   origin    - character, optional: where the tool came from ("builtin",
#               "host", "user", "package", "project"), set with class

# ---- Hard safety ----

# Paths where any operation requires an ask. These cannot be overridden
# by user config, and no saved answer counts for them.
.hard_secret_paths <- function() {
    c("~/.ssh", "~/.gnupg", "~/.aws/credentials", "~/.config/gcloud",
        "~/.kube/config", "~/.docker/config.json")
}

# ---- Default path categories ----

.default_personal_paths <- function() {
    c("~/Documents", "~/Downloads")
}

.default_code_paths <- function() {
    c("~/projects", "~/src")
}

get_personal_paths <- function() {
    getOption("corteza.personal_paths", .default_personal_paths())
}

get_code_paths <- function() {
    getOption("corteza.code_paths", .default_code_paths())
}

# ---- Tool classes ----

# What a tool does. read reports and changes nothing; write changes
# files or state a later session reads; exec runs code; control acts on
# this session's own work; continue runs work in a child, a worker or
# another room, whose mode was set elsewhere. "unknown" is the class of a
# tool that names none.
.tool_classes <- c("read", "write", "exec", "control", "continue")

# A declared class, checked. NULL and "unknown" mean none.
tool_class_check <- function(class, name = "") {
    if (is.null(class) || identical(class, "unknown")) {
        return("unknown")
    }
    if (!is.character(class) || length(class) != 1L || is.na(class) ||
        !class %in% .tool_classes) {
        stop(sprintf("%s: class must be one of %s", name,
                     paste(.tool_classes, collapse = ", ")), call. = FALSE)
    }
    class
}

# Tools a host adds to a room session's list, outside the registry:
# R/room-media.R, R/room-consult.R and R/delegation-bot.R. turn() queues
# edit_context (R/context-edit.R) before the gate, so its class is never
# read there.
.session_tool_classes <- c(send_room_file = "write", ask_room = "continue",
                           delegate_to = "continue", edit_context = "control")

# A tool's entry: the session's table's (R/tool-table.R) where it has
# one, else the registry's, registering the built-in tools first when
# nothing is registered yet.
.registered_tool <- function(tool_name, table = NULL) {
    if (!is.null(table)) {
        return(tool_entry(tool_name, table))
    }
    ensure_skills()
    get_skill(tool_name)
}

# Whether corteza wrote the tool's arguments, so that resolve_paths()
# can read its paths by argument name.
.args_named_by_corteza <- function(tool_name, table = NULL) {
    if (!is.character(tool_name) || length(tool_name) != 1L ||
        is.na(tool_name) || !nzchar(tool_name)) {
        return(FALSE)
    }
    if (tool_name %in% names(.session_tool_classes)) {
        return(TRUE)
    }
    isTRUE(.registered_tool(tool_name, table)$builtin)
}

# ---- Path / URL resolution ----

# Argument names that hold a path, in the tools corteza wrote.
.path_arg_keys <- c("path", "file", "filename", "dir", "directory",
                    "dest", "destination", "from", "to", "src", "target")

# Every string in a tool's arguments, at any depth.
.string_args <- function(args) {
    if (!length(args)) {
        return(character())
    }
    out <- as.character(unname(rapply(as.list(args), function(x) x,
                                      classes = "character", how = "unlist")))
    out[!is.na(out) & nzchar(out)]
}

# Pull filesystem paths out of tool arguments. For a tool corteza wrote,
# the arguments named in .path_arg_keys; shell commands and run_r code
# bodies are read by exec_scan() instead. For any other tool (a package
# function, a skill), every string argument counts as a path: a string
# that only looks like one may ask needlessly, and a path under another
# argument name is not missed.
resolve_paths <- function(call) {
    args <- call$args %||% list()
    if (!call_named_by_corteza(call)) {
        return(.string_args(args))
    }
    out <- character()
    for (key in .path_arg_keys) {
        v <- args[[key]]
        if (is.character(v)) {
            out <- c(out, v)
        }
    }
    out
}

# Pull URLs out of tool arguments.
resolve_urls <- function(call) {
    args <- call$args %||% list()
    out <- character()
    for (key in c("url", "uri", "link")) {
        v <- args[[key]]
        if (is.character(v)) {
            out <- c(out, v)
        }
    }
    out
}

# ---- Classifiers ----

# The class of a tool: the one it declared where it was registered, or
# "unknown". A session tool a host adds is looked up first. With a
# session's table, the table's entry decides, whatever the registry
# holds.
classify_op <- function(tool_name, table = NULL) {
    if (!is.character(tool_name) || length(tool_name) != 1L ||
        is.na(tool_name) || !nzchar(tool_name)) {
        return("unknown")
    }
    if (tool_name %in% names(.session_tool_classes)) {
        return(.session_tool_classes[[tool_name]])
    }
    .registered_tool(tool_name, table)$class %||% "unknown"
}

# turn() resolves a call's tool in the session's table once, when it
# dispatches the call, and records on the call the class and the origin
# of the entry it found: `call$class` and `call$origin`. Every reader of
# a call (the gate, the supervisor, the monitors, the approval prompt)
# asks these two, so all of them see the entry that runs. A call built
# elsewhere, with neither, is read from the registry.
call_class <- function(call) {
    class <- call$class
    if (is.character(class) && length(class) == 1L &&
        class %in% c(.tool_classes, "unknown")) {
        return(class)
    }
    classify_op(call$tool %||% "")
}

# Whether corteza wrote the call's tool, so that its paths are read by
# argument name. A tool a host adds to a room session is corteza's.
call_named_by_corteza <- function(call) {
    origin <- call$origin
    if (is.character(origin) && length(origin) == 1L) {
        return(identical(origin, "builtin") ||
               isTRUE((call$tool %||% "") %in% names(.session_tool_classes)))
    }
    .args_named_by_corteza(call$tool %||% "")
}

# Classify the data class of a tool call. Sticky: if any prior call in the
# same turn was classified "personal", this one is too, so personal data
# can't be laundered through a later random-classed call.
classify_data <- function(call, context = NULL) {
    recent <- context$recent_classes %||% character()
    if ("personal" %in% recent) {
        return("personal")
    }

    paths <- path.expand(call$paths %||% character())
    if (!length(paths)) return(if ("code" %in% recent) "code" else "random")

    personal <- path.expand(get_personal_paths())
    code <- path.expand(get_code_paths())

    for (p in paths) {
        for (pp in personal) {
            if (startsWith(p, pp)) {
                return("personal")
            }
        }
    }
    for (p in paths) {
        for (cp in code) {
            if (startsWith(p, cp)) {
                return("code")
            }
        }
    }
    "random"
}

# ---- Modes ----

# The verdict each mode gives each class. A mode other than the default
# only refuses: a call it lets through is decided as in the default, by
# the rules after it. A room with no directory (`unbound`) refuses the
# control tools too, but for those .unbound_control names.
.mode_verdicts <- list(
                       default = c(read = "allow", write = "ask", exec = "ask",
                                   control = "allow", continue = "allow", unknown = "ask"),
                       plan = c(read = "allow", write = "deny", exec = "deny", control = "allow",
                                continue = "deny", unknown = "deny"),
                       unbound = c(read = "allow", write = "deny", exec = "deny",
                                   control = "deny", continue = "deny", unknown = "deny")
)

# The control tools a room with no directory may still use. Each acts on
# the session's own conversation or task list, or stops a job the
# session may see (job_cancel checks). Not spawn_subagent: its child
# would not keep the room's limits. Not kill_subagent or bg_kill: their
# registries belong to the process, and neither checks whose a subagent
# or a background process is, so either could stop another room's work.
.unbound_control <- c("task_create", "task_update", "edit_context",
                      "exit_plan_mode", "job_cancel")

# Run the gate of a room with no directory (WP10). It reads and talks:
# a read passes, and so does a control tool .unbound_control names.
# Returns a deny decision for anything else, or NULL. The refusal says
# how the room gets a directory.
check_unbound_room <- function(call) {
    if (!isTRUE(call$context$unbound)) {
        return(NULL)
    }
    tool <- call$tool %||% ""
    op <- call_class(call)
    if (!identical(.mode_verdicts$unbound[[op]], "deny") ||
        (identical(op, "control") && tool %in% .unbound_control)) {
        return(NULL)
    }
    why <- switch(op, write = "would write files", exec = "would run commands",
                  continue = "would run work in a child, a worker or another room",
                  control = "would start work that does not keep the room's limits",
                  "has no class, so it may modify state")
    list(
         model = "cloud",
         approval = "deny",
         reason = sprintf("this room has no directory, so it only reads and talks: %s %s. %s",
                          tool, why, bot_unbound_how()),
         rule = "unbound_room"
    )
}

# Run the plan-mode gate. Returns a deny decision to short-circuit when
# the session is in plan mode and the tool's class is one plan mode
# refuses, or NULL. `exit_plan_mode` is a control tool, so it passes (it
# is the escape hatch). Reads pass through. Sticky data-class routing
# still applies to the read result.
check_plan_mode <- function(call) {
    if (!isTRUE(call$context$plan_mode)) {
        return(NULL)
    }
    tool <- call$tool %||% ""
    op <- call_class(call)
    if (!identical(.mode_verdicts$plan[[op]], "deny")) {
        return(NULL)
    }
    why <- switch(op, continue = "runs work that does not follow plan mode",
                  unknown = "has no class, so it may modify state",
                  "would modify state")
    # A job's worker cannot end plan mode (R/job-authority.R), so it is
    # not told to.
    then <- if (isTRUE(call$context$plan_mode_locked)) {
        "report the plan; only the person can end plan mode"
    } else {
        "present a plan via exit_plan_mode first"
    }
    list(
         model = "cloud",
         approval = "deny",
         reason = sprintf("plan mode: %s %s; %s", tool, why, then),
         rule = "plan_mode"
    )
}

# Leaving plan mode asks a person: the person set the mode, and the plan
# is theirs to approve. Before 0.8.1 nothing asked. exit_plan_mode is a
# control tool, which runs, and the tool answers "Plan approved", so the
# model left plan mode on its own word. The decision carries
# rule = "plan_exit", which .person_rule() names. turn()'s handler
# refuses the call by name first where the session may not end plan
# mode (a job's worker) and outside plan mode, so neither gets here.
check_plan_exit <- function(call) {
    if (!identical(call$tool %||% "", "exit_plan_mode") ||
        !isTRUE(call$context$plan_mode)) {
        return(NULL)
    }
    list(model = "cloud", approval = "ask",
         reason = "plan mode: ending it is yours to approve; the plan is above",
         rule = "plan_exit")
}

# The rules only a person answers, each time: a saved "always" does not,
# a prompt offers none, and a gate for unattended calls passes the call
# to a person (supervisor_route(), the hall monitor).
.person_rule <- function(decision) {
    isTRUE(decision$rule %in% c("credential", "plan_exit"))
}

# Run hard safety checks. Returns a decision list to short-circuit, or NULL.
# The decision carries rule = "credential": an approval callback asks for
# it whatever was saved, and offers no "always" (.person_rule()).
check_safety <- function(call) {
    paths <- path.expand(call$paths %||% character())
    if (!length(paths)) {
        return(NULL)
    }

    secrets <- path.expand(.hard_secret_paths())
    for (p in paths) {
        for (s in secrets) {
            if (startsWith(p, s)) {
                return(list(
                            model = "local",
                            approval = "ask",
                            reason = sprintf("safety: %s is a credential path", p),
                            rule = "credential"
                    ))
            }
        }
    }
    NULL
}

# ---- Default table ----

# Personal data routes to a local model; everything else to cloud.
# Nothing routes on it yet: the field is computed, recorded and returned.
.default_model_route <- function(data_class) {
    if (identical(data_class, "personal")) {
        "local"
    } else {
        "cloud"
    }
}

# Default decision: the class's verdict in the default mode. Personal data
# is refused for a write or a command in a room, where the person who
# would answer is not at a prompt. A read of it is allowed.
default_policy <- function(call) {
    data_class <- classify_data(call, call$context)
    op <- call_class(call)
    channel <- call$channel %||% "cli"

    approval <- .mode_verdicts$default[[op]]
    if (identical(data_class, "personal") && op %in% c("write", "exec") &&
        identical(channel, "matrix")) {
        approval <- "deny"
    }

    list(model = .default_model_route(data_class), approval = approval,
         reason = sprintf("default: %s/%s/%s", data_class, op, channel))
}

# ---- Main entry point ----

#' Evaluate policy for a tool call
#'
#' Returns a decision \code{list(model, approval, reason)}. \code{model} is
#' \code{"cloud"} or \code{"local"}; nothing routes on it yet.
#' \code{approval} is \code{"allow"}, \code{"ask"}, or \code{"deny"}. A
#' verdict from the credential rule also carries
#' \code{rule = "credential"}, one that asks before leaving plan mode
#' \code{rule = "plan_exit"}, one from plan mode
#' \code{rule = "plan_mode"}, and one from a room with no directory
#' \code{rule = "unbound_room"}. A person answers the first two each
#' time: a saved "always" does not, and a prompt offers none.
#'
#' Every tool has a class, declared where it is registered:
#' \code{"read"}, \code{"write"}, \code{"exec"}, \code{"control"} (acts on
#' the session's own work) or \code{"continue"} (runs work in a child, a
#' worker or another room). A tool that declares none is
#' \code{"unknown"}. By default \code{read}, \code{control} and
#' \code{continue} are allowed, and \code{write}, \code{exec} and
#' \code{unknown} ask. Plan mode refuses \code{write}, \code{exec},
#' \code{continue} and \code{unknown}. A Matrix room with no directory
#' (\code{context$unbound}) refuses everything but \code{read} and a few
#' named \code{control} tools. Nothing lifts a mode's refusal. A path
#' under a credential store asks next, whatever follows.
#'
#' When \code{config} is supplied, the user's settings are overlaid on
#' the table: a \code{permissions} entry replaces the verdict for its
#' tool, a tool named in \code{dangerous_tools} asks, and
#' \code{approval_mode} says what "ask" means for a tool with no
#' \code{permissions} entry. None of them lifts a deny.
#'
#' Both \code{corteza::chat()} and the CLI tool dispatch loop pass
#' their session's config through here so the \code{/permissions}
#' contract advertised by both surfaces is enforced consistently.
#' \code{serve()} does not: an MCP client asks its own user.
#'
#' @param call A list describing the tool call. See the file header in
#'   \code{R/policy.R} for the expected fields.
#' @param config Optional config list from \code{load_config()}. When
#'   \code{NULL} (default), only the table and user policy apply; this
#'   matches the historical \code{policy(call)} contract.
#'
#' @return A decision list with fields \code{model}, \code{approval},
#'   \code{reason}.
#' @examples
#' # A bare-environment read_file call resolves under the default
#' # built-in policy without needing any session config.
#' decision <- policy(list(tool = "read_file",
#'                         args = list(path = "DESCRIPTION")))
#' decision$approval
#' @export
policy <- function(call, config = NULL) {
    if (is.null(call$paths)) {
        call$paths <- resolve_paths(call)
    }
    if (is.null(call$urls)) {
        call$urls <- resolve_urls(call)
    }

    # The mode first: a credential prompt answered yes must not perform
    # what the mode refuses. A room with no directory before plan mode,
    # whose refusal it outlasts.
    unbound <- check_unbound_room(call)
    if (!is.null(unbound)) {
        return(unbound)
    }
    plan <- check_plan_mode(call)
    if (!is.null(plan)) {
        return(plan)
    }
    leave <- check_plan_exit(call)
    if (!is.null(leave)) {
        return(leave)
    }

    safety <- check_safety(call)
    if (!is.null(safety)) {
        return(safety)
    }

    user_fn <- getOption("corteza.policy")
    if (is.function(user_fn)) {
        user <- tryCatch(user_fn(call), error = function(e) NULL)
        if (is.list(user) && !is.null(user$model) && !is.null(user$approval)) {
            if (is.null(user$reason)) {
                user$reason <- "user policy"
            }
            # User policy is final. Project config does NOT overlay
            # here — a process-level user policy is explicitly the
            # "I know better than the project config" hook, so we
            # respect that precedence in both directions: config can't
            # tighten and can't relax it.
            return(user)
        }
    }

    .apply_config_overlay(default_policy(call), call, config)
}

#' Overlay the configured tool permissions on top of a base policy
#' decision, so the /permissions contract is enforced in both chat()
#' and the CLI.
#'
#' Resolution rules (in order):
#'
#' \itemize{
#'   \item A base \code{"deny"} stays. Config can tighten, never widen
#'     a deny.
#'   \item A \code{permissions} entry for the tool replaces the verdict:
#'     \code{"deny"}, \code{"ask"} or \code{"allow"}.
#'   \item Otherwise a tool named in \code{dangerous_tools} asks where
#'     the table allowed it, and \code{approval_mode} (\code{"ask"},
#'     \code{"allow"} or \code{"deny"}) says what an \code{"ask"}
#'     becomes. Nothing else turns an "ask" into an "allow".
#' }
#' @noRd
.apply_config_overlay <- function(base, call, config) {
    if (is.null(config)) {
        return(base)
    }
    tool <- call$tool %||% ""
    if (!nzchar(tool) || identical(base$approval, "deny")) {
        return(base)
    }
    verdict <- function(approval, reason) {
        list(model = base$model %||% "cloud", approval = approval,
             reason = reason)
    }

    perm <- get_tool_permission(tool, config)
    if (!is.null(perm)) {
        if (identical(perm, base$approval)) {
            return(base)
        }
        return(switch(perm,
                      deny = verdict("deny", sprintf("config: %s denied", tool)),
                      ask = verdict("ask", sprintf("config: %s requires approval", tool)),
                      allow = verdict("allow", sprintf("config: %s allowed", tool)),
                      base))
    }

    out <- base
    dangerous <- config$dangerous_tools %||% default_dangerous_tools()
    if (identical(out$approval, "allow") && tool %in% dangerous) {
        out <- verdict("ask", sprintf("config: %s is in dangerous_tools", tool))
    }
    mode <- config$approval_mode %||% "ask"
    if (identical(out$approval, "ask") && mode %in% c("allow", "deny")) {
        out <- verdict(mode, sprintf("config: approval_mode is %s (%s)", mode,
                                     out$reason))
    }
    out
}
