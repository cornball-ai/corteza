# Policy engine: one decision per tool call.
#
# policy(call) -> list(model, approval, reason)
#
# Precedence (most specific wins):
#   1. Hard safety rules (cannot be overridden by user config)
#   2. User policy function, if options(corteza.policy = ...) is set
#   3. Default tensor lookup on (data_class, op, channel)
#   4. Fallback: cloud + ask
#
# A "call" is a list with fields:
#   tool      - character, the tool name (e.g. "read_file")
#   args      - list of arguments the LLM supplied
#   paths     - character, filesystem paths touched (resolved from args if NULL)
#   urls      - character, URLs touched (resolved from args if NULL)
#   channel   - character, one of "cli", "console", "matrix"
#   context   - list, optional; context$recent_classes carries sticky
#               classifications from earlier calls in the same turn

# ---- Hard safety ----

# Paths where reads must route to a local model and any operation requires
# an ask. These cannot be overridden by user config.
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

# ---- Path / URL resolution ----

# Pull filesystem paths out of tool arguments. Best-effort: looks at common
# arg keys. Shell commands and run_r code bodies are not parsed.
resolve_paths <- function(call) {
    args <- call$args %||% list()
    out <- character()
    for (key in c("path", "file", "filename", "dir", "directory", "dest",
                  "destination", "from", "to", "src", "target")) {
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

# Classify the operation class of a tool.
classify_op <- function(tool_name) {
    read_tools <- c("read_file", "list_files", "grep_files", "git_log",
                    "git_diff", "git_status", "web_search", "fetch_url",
                    "r_help", "installed_packages")
    write_tools <- c("write_file", "replace_in_file")
    exec_tools <- c("bash", "cmd", "run_r", "run_r_script")

    if (tool_name %in% read_tools) {
        return("read")
    }
    if (tool_name %in% write_tools) {
        return("write")
    }
    if (tool_name %in% exec_tools) {
        return("exec")
    }
    "unknown"
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

# Run the plan-mode gate. Returns a deny decision to short-circuit when
# the session is in plan mode and the tool would mutate state, or NULL.
# `exit_plan_mode` is always allowed (it's the escape hatch). Reads pass
# through. Sticky data-class routing still applies to the read result.
check_plan_mode <- function(call) {
    if (!isTRUE(call$context$plan_mode)) {
        return(NULL)
    }
    tool <- call$tool %||% ""
    if (identical(tool, "exit_plan_mode")) {
        return(NULL)
    }
    op <- classify_op(tool)
    if (op %in% c("write", "exec")) {
        return(list(
                    model = "cloud",
                    approval = "deny",
                    reason = sprintf(
                                     "plan mode: %s would modify state; present a plan via exit_plan_mode first",
                                     tool)
            ))
    }
    NULL
}

# Run hard safety checks. Returns a decision list to short-circuit, or NULL.
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
                            reason = sprintf("safety: %s is a credential path", p)
                    ))
            }
        }
    }
    NULL
}

# ---- Default tensor ----

# (data_class, op, channel) -> "allow" | "ask" | "deny"
.default_tensor <- list(
                        personal = list(
                                        read = list(cli = "ask", console = "ask", matrix = "ask"),
                                        write = list(cli = "ask", console = "ask", matrix = "deny"),
                                        exec = list(cli = "ask", console = "ask", matrix = "deny")
    ),
                        code = list(
                                    read = list(cli = "allow", console = "allow", matrix = "allow"),
                                    write = list(cli = "ask", console = "ask", matrix = "ask"),
                                    exec = list(cli = "ask", console = "allow", matrix = "ask")
    ),
                        random = list(
                                      read = list(cli = "allow", console = "allow", matrix = "allow"),
                                      write = list(cli = "allow", console = "allow", matrix = "allow"),
                                      exec = list(cli = "allow", console = "allow", matrix = "allow")
    )
)

# Personal data routes to a local model; everything else to cloud.
.default_model_route <- function(data_class) {
    if (identical(data_class, "personal")) {
        "local"
    } else {
        "cloud"
    }
}

# Default decision based on the tensor.
default_policy <- function(call) {
    data_class <- classify_data(call, call$context)
    op <- classify_op(call$tool %||% "")
    channel <- call$channel %||% "cli"

    approval <- tryCatch(.default_tensor[[data_class]][[op]][[channel]],
                         error = function(e) NULL)
    if (is.null(approval)) {
        approval <- "ask"
    }

    list(
         model = .default_model_route(data_class),
         approval = approval,
         reason = sprintf("default: %s/%s/%s", data_class, op, channel)
    )
}

# ---- Main entry point ----

#' Evaluate policy for a tool call
#'
#' Returns a decision \code{list(model, approval, reason)}. \code{model} is
#' \code{"cloud"} or \code{"local"}; \code{approval} is \code{"allow"},
#' \code{"ask"}, or \code{"deny"}.
#'
#' When \code{config} is supplied, the project's
#' \code{approval_mode} / \code{dangerous_tools} / per-tool
#' \code{permissions} are overlaid on top of the default tensor: a
#' tool the user has configured as \code{"ask"} or \code{"deny"} will
#' have its decision promoted accordingly. Safety verdicts (credential
#' paths, plan mode) still win because those represent invariants the
#' user can't waive from config.
#'
#' Both \code{corteza::chat()} and the CLI tool dispatch loop pass
#' their session's config through here so the \code{/permissions}
#' contract advertised by both surfaces is enforced consistently.
#'
#' @param call A list describing the tool call. See the file header in
#'   \code{R/policy.R} for the expected fields.
#' @param config Optional config list from \code{load_config()}. When
#'   \code{NULL} (default), only the built-in tensor and user policy
#'   apply; this matches the historical \code{policy(call)} contract.
#'
#' @return A decision list with fields \code{model}, \code{approval},
#'   \code{reason}.
#' @examples
#' # A bare-environment read_file call resolves under the default
#' # built-in policy without needing any session config.
#' decision <- policy(list(name = "read_file",
#'                         arguments = list(path = "DESCRIPTION")))
#' decision$approval
#' @export
policy <- function(call, config = NULL) {
    if (is.null(call$paths)) {
        call$paths <- resolve_paths(call)
    }
    if (is.null(call$urls)) {
        call$urls <- resolve_urls(call)
    }

    safety <- check_safety(call)
    if (!is.null(safety)) {
        return(safety)
    }

    plan <- check_plan_mode(call)
    if (!is.null(plan)) {
        return(plan)
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
#' decision. Honors approval_mode + dangerous_tools + per-tool
#' permissions so the /permissions contract is actually enforced in
#' both chat() and CLI.
#'
#' Resolution rules (in order):
#'
#' \itemize{
#'   \item Safety verdicts (credential paths, etc.) have already
#'     short-circuited by the time we get here, so a base \code{"deny"}
#'     can only come from the data tensor or a user fn. We never
#'     downgrade that.
#'   \item Config \code{"deny"} wins over base \code{"allow"} or
#'     \code{"ask"} — config can always tighten.
#'   \item Config \code{"ask"} promotes base \code{"allow"} to
#'     \code{"ask"}. (It doesn't downgrade base \code{"deny"}.)
#'   \item Config \code{"allow"} downgrades base \code{"ask"} to
#'     \code{"allow"}. This mirrors the CLI's
#'     \code{requires_approval(name, dangerous_tools)} semantics: a
#'     tool the user has explicitly marked allow skips approval
#'     regardless of data class. It does **not** override a base
#'     \code{"deny"} — config can tighten or relax \code{"ask"},
#'     never widen a \code{"deny"}.
#' }
#' @noRd
.apply_config_overlay <- function(base, call, config) {
    if (is.null(config)) {
        return(base)
    }
    tool <- call$tool %||% ""
    if (!nzchar(tool)) {
        return(base)
    }
    perm <- tryCatch(get_tool_permission(tool, config),
                     error = function(e) NULL)
    if (is.null(perm)) {
        return(base)
    }

    if (identical(perm, "deny")) {
        return(list(model = base$model %||% "cloud",
                    approval = "deny",
                    reason = sprintf("config: %s denied", tool)))
    }
    if (identical(perm, "ask")) {
        if (identical(base$approval, "allow")) {
            return(list(model = base$model %||% "cloud",
                        approval = "ask",
                        reason = sprintf("config: %s requires approval", tool)))
        }
        return(base)
    }
    if (identical(perm, "allow")) {
        if (identical(base$approval, "ask")) {
            return(list(model = base$model %||% "cloud",
                        approval = "allow",
                        reason = sprintf("config: %s allowed", tool)))
        }
        return(base)
    }
    base
}

