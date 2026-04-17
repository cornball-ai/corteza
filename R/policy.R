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
    for (key in c("path", "file", "filename", "dir", "directory",
                  "dest", "destination", "from", "to", "src", "target")) {
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
    read_tools <- c("read_file", "list_files", "grep_files",
                    "git_log", "git_diff", "git_status",
                    "web_search", "fetch_url",
                    "r_help", "installed_packages")
    write_tools <- c("write_file", "replace_in_file")
    exec_tools <- c("bash", "run_r", "run_r_script")

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
                                    write = list(cli = "ask", console = "allow", matrix = "ask"),
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

    approval <- tryCatch(
                         .default_tensor[[data_class]][[op]][[channel]],
                         error = function(e) NULL
    )
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
#' @param call A list describing the tool call. See the file header in
#'   \code{R/policy.R} for the expected fields.
#'
#' @return A decision list with fields \code{model}, \code{approval},
#'   \code{reason}.
#' @export
policy <- function(call) {
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

    user_fn <- getOption("corteza.policy")
    if (is.function(user_fn)) {
        user <- tryCatch(user_fn(call), error = function(e) NULL)
        if (is.list(user) && !is.null(user$model) && !is.null(user$approval)) {
            if (is.null(user$reason)) {
                user$reason <- "user policy"
            }
            return(user)
        }
    }

    default_policy(call)
}

