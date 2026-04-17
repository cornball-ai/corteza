# Shared pre-session setup.
#
# Every channel (cli, console, matrix) needs the same configuration,
# skill loading, and system-prompt assembly before it can run turns.
# session_setup() does that work once, returning a session environment
# the channel's loop can drive.

#' Configure and construct a session for any channel
#'
#' Performs pre-turn setup common to all channels:
#'
#' \enumerate{
#'   \item Loads project + global corteza config from \code{cwd}.
#'   \item Resolves provider, model, and verifies the required API
#'     environment variable is set.
#'   \item Registers built-in skills and loads user/project skills and
#'     skill docs from \code{~/.corteza/skills} and
#'     \code{<cwd>/.corteza/skills}.
#'   \item Loads skill packages declared in the config.
#'   \item Optionally builds the system prompt via \code{load_context(cwd)}.
#'   \item Returns a \code{new_session()} built from the above.
#' }
#'
#' @param channel Character, one of \code{"cli"}, \code{"console"},
#'   \code{"matrix"}.
#' @param cwd Working directory. Defaults to the current directory.
#' @param provider Character or NULL. LLM provider override. NULL falls
#'   back to \code{config$provider}, then \code{"anthropic"}.
#' @param model Character or NULL. Model override. NULL falls back to
#'   \code{config$model}, then the provider default.
#' @param tools Character vector, NULL, or the string \code{"all"}.
#'   Tool filter passed through to \code{get_tools()}. NULL is treated
#'   as \code{"all"}.
#' @param system Character or NULL. System prompt. NULL auto-builds via
#'   \code{load_context(cwd)} when \code{load_project_context = TRUE},
#'   otherwise left NULL (channel supplies its own).
#' @param approval_cb Function or NULL. Approval callback for
#'   \code{"ask"} verdicts; see \code{\link{new_session}}.
#' @param load_project_context Logical. When TRUE, auto-call
#'   \code{load_context(cwd)} to assemble the system prompt. Channels
#'   with their own short system prompt (like matrix) pass FALSE.
#' @param validate_api_key Logical. When TRUE, error if the provider's
#'   API key env var is unset or empty.
#' @param verbose Logical. Passed through to \code{new_session}.
#' @param max_turns Integer. Passed through to \code{new_session}.
#'
#' @return A session environment from \code{\link{new_session}}, with
#'   an extra \code{cwd} field set.
#' @export
session_setup <- function(channel = c("cli", "console", "matrix"),
                          cwd = getwd(), provider = NULL, model = NULL,
                          tools = NULL, system = NULL, approval_cb = NULL,
                          history = NULL, load_project_context = TRUE,
                          validate_api_key = TRUE, verbose = FALSE,
                          max_turns = 10L) {
    channel <- match.arg(channel)
    cwd <- path.expand(cwd)
    config <- load_config(cwd)

    provider <- provider %||% config$provider %||% "anthropic"
    ensure_llm_api_provider(provider)
    model <- model %||% config$model

    if (isTRUE(validate_api_key)) {
        key_var <- switch(provider,
                          anthropic = "ANTHROPIC_API_KEY",
                          openai = "OPENAI_API_KEY",
                          moonshot = "MOONSHOT_API_KEY",
                          NULL
        )
        if (!is.null(key_var) && nchar(Sys.getenv(key_var, "")) == 0L) {
            stop(sprintf("%s not set. Add it to ~/.Renviron", key_var),
                 call. = FALSE)
        }
    }

    # Skill registration + user overrides
    ensure_skills()
    load_skills(path.expand("~/.corteza/skills"))
    load_skills(file.path(cwd, ".corteza", "skills"))
    load_skill_docs(path.expand("~/.corteza/skills"))
    load_skill_docs(file.path(cwd, ".corteza", "skills"))
    load_skill_packages(config)
    options(corteza.tools = tools)

    if (is.null(system) && isTRUE(load_project_context)) {
        system <- load_context(cwd)
    }

    session <- new_session(
                           channel = channel,
                           history = history,
                           model_map = list(cloud = model, local = default_local_model()),
                           provider = provider,
                           tools_filter = tools,
                           system = system,
                           approval_cb = approval_cb,
                           max_turns = max_turns,
                           verbose = verbose
    )
    session$cwd <- cwd
    session$config <- config
    session
}

