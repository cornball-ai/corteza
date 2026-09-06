# Context loading for corteza
# Builds a saber context manifest, then renders it for the system prompt.

#' Load context for the system prompt
#'
#' Assembles a system prompt for the LLM by combining:
#' \enumerate{
#'   \item corteza's preamble and runtime guidance
#'   \item \code{saber::briefing()} project metadata (if available)
#'   \item Shared and project instructions discovered by saber
#'   \item Workspace identity, configured context files, skill docs,
#'     harness lessons, and live-subagent state
#' }
#'
#' @param cwd Working directory.
#' @return Character string with assembled context, or NULL if empty.
#' @noRd
load_context <- function(cwd = getwd()) {
    load_context_bundle(cwd)$system
}

#' Assemble rendered context and its source manifest
#'
#' The character-returning load_context() contract stays small for callers
#' that only need a system prompt. Session constructors retain the manifest
#' from this bundle for /context diagnostics.
#' @param cwd Working directory.
#' @param prefix_sources Optional consumer-owned source descriptors rendered
#'   before corteza's normal preamble (used by the Matrix adapter).
#' @param instruction_catalog Optional prebuilt session catalog.
#' @param include_instruction_catalog Whether to render its compact header.
#' @return List with system, manifest, and instruction catalog.
#' @noRd
load_context_bundle <- function(cwd = getwd(), prefix_sources = list(),
                                instruction_catalog = NULL,
                                include_instruction_catalog = TRUE) {
    config <- load_config(cwd)
    if (is.null(instruction_catalog)) {
        instruction_catalog <- build_instruction_catalog(cwd)
    }
    sources <- c(prefix_sources, context_base_sources(cwd),
                 context_workspace_sources(config),
                 context_custom_sources(cwd, config),
                 context_dynamic_sources(
            cwd, config, instruction_catalog,
            include_instruction_catalog = include_instruction_catalog
        ))
    manifest <- saber::context_manifest(
                                        agent = "corteza",
                                        project_dir = cwd,
                                        workspace_dir = NULL,
                                        shared_path = if (isFALSE(config$context_include_user)) FALSE else NULL,
                                        extra_sources = sources
    )
    system <- saber::context_render(manifest)
    if (!nzchar(trimws(system))) {
        system <- NULL
    }
    list(system = system, manifest = manifest,
         prefix_sources = prefix_sources,
         instruction_catalog = instruction_catalog)
}

#' Build a generated manifest source
#' @noRd
context_text_source <- function(id, kind, text, order, origin, scope = "",
                                config_path = "") {
    if (is.null(text)) {
        return(NULL)
    }
    if (length(text) != 1L) {
        text <- paste(text, collapse = "\n")
    }
    text <- trimws(text, which = "right")
    if (!nzchar(text)) {
        return(NULL)
    }
    list(id = id, kind = kind, text = text, order = order, origin = origin,
         scope = scope, config_path = config_path)
}

#' Static and project-generated context sources
#' @noRd
context_base_sources <- function(cwd) {
    Filter(Negate(is.null), list(
                                 context_text_source("corteza_preamble", "runtime",
                corteza_preamble(), -300,
                "corteza::load_context", "session"),
                                 context_text_source(
                "corteza_runtime", "runtime", corteza_runtime_guidance(), -200,
                "corteza::corteza_runtime_guidance", "session"
            ),
                                 context_text_source(
                "project_briefing", "briefing", load_saber_briefing(cwd), -100,
                "saber::briefing", cwd
            )
        ))
}

#' Optional workspace identity sources
#' @noRd
context_workspace_sources <- function(config) {
    workspace <- get_workspace_dir()
    sources <- list()
    if (!isFALSE(config$context_include_user)) {
        sources <- c(sources, list(list(id = "workspace_user",
                                        kind = "shared",
                                        path = file.path(workspace, "USER.md"),
                                        order = 10,
                                        origin = "corteza workspace",
                                        scope = workspace)))
    }
    if (!isFALSE(config$context_include_soul)) {
        sources <- c(sources, list(list(
                                        id = "workspace_soul", kind = "identity",
                                        path = file.path(workspace, "SOUL.md"), order = 11,
                                        origin = "corteza workspace", scope = workspace
                )))
    }
    sources
}

#' Configured project context sources
#' @noRd
context_custom_sources <- function(cwd, config) {
    files <- config$context_files %||% character()
    if (!length(files)) {
        return(list())
    }
    config_path <- context_config_origin(cwd, "context_files")
    lapply(seq_along(files), function(i) {
        list(id = sprintf("configured_context_%03d", i), kind = "context",
             path = files[[i]], order = 100 + i,
             origin = "corteza context_files", scope = cwd,
             config_path = config_path)
    })
}

#' Locate the configuration file that supplied a merged value
#'
#' Project configuration wins only when it defines the requested key. Merely
#' having a project config file must not misattribute an inherited global value.
#' @noRd
context_config_origin <- function(cwd, key) {
    project_path <- file.path(cwd, ".corteza", "config.json")
    project <- load_config_file(project_path)
    if (key %in% names(project)) {
        return(project_path)
    }

    global_path <- corteza_config_path("config.json")
    global <- load_config_file(global_path)
    if (key %in% names(global)) {
        return(global_path)
    }

    ""
}

#' Dynamic runtime context sources
#' @noRd
context_dynamic_sources <- function(cwd, config, instruction_catalog = NULL,
                                    include_instruction_catalog = TRUE) {
    skill_text <- ""
    if (isTRUE(include_instruction_catalog) && !is.null(instruction_catalog)) {
        skill_text <- format_instruction_catalog(instruction_catalog)
    }
    harness_text <- tryCatch(harness_context_block(cwd, config),
                             error = function(e) "")
    Filter(Negate(is.null), list(
                                 context_text_source(
                "instruction_catalog", "skill", skill_text, 200,
                "corteza::format_instruction_catalog", "session"
            ),
                                 context_text_source(
                "harness_lessons", "memory", harness_text, 300,
                "corteza::harness_context_block", cwd
            ),
                                 context_text_source(
                "live_subagents", "subagents", format_live_subagents(), 400,
                "corteza::format_live_subagents", "session"
            )
        ))
}

#' Base system instructions owned by corteza
#' @noRd
corteza_preamble <- function() {
    paste(
          "You are an AI assistant with access to tools for working with R and the file system.",
          "Use the bash tool to run shell commands. Below is context about the current project",
          "and available skills.",
          "run_r and subagents may return a .h_NNN handle instead of inlining a large result;",
          "reference it by name when present, and don't re-read it. To get a structured value",
          "back from a subagent, give it run_r (the 'work' preset), have it leave the result",
          "bound to a name, and pass that name as query_subagent's return_name.",
          sep = "\n"
    )
}

#' Runtime guidance block for the system prompt
#'
#' Describes corteza's own runtime: the live, persistent R session, how
#' its tools behave, and that the bash tool makes it a general-purpose
#' agent, not an R-only one. Static text; corteza owns the tool names it
#' references, so this lives here rather than in saber.
#' @noRd
corteza_runtime_guidance <- function() {
    paste(
          "## Corteza Runtime Environment",
          "",
          "You are corteza, running inside a live, persistent R session. Your `run_r`",
          "tool evaluates code in that session, and the workspace survives across turns:",
          "objects you create stick around, attached packages stay attached, the working",
          "directory persists. You are not shelling out to `Rscript` for each call.",
          "Treat your conversation history and R workspace as persistent scratch state",
          "for the current agent session. Treat the workspace as a small evolving program:",
          "retain concise notes, helper functions, hypotheses, and intermediate analysis",
          "there. When an operation recurs, define and reuse a helper instead of emitting",
          "the same inline code again.",
          "",
          "Guidelines:",
          "",
          "- To inspect or transform data, just run R. Do not ask the user to run it,",
          "  and do not write a throwaway `.R` file unless the goal is a committed script.",
          "- Load data once, keep it in the workspace, reuse it across turns. If `dat`",
          "  already exists from an earlier turn, do not re-read the CSV.",
          "- `getwd()` is the project root. Paths resolve relative to it.",
          "- `run_r` runs an expression in the live session; `run_r_script` runs a file.",
          "  Prefer `run_r` for exploration, scripts for anything that gets committed.",
          "- Plots: in headless sessions, write to a file rather than assuming a screen",
          "  device.",
          "",
          "You are not limited to R. The bash tool makes you a general-purpose agent in",
          "the same way Claude Code is: use it for shell commands, git, file operations,",
          "builds, and tasks in other languages. Reach for bash whenever the work isn't R,",
          "and combine it with `run_r` as the task demands.",
          "",
          "## Communicating while you work",
          "",
          "Narrate as you go. Before a tool call or a batch of related calls, say in",
          "one line what you're about to do and why; after they return, say what you",
          "found and how it changes the plan. The user should never watch a silent",
          "streak of tool calls and have to guess what you're doing.",
          "",
          "- Give a short update between the steps of a multi-step task, not just at",
          "  the end. Don't go more than a couple of tool calls without a word.",
          "- If the user asks what you're doing, answer in plain prose first, before",
          "  any further tool calls.",
          "- Stop and check in before anything destructive, ambiguous, or expensive,",
          "  and whenever the task could reasonably go more than one way.",
          "- When the plan changes mid-task, say so and why before continuing.",
          sep = "\n"
    )
}

#' Render the live-subagents block for the system prompt.
#'
#' Empty string when no subagents are registered. The empty case skips
#' the block entirely so a default-off corteza session sees byte-
#' identical context to before archival landed.
#' @noRd
format_live_subagents <- function() {
    agents <- tryCatch(subagent_list(), error = function(e) list())
    if (length(agents) == 0L) {
        return("")
    }
    lines <- vapply(agents, function(a) {
        sprintf("- id: %s | task: %s", a$id %||% "?", a$task %||% "?")
    }, character(1))
    paste(c(
            "# Live Subagents",
            "",
            paste0("These subagents hold archived prior turns of this ",
                   "conversation. Use the `query_subagent(id, prompt)` tool ",
                   "to retrieve detail; spawn a new one with ",
                   "`spawn_subagent(task)` to fan out. Do not query unless ",
                   "you actually need the detail."),
            "",
            lines
        ), collapse = "\n")
}

#' Call saber::briefing() for project metadata
#'
#' Returns the briefing text or NULL on failure.
#' @noRd
load_saber_briefing <- function(cwd) {
    project <- basename(cwd)
    scan_dir <- dirname(cwd)
    tryCatch({
        # saber::briefing() emits its full text via message(); without
        # suppressMessages it leaks to the user's terminal every time a
        # subagent calls session_setup, which masquerades as a session
        # restart. capture.output() handles any stdout cat() too.
        utils::capture.output(
                              text <- suppressMessages(
                saber::briefing(project = project, scan_dir = scan_dir)
            )
        )
        if (is.null(text) || nchar(trimws(text)) == 0L) {
            NULL
        } else {
            text
        }
    }, error = function(e) NULL)
}

#' List custom context files that would be loaded
#'
#' Returns configured \code{context_files} that exist. Relative paths are
#' resolved from the project; absolute and tilde paths are preserved.
#'
#' @param cwd Working directory.
#' @return Character vector of existing custom context file paths.
#' @noRd
list_context_files <- function(cwd = getwd()) {
    config <- load_config(cwd)
    file_names <- config$context_files %||% character(0)
    if (length(file_names) == 0L) {
        return(character(0))
    }
    paths <- vapply(file_names, function(path) {
        expanded <- path.expand(path)
        if (grepl("^(/|[A-Za-z]:[/\\\\])", expanded)) {
            expanded
        } else {
            file.path(cwd, expanded)
        }
    }, character(1L), USE.NAMES = FALSE)
    paths[file.exists(paths)]
}
