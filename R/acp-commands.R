# The commands an ACP session takes (WP26, stage 3): the rows of the
# command table marked `acp` (R/commands.R). The session offers its
# client those that go by their own name (`available_commands_update`,
# sent after a session/new or session/resume answer). A prompt that
# begins with one runs it in place of a turn, and what the command
# prints is the reply. Commands run as the terminal runs them, through
# the same functions (R/commands-shared.R), with no colors, and as the
# prompt they are (.acp_prompt()), so a cancel reaches /compact, the one
# that asks a model.
#
# Starting over and moving belong to the client, which starts a new
# session for either: /clear and /cd, and their aliases, are not
# offered, and typed anyway they answer with that sentence.

.ACP_NOT_HERE <- c("clear", "reset", "new", "cd")

# The command a prompt names, without its slash, or NULL when the prompt
# is not one of the session's commands. Another word after a slash (a
# path, say) is text for the model.
.acp_command_name <- function(text) {
    if (!is.character(text) || length(text) != 1L || !startsWith(text, "/")) {
        return(NULL)
    }
    name <- tolower(sub("^/([^[:space:]]*).*$", "\\1", text))
    tbl <- corteza_commands()
    if (name %in% c(tbl$name[tbl$acp], .ACP_NOT_HERE)) {
        name
    } else {
        NULL
    }
}

# The update that tells the client which commands a session takes.
.acp_commands_update <- function() {
    tbl <- corteza_commands()
    rows <- tbl[tbl$acp & !nzchar(tbl$same_as),, drop = FALSE]
    list(sessionUpdate = "available_commands_update",
         availableCommands = lapply(seq_len(nrow(rows)), function(i) {
        cmd <- list(name = rows$name[[i]], description = rows$description[[i]])
        if (nzchar(rows$argument[[i]])) {
            cmd$input <- list(hint = rows$argument[[i]])
        }
        cmd
    }))
}

.acp_help <- function() {
    tbl <- corteza_commands()
    rows <- tbl[tbl$acp & !nzchar(tbl$same_as),, drop = FALSE]
    usage <- trimws(paste0("/", rows$name, " ", rows$argument))
    c("Commands:", sprintf("  %-18s %s", usage, rows$description),
        "To start over, or to work in another directory, start a new session.")
}

# What the terminal's commands read, for an ACP session: the session,
# its config, directory and record, and a palette with no colors.
.acp_ctx <- function(session) {
    ctx <- new.env(parent = emptyenv())
    ctx$session <- session
    ctx$config <- session$config
    ctx$cwd <- session$cwd
    palette <- ansi_colors()
    palette[] <- ""
    ctx$palette <- palette
    ctx$disk_session <- list(session = session$disk_session,
                             sessionId = session$sessionId)
    ctx
}

# Run the command a prompt names; its printed lines, as one text.
.acp_command <- function(st, session, text) {
    parts <- strsplit(trimws(text), "[[:space:]]+")[[1L]]
    cmd <- tolower(parts[1L])
    ctx <- .acp_ctx(session)
    here <- getwd()
    on.exit(setwd(here), add = TRUE)
    setwd(session$cwd)
    out <- utils::capture.output({
        if (cmd %in% c("/clear", "/reset", "/new", "/cd")) {
            cat(paste("An ACP client starts a new session for that: to begin",
                      "again, or to work in another directory."), "\n")
        } else if (cmd == "/help") {
            cat(.acp_help(), sep = "\n")
        } else if (cmd == "/trust") {
            # The session's own trust was fixed when it started; a new
            # answer applies to the sessions started after it.
            cat(trust_command(session$cwd, parts[-1L],
                              when = "a new session in this directory"),
                sep = "\n")
        } else if (cmd %in% c("/status", "/context")) {
            .repl_status(ctx)
        } else if (cmd == "/tools") {
            cat(chat_format_tools_list(session), "\n")
        } else if (cmd == "/tasks") {
            .repl_cmd_tasks(ctx, parts)
            session$disk_session <- ctx$disk_session$session
        } else if (cmd %in% c("/spent", "/cost")) {
            cat(format_spend(session, palette = ctx$palette), "\n", sep = "")
        } else if (cmd == "/compact") {
            # The summary is a model's: a cancel stops it before the
            # request, or leaves its answer uninstalled
            # (maybe_compact_turn_session()).
            if (length(session$history %||% list()) < 2L) {
                cat("Nothing to compact.\n")
            } else if (isTRUE(.compact_now(session, session$config))) {
                cat("Compacted.\n")
            } else if (isTRUE(st$live$cancelled) || isTRUE(st$ch$closed)) {
                cat("Cancelled: the conversation is as it was.\n")
            } else {
                cat("Nothing safe to compact.\n")
            }
        }
    }, type = "output")
    paste(out, collapse = "\n")
}
