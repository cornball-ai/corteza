library(tinytest)

# The commands each surface takes (R/commands.R). The table says which
# names each surface accepts; here each surface's own matching, and both
# help texts, are held to it. A command added to one and not the others
# fails here.

ns <- asNamespace("corteza")
tbl <- ns$corteza_commands()

# ---- The table ---------------------------------------------------------------
expect_identical(names(tbl), c("name", "same_as", "argument", "description",
                               "terminal", "room", "remote", "talker", "acp"))
# What a command acts on when talker mode is enabled. Each row states its target;
# test_talker_commands.R holds the room's commands to it, and
# test_repl_talker.R the terminal's.
expect_true(all(tbl$talker %in% c("", "talker", "doer", "both")))
said <- tbl$room | tbl$terminal
expect_true(all(nzchar(tbl$talker[said])),
            info = paste(tbl$name[said & !nzchar(tbl$talker)], collapse = " "))
expect_false(anyDuplicated(tbl$name) > 0L)
expect_true(all(grepl("^[a-z][a-z-]*$", tbl$name)))
expect_true(all(nzchar(tbl$description)))
# A name another command goes by names a command that goes by its own,
# and is taken nowhere that command is not.
alias <- tbl[nzchar(tbl$same_as), ]
main <- tbl[match(alias$same_as, tbl$name), ]
expect_true(all(main$same_as == ""))
for (s in c("terminal", "room", "remote", "acp")) {
    expect_true(all(!alias[[s]] | main[[s]]), info = s)
}
# Each row is taken somewhere.
expect_true(all(tbl$terminal | tbl$room | tbl$remote | tbl$acp))

# ---- What a surface matches --------------------------------------------------
# The string literals a function compares with (==, %in%, identical(),
# startsWith()), as command names: "/auto status" is "auto", and a bare
# "/" or "! " is none.
compared <- function(f) {
    found <- character()
    strings <- function(a) {
        if (is.character(a)) {
            return(a)
        }
        if (is.call(a) && identical(a[[1L]], as.name("c"))) {
            return(unlist(Filter(is.character, as.list(a)[-1L])))
        }
        character()
    }
    walk <- function(e) {
        if (!is.call(e)) {
            return(invisible(NULL))
        }
        args <- as.list(e)[-1L]
        op <- e[[1L]]
        compares <- is.name(op) &&
            as.character(op) %in% c("==", "%in%", "identical", "startsWith")
        for (i in seq_along(args)) {
            if (identical(args[[i]], quote(expr = ))) {
                next
            }
            if (compares) {
                found <<- c(found, strings(args[[i]]))
            }
            walk(args[[i]])
        }
    }
    walk(body(f))
    found <- found[startsWith(found, "/")]
    found <- sub("^/+", "", sub(" .*$", "", found))
    sort(unique(found[nzchar(found)]))
}

# The terminal and chat(): one dispatch.
expect_identical(compared(ns$run_repl_loop), sort(tbl$name[tbl$terminal]))

# A room controlling a terminal session.
expect_identical(sort(unique(c(compared(ns$.remote_input),
                               compared(ns$.remote_bot_route)))),
                 sort(tbl$name[tbl$remote]))

# An ACP session: the names the table gives it, and the ones it answers
# with "start a new session". Its help and the commands it offers its
# client come from the table, so they name the same commands.
expect_identical(compared(ns$.acp_command),
                 sort(c(tbl$name[tbl$acp], ns$.ACP_NOT_HERE)))
expect_true(all(ns$.ACP_NOT_HERE %in% tbl$name))
expect_false(any(tbl$acp[tbl$name %in% ns$.ACP_NOT_HERE]))
offered <- vapply(ns$.acp_commands_update()$availableCommands,
                  function(cmd) cmd$name, "")
expect_identical(offered, tbl$name[tbl$acp & !nzchar(tbl$same_as)])
for (name in c(tbl$name[tbl$acp], ns$.ACP_NOT_HERE)) {
    expect_identical(ns$.acp_command_name(paste0("/", name, " x")), name,
                     info = name)
}
expect_null(ns$.acp_command_name("/home/you/notes.md"))
expect_null(ns$.acp_command_name("/model x"))

# A bot's room matches with a parser for each command. Each name the
# table gives a room is one its parser takes, and no other is.
takes <- list(
    clear = function(b) ns$bot_is_clear_command(b),
    reset = function(b) ns$bot_is_clear_command(b),
    new = function(b) ns$bot_is_clear_command(b),
    status = function(b) ns$bot_is_status_command(b),
    trust = function(b) !is.null(ns$bot_parse_trust_command(b)),
    model = function(b) !is.null(ns$bot_parse_model_command(b)),
    sidebar = function(b) !is.null(tryCatch(ns$bot_parse_sidebar(b),
                                            error = function(e) NULL)),
    `approve-all` = function(b) !is.null(ns$bot_parse_approval_mode(b)),
    job = function(b) {
        p <- ns$bot_parse_job_resume(b)
        !is.null(p) && is.null(p$error)
    },
    effort = function(b) !is.null(ns$.effort_parse_command(b)),
    `context-edit` = function(b) !is.null(ns$.context_edit_parse_command(b)),
    cd = function(b) !is.null(ns$bot_parse_cd_command(b)),
    ls = function(b) !is.null(ns$bot_parse_ls_command(b)))
expect_identical(sort(names(takes)), sort(tbl$name[tbl$room]))
# The list above is by hand, so it is held to the loop: each command
# parser bot_poll() calls is one a row of it calls, and none goes
# unlisted. It missed /effort and /context-edit until 0.8.0.28.
calls_in <- function(f) {
    found <- character()
    walk <- function(e) {
        if (!is.call(e)) {
            return(invisible(NULL))
        }
        if (is.name(e[[1L]])) {
            found <<- c(found, as.character(e[[1L]]))
        } else {
            walk(e[[1L]])
        }
        for (a in as.list(e)[-1L]) {
            if (!identical(a, quote(expr = ))) {
                walk(a)
            }
        }
    }
    walk(body(f))
    unique(found)
}
parser_name <- "^bot_is_.*_command$|^bot_parse_|_parse_command$"
in_loop <- grep(parser_name, calls_in(ns$bot_poll), value = TRUE)
in_takes <- unique(unlist(lapply(takes, function(f) {
    grep(parser_name, sub("^ns\\$", "", all.names(body(f))), value = TRUE)
})))
expect_true(length(in_loop) >= 9L, info = paste(in_loop, collapse = " "))
expect_identical(sort(in_loop), sort(in_takes))
example <- function(name) {
    if (identical(name, "job")) {
        return("/job resume 20261119T000000-0000abcd")
    }
    paste0("/", name)
}
for (name in names(takes)) {
    expect_true(isTRUE(takes[[name]](example(name))), info = name)
}
# A room's parsers take none of the terminal's other commands.
others <- setdiff(tbl$name[tbl$terminal], tbl$name[tbl$room])
for (name in others) {
    taken <- vapply(takes, function(f) isTRUE(f(paste0("/", name))), NA)
    expect_false(any(taken), info = name)
}

# ---- The help texts ----------------------------------------------------------
# Each name the terminal takes is in /help, at the terminal and in chat().
cli_help <- local({
    env <- new.env()
    script <- system.file("bin", "corteza", package = "corteza")
    for (e in parse(script, keep.source = FALSE)) {
        if (is.call(e) && identical(e[[1L]], as.name("<-")) &&
            identical(e[[2L]], as.name("cli_help_text"))) {
            eval(e, env)
        }
    }
    env$cli_help_text()
})
names_in <- function(text) {
    vapply(tbl$name[tbl$terminal], function(n) {
        grepl(paste0("(^|[^a-z-])/", n, "([^a-z-]|$)"), text)
    }, NA)
}
help_chat <- names_in(ns$chat_help_text())
help_cli <- names_in(cli_help)
expect_identical(names(help_chat)[!help_chat], character(),
                 info = "a command chat()'s /help leaves out")
expect_identical(names(help_cli)[!help_cli], character(),
                 info = "a command the terminal's /help leaves out")
