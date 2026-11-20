# Where a room works (WP12, WP22a).
#
# A room works in the directory an operator bound it to with /cd, which
# lies inside one of the bot's project roots; with no binding, in the
# bot's `cwd`; with neither, or after `/cd none`, in a scratch folder
# where it reads and talks only (WP10). corteza creates none of them.
#
# A room's topic names no directory. Anyone with the power level may
# change a topic, and up to 0.8.0 the directory it named became, for
# the supervisor, the checkout where writes needed nobody's approval. A
# room whose topic still starts with a path is told once that the path
# is no longer read (bot_topic_notice()).
#
# The binding is the room's, not a thread's. It is kept in the room's
# state file (R/room-conversation.R) as `binding`, and outlives a
# restart. /cd ends every conversation the room holds, the main
# timeline's and each thread's, so the next one starts in the new
# directory with a new prompt, tools and doer, and a thread first seen
# after it starts there too (`moved`, read by bot_conversation_open()).

# ---- Project roots ---------------------------------------------------------

# Check a `project_roots` argument. NULL, or none, passes as NULL.
# Anything else names directories that exist, and comes back as absolute
# paths with links resolved, which is how a room's directory is compared
# with them.
bot_check_roots <- function(roots) {
    roots <- unlist(roots, use.names = FALSE)
    if (!length(roots)) {
        return(NULL)
    }
    if (!is.character(roots) || anyNA(roots) || !all(nzchar(roots))) {
        stop("`project_roots` must be directory paths, or NULL.", call. = FALSE)
    }
    missing <- roots[!dir.exists(path.expand(roots))]
    if (length(missing)) {
        stop("`project_roots` must name existing directories. corteza does ",
             "not create them: ", paste(missing, collapse = ", "),
             call. = FALSE)
    }
    unique(normalizePath(path.expand(roots), winslash = "/", mustWork = TRUE))
}

# The roots a run was given (bot_run_init(), bot_poll()), kept beside its
# `cwd` (bot_run_cwd()) for the same reason.
bot_run_roots <- function() {
    .bot_run$roots
}

bot_run_roots_set <- function(roots) {
    .bot_run$roots <- roots
    invisible(roots)
}

# The roots in force: the run's, else the saved ones, resolved. A saved
# root that has gone missing is reported once and left out.
bot_project_roots <- function(cfg) {
    roots <- as.character(unlist(bot_run_roots() %||% cfg$project_roots,
                                 use.names = FALSE))
    roots <- roots[!is.na(roots) & nzchar(roots)]
    if (!length(roots)) {
        return(character())
    }
    there <- dir.exists(path.expand(roots))
    gone <- roots[!there]
    if (length(gone) && !identical(.bot_run$roots_missing, gone)) {
        .bot_run$roots_missing <- gone
        message("matrix: project root(s) ", paste(gone, collapse = ", "),
                " do not exist and are left out")
    }
    unique(normalizePath(path.expand(roots[there]), winslash = "/",
                         mustWork = TRUE))
}

# `path` resolved, when it is a directory inside one of `roots` (as
# bot_project_roots() gives them), else NULL. Links are resolved on both
# sides: a link inside a root that leads out of it is out, and a root
# that is itself a link holds what lies under its target.
bot_path_in_roots <- function(path, roots) {
    if (!length(roots) || !dir.exists(path.expand(path))) {
        return(NULL)
    }
    real <- normalizePath(path.expand(path), winslash = "/", mustWork = TRUE)
    inside <- vapply(roots, function(root) {
        identical(real, root) ||
        startsWith(real, paste0(sub("/+$", "", root), "/"))
    }, logical(1))
    if (any(inside)) {
        return(real)
    }
    NULL
}

# Where a name given to /cd or /ls points: an absolute or `~` path as it
# is, a bare name under whichever root holds it. list(path) or
# list(refusal).
bot_root_path <- function(arg, roots) {
    if (grepl("^(~|/)", arg) || grepl("^[A-Za-z]:[/\\\\]", arg)) {
        return(list(path = path.expand(arg)))
    }
    under <- file.path(roots, arg)
    under <- under[dir.exists(under)]
    if (length(under) == 1L) {
        return(list(path = under))
    }
    if (!length(under)) {
        return(list(refusal = sprintf(
                                      "No directory %s under a project root (%s).", arg,
                                      paste(roots, collapse = ", "))))
    }
    list(refusal = sprintf("%s is under more than one project root: %s. Give the whole path.",
                           arg, paste(under, collapse = ", ")))
}

# ---- A room's binding --------------------------------------------------------

# list(state = "bound", dir = ...), list(state = "none"),
# list(state = "default"), or list(state = "unknown").
#
# Only a room with no binding on record takes the default. A binding
# that is there and is none of the others (the marker
# bot_room_state_read() leaves for a file it could not read, or one
# malformed) is unknown, and holds the room to no directory until an
# operator runs /cd: what was lost may have been `/cd none`.
bot_room_binding <- function(cfg, room_id) {
    b <- bot_room_state_read(cfg, room_id)$binding
    if (is.null(b)) {
        return(list(state = "default"))
    }
    if (is.list(b) && identical(b$state, "none")) {
        return(list(state = "none"))
    }
    dir <- if (is.list(b)) b$dir
    if (is.list(b) && identical(b$state, "bound") && is.character(dir) &&
        length(dir) == 1L && !is.na(dir) && nzchar(dir)) {
        return(list(state = "bound", dir = dir))
    }
    list(state = "unknown")
}

# Where a room works: its binding's directory while that exists and lies
# inside a root; the scratch folder for a room bound to none, bound to a
# directory that is gone or outside every root now, or whose binding is
# unknown; else the bot's `cwd`, else the scratch folder. Never a
# directory that does not exist, and none created outside the session's
# temp directory.
bot_room_cwd <- function(cfg, room_id = NULL) {
    if (is.null(room_id)) {
        return(bot_default_cwd(cfg))
    }
    b <- bot_room_binding(cfg, room_id)
    if (b$state %in% c("none", "unknown")) {
        return(bot_scratch_cwd())
    }
    if (identical(b$state, "bound")) {
        dir <- bot_path_in_roots(b$dir, bot_project_roots(cfg))
        if (!is.null(dir)) {
            return(dir)
        }
        message(sprintf(paste("matrix: room %s is bound to %s, which is gone",
                              "or outside every project root; it reads and",
                              "talks only until an operator runs /cd there"),
                        room_id, b$dir))
        return(bot_scratch_cwd())
    }
    bot_default_cwd(cfg)
}

# A room the host files a cleared conversation into (the on_clear hook)
# takes the binding of the room it came from.
bot_room_binding_copy <- function(cfg, from, to) {
    binding <- bot_room_state_read(cfg, from)$binding
    if (is.null(binding) || !is.character(to) || length(to) != 1L ||
        is.na(to) || !nzchar(to) || identical(from, to)) {
        return(invisible(FALSE))
    }
    state <- bot_room_state_read(cfg, to)
    state$binding <- binding
    invisible(isTRUE(tryCatch({
        bot_room_state_write(cfg, to, state)
        TRUE
    }, error = function(e) {
        message("bot: could not give room ", to, " the directory of ", from,
                ": ", conditionMessage(e))
        FALSE
    })))
}

# ---- The topic -----------------------------------------------------------------

# The directory a topic starts with, as "<path> | <description>" named
# one up to 0.8.0, or NULL. Read only to tell the room that it is no
# longer used.
bot_topic_path <- function(topic) {
    if (!is.character(topic) || length(topic) != 1L || is.na(topic)) {
        return(NULL)
    }
    parts <- strsplit(trimws(topic), "\\s*\\|\\s*", perl = TRUE)[[1L]]
    if (length(parts) >= 2L && bot_path_like(parts[1L])) {
        return(parts[1L])
    }
    NULL
}

# What a room whose topic starts with a directory is told, once: the
# topic does not set where the room works, and an operator can. NULL
# when there is nothing to say. bot_topic_told() records in the room's
# state that it was delivered, so a restart does not repeat it.
bot_topic_notice <- function(cfg, session) {
    path <- session$topic_path
    room <- session$room_id
    if (is.null(path) || is.null(room)) {
        return(NULL)
    }
    if (identical(bot_room_state_read(cfg, room)$topic_told, path)) {
        return(NULL)
    }
    shown <- .sanitize_inline(path, max_chars = 200L)
    where <- if (bot_is_scratch(session$cwd)) {
        "This room has no directory now, so it reads and talks only."
    } else {
        sprintf("This room works in %s.", session$cwd)
    }
    paste(sprintf(paste("This room's topic starts with a directory, %s. From",
                        "corteza 0.8.1 a topic does not set where a room works."),
                  shown),
          where,
          sprintf(paste("An operator of this bot can bind the room there with",
                        "`/cd %s`, inside one of the bot's project roots."),
                  shown))
}

bot_topic_told <- function(cfg, session) {
    state <- bot_room_state_read(cfg, session$room_id)
    state$topic_told <- session$topic_path
    tryCatch(bot_room_state_write(cfg, session$room_id, state),
             error = function(e) NULL)
    invisible(TRUE)
}

# ---- /cd and /ls -----------------------------------------------------------------

# `/cd` or `/ls` and what follows it, or NULL. The slash is required:
# "cd" and "ls" alone are words a person may write.
bot_parse_cd_command <- function(body) {
    bot_parse_room_word(body, "cd")
}

bot_parse_ls_command <- function(body) {
    bot_parse_room_word(body, "ls")
}

bot_parse_room_word <- function(body, word) {
    cmd <- trimws(bot_command_text(body))
    head <- paste0("^/", word)
    if (!grepl(paste0(head, "(\\s|$)"), cmd, perl = TRUE, ignore.case = TRUE)) {
        return(NULL)
    }
    list(arg = trimws(sub(head, "", cmd, ignore.case = TRUE)))
}

# What /cd alone says: the room's state, and the 3 forms.
bot_cd_state_text <- function(cfg, room_id) {
    b <- bot_room_binding(cfg, room_id)
    roots <- bot_project_roots(cfg)
    default <- bot_configured_cwd(cfg)
    now <- switch(b$state,
                  bound = sprintf("This room is bound to %s.", b$dir),
                  none = "This room is bound to no directory: it reads and talks only.",
                  unknown = paste("This room's binding is unknown: its state file",
                                  "could not be read and was set aside. It reads",
                                  "and talks only until an operator runs /cd."),
        if (is.null(default)) {
            "This room takes the bot's default, which is no directory: it reads and talks only."
        } else {
            sprintf("This room takes the bot's default, %s.", default)
        })
    paste(c(now, "",
            "- `/cd <dir>`: work in <dir>, inside a project root.",
            "- `/cd none`: no directory; read and talk only.",
            "- `/cd default`: the bot's `cwd`, or none where it has none.",
            "",
            if (length(roots)) {
                paste("Project roots:", paste(roots, collapse = ", "))
            } else {
                "This bot has no project roots (`project_roots`), so `/cd <dir>` is refused."
            },
            paste("Only an operator of this bot can move the room. A move ends",
                  "every conversation here, as /clear does.")),
          collapse = "\n")
}

# The binding `/cd <arg>` asks for, or a refusal: a string.
bot_cd_binding <- function(cfg, arg) {
    word <- tolower(arg)
    if (identical(word, "none")) {
        return(list(state = "none"))
    }
    if (identical(word, "default")) {
        return(list(state = "default"))
    }
    roots <- bot_project_roots(cfg)
    if (!length(roots)) {
        return(paste("Not moved: this bot has no project roots. Its operator",
                     "lists them in `project_roots` (bot_configure(),",
                     "bot_run()), and /cd takes a directory inside one."))
    }
    where <- bot_root_path(arg, roots)
    if (!is.null(where$refusal)) {
        return(paste("Not moved:", where$refusal))
    }
    if (!dir.exists(where$path)) {
        return(sprintf("Not moved: %s is no existing directory. corteza creates none.",
                       arg))
    }
    dir <- bot_path_in_roots(where$path, roots)
    if (is.null(dir)) {
        return(sprintf(paste("Not moved: %s is outside every project root (%s),",
                             "links resolved."), arg, paste(roots, collapse = ", ")))
    }
    list(state = "bound", dir = dir)
}

# The key's room, and the thread of a session key or job origin.
bot_key_room <- function(key) {
    if (!is.character(key) || length(key) != 1L || is.na(key)) {
        return(NA_character_)
    }
    strsplit(key, "\r", fixed = TRUE)[[1L]][[1L]]
}

# The room's sessions in the registry: its main timeline and its threads.
bot_room_session_keys <- function(sessions, room_id) {
    keys <- ls(envir = sessions)
    keep <- vapply(keys, function(k) {
        identical(bot_key_room(k), room_id) &&
        is.environment(get(k, envir = sessions))
    }, logical(1))
    keys[keep]
}

# The open jobs (queued or running) that any of the room's conversations
# asked for, the main timeline's and each thread's.
bot_room_open_jobs <- function(cfg, room_id) {
    jobs <- job_list(status = JOB_STATUSES_OPEN, owner = bot_job_owner(cfg))
    Filter(function(j) {
        identical(j$origin$room, room_id) ||
        identical(bot_key_room(j$origin$session_key %||% NA_character_),
                  room_id)
    }, jobs)
}

# Write the room's new binding and a new conversation for each of its
# timelines in one write: the main timeline, each thread on record and
# each one in `keys`, with the /cd as their start, and the start a
# thread first seen later takes. The state as written, or NULL when it
# could not be written; then nothing changed.
bot_room_move <- function(cfg, room_id, binding, keys, start) {
    state <- bot_room_state_read(cfg, room_id)
    if (is.list(state$threads)) {
        threads <- state$threads
    } else {
        threads <- list()
    }
    for (key in keys) {
        thread <- bot_key_thread(key)
        if (!is.null(thread)) {
            threads[[thread]] <- list()
        }
    }
    for (thread in names(threads)) {
        threads[[thread]] <- bot_conversation_new(start)
    }
    state$main <- bot_conversation_new(start)
    state$threads <- if (length(threads)) threads
    state$moved <- list(id = start$id, ts = start$ts)
    if (identical(binding$state, "default")) {
        state$binding <- NULL
    } else {
        state$binding <- binding
    }
    ok <- tryCatch({
        bot_room_state_write(cfg, room_id, state)
        TRUE
    }, error = function(e) {
        message("bot: could not save the move of room ", room_id, ": ",
                conditionMessage(e))
        FALSE
    })
    if (!ok) {
        return(NULL)
    }
    state
}

# The record the move gave `key` (bot_room_move()).
bot_room_moved_record <- function(state, key) {
    thread <- bot_key_thread(key)
    if (is.null(thread)) {
        return(state$main)
    }
    state$threads[[thread]]
}

# /cd in a room. Alone, it says where the room works. From an operator,
# with a directory, `none` or `default`, it moves the room: each
# conversation the room holds goes to the host as /clear sends it, then
# the binding and a new conversation for each timeline are saved in one
# write. The caller answers and replaces each session in `keys`
# (bot_reset_session()), which then start where the room now works.
# Refused, with nothing changed, while any of the room's conversations
# has an open job, and when an archive or the state cannot be written.
# list(ack, moved, keys, state).
bot_cd_command <- function(m, sender, sessions, cfg, chat = NULL,
                           hooks = list()) {
    arg <- bot_parse_cd_command(m$body)$arg %||% ""
    room <- m$channel
    refused <- function(text) list(ack = text, moved = FALSE)
    if (!nzchar(arg)) {
        return(refused(bot_cd_state_text(cfg, room)))
    }
    if (!sender %in% bot_operators(cfg)) {
        return(refused("Only an operator of this bot can move a room with /cd."))
    }
    binding <- bot_cd_binding(cfg, arg)
    if (is.character(binding)) {
        return(refused(binding))
    }
    open <- bot_room_open_jobs(cfg, room)
    if (length(open)) {
        where <- vapply(open, function(j) {
            thread <- bot_key_thread(j$origin$session_key %||% "")
            sprintf("%s (%s, %s)", j$id, j$status,
                if (is.null(thread)) {
                    "the main timeline"
                } else {
                    paste("thread", thread)
                })
        }, character(1))
        return(refused(paste("Not moved: these jobs of this room are open, and",
                             "a move would leave them in the old directory:",
                             paste(where, collapse = "; "),
                             ". Wait for them, or /cancel them, then /cd again.")))
    }
    keys <- bot_room_session_keys(sessions, room)
    for (key in keys) {
        filing <- tryCatch(bot_archive_session(get(key, envir = sessions),
                key, chat, hooks$on_archive, cfg, reason = "clear"),
                           error = function(e) {
            message("bot_poll: archive failed: ", conditionMessage(e))
            list(durable = FALSE)
        })
        if (!isTRUE(filing$durable)) {
            thread <- bot_key_thread(key)
            return(refused(sprintf(paste("Not moved: the conversation in %s could",
                            "not be archived or saved for a retry, so",
                            "it stays, and so does the room. Try /cd",
                            "again."),
                        if (is.null(thread)) {
                            "the main timeline"
                        } else {
                            paste("thread", thread)
                        })))
        }
    }
    state <- bot_room_move(cfg, room, binding, keys, m)
    if (is.null(state)) {
        return(refused(paste("Not moved: the room's state could not be saved,",
                             "so a restart would not know. Nothing changed.",
                             "Try /cd again.")))
    }
    cwd <- bot_room_cwd(cfg, room)
    where <- if (bot_is_scratch(cwd)) {
        "no directory: it reads and talks only"
    } else {
        cwd
    }
    list(ack = sprintf("Moved. This room now works in %s. Every conversation here starts again.",
                       where),
         moved = TRUE, keys = keys, state = state)
}

# /ls in a room, for an operator: the roots, or the directories directly
# under one inside a root, names only, at most 50.
bot_ls_command <- function(cfg, sender, arg = "") {
    if (!sender %in% bot_operators(cfg)) {
        return("Only an operator of this bot can list directories with /ls.")
    }
    roots <- bot_project_roots(cfg)
    if (!length(roots)) {
        return("This bot has no project roots (`project_roots`), so there is nothing to list.")
    }
    if (!nzchar(arg)) {
        return(paste(c("Project roots:", paste0("- ", roots)), collapse = "\n"))
    }
    where <- bot_root_path(arg, roots)
    if (!is.null(where$refusal)) {
        return(where$refusal)
    }
    dir <- bot_path_in_roots(where$path, roots)
    if (is.null(dir)) {
        return(sprintf("%s is no directory inside a project root (%s).", arg,
                       paste(roots, collapse = ", ")))
    }
    subs <- sort(list.dirs(dir, full.names = FALSE, recursive = FALSE))
    subs <- subs[!startsWith(subs, ".")]
    if (!length(subs)) {
        return(sprintf("%s has no directories in it.", dir))
    }
    shown <- utils::head(subs, 50L)
    more <- length(subs) - length(shown)
    paste(c(sprintf("%s:", dir), paste0("- ", shown),
            if (more > 0L) sprintf("and %d more", more)),
          collapse = "\n")
}
