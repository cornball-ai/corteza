# Session management for corteza
# Full compatibility with openclaw session format
#
# Storage format (matches openclaw / pi-coding-agent):
#   <data_dir>/agents/main/sessions/sessions.json  - Session metadata store
#   <data_dir>/agents/main/sessions/{id}.jsonl     - Transcript per session
# where <data_dir> is tools::R_user_dir("corteza", "data").
#
# JSONL transcript format:
#   Line 1: {"type":"session","version":2,"id":"...","timestamp":"...","cwd":"..."}
#   Lines 2+: {"role":"user|assistant","content":[{"type":"text","text":"..."}],...}
#
# A store entry also says where its session was started (`cwd`), where
# it runs (`surface`) and what it is about (`title`, the first prompt).
# Lists and resumes go by `cwd`: a session belongs to its directory.
# session_new() writes `cwd` and `surface`, session_title_once() writes
# `title`, and session_save() leaves all 3 alone. A Matrix room's
# session has its entry under agent "matrix" (bot_session_record()).
# vignette("configuration") states the fields as a contract for other
# programs: add fields, never rename one.

# Constants
SESSION_VERSION <- 2L
DEFAULT_AGENT_ID <- "main"

#' Get the sessions directory (openclaw-compatible)
#' @param agent_id Agent ID (default: "main")
#' @return Path to sessions directory
#' @noRd
sessions_dir <- function(agent_id = DEFAULT_AGENT_ID) {
    corteza_data_path("agents", agent_id, "sessions")
}

#' Get path to sessions store (metadata for all sessions)
#' @param agent_id Agent ID
#' @return Path to sessions.json
#' @noRd
sessions_store_path <- function(agent_id = DEFAULT_AGENT_ID) {
    file.path(sessions_dir(agent_id), "sessions.json")
}

#' Adjectives + surnames for human-readable session names, in the
#' docker-container style ("boring_wozniak", "elated_turing").
#' Adjectives lifted from Docker's own moby/moby names-generator
#' wordlist; surnames are scientists / mathematicians /
#' computer-scientists who are uncontroversial enough to use as
#' identifiers.
#' @noRd
.SESSION_ADJECTIVES <- c("admiring", "adoring", "affectionate", "agitated",
                         "amazing", "angry", "awesome", "blissful", "bold",
                         "boring", "brave", "busy", "charming", "clever",
                         "cool", "compassionate", "competent",
                         "condescending", "confident", "cranky", "crazy",
                         "dazzling", "determined", "distracted", "dreamy",
                         "eager", "ecstatic", "elastic", "elated",
                         "elegant", "eloquent", "epic", "exciting",
                         "fervent", "festive", "flamboyant", "focused",
                         "friendly", "frosty", "funny", "gallant",
                         "gifted", "goofy", "gracious", "great", "happy",
                         "hardcore", "heuristic", "hopeful", "hungry",
                         "infallible", "inspiring", "intelligent",
                         "interesting", "jolly", "jovial", "keen", "kind",
                         "laughing", "loving", "lucid", "magical",
                         "mystifying", "modest", "musing", "naughty",
                         "nervous", "nice", "nifty", "nostalgic",
                         "objective", "optimistic", "peaceful", "pedantic",
                         "pensive", "practical", "priceless", "quirky",
                         "quizzical", "recursing", "relaxed", "reverent",
                         "romantic", "serene", "sharp", "silly", "sleepy",
                         "stoic", "strange", "stupefied", "suspicious",
                         "sweet", "tender", "thirsty", "trusting",
                         "unruffled", "upbeat", "vibrant", "vigilant",
                         "vigorous", "wizardly", "wonderful", "youthful",
                         "zealous")

.SESSION_SURNAMES <- c("turing", "wozniak", "knuth", "ritchie",
                       "kernighan", "torvalds", "lovelace", "hopper",
                       "shannon", "dijkstra", "tesla", "edison", "newton",
                       "darwin", "einstein", "curie", "feynman", "hawking",
                       "galileo", "kepler", "neumann", "lamarr", "boole",
                       "babbage", "godel", "church", "fermat", "euler",
                       "gauss", "ramanujan", "noether", "hilbert",
                       "lagrange", "laplace", "poincare", "fourier",
                       "leibniz", "pascal", "fermi", "bohr", "planck",
                       "schrodinger", "heisenberg", "mendel", "watson",
                       "crick", "franklin", "lovelace", "hamilton", "wing",
                       "liskov", "perlis", "stallman", "stroustrup",
                       "matsumoto", "vanrossum", "hejlsberg", "engelbart",
                       "mccarthy", "minsky", "papert", "djikstra",
                       "iverson")

#' Generate a new session ID in docker-container style
#' (`adjective_surname`, e.g. `boring_wozniak`). Replaces an older
#' UUID v4 generator. The names are used as both the in-memory
#' sessionId and the on-disk filename root, so they stay short
#' and filesystem-safe.
#'
#' Collisions across the lifetime of a project are possible (the
#' wordlist is ~100 x ~60 = ~6000 names). If a collision is
#' detected against an existing session in the **target agent's**
#' store, retry up to 10 times before falling back to appending a
#' 4-char hex suffix.
#' @param agent_id Agent whose store to check for collisions
#'   (codex 2026-05-20: previously hardcoded to the default agent,
#'   so a non-main agent could reuse an existing session's
#'   transcript silently).
#' @return Character string.
#' @noRd
session_id <- function(agent_id = DEFAULT_AGENT_ID) {
    for (attempt in seq_len(10L)) {
        nm <- paste0(sample(.SESSION_ADJECTIVES, 1L), "_",
                     sample(.SESSION_SURNAMES, 1L))
        if (!.session_name_exists(nm, agent_id = agent_id)) {
            return(nm)
        }
    }
    # Fallback: append a hex suffix to disambiguate.
    hex <- c(0:9, letters[1:6])
    paste0(sample(.SESSION_ADJECTIVES, 1L), "_",
           sample(.SESSION_SURNAMES, 1L), "_",
           paste0(sample(hex, 4L, replace = TRUE), collapse = ""))
}

#' Probe whether a session with this id already has a transcript or
#' store entry in the given agent. Checks both the on-disk
#' transcript path and the in-store metadata so a session living
#' under either path still counts as occupied.
#' @noRd
.session_name_exists <- function(nm, agent_id = DEFAULT_AGENT_ID) {
    transcript <- file.path(sessions_dir(agent_id), paste0(nm, ".jsonl"))
    if (file.exists(transcript)) {
        return(TRUE)
    }
    store <- tryCatch(store_load(agent_id), error = function(e) list())
    if (length(store) == 0L) {
        return(FALSE)
    }
    nm %in% vapply(store, function(s) s$sessionId %||% "", character(1))
}

#' Get path to session transcript file
#' @param id Session ID
#' @param agent_id Agent ID
#' @return Path to transcript JSONL file
#' @noRd
session_transcript_path <- function(id, agent_id = DEFAULT_AGENT_ID) {
    file.path(sessions_dir(agent_id), paste0(id, ".jsonl"))
}

# Store operations ----

#' Read a store file. NULL when it cannot be read as one.
#' @noRd
store_read <- function(path) {
    store <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                      error = function(e) NULL)
    if (!is.list(store)) {
        return(NULL)
    }
    store
}

#' Load all session metadata from store
#' @param agent_id Agent ID
#' @return Named list of session entries (keyed by session key)
#' @noRd
store_load <- function(agent_id = DEFAULT_AGENT_ID) {
    path <- sessions_store_path(agent_id)

    if (!file.exists(path)) {
        return(list())
    }

    store_read(path) %||% list()
}

#' The store as a writer needs it. A file that is there and cannot be
#' read is moved aside first: a save over it would replace every entry
#' with the one being written.
#' @noRd
store_load_for_write <- function(agent_id = DEFAULT_AGENT_ID) {
    path <- sessions_store_path(agent_id)
    if (!file.exists(path)) {
        return(list())
    }
    store <- store_read(path)
    if (is.null(store)) {
        aside <- paste0(path, ".unreadable")
        if (!isTRUE(file.rename(path, aside))) {
            stop("could not read session store ", path,
                 " and could not move it aside", call. = FALSE)
        }
        log_event("session_store_unreadable", level = "warn",
                  agent_id = agent_id)
        return(list())
    }
    store
}

#' Save session metadata store
#'
#' Written beside the store and renamed over it. Every session of an
#' agent shares this file, and a process that read it half written took
#' it for empty. Private like the transcripts: an entry holds the start
#' of a prompt.
#' @param store Named list of session entries
#' @param agent_id Agent ID
#' @return Invisible path to store file
#' @noRd
store_save <- function(store, agent_id = DEFAULT_AGENT_ID) {
    previous <- Sys.umask("0077")
    on.exit(Sys.umask(previous), add = TRUE)
    dir <- sessions_dir(agent_id)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, mode = "0700")
    }

    path <- sessions_store_path(agent_id)
    tmp <- tempfile("sessions.json.", tmpdir = dir)
    on.exit(unlink(tmp), add = TRUE)
    writeLines(jsonlite::toJSON(store, auto_unbox = TRUE, pretty = TRUE), tmp)
    if (!isTRUE(file.rename(tmp, path))) {
        stop("could not write session store ", path, call. = FALSE)
    }
    invisible(path)
}

#' Update a single session in the store
#' @param session_key Session key (identifier in store)
#' @param updates Named list of fields to update
#' @param agent_id Agent ID
#' @return Updated session entry
#' @noRd
store_update <- function(session_key, updates, agent_id = DEFAULT_AGENT_ID) {
    store <- store_load_for_write(agent_id)

    if (is.null(store[[session_key]])) {
        store[[session_key]] <- list(sessionId = updates$sessionId %||%
                                     session_id(agent_id = agent_id))
    }

    # Merge updates
    for (key in names(updates)) {
        store[[session_key]][[key]] <- updates[[key]]
    }

    store[[session_key]]$updatedAt <- as.numeric(Sys.time()) * 1000 # milliseconds

    store_save(store, agent_id)
    store[[session_key]]
}

# Session operations ----

#' A directory as the store records it and as 2 are compared: resolved
#' when it exists, so a link and its target are 1 directory, with
#' forward slashes on every system and no slash at the end.
#' @return The path, or NULL for no directory.
#' @noRd
session_cwd <- function(cwd) {
    if (!is.character(cwd) || !length(cwd) || is.na(cwd[[1L]]) ||
        !nzchar(cwd[[1L]])) {
        return(NULL)
    }
    path <- normalizePath(cwd[[1L]], winslash = "/", mustWork = FALSE)
    if (nchar(path) > 1L && !grepl("^[A-Za-z]:/$", path)) {
        path <- sub("/+$", "", path)
    }
    path
}

SESSION_TITLE_CHARS <- 100L

#' A conversation's title: its first prompt on 1 line, cut to 100
#' characters.
#' @return The title, or NULL when the prompt has no text.
#' @noRd
session_title <- function(prompt) {
    # A prompt that is not valid text in this locale has no title.
    text <- tryCatch({
        text <- paste(as.character(prompt), collapse = " ")
        text <- trimws(gsub("[[:space:][:cntrl:]]+", " ", text))
        substr(text, 1L, SESSION_TITLE_CHARS)
    }, error = function(e) "")
    if (is.na(text) || !nzchar(text)) {
        return(NULL)
    }
    text
}

#' Create a new session
#' @param provider LLM provider name
#' @param model Model name
#' @param cwd Working directory for session
#' @param session_key Session key (default: generates new)
#' @param agent_id Agent ID
#' @param surface Where the session runs: "cli" or "console". NULL
#'   records none.
#' @return Session list object
#' @noRd
session_new <- function(provider = "anthropic", model = NULL, cwd = getwd(),
                        session_key = NULL, agent_id = DEFAULT_AGENT_ID,
                        surface = NULL) {
    id <- session_id(agent_id = agent_id)
    now <- as.numeric(Sys.time()) * 1000
    session_key <- session_key %||% paste0("corteza:", id)

    session <- list(sessionId = id, sessionKey = session_key,
                    createdAt = now, updatedAt = now, provider = provider,
                    model = model, cwd = normalizePath(cwd, mustWork = FALSE),
                    inputTokens = 0L, outputTokens = 0L, totalTokens = 0L,
                    compactionCount = 0L, memoryFlushCompactionCount = 0L,
                    tasks = list(), messages = list())
    session$surface <- surface

    # Write session header to transcript
    transcript_write_header(id, cwd, agent_id)

    # Save to store. The directory is what lists and resumes a session
    # by where it was started; session_save() leaves it as written here.
    entry <- list(
                  sessionId = id,
                  sessionFile = session_transcript_path(id, agent_id),
                  createdAt = now,
                  modelProvider = provider,
                  model = model,
                  inputTokens = 0L,
                  outputTokens = 0L,
                  totalTokens = 0L,
                  compactionCount = 0L,
                  memoryFlushCompactionCount = 0L,
                  tasks = list()
    )
    entry$cwd <- session_cwd(cwd)
    entry$surface <- surface
    store_update(session_key, entry, agent_id)

    session
}

#' Save session metadata
#' @param session Session object
#' @param agent_id Agent ID
#' @return Invisible session key
#' @noRd
session_save <- function(session, agent_id = DEFAULT_AGENT_ID) {
    session_key <- session$sessionKey %||% paste0("corteza:", session$sessionId)

    store_update(session_key, list(
                                   sessionId = session$sessionId,
                                   sessionFile = session_transcript_path(session$sessionId, agent_id),
                                   modelProvider = session$provider,
                                   model = session$model,
                                   inputTokens = session$inputTokens %||% 0L,
                                   outputTokens = session$outputTokens %||% 0L,
                                   totalTokens = session$totalTokens %||% 0L,
                                   compactionCount = session$compactionCount %||% 0L,
                                   memoryFlushCompactionCount = session$memoryFlushCompactionCount %||% 0L,
                                   tasks = session$tasks %||% list()
        ), agent_id)

    invisible(session_key)
}

#' Load session from disk
#' @param session_key Session key
#' @param agent_id Agent ID
#' @param from_compaction If TRUE, only load messages after last compaction
#' @return Session object, or NULL if not found
#' @noRd
session_load <- function(session_key, agent_id = DEFAULT_AGENT_ID,
                         from_compaction = TRUE) {
    store <- store_load(agent_id)
    entry <- store[[session_key]]

    if (is.null(entry)) {
        return(NULL)
    }

    id <- entry$sessionId
    if (is.null(id)) {
        return(NULL)
    }

    # Build session object from store entry + transcript
    session <- list(
                    sessionId = id,
                    sessionKey = session_key,
                    createdAt = entry$createdAt,
                    updatedAt = entry$updatedAt,
                    provider = entry$modelProvider,
                    model = entry$model,
                    # From the transcript's header for a session made
                    # before the store recorded it.
                    cwd = entry$cwd %||% transcript_cwd(id, agent_id),
                    inputTokens = entry$inputTokens %||% 0L,
                    outputTokens = entry$outputTokens %||% 0L,
                    totalTokens = entry$totalTokens %||% 0L,
                    compactionCount = entry$compactionCount %||% 0L,
                    memoryFlushCompactionCount = entry$memoryFlushCompactionCount %||% 0L,
                    tasks = entry$tasks %||% list(),
                    messages = transcript_load(id, agent_id,
            from_compaction = from_compaction)
    )
    session$surface <- entry$surface
    # A session from before titles were recorded takes its own first
    # prompt, never the next one typed into it.
    session$title <- entry$title %||%
    session_title(session_first_prompt(session$messages))

    session
}

#' The text of the first user message among `messages`, or NULL.
#' @noRd
session_first_prompt <- function(messages) {
    for (m in messages) {
        if (!identical(m$role, "user")) {
            next
        }
        content <- m$content
        if (is.character(content)) {
            return(content)
        }
        if (is.list(content) && length(content) && is.list(content[[1L]]) &&
            is.character(content[[1L]]$text)) {
            return(content[[1L]]$text)
        }
        return(NULL)
    }
    NULL
}

#' Give a session its title from its first prompt, once.
#'
#' The store is written the first time and never again for this
#' session: session_save() leaves the title as it is.
#' @param session Session object (the on-disk one).
#' @param prompt What the person typed.
#' @param agent_id Agent ID
#' @return The session, with `title` set when the prompt gave one.
#' @noRd
session_title_once <- function(session, prompt, agent_id = DEFAULT_AGENT_ID) {
    if (!is.null(session$title)) {
        return(session)
    }
    title <- session_title(prompt)
    if (is.null(title)) {
        return(session)
    }
    session$title <- title
    key <- session$sessionKey %||% paste0("corteza:", session$sessionId)
    # A title is for lists. Failing to record one must not stop a turn.
    tryCatch(store_update(key,
                          list(sessionId = session$sessionId, title = title),
                          agent_id),
             error = function(e) NULL)
    session
}

#' TRUE for the key of a session a person started. Subagents share the
#' store under "agent:<id>:subagent:<uuid>" keys and mean nothing in a
#' list of conversations.
#' @noRd
session_key_listed <- function(key) {
    !startsWith(key, "agent:")
}

#' The first lines of a transcript, parsed. Lines that do not parse are
#' dropped.
#' @noRd
transcript_head <- function(id, agent_id = DEFAULT_AGENT_ID, n = 1L) {
    path <- session_transcript_path(id, agent_id)
    if (!file.exists(path)) {
        return(list())
    }
    lines <- tryCatch(readLines(path, n = n, warn = FALSE),
                      error = function(e) character())
    entries <- lapply(lines, function(line) {
        tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                 error = function(e) NULL)
    })
    Filter(is.list, entries)
}

#' The directory a session was started in, from its transcript's header.
#' @noRd
transcript_cwd <- function(id, agent_id = DEFAULT_AGENT_ID) {
    head <- transcript_head(id, agent_id, n = 1L)
    if (!length(head) || !identical(head[[1L]]$type, "session")) {
        return(NULL)
    }
    session_cwd(head[[1L]]$cwd)
}

# How far into a transcript the first prompt is looked for.
SESSION_TITLE_LINES <- 5L

#' A title from the first prompt in a transcript's first lines.
#' @noRd
transcript_title <- function(id, agent_id = DEFAULT_AGENT_ID) {
    head <- transcript_head(id, agent_id, n = SESSION_TITLE_LINES)
    session_title(session_first_prompt(head))
}

#' Record fields that entries lack, in 1 write.
#'
#' The store is read again just before, and a field another process
#' recorded meanwhile is left as that process wrote it.
#' @param fills Named list by session key; each element a named list of
#'   fields.
#' @param agent_id Agent ID
#' @return Invisible NULL.
#' @noRd
store_fill <- function(fills, agent_id = DEFAULT_AGENT_ID) {
    # A transcript that gave nothing leaves nothing to record.
    fills <- lapply(fills, function(f) f[!vapply(f, is.null, logical(1))])
    fills <- fills[lengths(fills) > 0L]
    if (!length(fills)) {
        return(invisible())
    }
    store <- store_load_for_write(agent_id)
    changed <- FALSE
    for (key in names(fills)) {
        if (is.null(store[[key]])) {
            next
        }
        for (field in names(fills[[key]])) {
            if (is.null(store[[key]][[field]])) {
                store[[key]][[field]] <- fills[[key]][[field]]
                changed <- TRUE
            }
        }
    }
    if (changed) {
        store_save(store, agent_id)
    }
    invisible()
}

#' Every session a person started, newest first, each with its key and
#' its directory.
#'
#' Sessions made before 0.8.1 have their directory on line 1 of the
#' transcript and nowhere else. It is read from there the first time
#' they are listed and recorded in the store.
#' @noRd
session_entries <- function(agent_id = DEFAULT_AGENT_ID) {
    store <- store_load(agent_id)
    keys <- names(store)[vapply(names(store), session_key_listed, logical(1))]
    keys <- keys[vapply(store[keys], function(e) {
        is.list(e) && is.character(e$sessionId) && length(e$sessionId) == 1L
    }, logical(1))]

    fills <- list()
    entries <- vector("list", length(keys))
    for (i in seq_along(keys)) {
        entry <- store[[keys[[i]]]]
        entry$sessionKey <- keys[[i]]
        if (is.null(entry$cwd)) {
            entry$cwd <- transcript_cwd(entry$sessionId, agent_id)
            fills[[keys[[i]]]] <- list(cwd = entry$cwd)
        }
        entries[[i]] <- entry
    }
    tryCatch(store_fill(fills, agent_id), error = function(e) NULL)

    # A time that is no number sorts last.
    updated <- vapply(entries, function(e) {
        suppressWarnings(as.numeric(e$updatedAt %||% 0)[[1L]])
    }, numeric(1))
    entries[order(updated, decreasing = TRUE)]
}

#' TRUE for each of `entries` started in `cwd`.
#' @noRd
session_in_dir <- function(entries, cwd) {
    here <- session_cwd(cwd)
    vapply(entries, function(e) {
        !is.null(here) && identical(session_cwd(e$cwd), here)
    }, logical(1))
}

#' List sessions
#' @param agent_id Agent ID
#' @param n Maximum number of sessions to return (most recent first)
#' @param cwd List the sessions started in this directory.
#' @param all TRUE lists the sessions of every directory.
#' @return List of session summaries. Its attribute "elsewhere" is the
#'   number of sessions in other directories (0 with `all`), and "cwd"
#'   is the directory listed (NULL with `all`).
#' @noRd
session_list <- function(agent_id = DEFAULT_AGENT_ID, n = 20, cwd = getwd(),
                         all = FALSE) {
    entries <- session_entries(agent_id)
    elsewhere <- 0L
    if (!isTRUE(all)) {
        here <- session_in_dir(entries, cwd)
        elsewhere <- sum(!here)
        entries <- entries[here]
    }
    entries <- head(entries, n)

    # Only what is shown is opened: a count of its messages, and a title
    # for a session from before titles were recorded.
    fills <- list()
    sessions <- vector("list", length(entries))
    for (i in seq_along(entries)) {
        entry <- entries[[i]]
        entry$messages <- transcript_count(entry$sessionId, agent_id)
        if (is.null(entry$title)) {
            entry$title <- transcript_title(entry$sessionId, agent_id)
            fills[[entry$sessionKey]] <- list(title = entry$title)
        }
        sessions[[i]] <- entry
    }
    tryCatch(store_fill(fills, agent_id), error = function(e) NULL)

    attr(sessions, "elsewhere") <- elsewhere
    if (!isTRUE(all)) {
        attr(sessions, "cwd") <- session_cwd(cwd)
    }
    sessions
}

#' Get the latest session
#' @param agent_id Agent ID
#' @param cwd The latest session started in this directory.
#' @param all TRUE takes the latest of every directory.
#' @return Session object, or NULL if no sessions exist
#' @noRd
session_latest <- function(agent_id = DEFAULT_AGENT_ID, cwd = getwd(),
                           all = FALSE) {
    sessions <- session_list(agent_id, n = 1, cwd = cwd, all = all)

    if (length(sessions) == 0) {
        return(NULL)
    }

    session_load(sessions[[1]]$sessionKey, agent_id)
}

#' One line for a person who asked for the sessions of a directory that
#' has none: how many there are elsewhere and what reaches them.
#' @param elsewhere Number of sessions in other directories.
#' @param how What reaches them, as the person would type it.
#' @return The line.
#' @noRd
session_none_here <- function(elsewhere, how) {
    if (elsewhere <= 0L) {
        return("No sessions found.")
    }
    sprintf("No session was started in this directory. %d %s elsewhere (%s).",
            elsewhere, if (elsewhere == 1L) "is" else "are", how)
}

#' The same line, counted for `cwd`.
#' @noRd
session_none_note <- function(cwd, how, agent_id = DEFAULT_AGENT_ID) {
    none <- session_list(agent_id, n = 0L, cwd = cwd)
    session_none_here(attr(none, "elsewhere"), how)
}

#' One line when a session is resumed in another directory than the one
#' it was started in: the paths in its history belong there.
#' @return The line, or NULL when the directories are the same or the
#'   session's is not known.
#' @noRd
session_elsewhere_note <- function(session, cwd) {
    started <- session_cwd(session$cwd)
    if (is.null(started) || identical(started, session_cwd(cwd))) {
        return(NULL)
    }
    sprintf("This session was started in %s.", session_dir_show(started))
}

# Transcript operations ----

#' Write session header to transcript file
#' @param id Session ID
#' @param cwd Working directory
#' @param agent_id Agent ID
#' @return Invisible path to transcript file
#' @noRd
transcript_write_header <- function(id, cwd, agent_id = DEFAULT_AGENT_ID,
                                    extra = NULL) {
    path <- session_transcript_path(id, agent_id)

    # Only write if file doesn't exist
    if (file.exists(path)) {
        return(invisible(path))
    }

    header <- list(
                   type = "session",
                   version = SESSION_VERSION,
                   id = id,
                   timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                   cwd = normalizePath(cwd, mustWork = FALSE)
    )
    # Optional caller-supplied fields (e.g. the auto_run_id a monitor
    # subagent was spawned for). NULLs are dropped so the attended
    # header format is byte-for-byte what it always was.
    if (is.list(extra)) {
        extra <- extra[!vapply(extra, is.null, logical(1))]
        header[names(extra)] <- extra
    }

    json <- jsonlite::toJSON(header, auto_unbox = TRUE)
    .private_record_write(path, json, append = FALSE)

    invisible(path)
}

#' Append a message to session transcript (openclaw format)
#' @param session Session object
#' @param role Message role (user/assistant)
#' @param content Message content (string)
#' @param provider LLM provider (for assistant messages)
#' @param model Model name (for assistant messages)
#' @param usage Token usage list (for assistant messages)
#' @param audit Canonical turn reference for copied usage, or NULL.
#' @param agent_id Agent ID
#' @return Invisible path to transcript file
#' @param auto_run_id Auto-run id when this message belongs to an auto
#'   run; NULL (the attended default) writes no field, so the attended
#'   message format is unchanged. This is what lets two runs with the
#'   same goal in one session be partitioned from the transcript alone.
#' @param typed For a prompt, what the person typed. The entry then says
#'   whether anything was sent with it, `staged` FALSE when the content
#'   is what was typed, and TRUE with `typed` beside the whole content
#'   when it is not: R or shell output staged with the prompt, or an
#'   /auto run's prompt, which its driver writes, beside what the person
#'   typed after /auto (maybe nothing). NULL writes neither field, as
#'   every entry before 0.8.1 has neither. The reviewer (R/review.R)
#'   reads them.
#' @param job For a delegated job's result, the job's id, written as
#'   `job`. The result is not the reply to a prompt, and the reviewer
#'   reads it as such.
#' @noRd
transcript_append <- function(session, role, content, provider = NULL,
                              model = NULL, usage = NULL,
                              agent_id = DEFAULT_AGENT_ID,
                              auto_run_id = NULL, audit = NULL, typed = NULL,
                              job = NULL) {
    path <- session_transcript_path(session$sessionId, agent_id)

    # Ensure header exists
    if (!file.exists(path)) {
        transcript_write_header(session$sessionId, session$cwd %||% getwd(),
                                agent_id)
    }

    # Build message in pi-coding-agent format
    msg <- list(
                role = role,
                content = list(list(type = "text", text = content))
    )
    if (!is.null(auto_run_id)) {
        msg$auto_run_id <- auto_run_id
    }
    if (!is.null(typed)) {
        msg$staged <- !identical(typed, content)
        if (msg$staged) {
            msg$typed <- typed
        }
    }
    if (!is.null(job)) {
        msg$job <- as.character(job)
    }

    if (role == "assistant") {
        msg$stopReason <- "stop"
        msg$api <- "openai-responses"
        msg$provider <- provider %||% session$provider %||% "corteza"
        msg$model <- model %||% session$model %||% "unknown"
        msg["usage"] <- list(usage)
        msg$usage_format <- "llm.api"
        msg$usage_source <- "derived"
        if (is.null(audit)) {
            msg$usage_turns <- list()
        } else {
            msg$usage_turns <- list(audit)
        }
        msg$timestamp <- as.numeric(Sys.time()) * 1000
    }

    json <- jsonlite::toJSON(msg, auto_unbox = TRUE, null = "null")
    .private_record_write(path, json)

    invisible(path)
}

#' Load transcript from disk
#' @param id Session ID
#' @param agent_id Agent ID
#' @param from_compaction If TRUE, only load messages after last compaction
#' @return List of messages
#' @noRd
transcript_load <- function(id, agent_id = DEFAULT_AGENT_ID,
                            from_compaction = TRUE) {
    path <- session_transcript_path(id, agent_id)

    if (!file.exists(path)) {
        return(list())
    }

    lines <- readLines(path, warn = FALSE)
    lines <- lines[nchar(trimws(lines)) > 0]

    if (length(lines) == 0) {
        return(list())
    }

    # Parse all lines
    entries <- lapply(lines, function(line) {
        tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                 error = function(e) NULL)
    })

    entries <- Filter(Negate(is.null), entries)

    # Separate header from messages
    messages <- list()
    for (entry in entries) {
        if (identical(entry$type, "session")) {
            # This is the header, skip
            next
        }
        if (!is.null(entry$role)) {
            messages <- c(messages, list(entry))
        }
    }

    # TODO: Handle compaction markers when implemented
    # For now, return all messages

    messages
}

#' Load a transcript for a decision that has to see all of it: the
#' reviewer's (R/review.R). transcript_load() passes over a line it
#' cannot read; this one counts it.
#' @param id Session ID
#' @param agent_id Agent ID
#' @return NULL when there is no transcript. Otherwise list(entries,
#'   bad): the messages in order, and the number of lines that are
#'   neither the header nor a message, a line that is not JSON among
#'   them.
#' @noRd
transcript_load_strict <- function(id, agent_id = DEFAULT_AGENT_ID) {
    path <- session_transcript_path(id, agent_id)
    if (!file.exists(path)) {
        return(NULL)
    }
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    entries <- list()
    bad <- 0L
    for (line in lines[nzchar(trimws(lines))]) {
        e <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                      error = function(err) NULL)
        if (!is.list(e) || is.null(names(e))) {
            bad <- bad + 1L
        } else if (identical(e$type, "session") && is.null(e$role)) {
            next
        } else if (is.character(e$role) && length(e$role) == 1L) {
            entries[[length(entries) + 1L]] <- e
        } else {
            bad <- bad + 1L
        }
    }
    list(entries = entries, bad = bad)
}

#' Count messages in transcript
#' @param id Session ID
#' @param agent_id Agent ID
#' @return Number of messages (excluding header)
#' @noRd
transcript_count <- function(id, agent_id = DEFAULT_AGENT_ID) {
    path <- session_transcript_path(id, agent_id)

    if (!file.exists(path)) {
        return(0L)
    }

    lines <- readLines(path, warn = FALSE)
    # Subtract 1 for header line
    max(0L, length(lines) - 1L)
}

#' Append compaction marker to transcript
#' @param session Session object
#' @param summary The compaction summary text
#' @param agent_id Agent ID
#' @return Invisible path to transcript file
#' @noRd
transcript_compact <- function(session, summary, agent_id = DEFAULT_AGENT_ID) {
    # Compaction in pi-coding-agent format is an assistant message with special marker
    # For now, just append as a regular assistant message
    # TODO: Match exact compaction format from pi-coding-agent
    transcript_append(session, role = "assistant",
                      content = paste0("[Compaction Summary]\n\n", summary),
                      provider = "corteza", model = "compaction",
                      agent_id = agent_id)
}

#' A directory as a person reads it: the home directory as "~".
#' @noRd
session_dir_show <- function(path) {
    home <- session_cwd(path.expand("~"))
    if (is.null(path) || is.null(home)) {
        return(path)
    }
    if (identical(path, home)) {
        return("~")
    }
    if (startsWith(path, paste0(home, "/"))) {
        return(paste0("~", substring(path, nchar(home) + 1L)))
    }
    path
}

#' Format session list for display
#' @param sessions List of session summaries from session_list()
#' @param how What lists the sessions of every directory, as the person
#'   would type it. Shown when some are elsewhere.
#' @return Character string for printing
#' @noRd
format_session_list <- function(sessions, how = "session_list(all = TRUE)") {
    elsewhere <- attr(sessions, "elsewhere") %||% 0L
    listed_dir <- attr(sessions, "cwd")
    if (length(sessions) == 0) {
        return(session_none_here(elsewhere, how))
    }

    safe_str <- function(x, default = "?") {
        if (is.null(x) || length(x) == 0 || identical(x, list())) {
            default
        } else {
            as.character(x)
        }
    }

    format_time <- function(ms) {
        if (is.null(ms) || !is.numeric(ms)) {
            return("?")
        }
        format(as.POSIXct(ms / 1000, origin = "1970-01-01"), "%Y-%m-%d %H:%M")
    }

    lines <- vapply(sessions, function(s) {
        if (is.null(s)) return("")
        compactions <- if ((s$compactionCount %||% 0) > 0) {
            sprintf(" [%d compactions]", s$compactionCount)
        } else ""
        line <- sprintf("  %s  %s  %d msgs  %s/%s%s",
                        safe_str(s$sessionKey, "?"), format_time(s$updatedAt),
            if (is.numeric(s$messages)) s$messages else 0L,
                        safe_str(s$modelProvider, "?"),
                        safe_str(s$model, "default"), compactions)
        # Under it: what the conversation is, and where when the list
        # covers every directory.
        about <- if (is.character(s$title)) s$title else NULL
        if (is.null(listed_dir) && is.character(s$cwd)) {
            about <- paste(c(session_dir_show(s$cwd), about), collapse = ": ")
        }
        if (!is.null(about)) {
            line <- paste0(line, "\n      ", about)
        }
        line
    }, character(1))

    lines <- lines[nchar(lines) > 0]
    header <- "Sessions:"
    if (!is.null(listed_dir)) {
        header <- sprintf("Sessions in %s:", session_dir_show(listed_dir))
    }
    footer <- NULL
    if (elsewhere > 0L) {
        footer <- sprintf("%d more in other directories (%s).", elsewhere, how)
    }
    paste(c(header, lines, footer), collapse = "\n")
}

# Trace storage ----

#' Get path to trace file for a session
#' @param session_id Session ID
#' @param agent_id Agent ID
#' @return Path to trace file
#' @noRd
trace_path <- function(session_id, agent_id = DEFAULT_AGENT_ID) {
    file.path(sessions_dir(agent_id), paste0(session_id, "_trace.jsonl"))
}

#' Add a trace entry for a tool execution
#' @param session_id Session ID
#' @param tool Tool name
#' @param args Tool arguments
#' @param result Result text
#' @param success TRUE if successful
#' @param elapsed_ms Execution time in milliseconds
#' @param approved_by How tool was approved
#' @param turn Conversation turn number
#' @param agent_id Agent ID
#' @param auto_run_id Auto-run id when this call executed inside an
#'   auto run; NULL (the attended default) writes no field at all, so
#'   the format of attended traces is unchanged.
#' @param call_id Per-call id minted at the auto gate; NULL outside
#'   auto runs. This is what joins one trace row to one gate record
#'   when the same tool is called twice with the same arguments.
#' @return Invisible path to trace file
#' @noRd
trace_add <- function(session_id, tool, args, result, success, elapsed_ms,
                      approved_by = NULL, turn = NULL,
                      agent_id = DEFAULT_AGENT_ID, auto_run_id = NULL,
                      call_id = NULL, turn_id = NULL) {
    path <- trace_path(session_id, agent_id)

    args_summary <- .trace_preview(args, 200L)
    result_summary <- .trace_preview(result, 500L)
    args_cut <- !identical(args, args_summary)
    result_cut <- !identical(result, result_summary)

    entry <- list(
                  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                  turn = turn,
                  tool = tool,
                  args = args_summary,
                  result = result_summary,
                  truncated = args_cut || result_cut,
                  truncation = list(args = args_cut, result = result_cut,
                                    args_character_limit = 200L,
                                    result_character_limit = 500L),
                  success = success,
                  elapsed_ms = elapsed_ms,
                  approved_by = approved_by
    )
    if (!is.null(auto_run_id)) {
        entry$auto_run_id <- auto_run_id
    }
    if (!is.null(call_id)) {
        entry$call_id <- call_id
    }
    if (!is.null(turn_id)) {
        entry$turn_id <- turn_id
    }

    json <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")
    .private_record_write(path, json)

    invisible(path)
}

#' Load trace for a session
#' @param session_id Session ID
#' @param agent_id Agent ID
#' @param n Maximum number of entries to return (NULL for all)
#' @return List of trace entries
#' @noRd
trace_load <- function(session_id, agent_id = DEFAULT_AGENT_ID, n = NULL) {
    path <- trace_path(session_id, agent_id)

    if (!file.exists(path)) {
        return(list())
    }

    lines <- readLines(path, warn = FALSE)

    if (length(lines) == 0) {
        return(list())
    }

    if (!is.null(n) && n < length(lines)) {
        lines <- tail(lines, n)
    }

    lapply(lines, function(line) {
        tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                 error = function(e) NULL)
    })
}

#' Format trace for display
#' @param trace List of trace entries from trace_load()
#' @param show_args Whether to show arguments
#' @return Character string for printing
#' @noRd
format_trace <- function(trace, show_args = FALSE) {
    if (length(trace) == 0) {
        return("No tool calls recorded.")
    }

    lines <- vapply(trace, function(entry) {
        if (is.null(entry)) return("")

        status <- if (isTRUE(entry$success)) "OK" else "ERR"
        time_str <- if (!is.null(entry$elapsed_ms)) {
            sprintf("%dms", entry$elapsed_ms)
        } else {
            "?"
        }

        base <- sprintf("  [%s] %s %s (%s)", status, entry$tool, time_str,
                        substr(entry$timestamp, 12, 19))

        if (show_args && length(entry$args) > 0) {
            args_str <- paste(names(entry$args), "=",
                              vapply(entry$args, function(x) {
                if (is.character(x)) {
                    s <- if (nchar(x) > 30) paste0(substr(x, 1, 27), "...") else x
                    sprintf('"%s"', s)
                } else {
                    as.character(x)
                }
            }, character(1)),
                              collapse = ", ")
            base <- paste0(base, "\n    ", args_str)
        }

        base
    }, character(1))

    lines <- lines[nchar(lines) > 0]
    paste(c("Tool execution trace:", lines), collapse = "\n")
}

# Backward compatibility - project-local sessions ----
# These functions allow using project-local .corteza/sessions/ if needed
