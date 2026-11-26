# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

# Sessions know the directory they were started in: what the store
# records, what a list and a resume find from a directory, what happens
# to sessions made before the store recorded it, and how the store file
# itself is written.

local({
root <- tempfile("session-dirs-")
dir.create(root)
# macOS keeps tempdir() under /var, which resolves to /private/var. The
# root is resolved once, so paths under it that do not exist yet still
# compare equal to what the code records.
root <- normalizePath(root, winslash = "/")
old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"))
on.exit({
    if (is.na(old_data)) {
        Sys.unsetenv("R_USER_DATA_DIR")
    } else {
        Sys.setenv(R_USER_DATA_DIR = old_data)
    }
    unlink(root, recursive = TRUE)
}, add = TRUE)

project <- function(name) {
    path <- file.path(root, name)
    dir.create(path, showWarnings = FALSE)
    path
}
dir_a <- project("a")
dir_b <- project("b")
dir_none <- project("none")

# Each test block gets an agent of its own, so one block's sessions are
# never another block's "elsewhere".
agent_n <- 0L
new_agent <- function() {
    agent_n <<- agent_n + 1L
    sprintf("dirs%02d", agent_n)
}
store_file <- function(agent) corteza:::sessions_store_path(agent)
store_bytes <- function(agent) readBin(store_file(agent), "raw", 1e7)
entry_of <- function(agent, key) corteza:::store_load(agent)[[key]]
keys_of <- function(sessions) {
    vapply(sessions, function(s) s$sessionKey, character(1))
}
# Sessions are ordered by the store's updatedAt, in milliseconds. A
# session is dated here, so no test waits on the clock.
date_session <- function(agent, key, ms) {
    store <- corteza:::store_load(agent)
    store[[key]]$updatedAt <- ms
    corteza:::store_save(store, agent)
}
new_session <- function(agent, dir, at, prompt = NULL, ...) {
    s <- corteza:::session_new("ollama", "m", dir, agent_id = agent, ...)
    if (!is.null(prompt)) {
        corteza:::transcript_append(s, "user", prompt, agent_id = agent)
        s <- corteza:::session_title_once(s, prompt, agent_id = agent)
    }
    date_session(agent, s$sessionKey, at)
    s
}

# ---- what a new session records ----
agent <- new_agent()
s <- corteza:::session_new("ollama", "m", dir_a, agent_id = agent,
                           surface = "cli")
e <- entry_of(agent, s$sessionKey)
expect_equal(e$cwd, dir_a)
expect_equal(e$surface, "cli")
expect_null(e$title)
expect_equal(s$surface, "cli")

# No surface given, none recorded.
bare <- corteza:::session_new("ollama", "m", dir_a, agent_id = agent)
expect_false("surface" %in% names(entry_of(agent, bare$sessionKey)))

# The first prompt is the title, on 1 line.
s <- corteza:::session_title_once(s, "  Fix the\n\tthing  ", agent_id = agent)
expect_equal(s$title, "Fix the thing")
expect_equal(entry_of(agent, s$sessionKey)$title, "Fix the thing")

# Once. The second prompt changes nothing and writes nothing.
before <- store_bytes(agent)
s <- corteza:::session_title_once(s, "and now something else",
                                  agent_id = agent)
expect_equal(s$title, "Fix the thing")
expect_identical(store_bytes(agent), before)

# Saving a session keeps what it was started with.
s$inputTokens <- 12L
corteza:::session_save(s, agent)
e <- entry_of(agent, s$sessionKey)
expect_equal(e$inputTokens, 12L)
expect_equal(e$cwd, dir_a)
expect_equal(e$surface, "cli")
expect_equal(e$title, "Fix the thing")

# A prompt with no text is no title, and the next prompt still is one.
blank <- corteza:::session_new("ollama", "m", dir_a, agent_id = agent)
blank <- corteza:::session_title_once(blank, " \n\t ", agent_id = agent)
expect_null(blank$title)
expect_false("title" %in% names(entry_of(agent, blank$sessionKey)))
blank <- corteza:::session_title_once(blank, "real", agent_id = agent)
expect_equal(entry_of(agent, blank$sessionKey)$title, "real")

# 100 characters, and nothing that moves a terminal's cursor.
expect_equal(nchar(corteza:::session_title(strrep("x", 300L))), 100L)
expect_equal(corteza:::session_title("a\033[31mb\007c"), "a [31mb c")
expect_null(corteza:::session_title(NULL))
expect_null(corteza:::session_title(character()))

# ---- a directory as recorded ----
expect_equal(corteza:::session_cwd(paste0(dir_a, "/")), dir_a)
expect_equal(corteza:::session_cwd(paste0(dir_a, "///")), dir_a)
# The root keeps its slash: "/", or a drive's root on Windows ("C:/").
expect_true(grepl("^([A-Za-z]:)?/$", corteza:::session_cwd("/")))
expect_null(corteza:::session_cwd(NULL))
expect_null(corteza:::session_cwd(""))
expect_null(corteza:::session_cwd(NA_character_))
# A directory that does not exist is kept as named.
expect_equal(corteza:::session_cwd(file.path(root, "not-there")),
             file.path(root, "not-there"))

# ---- lists and resumes by directory ----
agent <- new_agent()
a_old <- new_session(agent, dir_a, 1000, "first in a")
b_one <- new_session(agent, dir_b, 2000, "only in b")
a_new <- new_session(agent, dir_a, 3000, "second in a")

in_a <- corteza:::session_list(agent, cwd = dir_a)
expect_equal(keys_of(in_a), c(a_new$sessionKey, a_old$sessionKey))
expect_equal(attr(in_a, "elsewhere"), 1L)
expect_equal(attr(in_a, "cwd"), dir_a)

in_b <- corteza:::session_list(agent, cwd = dir_b)
expect_equal(keys_of(in_b), b_one$sessionKey)
expect_equal(attr(in_b, "elsewhere"), 2L)

everywhere <- corteza:::session_list(agent, cwd = dir_none, all = TRUE)
expect_equal(keys_of(everywhere),
             c(a_new$sessionKey, b_one$sessionKey, a_old$sessionKey))
expect_equal(attr(everywhere, "elsewhere"), 0L)
expect_null(attr(everywhere, "cwd"))

nowhere <- corteza:::session_list(agent, cwd = dir_none)
expect_equal(length(nowhere), 0L)
expect_equal(attr(nowhere, "elsewhere"), 3L)

# A slash at the end names the same directory.
expect_equal(keys_of(corteza:::session_list(agent, cwd = paste0(dir_a, "/"))),
             c(a_new$sessionKey, a_old$sessionKey))

# The latest session is this directory's. In b that is not the newest
# session there is, which is what 0.8.0 resumed.
expect_equal(corteza:::session_latest(agent, cwd = dir_a)$sessionKey,
             a_new$sessionKey)
expect_equal(corteza:::session_latest(agent, cwd = dir_b)$sessionKey,
             b_one$sessionKey)
expect_null(corteza:::session_latest(agent, cwd = dir_none))
expect_equal(corteza:::session_latest(agent, cwd = dir_none,
                                      all = TRUE)$sessionKey,
             a_new$sessionKey)

# n cuts the list after the directory is chosen.
first <- corteza:::session_list(agent, n = 1L, cwd = dir_a)
expect_equal(keys_of(first), a_new$sessionKey)
expect_equal(attr(first, "elsewhere"), 1L)

# A session keeps the directory it was started in when it is saved
# from another.
local({
    old_wd <- setwd(dir_b)
    on.exit(setwd(old_wd))
    corteza:::session_save(a_old, agent)
})
expect_equal(entry_of(agent, a_old$sessionKey)$cwd, dir_a)
date_session(agent, a_old$sessionKey, 1000)

# A session with a key of the person's own is listed. A subagent's
# entry, which shares the store, is not.
named <- new_session(agent, dir_a, 500, "named", session_key = "work")
store <- corteza:::store_load(agent)
store[["agent:main:subagent:abc"]] <- list(sessionId = "sub-abc",
                                           cwd = dir_a, updatedAt = 9000)
corteza:::store_save(store, agent)
in_a <- corteza:::session_list(agent, cwd = dir_a)
expect_equal(keys_of(in_a), c(a_new$sessionKey, a_old$sessionKey, "work"))
expect_equal(attr(in_a, "elsewhere"), 1L)

# A link to a directory is that directory. Where the file system makes
# no links there is nothing to test.
link <- file.path(root, "link-to-a")
if (.Platform$OS.type == "unix" &&
    isTRUE(suppressWarnings(file.symlink(dir_a, link)))) {
    expect_equal(keys_of(corteza:::session_list(agent, cwd = link)),
                 c(a_new$sessionKey, a_old$sessionKey, "work"))
    via_link <- new_session(agent, link, 4000, "through the link")
    expect_equal(entry_of(agent, via_link$sessionKey)$cwd, dir_a)
    expect_equal(keys_of(corteza:::session_list(agent, cwd = dir_a))[[1L]],
                 via_link$sessionKey)
}

# A directory that is gone still has its sessions.
dir_gone <- project("gone")
gone <- new_session(agent, dir_gone, 5000, "before it went")
unlink(dir_gone, recursive = TRUE)
expect_equal(keys_of(corteza:::session_list(agent, cwd = dir_gone)),
             gone$sessionKey)

# ---- sessions from before the store recorded a directory ----
agent <- new_agent()
legacy_session <- function(dir, at, prompt = NULL) {
    s <- new_session(agent, dir, at)
    if (!is.null(prompt)) {
        corteza:::transcript_append(s, "user", prompt, agent_id = agent)
    }
    store <- corteza:::store_load(agent)
    store[[s$sessionKey]][c("cwd", "surface", "title")] <- NULL
    corteza:::store_save(store, agent)
    s
}
old_a <- legacy_session(dir_a, 1000, "an old prompt")
old_a2 <- legacy_session(dir_a, 900, "an older prompt")
old_b <- legacy_session(dir_b, 800, "elsewhere")
expect_null(entry_of(agent, old_a$sessionKey)$cwd)

# Listed from its directory, which is read from the transcript's header.
listed <- corteza:::session_list(agent, n = 1L, cwd = dir_a)
expect_equal(keys_of(listed), old_a$sessionKey)
expect_equal(listed[[1L]]$title, "an old prompt")
expect_equal(attr(listed, "elsewhere"), 1L)

# And recorded: the directory for every session, the title for the one
# that was shown.
expect_equal(entry_of(agent, old_a$sessionKey)$cwd, dir_a)
expect_equal(entry_of(agent, old_a2$sessionKey)$cwd, dir_a)
expect_equal(entry_of(agent, old_b$sessionKey)$cwd, dir_b)
expect_equal(entry_of(agent, old_a$sessionKey)$title, "an old prompt")
expect_null(entry_of(agent, old_a2$sessionKey)$title)

# Once: the same list again writes nothing.
before <- store_bytes(agent)
again <- corteza:::session_list(agent, n = 1L, cwd = dir_a)
expect_equal(keys_of(again), old_a$sessionKey)
expect_identical(store_bytes(agent), before)

# Resumed by key before it was ever listed, a session still knows its
# directory and its title, and the next prompt does not rename it.
fresh_agent <- new_agent()
agent <- fresh_agent
unlisted <- legacy_session(dir_a, 1000, "what it was about")
loaded <- corteza:::session_load(unlisted$sessionKey, agent)
expect_equal(loaded$cwd, dir_a)
expect_equal(loaded$title, "what it was about")
loaded <- corteza:::session_title_once(loaded, "a later prompt",
                                       agent_id = agent)
expect_equal(loaded$title, "what it was about")
expect_null(entry_of(agent, unlisted$sessionKey)$title)

# A session with no prompt yet has no title, and its first prompt after
# a resume is one.
quiet <- legacy_session(dir_a, 900)
loaded <- corteza:::session_load(quiet$sessionKey, agent)
expect_null(loaded$title)
loaded <- corteza:::session_title_once(loaded, "first words",
                                       agent_id = agent)
expect_equal(entry_of(agent, quiet$sessionKey)$title, "first words")

# A session whose transcript is gone has no directory. It is listed
# with every directory's sessions and with no directory's own, and
# asking again does not write the store again.
lost <- legacy_session(dir_a, 800, "lost")
unlink(corteza:::session_transcript_path(lost$sessionId, agent))
invisible(corteza:::session_list(agent, cwd = dir_a))
before <- store_bytes(agent)
expect_false(lost$sessionKey %in%
             keys_of(corteza:::session_list(agent, cwd = dir_a)))
expect_true(lost$sessionKey %in%
            keys_of(corteza:::session_list(agent, all = TRUE)))
expect_identical(store_bytes(agent), before)

# ---- the store file ----
agent <- new_agent()
one <- new_session(agent, dir_a, 1000, "one")
two <- new_session(agent, dir_a, 2000, "two")
# Written beside the store and renamed: nothing is left beside it.
expect_equal(sort(list.files(corteza:::sessions_dir(agent),
                             all.files = TRUE, no.. = TRUE)),
             sort(c("sessions.json", paste0(one$sessionId, ".jsonl"),
                    paste0(two$sessionId, ".jsonl"))))
if (.Platform$OS.type == "unix") {
    # It holds the start of prompts, so it is private like a transcript.
    expect_equal(format(file.info(store_file(agent))$mode), "600")
    # A save replaces the file and never writes into it. A second name
    # for the file as it was still reads whole afterwards, which is what
    # a process that had it open would read.
    was <- file.path(root, "store-as-it-was.json")
    if (isTRUE(suppressWarnings(file.link(store_file(agent), was)))) {
        old_text <- readLines(was)
        date_session(agent, one$sessionKey, 1500)
        expect_identical(readLines(was), old_text)
        expect_false(identical(readLines(store_file(agent)), old_text))
        unlink(was)
    }
}

# A store that cannot be read is not written over. 0.8.0 took it for
# empty and saved a store of 1 entry in its place.
garbage <- "{\"corteza:one\": {\"sessionId\": \"one\", "
writeLines(garbage, store_file(agent))
expect_equal(length(corteza:::store_load(agent)), 0L)
# Reading moves nothing.
expect_equal(length(corteza:::session_list(agent, all = TRUE)), 0L)
expect_equal(readLines(store_file(agent)), garbage)
expect_false(file.exists(paste0(store_file(agent), ".unreadable")))
# Writing keeps it aside and starts a new store.
three <- corteza:::session_new("ollama", "m", dir_a, agent_id = agent)
aside <- paste0(store_file(agent), ".unreadable")
expect_true(file.exists(aside))
if (file.exists(aside)) {
    expect_equal(readLines(aside), garbage)
}
expect_equal(names(corteza:::store_load(agent)), three$sessionKey)

# ---- what a person reads ----
agent <- new_agent()
t_a <- new_session(agent, dir_a, 2000, "tidy the tests")
t_a2 <- new_session(agent, dir_a, 1000)
t_b <- new_session(agent, dir_b, 3000, "other work")

# A directory under the home directory is shown from "~" (tested
# below); a temp directory may be one.
shown <- corteza:::session_dir_show
text <- corteza:::format_session_list(
    corteza:::session_list(agent, cwd = dir_a), how = "LIST --all")
lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
expect_equal(lines[[1L]], sprintf("Sessions in %s:", shown(dir_a)))
expect_true(startsWith(lines[[2L]], paste0("  ", t_a$sessionKey, "  ")))
expect_equal(lines[[3L]], "      tidy the tests")
# No title, no second line.
expect_true(startsWith(lines[[4L]], paste0("  ", t_a2$sessionKey, "  ")))
expect_equal(lines[[5L]], "1 more in other directories (LIST --all).")
expect_equal(length(lines), 5L)

# Every directory: each session says where it is.
text <- corteza:::format_session_list(
    corteza:::session_list(agent, all = TRUE), how = "LIST --all")
lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
expect_equal(lines[[1L]], "Sessions:")
expect_true(startsWith(lines[[2L]], paste0("  ", t_b$sessionKey, "  ")))
expect_equal(lines[[3L]], sprintf("      %s: other work", shown(dir_b)))
expect_equal(lines[[5L]], sprintf("      %s: tidy the tests", shown(dir_a)))
expect_equal(lines[[7L]], sprintf("      %s", shown(dir_a)))
expect_equal(length(lines), 7L)

# A directory with none says how many there are and what reaches them.
expect_equal(corteza:::format_session_list(
                 corteza:::session_list(agent, cwd = dir_none),
                 how = "LIST --all"),
             paste("No session was started in this directory.",
                   "3 are elsewhere (LIST --all)."))
expect_equal(corteza:::session_none_note(dir_none, how = "RESUME --all",
                                         agent_id = agent),
             paste("No session was started in this directory.",
                   "3 are elsewhere (RESUME --all)."))
expect_equal(corteza:::session_none_here(1L, "X"),
             "No session was started in this directory. 1 is elsewhere (X).")
expect_equal(corteza:::session_none_here(0L, "X"), "No sessions found.")
expect_equal(corteza:::format_session_list(list()), "No sessions found.")

# The home directory is shown as "~".
home <- corteza:::session_cwd(path.expand("~"))
expect_equal(corteza:::session_dir_show(home), "~")
expect_equal(corteza:::session_dir_show(file.path(home, "proj", "x")),
             "~/proj/x")
expect_equal(corteza:::session_dir_show(paste0(home, "-other")),
             paste0(home, "-other"))

# A session resumed where it was not started says so.
expect_null(corteza:::session_elsewhere_note(t_a, dir_a))
expect_null(corteza:::session_elsewhere_note(t_a, paste0(dir_a, "/")))
expect_equal(corteza:::session_elsewhere_note(t_a, dir_b),
             sprintf("This session was started in %s.", shown(dir_a)))
expect_null(corteza:::session_elsewhere_note(list(cwd = NULL), dir_b))
})

# ---- chat(): what `session = TRUE` and a key resume ----
local({
root <- tempfile("session-chat-")
dir.create(root)
root <- normalizePath(root, winslash = "/")
old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"))
on.exit({
    if (is.na(old_data)) {
        Sys.unsetenv("R_USER_DATA_DIR")
    } else {
        Sys.setenv(R_USER_DATA_DIR = old_data)
    }
    unlink(root, recursive = TRUE)
}, add = TRUE)
dir_a <- file.path(root, "a")
dir_b <- file.path(root, "b")
dir_none <- file.path(root, "none")
for (d in c(dir_a, dir_b, dir_none)) dir.create(d)
dated <- function(dir, at) {
    s <- corteza:::session_new("ollama", "m", dir)
    store <- corteza:::store_load()
    store[[s$sessionKey]]$updatedAt <- at
    corteza:::store_save(store)
    s
}
in_b <- dated(dir_b, 1000)
in_a <- dated(dir_a, 2000)

# The latest session of this directory, not the latest there is.
got <- corteza:::resolve_disk_session(TRUE, "ollama", "m", dir_b)
expect_true(got$resumed)
expect_equal(got$sessionId, in_b$sessionId)
expect_null(got$note)

# None here: a new session, recorded as the console's, and a line that
# says where the others are.
got <- corteza:::resolve_disk_session(TRUE, "ollama", "m", dir_none)
expect_false(got$resumed)
expect_false(got$sessionId %in% c(in_a$sessionId, in_b$sessionId))
expect_equal(got$note, paste("No session was started in this directory.",
                             "2 are elsewhere (/sessions all)."))
made <- corteza:::store_load()[[got$session$sessionKey]]
expect_equal(made$surface, "console")
expect_equal(made$cwd, dir_none)

# A key resumes its session from anywhere, and says where it is from.
got <- corteza:::resolve_disk_session(in_a$sessionKey, "ollama", "m", dir_b)
expect_true(got$resumed)
expect_equal(got$sessionId, in_a$sessionId)
expect_equal(got$note, sprintf("This session was started in %s.",
                               corteza:::session_dir_show(dir_a)))
got <- corteza:::resolve_disk_session(in_a$sessionKey, "ollama", "m", dir_a)
expect_null(got$note)

# No resume asked for: a new session and nothing to say.
got <- corteza:::resolve_disk_session(NULL, "ollama", "m", dir_none)
expect_false(got$resumed)
expect_null(got$note)
})

# ---- the loop: /sessions, and the title of a conversation ----
local({
root <- tempfile("session-loop-")
dir.create(root)
root <- normalizePath(root, winslash = "/")
old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"))
on.exit({
    if (is.na(old_data)) {
        Sys.unsetenv("R_USER_DATA_DIR")
    } else {
        Sys.setenv(R_USER_DATA_DIR = old_data)
    }
    unlink(root, recursive = TRUE)
}, add = TRUE)
dir_a <- file.path(root, "a")
dir_b <- file.path(root, "b")
for (d in c(dir_a, dir_b)) dir.create(d)

scripted_input <- function(lines) {
    i <- 0L
    function(prompt_str) {
        i <<- i + 1L
        if (i <= length(lines)) lines[[i]] else character(0)
    }
}
loop_ctx <- function(lines, cwd, disk) {
    ctx <- new.env(parent = emptyenv())
    ctx$palette <- list(dim = "", reset = "", cyan = "", bold = "",
                        yellow = "", green = "", bright_magenta = "",
                        red = "", magenta = "")
    ctx$read_input <- scripted_input(lines)
    ctx$help_text <- function() "HELP"
    ctx$handle_copy <- function(x) invisible(NULL)
    ctx$format_tools <- function(s) "TOOLS"
    ctx$pending_r_context <- character(0)
    ctx$last_assistant_response <- ""
    ctx$cwd <- cwd
    ctx$provider <- "ollama"
    ctx$model <- "m"
    ctx$config <- list()
    ctx$session <- new.env(parent = emptyenv())
    ctx$session$history <- list()
    ctx$session$tasks <- list()
    ctx$session$tasks_dirty <- FALSE
    ctx$disk_session <- list(session = disk, sessionId = disk$sessionId,
                             resumed = FALSE)
    ctx$render_reply <- function(txt) invisible(NULL)
    ctx$turn_fn <- function(prompt, session) {
        list(reply = "ok", usage = NULL)
    }
    ctx
}

other <- corteza:::session_new("ollama", "m", dir_b)
mine <- corteza:::session_new("ollama", "m", dir_a, surface = "cli")

# /sessions lists this directory; "/sessions all" every one. The R
# session's own directory is neither of them.
ctx <- loop_ctx(c("/sessions", "/sessions all"), dir_a, mine)
out <- trimws(capture.output(corteza:::run_repl_loop(ctx)), "right")
here <- which(out == sprintf("Sessions in %s:",
                             corteza:::session_dir_show(dir_a)))
every <- which(out == "Sessions:")
expect_equal(length(here), 1L)
expect_equal(length(every), 1L)
if (length(here) == 1L && length(every) == 1L) {
    between <- out[seq(here, every - 1L)]
    expect_true(any(grepl(mine$sessionKey, between, fixed = TRUE)))
    expect_false(any(grepl(other$sessionKey, between, fixed = TRUE)))
    expect_true(any(between ==
                    "1 more in other directories (/sessions all)."))
    after <- out[seq(every, length(out))]
    expect_true(any(grepl(mine$sessionKey, after, fixed = TRUE)))
    expect_true(any(grepl(other$sessionKey, after, fixed = TRUE)))
}

# The first prompt names the conversation. Text staged by /r goes to
# the model with it and stays out of the title; a later prompt does not
# rename it.
ctx <- loop_ctx(c("/r 40 + 2", "what is this number", "and this one"),
                dir_a, mine)
invisible(capture.output(corteza:::run_repl_loop(ctx)))
expect_equal(ctx$disk_session$session$title, "what is this number")
expect_equal(corteza:::store_load()[[mine$sessionKey]]$title,
             "what is this number")
sent <- corteza:::transcript_load(mine$sessionId)
expect_true(grepl("42", sent[[1L]]$content[[1L]]$text, fixed = TRUE))
expect_true(grepl("what is this number", sent[[1L]]$content[[1L]]$text,
                  fixed = TRUE))
})

# ---- a room's record ----
# A room has a journal and no transcript. Its entry in the store, under
# agent "matrix", is what says which room a journal folder belongs to
# and where that room works.
local({
root <- tempfile("session-rooms-")
dir.create(root)
root <- normalizePath(root, winslash = "/")
old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"))
on.exit({
    if (is.na(old_data)) {
        Sys.unsetenv("R_USER_DATA_DIR")
    } else {
        Sys.setenv(R_USER_DATA_DIR = old_data)
    }
    unlink(root, recursive = TRUE)
}, add = TRUE)
project <- file.path(root, "project")
dir.create(project)

room <- "!r:ex"
bot_a <- list(user_id = "@a:ex")
bot_b <- list(user_id = "@b:ex")
journal_of <- function(cfg, key) corteza:::bot_journal_dir(cfg, key)
name_of <- function(cfg, key) {
    sub("[.]journal$", "", basename(journal_of(cfg, key)))
}
record <- function(cfg, key, cwd, name = NULL) {
    s <- new.env(parent = emptyenv())
    s$room_id <- room
    s$cwd <- cwd
    corteza:::bot_session_record(cfg, s, key, journal_of(cfg, key),
                                 room_name = name)
}

# 2 bots in 1 room with 1 data directory: 2 entries, 1 each.
record(bot_a, room, project, "Project room")
record(bot_b, room, project, "Project room")
store <- corteza:::store_load("matrix")
expect_equal(sort(names(store)),
             sort(c(name_of(bot_a, room), name_of(bot_b, room))))
expect_false(identical(name_of(bot_a, room), name_of(bot_b, room)))

e <- store[[name_of(bot_a, room)]]
expect_equal(e$sessionId, name_of(bot_a, room))
expect_equal(e$surface, "matrix")
expect_equal(e$bot, "@a:ex")
expect_equal(e$room, room)
expect_equal(e$cwd, project)
expect_equal(e$title, "Project room")
expect_equal(e$journal, journal_of(bot_a, room))
expect_true(is.numeric(e$updatedAt))
expect_null(e$thread)
expect_null(e$voice)
expect_equal(store[[name_of(bot_b, room)]]$bot, "@b:ex")
# The journal folder is the entry's key with ".journal": a reader goes
# from one to the other without the bot's name.
expect_equal(basename(e$journal %||% ""), paste0(e$sessionId, ".journal"))

# A thread in the room is a conversation of its own, and so is a voice
# session.
thread_key <- corteza:::bot_session_key(room, "$root")
record(bot_a, thread_key, project)
record(bot_a, paste0("voice:", room), project)
store <- corteza:::store_load("matrix")
expect_equal(length(store), 4L)
in_thread <- store[[name_of(bot_a, thread_key)]]
expect_equal(in_thread$thread, "$root")
expect_equal(in_thread$room, room)
expect_null(in_thread$voice)
spoken <- store[[name_of(bot_a, paste0("voice:", room))]]
expect_true(isTRUE(spoken$voice))
expect_null(spoken$thread)

# A room in the scratch folder has no directory: that folder is gone
# with the process. A room that had one and lost it loses the record of
# it, and keeps its name when the name could not be fetched this time.
scratch <- corteza:::bot_scratch_cwd()
record(bot_a, room, scratch)
e <- corteza:::store_load("matrix")[[name_of(bot_a, room)]]
expect_false("cwd" %in% names(e))
expect_equal(e$title, "Project room")
expect_equal(length(corteza:::store_load("matrix")), 4L)
# And gets it back.
record(bot_a, room, project)
expect_equal(corteza:::store_load("matrix")[[name_of(bot_a, room)]]$cwd,
             project)

# A terminal's lists are the terminal's: rooms are under another agent.
expect_equal(length(corteza:::session_list(all = TRUE)), 0L)

# A record that cannot be written costs the room nothing.
Sys.setenv(R_USER_DATA_DIR = file.path(root, "blocked"))
agents <- corteza:::corteza_data_path("agents")
dir.create(agents, recursive = TRUE)
# A file where the agent's folder would go.
file.create(file.path(agents, "matrix"))
got <- "not run"
expect_silent(got <- suppressWarnings(
    record(bot_a, room, project, "Project room")))
expect_null(got)
expect_false(dir.exists(file.path(agents, "matrix")))
})

# ---- a room's session writes its record ----
# The whole constructor, which is slow: skipped on CRAN, run in CI.
if (tinytest::at_home() || identical(Sys.getenv("NOT_CRAN"), "true")) {
    local({
        root <- tempfile("session-room-built-")
        dir.create(root)
        root <- normalizePath(root, winslash = "/")
        old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
        old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA_character_)
        Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
                   CORTEZA_STATE_DIR = file.path(root, "state"))
        orig <- corteza:::bot_channel_info
        on.exit({
            assignInNamespace("bot_channel_info", orig, ns = "corteza")
            if (is.na(old_data)) {
                Sys.unsetenv("R_USER_DATA_DIR")
            } else {
                Sys.setenv(R_USER_DATA_DIR = old_data)
            }
            if (is.na(old_state)) {
                Sys.unsetenv("CORTEZA_STATE_DIR")
            } else {
                Sys.setenv(CORTEZA_STATE_DIR = old_state)
            }
            unlink(root, recursive = TRUE)
        }, add = TRUE)
        project <- file.path(root, "project")
        dir.create(project)
        # The room's name and topic, as the homeserver would give them.
        assignInNamespace("bot_channel_info", function(chat, room_id) {
            list(id = room_id, name = "Project room",
                 topic = "what the room is for")
        }, ns = "corteza")

        # A topic names no directory since 0.8.1 (WP12): an operator's
        # /cd bound this room, inside the bot's project roots.
        cfg <- list(user_id = "@a:ex", room_id = "!r:ex",
                    model = "qwen3:8b", provider = "ollama",
                    project_roots = root)
        corteza:::bot_room_state_write(cfg, "!r:ex",
            list(binding = list(state = "bound", dir = project)))
        reg <- corteza:::bot_new_session_registry()
        s <- corteza:::bot_get_or_create_session(reg, "!r:ex", cfg)
        store <- corteza:::store_load("matrix")
        expect_equal(length(store), 1L)
        e <- store[[1L]]
        expect_equal(e$room, "!r:ex")
        expect_equal(e$bot, "@a:ex")
        expect_equal(e$cwd, project)
        expect_equal(e$title, "Project room")
        expect_equal(e$journal, corteza:::bot_journal_dir(cfg, "!r:ex"))
        expect_equal(corteza:::session_cwd(s$cwd), project)
    })
}
