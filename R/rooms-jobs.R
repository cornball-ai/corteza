# Delegated jobs in Matrix rooms: presenting job events and collecting
# approvals without blocking the poll loop.
#
# bot_run_step() pumps every room session's jobs after each poll. A
# finished job is posted back to the room (or thread) it came from and
# recorded in that session's history, so the talker that delegated it
# knows the result. A job that needs approval gets a prompt; the answer
# arrives as a reaction in a later poll, handled by
# bot_handle_job_reactions(), and nothing waits for it in between. That
# is the difference from bot_reaction_approval(), which holds the whole
# loop until someone taps.

# Poll interval while any room has work in flight. Short enough that a
# finished job or an approval prompt shows up promptly, long enough not
# to spin.
BOT_JOB_POLL_MS <- 2000L

bot_sessions_list <- function(sessions) {
    if (is.null(sessions)) {
        return(list())
    }
    mget(ls(sessions), envir = sessions)
}

# Room sessions with something to pump: a running job, events held from
# a submit, or a queued job under their key. The ledger is read once for
# all of them rather than once per room.
bot_job_sessions <- function(sessions) {
    all <- bot_sessions_list(sessions)
    if (!length(all)) {
        return(list())
    }
    # Owner and key together: another bot's queued job in the same room
    # is not this bot's to start.
    queued <- vapply(job_list(status = "queued"), function(j) {
        paste(j$owner %||% "local", j$origin$session_key %||% "", sep = "\n")
    }, "")
    Filter(function(s) {
        !is.null(s$.job_current) || length(s$.job_events) ||
        paste(job_worker_owner(s), job_worker_key(s), sep = "\n") %in% queued
    }, all)
}

# TRUE when any room has work in flight or an approval prompt waiting on
# a reaction.
bot_jobs_active <- function(sessions) {
    if (length(bot_job_sessions(sessions))) {
        return(TRUE)
    }
    any(vapply(bot_sessions_list(sessions),
               function(s) length(s$.job_prompts) > 0L, logical(1)))
}

# The ledger owner for this bot's jobs: its Matrix id, read from the
# config because it is needed before any client exists.
bot_job_owner <- function(cfg) {
    cfg$user_id %||% paste0("bot:", cfg$user %||% "unknown")
}

# Send into a job's originating room or thread and record the message as
# the bot's own, the same way every other bot reply is recorded.
bot_job_send <- function(chat, s, job, text) {
    room <- job$origin$room %||% s$room_id
    sent <- tryCatch(bot_reply_send(chat, room, text, markdown = TRUE,
                                    thread = job$origin$thread),
                     error = function(e) NULL)
    if (!is.null(sent)) {
        s$seen_event_ids <- bot_remember_event(s$seen_event_ids, sent)
        bot_transcript_add(s, sent, "assistant", text)
    }
    sent
}

# ---- Posts that have to arrive -----------------------------------------
#
# A job's result, and the notice that a room's doer has been given
# another room's work, are posted once by code and by nobody else: no
# later turn repeats them. A send that fails (the homeserver is down, a
# token is mid-rotation) used to be dropped, and the caller carried on
# as if the room had been told.
#
# These posts are tried at once and, on failure, written to the state
# directory and retried by the poll loop until they go out. The job they
# are about is never resubmitted; only the message is retried. Callers
# get back whether it was delivered, so what they report is true.

# Seconds to wait before each retry; the last value repeats.
BOT_NOTICE_BACKOFF <- c(5, 15, 60, 300)
# A post not delivered after this long is set aside and logged.
BOT_NOTICE_MAX_AGE <- 24 * 3600

bot_notice_dir <- function() {
    file.path(bot_signal_dir(), "notices")
}

# Send one notice. On success, record it as the bot's own on the session
# it belongs to (so its echo through sync is skipped) and return the
# event id; NULL when it did not go out.
bot_notice_send <- function(chat, n, session = NULL) {
    sent <- tryCatch(bot_reply_send(chat, n$room, n$text, markdown = TRUE,
                                    thread = n$thread),
                     error = function(e) NULL)
    if (!is.character(sent) || length(sent) != 1L || is.na(sent) ||
        !nzchar(sent)) {
        return(NULL)
    }
    if (is.environment(session)) {
        session$seen_event_ids <- bot_remember_event(session$seen_event_ids,
            sent)
        bot_transcript_add(session, sent, "assistant", n$text)
    }
    sent
}

# Post `text` to a room, or keep it for retry if it cannot be sent now.
# Returns the event id, or NULL when it was queued instead.
bot_post_or_queue <- function(chat, owner, room, text, thread = NULL,
                              session = NULL, session_key = NULL, job = NULL) {
    n <- list(owner = owner, room = room, text = text, job = job,
              session_key = session_key)
    if (!is.null(thread)) {
        n$thread <- thread
    }
    if (is.null(chat)) {
        sent <- NULL
    } else {
        sent <- bot_notice_send(chat, n, session)
    }
    if (!is.null(sent)) {
        return(sent)
    }
    n$created_at <- as.numeric(Sys.time())
    n$attempts <- 1L
    n$next_at <- n$created_at + BOT_NOTICE_BACKOFF[[1L]]
    dir <- bot_notice_dir()
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    job_write_file(file.path(dir, paste0(job_new_id(), ".json")), n)
    NULL
}

bot_notices_pending <- function() {
    dir <- bot_notice_dir()
    dir.exists(dir) &&
    length(list.files(dir, pattern = "^[0-9T-]+-[0-9a-f]+[.]json$")) > 0L
}

# Retry this bot's queued posts that are due. Delivered ones are removed;
# failed ones wait longer each time; one older than BOT_NOTICE_MAX_AGE is
# set aside (renamed, kept for inspection) and logged, so a room that is
# gone for good does not keep the loop retrying forever.
bot_notices_retry <- function(chat, sessions, owner, now = Sys.time()) {
    dir <- bot_notice_dir()
    if (!dir.exists(dir)) {
        return(invisible(0L))
    }
    now <- as.numeric(now)
    delivered <- 0L
    files <- sort(list.files(dir, pattern = "^[0-9T-]+-[0-9a-f]+[.]json$",
                             full.names = TRUE))
    for (path in files) {
        n <- tryCatch(job_read_file(path), error = function(e) NULL)
        # Another bot's post in a shared state directory is not ours to
        # send: it would go out under the wrong name.
        if (is.null(n) || !identical(n$owner, owner) ||
            isTRUE(n$next_at > now)) {
            next
        }
        key <- n$session_key
        session <- if (!is.null(sessions) && !is.null(key) &&
            exists(key, envir = sessions, inherits = FALSE)) {
            get(key, envir = sessions)
        }
        if (!is.null(bot_notice_send(chat, n, session))) {
            unlink(path)
            delivered <- delivered + 1L
            next
        }
        if (now - n$created_at > BOT_NOTICE_MAX_AGE) {
            file.rename(path, sub("[.]json$", ".undelivered", path))
            message(sprintf("bot: gave up posting to %s after %d tries: %s",
                            n$room, n$attempts,
                            .sanitize_inline(n$text, max_chars = 80L)))
            next
        }
        n$attempts <- as.integer(n$attempts) + 1L
        wait <- BOT_NOTICE_BACKOFF[[min(n$attempts, length(BOT_NOTICE_BACKOFF))]]
        n$next_at <- now + wait
        job_write_file(path, n)
    }
    invisible(delivered)
}

# Retire the bot's idle job workers (job_workers_retire()). A bot has a
# session per room and per thread, each of which keeps a doer and a
# reviewer process once it has used them, so without this the count only
# grows while the bot runs.
#
# The two settings are the bot's, not a room's: `jobs$worker_idle_minutes`
# and `jobs$max_workers` in the corteza config that applies to the bot's
# own directory (the global config, with the bot's project config over
# it). The config is read only while there is a worker to retire.
bot_retire_workers <- function(sessions, cfg = NULL, now = Sys.time()) {
    all <- bot_sessions_list(sessions)
    if (!length(job_workers_live(all))) {
        return(invisible(0L))
    }
    if (is.null(cfg)) {
        cfg <- tryCatch(bot_load_config(), error = function(e) NULL)
    }
    jobs <- if (is.null(cfg)) {
        list()
    } else {
        tryCatch(load_config(bot_default_cwd(cfg))$jobs,
                 error = function(e) NULL) %||% list()
    }
    job_workers_retire(all,
                       idle_minutes = job_worker_limit(jobs$worker_idle_minutes,
            JOB_WORKER_DEFAULTS$idle_minutes),
                       max_workers = job_worker_limit(jobs$max_workers,
            JOB_WORKER_DEFAULTS$max_workers),
                       now = now)
}

bot_pump_jobs <- function(sessions, chat, cfg) {
    for (s in bot_job_sessions(sessions)) {
        events <- tryCatch(job_pump(s), error = function(e) {
            message("bot_pump_jobs: ", conditionMessage(e))
            list()
        })
        for (ev in events) {
            bot_present_job_event(chat, cfg, s, ev, sessions = sessions)
        }
        # After the events: a request raised this step has just been
        # sent to the monitor, and one sent earlier may have its answer.
        bot_monitor_answer(chat, cfg, s)
    }
    invisible(TRUE)
}

# Poll interval while a monitor is ruling on a call. A worker is stopped
# on that call until the answer is collected, so this is shorter than
# BOT_JOB_POLL_MS.
BOT_MONITOR_POLL_MS <- 500L

# TRUE when any room's monitor has a call it has not answered.
bot_monitor_waiting <- function(sessions) {
    any(vapply(bot_sessions_list(sessions),
               function(s) !is.null(s$.job_monitor_pending), logical(1)))
}

# `sessions` is the room registry, needed only to reach the session of a
# room that handed this job over (bot_job_report_back()).
bot_present_job_event <- function(chat, cfg, s, ev, sessions = NULL) {
    switch(ev$type,
           settled = bot_job_settled(chat, s, ev$job, sessions = sessions),
           restored = bot_job_send(chat, s, ev$job,
                                   bot_job_restored_text(ev$restore)),
           approval = bot_job_approval(chat, cfg, s, ev),
           blocked = bot_job_send(chat, s, ev$job, job_blocked_text(ev)),
           # "started" needs no post: the talker already said so.
           NULL)
}

bot_job_settled <- function(chat, s, job, sessions = NULL) {
    result <- bot_job_result_text(job)
    from <- job$origin$from
    text <- if (is.null(from$room)) {
        result
    } else {
        paste0(result, "\n\n_Requested from ", from$label %||% from$room, "._")
    }
    # A result is posted once; if it cannot go out now it is retried
    # (bot_post_or_queue()) rather than lost.
    sent <- bot_post_or_queue(chat, job$owner %||% job_worker_owner(s),
                              job$origin$room %||% s$room_id, text,
                              thread = job$origin$thread, session = s,
                              session_key = job_worker_key(s), job = job$id)
    # Into the talker's history either way. The post can fail; the
    # talker still has to know the job ended, or it will keep telling the
    # user the work is in progress.
    s$history <- c(s$history %||% list(),
                   list(list(role = "assistant", content = text)))
    if (!is.null(from$room)) {
        bot_job_report_back(chat, sessions, job, result)
    }
    invisible(sent)
}

# A job another room handed over reports to that room as well: posted
# where it was asked for, and put in that talker's history, for the same
# reason a room's own result is. The asking session can be gone (a
# restart does not rebuild every thread's session); the post still goes
# out, and its echo reaches whichever session later takes that key.
bot_job_report_back <- function(chat, sessions, job, result) {
    from <- job$origin$from
    text <- paste0(result, "\n\n_Run by the doer in ",
                   job$origin$label %||% job$origin$room, "._")
    key <- from$session_key
    asker <- if (!is.null(sessions) && !is.null(key) &&
        exists(key, envir = sessions, inherits = FALSE)) {
        get(key, envir = sessions)
    }
    sent <- bot_post_or_queue(chat, job$owner %||% "local", from$room, text,
                              thread = from$thread, session = asker,
                              session_key = key, job = job$id)
    if (is.environment(asker)) {
        asker$history <- c(asker$history %||% list(),
                           list(list(role = "assistant", content = text)))
    }
    invisible(sent)
}

# ---- Handing a job to another room --------------------------------------
#
# One bot serves many rooms, each with its own session, directory, and
# doer. A talker can hand a task to another room's doer (`delegate` with
# `room`). The job is then that room's in every way that matters for
# running it -- its session key, its directory and checkout lock, its
# worker's memory, its approvals -- and its record says who asked
# (origin$from).
#
# The room that runs it is told at once, in the room and in its talker's
# history, so neither the people there nor its talker find the doer busy
# with something they never heard of. The result is posted in both
# rooms.

# The rooms this bot is in: id, name, working directory. Asked of the
# transport at the time of the hand-off, since names and topics change
# and a room joined a minute ago has no session yet.
bot_room_directory <- function(chat, cfg) {
    ids <- tryCatch(chat.api::chat_channels(chat),
                    error = function(e) character())
    lapply(as.character(ids), function(id) {
        info <- bot_channel_info(chat, id)
        list(id = id, name = info$name, cwd = bot_room_cwd(cfg, info$topic))
    })
}

bot_room_label <- function(room) {
    name <- room$name
    if (is.null(name) || !length(name) || is.na(name[[1L]]) ||
        !nzchar(name[[1L]])) {
        return(room$id)
    }
    name[[1L]]
}

# The most rooms a tool result lists. A long result is cut before the
# model sees it (R/tool-output-cap.R), and a list cut short reads as
# "the room may be in the part I was not shown".
BOT_ROOM_LIST_MAX <- 12L

bot_room_listing <- function(rooms, max = BOT_ROOM_LIST_MAX) {
    lines <- vapply(utils::head(rooms, max), function(r) {
        sprintf("- %s (%s), working in %s", bot_room_label(r), r$id, r$cwd)
    }, "")
    if (length(rooms) > max) {
        lines <- c(lines, sprintf("(and %d more)", length(rooms) - max))
    }
    paste(lines, collapse = "\n")
}

# Rooms whose name or directory is close to `want`: one contains the
# other, or they differ by a character or two.
bot_rooms_near <- function(rooms, want) {
    want <- tolower(basename(trimws(want)))
    if (!nzchar(want)) {
        return(list())
    }
    near <- function(x) {
        x <- tolower(x)
        grepl(want, x, fixed = TRUE) ||
        (nchar(x) > 2L && grepl(x, want, fixed = TRUE)) ||
        (nchar(want) > 3L && isTRUE(agrepl(want, x, max.distance = 0.2)))
    }
    Filter(function(r) near(bot_room_label(r)) || near(basename(r$cwd)), rooms)
}

# A directory `want` names that no room works in: `want` itself when it
# is a path, otherwise a directory of that name beside a room's own.
# NULL when there is none.
bot_room_unclaimed_dir <- function(rooms, want) {
    want <- trimws(want)
    cwds <- vapply(rooms, function(r) r$cwd, "")
    found <- if (grepl("^(~/|/|\\./)", want)) {
        path.expand(want)
    } else {
        file.path(unique(dirname(cwds)), want)
    }
    found <- found[dir.exists(found)]
    claimed <- normalizePath(cwds, mustWork = FALSE)
    found <- found[!normalizePath(found, mustWork = FALSE) %in% claimed]
    if (length(found)) {
        found[[1L]]
    } else {
        NULL
    }
}

# What a talker is told when `want` matches no room. Every room was
# checked, and the reply says so and stays short: the nearest names
# rather than the whole list, and the reason when the project exists
# and no room works in it.
bot_room_no_match <- function(rooms, want) {
    out <- sprintf(paste0("No room matches '%s'. All %d of this bot's rooms ",
                          "were checked, by id, name, and working directory."),
                   want, length(rooms))
    near <- bot_rooms_near(rooms, want)
    if (length(near)) {
        out <- c(out, "The closest:", bot_room_listing(near))
    } else if (length(rooms) <= BOT_ROOM_LIST_MAX) {
        out <- c(out, "This bot's rooms:", bot_room_listing(rooms))
    }
    dir <- bot_room_unclaimed_dir(rooms, want)
    if (!is.null(dir)) {
        out <- c(out, sprintf(
                              paste0("The directory %s exists, but no room works in it. A ",
                                     "room works in the directory its topic names, so ",
                                     "handing work there needs a room with that topic. ",
                                     "This room's own doer (delegate without `room`) can ",
                                     "work on it from here; its writes outside this ",
                                     "room's project each need approval."), dir))
    }
    paste(out, collapse = "\n")
}

# The rooms `want` could mean, most specific reading first: a room id,
# then a room name, then a project directory, then a directory's last
# component. The first reading with any match decides; more than one
# match there is ambiguous and the caller says so. Rooms often share a
# directory (every room with no path in its topic uses the bot's own),
# which is why a name outranks one.
bot_resolve_room <- function(rooms, want) {
    want <- trimws(want)
    lower <- tolower(want)
    norm <- function(p) normalizePath(path.expand(p), mustWork = FALSE)
    as_path <- grepl("^(~/|/|\\./)", want)
    readings <- list(function(r) identical(r$id, want),
                     function(r) identical(tolower(bot_room_label(r)), lower),
                     function(r) as_path && identical(norm(r$cwd), norm(want)),
                     function(r) identical(tolower(basename(r$cwd)), lower))
    for (reading in readings) {
        hits <- Filter(reading, rooms)
        if (length(hits)) {
            return(hits)
        }
    }
    list()
}

# Hand `task` to the doer of the room `room` names. Returns a tool
# result for the talker that asked.
#
# Only an operator's request is handed over. Any member of a room can
# talk to its talker; working in another room's project, in front of
# another room's people, is for whoever runs the bot. The check comes
# before the room list is read, so a refusal says nothing about what
# other rooms exist.
#
# `new_session` gets or creates the session for a room id; the poll loop
# supplies it, with the run's own session options.
bot_job_handoff <- function(sessions, session, cfg, chat, sender, origin,
                            room, task, review = NULL, new_session,
                            rooms = bot_room_directory(chat, cfg)) {
    if (!isTRUE(sender %in% bot_operators(cfg))) {
        return(err(paste("Only an operator of this bot can hand work to",
                         "another room.")))
    }
    hits <- bot_resolve_room(rooms, room)
    if (!length(hits)) {
        return(err(bot_room_no_match(rooms, room)))
    }
    if (length(hits) > 1L) {
        return(err(sprintf(paste0("'%s' matches more than one room (%d). ",
                                  "Name one by its id:\n%s"),
                           room, length(hits),
                           bot_room_listing(hits, max = 25L))))
    }
    target_room <- hits[[1L]]
    if (identical(target_room$id, origin$room)) {
        return(err("That is this room. Call delegate without `room`."))
    }
    target <- new_session(target_room$id)
    if (!is.environment(target)) {
        return(err(sprintf("Could not open a session for %s.",
                           bot_room_label(target_room))))
    }
    # The room was matched, and will be described, by the directory its
    # topic names now. The job runs where the room's session works, and a
    # session takes its directory when it starts: a topic edited since
    # leaves the two apart until that session is replaced. Accepting the
    # job anyway would run it in the old project while saying the new.
    listed <- normalizePath(path.expand(target_room$cwd), mustWork = FALSE)
    if (!identical(listed, job_worker_dir(target))) {
        return(err(sprintf(paste0(
                                  "%s is set to work in %s, but its running session is still ",
                                  "working in %s, so nothing was handed over. The room takes ",
                                  "its directory when its session starts: /clear in that room ",
                                  "starts one in the new directory. Then ask again."),
                           bot_room_label(target_room), listed, job_worker_dir(target))))
    }
    # The default is the running room's: its project decides whether its
    # work is reviewed.
    review <- if (is.null(review)) {
        isTRUE(target$config$jobs$review)
    } else {
        isTRUE(review)
    }
    asking <- Filter(function(r) identical(r$id, origin$room), rooms)
    from_label <- if (length(asking)) {
        bot_room_label(asking[[1L]])
    } else {
        origin$room %||% "another room"
    }
    # `label`, not `room_name`: `$room` on a record holding only the
    # latter would match it by prefix and return a name as a room id.
    from <- list(session_key = job_worker_key(session), room = origin$room,
                 label = from_label)
    if (!is.null(origin$thread)) {
        from$thread <- origin$thread
    }
    label <- bot_room_label(target_room)
    id <- job_submit(target, task, requester = sender,
                     origin = list(room = target_room$id, label = label, from = from),
                     review = review)
    job <- job_read(id)
    waiting <- target$.job_current
    queued <- if (!is.null(waiting) && !identical(waiting, id)) {
        sprintf(" It is queued behind job %s.", waiting)
    } else {
        ""
    }
    notice <- sprintf(paste0("**Job %s accepted from %s**: %s\n\n",
                             "Requested there by %s. This room's doer runs ",
                             "it; the result will be posted here and in %s.%s"),
                      id, from_label, job_title(job, max_chars = 120L),
                      sender, from_label, queued)
    told <- bot_post_or_queue(chat, job$owner %||% job_worker_owner(target),
                              target_room$id, notice, session = target,
                              session_key = job_worker_key(target), job = id)
    # In the running room's talker's history whether or not the post went
    # out: asked what its doer is busy with, it has to be able to say.
    target$history <- c(target$history %||% list(),
                        list(list(role = "assistant", content = notice)))
    # What the talker is told about the notice is what happened to it.
    # Either way the job is accepted, and saying so plainly is what keeps
    # a talker from delegating it a second time.
    told_text <- if (is.null(told)) {
        paste("The notice to that room could not be posted just now and",
              "will be retried; the job is accepted all the same, so do",
              "not delegate it again.")
    } else {
        "That room has been told."
    }
    ok(sprintf(paste0("Handed to %s as job %s. That room's doer runs it in ",
                      "%s. %s The result will ",
                      "arrive here as its own message and is posted there ",
                      "too; tell the user which room took it, and do not ",
                      "answer the delegated question yourself.%s%s"),
               label, id, job$workspace, told_text, queued,
            if (review) {
                " A review of the work will follow it."
            } else {
                ""
            }))
}

# The hand-off for one turn: what `delegate` calls when it is given a
# room. Built per turn and removed after it (bot_poll()), with the
# turn's own values forced here, so it never outlives the client and the
# sender it was built with.
bot_job_handoff_fn <- function(sessions, session, cfg, chat, sender, origin,
                               new_session) {
    force(sessions)
    force(session)
    force(cfg)
    force(chat)
    force(sender)
    force(origin)
    force(new_session)
    function(room, task, review = NULL) {
        bot_job_handoff(sessions, session, cfg, chat, sender, origin,
                        room = room, task = task, review = review,
                        new_session = new_session)
    }
}

bot_job_result_text <- function(job) {
    head <- sprintf("**Job %s %s**: %s", job$id, job$status,
                    job_title(job, max_chars = 120L))
    o <- job$outcome
    body <- switch(job$status,
                   done = o$result %||% "",
                   failed = o$error %||% o$reason %||% "",
                   o$reason %||% "")
    note <- if (identical(job$status, "done") && !is.null(o$reason)) {
        paste0("\n\n_", o$reason, "_")
    } else {
        ""
    }
    notes <- job_outcome_notes(job)
    paste0(head, if (nzchar(body)) paste0("\n\n", body) else "", note,
        if (length(notes)) paste0("\n\n", paste(notes, collapse = "\n")))
}

bot_job_restored_text <- function(restore) {
    # Closed for sitting idle, between jobs: nothing was interrupted.
    if (isTRUE(restore$retired)) {
        return(sprintf(paste("The job worker had been closed after sitting",
                             "idle. It is running again with its workspace",
                             "from the end of job %s (%d objects); objects",
                             "holding connections or external pointers do",
                             "not survive that."),
                       restore$job %||% "?", length(restore$objects)))
    }
    sprintf(paste("The job worker restarted and restored its workspace from",
                  "the end of job %s (%d objects). Anything an unfinished",
                  "job held in memory is gone; objects holding connections",
                  "or external pointers do not survive a restore."),
            restore$job %||% "?", length(restore$objects))
}

# An approval request from a running job.
#
# A room's workers are supervised (R/supervisor.R), so the request says
# who should answer it. One the rules in code left open (`route` is
# "monitor") goes to the session's monitor when the bot is configured
# to approve without asking (`auto_approve_asks`); the answer is
# collected by bot_monitor_answer() on a later step, and nothing waits
# for it here. Every other request goes to a person: one the rules
# caught, one from a room that asks, and one that names no route.
#
# `auto_approve_asks` used to answer every request "yes" on the spot.
# Nothing does that now.
bot_job_approval <- function(chat, cfg, s, ev) {
    job <- ev$job
    req <- ev$request
    if (identical(req$route, "monitor") && isTRUE(cfg$auto_approve_asks)) {
        asked <- tryCatch({
            job_monitor_ask(s, bot_monitor_question(job, req))
            TRUE
        }, error = function(e) conditionMessage(e))
        if (isTRUE(asked)) {
            return(invisible(NULL))
        }
        req$reason <- sprintf("the monitor could not be asked (%s); policy said: %s",
                              asked, req$reason %||% "ask")
    }
    bot_job_ask_person(chat, cfg, s, job, req)
}

# What the monitor is asked about a job's request.
bot_monitor_question <- function(job, req) {
    list(scope = job$id, request_id = req$id, job = job$id, req = req,
         goal = job$task %||% "", tool = req$tool, args = req$args,
         reason = req$reason, notes = req$notes,
         earlier = tryCatch(job_monitor_earlier(job$id),
                            error = function(e) character()))
}

# Collect the monitor's answer for this session, if it has one. An
# approval or a refusal answers the job's request. Anything else (the
# monitor passed the call on, failed, or ran out of time) becomes a
# prompt for a person, with the monitor's reason.
bot_monitor_answer <- function(chat, cfg, s) {
    v <- tryCatch(job_monitor_poll(s), error = function(e) {
        p <- s$.job_monitor_pending
        s$.job_monitor_pending <- NULL
        if (is.null(p)) {
            return(NULL)
        }
        list(verdict = "escalate", q = p$q,
             reason = paste("the monitor failed:", conditionMessage(e)))
    })
    if (is.null(v)) {
        return(invisible(NULL))
    }
    q <- v$q
    if (v$verdict %in% c("approve", "refuse")) {
        tryCatch(job_answer(s, q$job, q$request_id,
                            identical(v$verdict, "approve"), by = "monitor",
                            reason = v$reason),
                 error = function(e) FALSE)
        return(invisible(v))
    }
    # The request may have closed while the monitor worked: the job was
    # cancelled, or the worker stopped waiting.
    open <- vapply(tryCatch(job_approval_pending(q$job),
                            error = function(e) list()),
                   function(r) r$id, character(1))
    if (q$request_id %in% open) {
        req <- q$req
        req$reason <- sprintf("the monitor passed this to you: %s",
                              v$reason %||% "no reason given")
        bot_job_ask_person(chat, cfg, s, job_read(q$job), req)
    }
    invisible(v)
}

# Seconds a request's worker will still wait for an answer.
bot_job_request_remaining <- function(s, req) {
    total <- as.numeric(s$config$jobs$approval_timeout_sec %||% 600)
    asked <- tryCatch(as.POSIXct(req$requested_at,
                                 format = "%Y-%m-%dT%H:%M:%OS%z"),
                      error = function(e) NA)
    if (length(asked) != 1L || is.na(asked)) {
        return(as.integer(total))
    }
    spent <- as.numeric(difftime(Sys.time(), asked, units = "secs"))
    as.integer(max(total - spent, 0))
}

# Ask a person to answer a job's request. The approvers are worked out
# as for a turn's approval (bot_approvers()), a prompt is posted with
# both reactions seeded, and the prompt is remembered on the session
# until a reaction answers it.
bot_job_ask_person <- function(chat, cfg, s, job, req) {
    room <- job$origin$room %||% s$room_id
    self_id <- tryCatch(chat.api::chat_whoami(chat)$id,
                        error = function(e) cfg$user_id)
    members <- tryCatch(chat.api::chat_members(chat, room),
                        error = function(e) NULL)
    approvers <- bot_approvers(cfg, members, bot_known_bots(cfg, self_id))
    call <- list(tool = req$tool, args = req$args)
    if (!length(approvers)) {
        job_answer(s, job$id, req$id, FALSE, by = "no approver")
        bot_job_send(chat, s, job, bot_no_approver_notice(call))
        return(invisible(NULL))
    }
    text <- paste0(sprintf("Job %s: ", job$id),
                   bot_approval_prompt(call, list(reason = req$reason),
                                       timeout_sec = bot_job_request_remaining(s, req)))
    eid <- bot_job_send(chat, s, job, text)
    if (is.null(eid)) {
        # Nobody can see a prompt that was never posted.
        job_answer(s, job$id, req$id, FALSE, by = "prompt not delivered")
        return(invisible(NULL))
    }
    for (k in c(intToUtf8(0x1F44D), intToUtf8(0x1F44E))) {
        tryCatch(chat.api::chat_react(chat, room, eid, k),
                 error = function(e) NULL)
    }
    prompts <- s$.job_prompts %||% list()
    prompts[[eid]] <- list(job = job$id, req = req$id, room = room,
                           approvers = approvers)
    s$.job_prompts <- prompts
    invisible(eid)
}

# Answer job approval prompts from this poll's reactions. Uses the same
# verdict reader as a turn's approval, so the same rules hold: only an
# approver counts, the bot's own seeds never do, first verdict wins.
bot_handle_job_reactions <- function(reactions, sessions, chat, cfg) {
    if (!length(reactions)) {
        return(invisible(0L))
    }
    answered <- 0L
    for (s in bot_sessions_list(sessions)) {
        prompts <- s$.job_prompts
        for (eid in names(prompts)) {
            p <- prompts[[eid]]
            verdict <- bot_reaction_verdict(reactions, p$room, eid,
                bot_approve_keys(cfg), bot_deny_keys(cfg), p$approvers)
            if (is.null(verdict)) {
                next
            }
            who <- bot_job_reactor(reactions, p, eid, cfg)
            recorded <- tryCatch(job_answer(s, p$job, p$req, verdict, by = who),
                                 error = function(e) FALSE)
            prompts[[eid]] <- NULL
            answered <- answered + 1L
            if (!isTRUE(recorded)) {
                job <- job_read(p$job)
                bot_job_send(chat, s, job, sprintf(
                        "Job %s: that answer came too late; the request had already closed.",
                        p$job))
            }
        }
        s$.job_prompts <- prompts
    }
    invisible(answered)
}

# Who gave the verdict, for the record: the first approver whose
# reaction on this prompt carries a verdict key.
bot_job_reactor <- function(reactions, p, eid, cfg) {
    keys <- c(bot_approve_keys(cfg), bot_deny_keys(cfg))
    for (r in reactions) {
        if (!isTRUE(r$self) && identical(r$target, eid) &&
            isTRUE(r$sender %in% p$approvers) && r$key %in% keys) {
            return(r$sender)
        }
    }
    "unknown"
}

# On startup: classify jobs a previous process left unfinished and tell
# each originating room what was found. Queued jobs never reached a
# worker, so they stay queued and run once their room's session is
# pumped (startup backfill creates it for joined rooms). Dispatched ones
# are indeterminate and are not re-run.
bot_recover_jobs <- function(chat, owner) {
    v <- tryCatch(job_recover(owner = owner), error = function(e) {
        message("bot_run: job recovery failed: ", conditionMessage(e))
        NULL
    })
    if (is.null(v) || !nrow(v)) {
        return(invisible(v))
    }
    for (i in seq_len(nrow(v))) {
        job <- job_read(v$id[[i]])
        room <- job$origin$room
        if (is.null(room) || is.null(chat)) {
            next
        }
        text <- if (identical(v$verdict[[i]], "indeterminate")) {
            sprintf(paste("Job %s was interrupted by a restart after it",
                          "started (%s). It may have partly run, so it",
                          "will not be re-run; ask again if you want it."),
                    job$id, .sanitize_inline(job$task, max_chars = 80L))
        } else {
            sprintf(paste("Job %s had not started before a restart (%s).",
                          "It is still queued and will run."),
                    job$id, .sanitize_inline(job$task, max_chars = 80L))
        }
        tryCatch(bot_reply_send(chat, room, text, thread = job$origin$thread),
                 error = function(e) NULL)
        # A job handed over from another room: the people who asked for
        # it are waiting there, not here.
        from <- job$origin$from
        if (!is.null(from$room)) {
            tryCatch(bot_reply_send(chat, from$room, text,
                                    thread = from$thread),
                     error = function(e) NULL)
        }
    }
    message(sprintf("bot_run: recovered %d unfinished job(s)", nrow(v)))
    invisible(v)
}
