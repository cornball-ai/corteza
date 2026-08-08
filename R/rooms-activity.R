# The activity trail a room sees while the agent works.
#
# A terminal shows tool calls as they happen; a room used to show
# nothing between the typing indicator and a wall of prose, so a long
# turn was indistinguishable from a hung one. This accumulates what the
# agent did and renders it as a single collapsible line -- "Ran 3
# commands, read 2 files" -- that a reader can open if they care.
#
# Two costs shape every decision here, and neither applies to a
# terminal. A Matrix edit is an ordinary timeline event, so it is rate
# limited alongside real messages, and it is permanent: only the latest
# renders, but every frame stays in the room forever and comes back out
# of chat_history() on the next restart. So tool calls are coalesced
# into one frame per interval rather than reported as they happen.
# Liveness is the typing indicator's job -- that is an ephemeral EDU,
# costs nothing, and is already sent.
#
# The trail goes out as a notice (m.notice) while replies stay ordinary
# messages. That is what the msgtype is for: automated output another
# bot should not answer. It is not a mute -- the spec asks clients not
# to auto-respond, and says nothing about whether a push gateway rings
# a phone, so a room that wants silence still needs a push rule or a
# mute. Worth having anyway, because a room with several agents in it
# is exactly where a progress trail would otherwise start a
# conversation with itself.

# Seconds of tool calls to coalesce into one frame.
#
# The interval, not the content check, is what bounds the cost. Almost
# every completed tool call changes the summary -- "Ran 3 commands"
# becomes "Ran 4 commands" -- so a content check alone filters very
# little and a trail would cost roughly one permanent event per tool
# call. At fifteen seconds a ten-minute turn spends at most forty.
#
# Not a timer. Nothing runs between tool calls, so a frame goes out on
# the first completed call after the interval has elapsed rather than
# at the instant it does. A quiet stretch simply has nothing to report,
# and the final flush covers whatever the last interval did not.
#
# Set `activity_interval` in the config to change it. Zero or less
# disables coalescing entirely, which is a debugging setting rather than
# a supported one -- it costs an event per tool call and will be
# throttled on any homeserver with default rate limits.
.ROOMS_ACTIVITY_INTERVAL <- 15

rooms_activity_interval <- function(cfg = NULL) {
    v <- suppressWarnings(as.numeric(cfg$activity_interval %||%
                                     .ROOMS_ACTIVITY_INTERVAL))
    if (length(v) != 1L || is.na(v)) {
        return(.ROOMS_ACTIVITY_INTERVAL)
    }
    v
}

# What each tool counts as in the summary. The vocabulary is the
# reader's, not the tool registry's: someone watching a room wants to
# know the agent ran things, read things, or changed things, and does
# not need list_files and grep_files told apart.
rooms_activity_category <- function(tool) {
    switch(tool %||% "",
           bash = "ran", cmd = "ran", run_r = "ran", run_r_script = "ran",
           git_status = "ran", git_diff = "ran", git_log = "ran",
           read_file = "read", "base::readLines" = "read", r_help = "read",
           installed_packages = "read",
           write_file = "edited", "base::writeLines" = "edited",
           replace_in_file = "edited",
           list_files = "searched", "base::list.files" = "searched",
           grep_files = "searched", web_search = "searched",
           fetch_url = "searched",
           # An unknown tool is still work, and "did 1 thing" beats
           # dropping it: a summary that silently omits what the agent
           # did is worse than a vague one.
           "used")
}

# A fresh accumulator for one turn. An environment because the observer
# writes to it from inside turn(), which has no way to hand anything
# back.
rooms_activity_new <- function() {
    acc <- new.env(parent = emptyenv())
    acc$events <- list()
    acc$message_id <- NULL
    acc$last_text <- NULL
    acc$last_at <- NULL
    acc
}

# Records completed tool calls onto `acc`, and pushes the trail to the
# room when it is due. Registered with add_observer(), which fires this
# for every tool call in the turn.
#
# Flushing from inside the observer rather than from a timer elsewhere,
# because the observer is the only thing that runs during a turn:
# bot_run_turn_in_cwd() does not return until the agent is finished, so
# there is nowhere else to put it.
rooms_activity_observer <- function(acc, chat = NULL, channel = NULL,
                                    interval = .ROOMS_ACTIVITY_INTERVAL) {
    function(event) {
        # "start" fires before the call runs and carries no result, so
        # counting it would double every tool. The three terminal
        # outcomes are what happened.
        if (!(identical(event$outcome, "ran") ||
                identical(event$outcome, "deny") ||
                identical(event$outcome, "declined"))) {
            return(invisible(NULL))
        }
        tool <- event$call$tool %||% event$tool %||% ""
        acc$events[[length(acc$events) + 1L]] <- list(
            tool = tool,
            category = rooms_activity_category(tool),
            success = isTRUE(event$success),
            refused = identical(event$outcome, "deny") ||
            identical(event$outcome, "declined"),
            added = event$diff$added %||% 0L,
            removed = event$diff$removed %||% 0L)
        if (!is.null(chat) && !is.null(channel) &&
            rooms_activity_due(acc, min_interval = interval)) {
            rooms_activity_flush(acc, chat, channel)
        }
        invisible(NULL)
    }
}

# Can this client replace a message it has already sent?
#
# Asked before the first frame, not discovered on the second. An
# encrypted Matrix room refuses edits -- the replacement text would ride
# in an ordinary event -- so a trail posted there would put up "Ran a
# command", have every update after it rejected, and leave that first
# frame on screen for the rest of the turn. The observer catches those
# errors, so nothing would be logged and nothing would look wrong: the
# room would simply be lying about what the agent had done.
rooms_activity_live <- function(chat) {
    isTRUE(tryCatch(chat.api::chat_capabilities(chat)$edits,
                    error = function(e) FALSE))
}

# Run `expr` with the activity trail attached to `session`, then detach
# it and show the final state.
#
# Attached per turn and removed after, because a session outlives the
# turn: a leftover observer would keep appending to an accumulator whose
# message was already finalized, and the next turn's first tool call
# would silently edit the previous turn's trail.
#
# The final flush goes through rooms_activity_flush() rather than the
# observer's gate, so it ignores the interval floor: the last frame is
# the one a reader is left looking at, and stopping five seconds short
# of the truth to save an event is the wrong trade. It still skips a
# frame identical to the last, which is the common case on a turn whose
# final tool call was a while ago.
#
# Where edits are refused there are no intermediate frames at all, only
# that final one. A room that cannot be kept current gets one accurate
# message instead of a stale one, which is the whole difference between
# a summary and a lie.
rooms_with_activity <- function(session, chat, channel, expr, cfg = NULL) {
    acc <- rooms_activity_new()
    before <- session$on_tool
    live <- rooms_activity_live(chat)
    add_observer(session, rooms_activity_observer(acc,
            chat = if (live) chat else NULL,
            channel = if (live) channel else NULL,
            interval = rooms_activity_interval(cfg)))
    on.exit({
        session$on_tool <- before
        if (length(acc$events)) {
            rooms_activity_flush(acc, chat, channel)
        }
    }, add = TRUE)
    expr()
}

# "a command" for one, "3 commands" for more. The article rather than
# the digit is deliberate: "Ran 1 command" reads like a machine counting,
# and this line is for a human glancing at a room.
rooms_activity_count <- function(n, noun) {
    if (n == 1L) {
        sprintf("a %s", noun)
    } else {
        sprintf("%d %ss", n, noun)
    }
}

# "Ran 3 commands, read 2 files". Ordered by how much a reader cares
# rather than by when it happened: an edit is the thing worth noticing,
# and a turn that edited six files and read one should not lead with the
# read.
rooms_activity_summary <- function(events) {
    if (!length(events)) {
        return("")
    }
    cats <- vapply(events, function(e) e$category, character(1))
    order <- c(edited = "file", ran = "command", read = "file",
               searched = "search", used = "tool")
    parts <- character()
    for (cat in names(order)) {
        n <- sum(cats == cat)
        if (n > 0L) {
            parts <- c(parts, sprintf("%s %s", cat,
                                      rooms_activity_count(n, order[[cat]])))
        }
    }
    out <- paste(parts, collapse = ", ")
    paste0(toupper(substr(out, 1L, 1L)), substr(out, 2L, nchar(out)))
}

# Total lines added and removed across the turn's edits.
rooms_activity_stats <- function(events) {
    list(added = sum(vapply(events, function(e) as.integer(e$added %||% 0L),
                            integer(1))),
         removed = sum(vapply(events, function(e)
                              as.integer(e$removed %||% 0L), integer(1))))
}

# The plain-text body: what a client with no HTML shows, and what a push
# notification carries. It has to stand alone.
rooms_activity_text <- function(events) {
    summary <- rooms_activity_summary(events)
    if (!nzchar(summary)) {
        return("")
    }
    s <- rooms_activity_stats(events)
    if (s$added > 0L || s$removed > 0L) {
        summary <- sprintf("%s  +%d -%d", summary, s$added, s$removed)
    }
    refused <- sum(vapply(events, function(e) isTRUE(e$refused), logical(1)))
    if (refused > 0L) {
        summary <- sprintf("%s (%s refused)", summary, refused)
    }
    summary
}

# The collapsed row, as Matrix HTML. <details> and <summary> are both in
# the spec's allowed subset and Element renders them natively, so this
# is a real disclosure widget rather than a wall of text with a heading.
rooms_activity_html <- function(events) {
    if (!length(events)) {
        return("")
    }
    s <- rooms_activity_stats(events)
    head <- rooms_escape_html(rooms_activity_summary(events))
    if (s$added > 0L || s$removed > 0L) {
        # font/color is what the spec allows; a CSS style attribute is
        # stripped by every client that follows it.
        head <- sprintf('%s <font color="#2da44e">+%d</font> <font color="#cf222e">-%d</font>',
                        head, s$added, s$removed)
    }
    items <- vapply(events, function(e) {
        label <- rooms_escape_html(cli_tool_label(e$tool))
        if (isTRUE(e$refused)) {
            sprintf("<li>%s — refused</li>", label)
        } else if (!isTRUE(e$success)) {
            sprintf("<li>%s — failed</li>", label)
        } else {
            sprintf("<li>%s</li>", label)
        }
    }, character(1))
    sprintf("<details><summary>%s</summary><ul>%s</ul></details>", head,
            paste(items, collapse = ""))
}

# The five characters that change meaning inside markup. Tool labels are
# built from model-supplied tool names, so an unescaped one could close
# the summary element and write its own markup into the room.
rooms_escape_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    gsub("'", "&#39;", x, fixed = TRUE)
}

# Should the trail be pushed to the room right now?
#
# Content first, then the floor. A timer alone spends the burst budget
# on frames that say what the last one said; content alone can fire
# twice in a second when two tools finish together.
rooms_activity_due <- function(acc, now = Sys.time(),
                               min_interval = .ROOMS_ACTIVITY_INTERVAL) {
    text <- rooms_activity_text(acc$events)
    if (!nzchar(text) || identical(text, acc$last_text)) {
        return(FALSE)
    }
    is.null(acc$last_at) ||
    as.numeric(difftime(now, acc$last_at, units = "secs")) >= min_interval
}

# Post the trail, or edit the one already posted. Best-effort
# throughout: a room that cannot show progress is a cosmetic failure,
# and letting it take the reply with it would turn one into an outage.
# send and edit are injectable for tests, as elsewhere in this package.
# The alternative was registering S3 methods for a fake client class,
# which needs the generic in scope at registration time and fails
# differently depending on whether chat.api happens to be attached.
rooms_activity_flush <- function(acc, chat, channel, now = Sys.time(),
                                 send = NULL, edit = NULL) {
    text <- rooms_activity_text(acc$events)
    if (!nzchar(text)) {
        return(invisible(FALSE))
    }
    # Already said. The observer's gate would have caught this, but the
    # final flush deliberately bypasses that gate to get the last frame
    # out -- and on a turn whose last tool call was more than the floor
    # ago, the last frame is already on screen. Without this, every turn
    # ends by spending a permanent event re-sending its own text.
    if (identical(text, acc$last_text)) {
        return(invisible(FALSE))
    }
    html <- rooms_activity_html(acc$events)
    ok <- tryCatch({
        # rich carries the collapsible block; text is the fallback and
        # carries the same summary in one line, so a client that cannot
        # render <details> -- and the push notification, which never
        # can -- still says what the agent is doing.
        if (is.null(acc$message_id)) {
            send <- send %||% chat.api::chat_send
            id <- send(chat, channel, text, markup = "plain", rich = html,
                       kind = "notice")
            acc$message_id <- bot_event_id(id)
        } else {
            edit <- edit %||% chat.api::chat_edit
            edit(chat, channel, acc$message_id, text, markup = "plain",
                 rich = html, kind = "notice")
        }
        TRUE
    }, error = function(e) FALSE)
    if (ok) {
        acc$last_text <- text
        acc$last_at <- now
    }
    invisible(ok)
}
