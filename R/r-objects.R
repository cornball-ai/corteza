# The R objects a compaction leaves, told to the model.
#
# A compaction replaces the turns that made the session's R objects with
# a summary, which may not name them, while the objects stay where
# run_r put them, whatever their size. So the summary ends with their
# names, then the handles still open. The model can then use an object
# instead of rebuilding it.
#
# Names only, from ls(), which reads no binding. Reading a value would
# run code the session did not ask for at that moment: an active
# binding's function, the expression delayedAssign() deferred, a class's
# own methods. So nothing about an object but its name is listed.
#
# Only where the session owns the environment run_r writes to: the
# terminal and chat(), whose run_r writes to the global environment of
# their R process, and a child (a subagent or a doer), which has a
# process of its own. A bot's rooms share the bot's global environment,
# so a room gets no list. Nor does a session whose run_r runs in a
# worker process (run_r_mode "worker"): its objects are over there.
#
# Only what run_r made: the names the global environment held before
# the session's first run_r are left out. The terminal's own script
# keeps its functions and settings there, and chat() runs in the
# person's own R session.

R_OBJECTS_MAX <- 30L
R_OBJECTS_CHARS <- 2000L

# Record what the global environment holds before the session's first
# run_r (.tool_run_r_session()). Later calls leave the record alone.
r_objects_mark <- function(session, envir = globalenv()) {
    if (is.environment(session) && is.null(session$r_objects_before)) {
        session$r_objects_before <- ls(envir)
    }
    invisible(NULL)
}

# Whether the objects in the global environment are this session's.
r_objects_owned <- function(session) {
    mode <- tryCatch(.run_r_mode(list(session = session)),
                     error = function(e) "unknown")
    if (!identical(mode, "in_process")) {
        return(FALSE)
    }
    isTRUE(session$is_subagent) ||
    (session$channel %||% "") %in% c("cli", "console")
}

# A name as R prints it: in backticks unless it is syntactic, so a name
# holding a comma or a line break cannot break the list.
r_object_label <- function(name) {
    if (identical(make.names(name), name)) {
        return(name)
    }
    paste0("`", encodeString(name), "`")
}

# The labels that fit in `max` entries and `chars` characters, in order.
r_objects_fit <- function(labels, max, chars) {
    used <- cumsum(nchar(labels) + 2L) - 2L
    labels[seq_along(labels) <= max & used <= chars]
}

# The list a compaction's summary ends with, or "" when there is nothing
# to list or the objects are not the session's. At most `max` names and
# `chars` characters of them; the rest are counted.
r_objects_note <- function(session, envir = globalenv(), max = R_OBJECTS_MAX,
                           chars = R_OBJECTS_CHARS) {
    if (!isTRUE(r_objects_owned(session))) {
        return("")
    }
    names <- character()
    if (!is.null(session$r_objects_before)) {
        names <- sort(setdiff(ls(envir), session$r_objects_before))
    }
    handles <- sort(list_handles(handle_store_for(envir)))
    if (!length(names) && !length(handles)) {
        return("")
    }
    out <- character()
    if (length(names)) {
        shown <- r_objects_fit(vapply(names, r_object_label, "",
                                      USE.NAMES = FALSE), max, chars)
        rest <- length(names) - length(shown)
        listed <- paste(shown, collapse = ", ")
        if (rest > 0L && length(shown)) {
            listed <- sprintf("%s, and %d more; ls() lists them", listed, rest)
        } else if (rest > 0L) {
            listed <- sprintf("%d, which ls() lists", rest)
        }
        out <- paste0("R objects that run_r made, still in the session: ",
                      listed, ".")
    }
    if (length(handles)) {
        shown <- utils::head(handles, max)
        more <- ""
        if (length(handles) > length(shown)) {
            more <- sprintf(", and %d more", length(handles) - length(shown))
        }
        out <- c(out, paste0("Open handles, which read_handle reads: ",
                             paste(shown, collapse = ", "), more, "."))
    }
    paste(out, collapse = "\n")
}
