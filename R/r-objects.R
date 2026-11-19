# The R objects a compaction leaves, told to the model.
#
# A compaction replaces the turns that made the session's R objects with
# a summary, which may not name them, while the objects stay where
# run_r put them. So the summary ends with a short list: each object's
# name, class, dimensions or length and size, then the handles still
# open. The model can then use an object instead of rebuilding it.
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

# One object: its name, class, shape (r_object_shape()) and size. An
# environment has no size: object.size() does not count what it holds.
r_object_line <- function(name, value) {
    parts <- c(paste(class(value), collapse = "/"), r_object_shape(value))
    if (!is.environment(value)) {
        parts <- c(parts, format(utils::object.size(value), units = "auto",
                                 standard = "SI"))
    }
    paste0("- ", name, ": ", paste(parts, collapse = ", "))
}

# An object's shape, read from its attributes and its storage, never
# from dim() or length(): both are generics, and a class's method may
# run a query or read a file. A data frame's rows and columns, an
# array's dimensions or a vector's length; NULL for anything else. A
# list with a class has no length here, since its storage need not say
# how many items the class holds (a POSIXlt date is a list of 11), and
# neither has an S4 object.
r_object_shape <- function(value) {
    if (isS4(value)) {
        return(NULL)
    }
    if (is.list(value) && inherits(value, "data.frame")) {
        return(sprintf("%s x %s", .row_names_info(value, 2L),
                       length(unclass(value))))
    }
    d <- attr(value, "dim", exact = TRUE)
    if (!is.null(d)) {
        return(paste(d, collapse = " x "))
    }
    if (is.atomic(value) || (is.list(value) && is.null(oldClass(value)))) {
        return(paste("length", format(length(unclass(value)),
                                      scientific = FALSE)))
    }
    NULL
}

# An object's line, or a note that it could not be read. An active
# binding is named and not read, since reading one runs its function.
r_object_entry <- function(name, envir) {
    tryCatch({
        if (bindingIsActive(name, envir)) {
            paste0("- ", name, ": an active binding, not read")
        } else {
            r_object_line(name, get(name, envir = envir, inherits = FALSE))
        }
    }, error = function(e) paste0("- ", name, ": (could not be read)"))
}

# The list a compaction's summary ends with, or "" when there is nothing
# to list or the objects are not the session's. At most `max` objects
# and `chars` characters; the rest are counted.
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
    lines <- character()
    used <- 0L
    for (name in names) {
        if (length(lines) >= max) {
            break
        }
        line <- r_object_entry(name, envir)
        if (used + nchar(line) > chars) {
            break
        }
        lines <- c(lines, line)
        used <- used + nchar(line) + 1L
    }
    out <- character()
    if (length(names)) {
        out <- c("R objects that run_r made, still in the session:", lines)
        rest <- length(names) - length(lines)
        if (rest > 0L) {
            out <- c(out, sprintf("And %d more; ls() lists them.", rest))
        }
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
