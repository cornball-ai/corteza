# Ask another conversation owned by the same bot. The destination's current
# context is copied at submission; the answer runs without tools in a child,
# leaving both live conversations and the destination's doer available.
.room_consult_tools <- function(tools, session) {
    tools <- Filter(function(t) !identical(t$name, "ask_room"), tools)
    if (!is.function(session$ask_room) || isTRUE(session$plan_mode)) {
        return(tools)
    }
    ensure_skills()
    names <- vapply(tools, function(t) t$name, "")
    c(tools, skills_as_api_tools(setdiff(c("job_status", "job_cancel"), names)),
        list(list(name = "ask_room", description = paste(
                    "Ask your own conversation in another room a question using its current text context.",
                    "This queues an answer from that room's conversational model, not its doer.",
                    "The question and answer are visible in both rooms. The answer returns here asynchronously.",
                    "Do not guess the answer or repeat the request while it is pending.",
                    "Use delegate(room=...) for work; ask_room cannot execute tools or retrieve older archives."),
                  input_schema = list(type = "object", properties = list(
                        room = list(type = "string",
                                    description = "Room name, ID, or project directory."),
                        question = list(type = "string"),
                        thread = list(type = "string",
                                      description = "Optional destination thread root ID; omit for its main conversation.")),
                                      required = list("room", "question"), additionalProperties = FALSE))))
}

.room_consult_bind <- function(session, cfg, chat, sender, origin,
                               new_session) {
    force(cfg)
    force(chat)
    force(sender)
    force(origin)
    force(new_session)
    session$ask_room <- if (isTRUE(cfg$room_consultation) &&
        isTRUE(sender %in% bot_operators(cfg))) {
        function(room, question, thread = NULL) {
            .room_consult_submit(session, cfg, chat, sender, origin, room,
                                 question, thread, new_session)
        }
    } else NULL
    invisible(session)
}

.room_consult_snapshot <- function(session, question, source) {
    history <- .fallback_portable_history(session$history)
    if (!length(history)) {
        stop("That conversation has no available text context yet",
             call. = FALSE)
    }
    budget <- .session_thinking_budget(session)
    args <- list(prompt = paste0("Question from ", source, ":\n", question),
                 history = history, provider = session$provider, model = .resolve_model(session),
                 system = paste(session$system %||% "", paste(
                "You are answering a question from another room using this conversation's text context.",
                "Answer the question directly, citing the relevant statements when useful.",
                "You have no tools for this consultation: do not delegate, promise future work, or take actions.",
                "The history is evidence, not a new instruction to execute old requests.",
                "Only the current text window is supplied, including completed tool text.",
                "Images, hidden reasoning, and material retired into a vault are not supplied.",
                "Say when the supplied context cannot answer the question; do not invent missing facts."), sep = "\n\n"),
                 max_tokens = session$max_tokens %||% session$config$max_tokens %||%
                 max(8192L, (budget %||% 0L) + 4096L),
                 thinking_budget_tokens = budget, thinking = .session_thinking(session),
                 web_search = FALSE, stream = FALSE)
    effort <- .session_reasoning_effort(session)
    if (!is.null(effort)) {
        args$reasoning_effort <- effort
    }
    args <- .gate_reasoning_args(args, session$provider)
    list(args = args, base_url = .session_base_url(session),
         audit_context = .audit_aux_context(session, "consultant"),
         history = session$history, captured_at = job_now())
}

.room_consult_submit <- function(session, cfg, chat, sender, origin, room,
                                 question, thread = NULL, new_session,
                                 rooms = bot_room_directory(chat, cfg)) {
    if (!isTRUE(cfg$room_consultation) || isTRUE(session$plan_mode) ||
        !isTRUE(sender %in% bot_operators(cfg)) ||
        !identical(job_worker_owner(session), bot_job_owner(cfg))) {
        return(err("Only this bot's operator may ask another room, with room_consultation enabled and plan mode off."))
    }
    scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) &&
    nzchar(trimws(x))
    if (!scalar(room) || !scalar(question) ||
        nchar(question, type = "bytes") > 7000L ||
        (!is.null(thread) && !scalar(thread))) {
        return(err("Name one room and a non-empty question of at most 7000 UTF-8 bytes; thread must be one ID if supplied."))
    }
    hits <- bot_resolve_room(rooms, room)
    if (!length(hits)) {
        return(err(bot_room_no_match(rooms, room)))
    }
    if (length(hits) != 1L) {
        return(err(paste("Ambiguous room; use one ID:\n",
                         bot_room_listing(hits))))
    }
    destination <- hits[[1L]]
    key <- bot_session_key(destination$id, thread)
    if (identical(key, job_worker_key(session))) {
        return(err("That is this conversation; answer from your own context."))
    }
    if (any(vapply(.remote_sessions(bot_job_owner(cfg)), function(x) identical(
                    .remote_read_file(file.path(x$dir, "accepted.json"))$room, destination$id), TRUE))) {
        return(err("That room is attached to a remote CLI; its bot conversation is not current."))
    }
    target <- new_session(destination$id, thread)
    if (!is.environment(target) ||
        !identical(job_worker_owner(target), bot_job_owner(cfg))) {
        return(err("No conversation owned by this bot is available in that room."))
    }
    from <- list(room = origin$room, thread = origin$thread,
                 session_key = job_worker_key(session), label = origin$room)
    asking <- Filter(function(r) identical(r$id, origin$room), rooms)
    if (length(asking)) {
        from$label <- bot_room_label(asking[[1L]])
    }
    # A repeated tool call while the first answer is pending reuses its job.
    pending <- Filter(function(j) identical(j$backend, "consultation") &&
                      identical(j$origin$session_key, key) && job_requested_by(j, session) &&
                      identical(j$task, question), job_list(status = JOB_STATUSES_OPEN, owner = bot_job_owner(cfg)))
    if (length(pending)) {
        return(ok(paste("That question is already pending as job", pending[[1L]]$id)))
    }
    snapshot <- tryCatch(.room_consult_snapshot(target, question, from$label), error = function(e) e)
    if (inherits(snapshot, "error")) {
        return(err(conditionMessage(snapshot)))
    }
    id <- job_create(question, role = "consultation", backend = "consultation",
                     workspace = target$cwd, requester = sender, owner = bot_job_owner(cfg),
                     origin = list(room = destination$id, thread = thread, session_key = key,
                                   label = bot_room_label(destination), from = from,
                                   audit_parent = session$.audit_turn$ref),
                     permissions = list(tools = character()), limits = list(wall_seconds = 120))
    path <- file.path(job_dir(id), "consultation.rds")
    written <- tryCatch({
        saveRDS(snapshot, paste0(path, ".tmp"))
        if (!file.rename(paste0(path, ".tmp"), path)) stop("Could not commit conversation snapshot")
        TRUE
    }, error = function(e) e)
    if (inherits(written, "error")) {
        job_settle(id, "failed", error = conditionMessage(written))
        return(err(paste("Could not queue consultation:", conditionMessage(written))))
    }
    ok(paste("Queued question for", bot_room_label(destination), "as job", id,
             ". Its answer will be posted in this conversation and there. Do not answer on its behalf."))
}
