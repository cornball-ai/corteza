# The commands each surface takes (WP20, the command audit). One row a
# name: what it is the same command as (`same_as`, empty for the name
# the command goes by), the argument it takes, a line saying what it
# does, and the surfaces that accept it:
#
# - `terminal`: the CLI and chat(), which share run_repl_loop()'s
#   dispatch;
# - `room`: a bot's room (bot_poll()'s command parsers);
# - `remote`: a room that controls a terminal session (.remote_input()
#   and .remote_bot_route()).
#
# `talker` says what the command acts on in a talker session (R/talker.R):
# the talker's conversation, the doer and its jobs, or both. /model and
# /effort are the doer's (R/model-controls.R): the talker follows /model
# only where its config names no model or provider of its own. Each name
# a room takes has one, since a room is a talker by default (WP20b); the
# terminal's others get theirs with its default (WP20c).
#
# An ACP client is offered from this table too, once WP26 says which
# rows. Nothing dispatches from it: each surface matches its names as
# before. test_commands.R holds each surface's names, and both help
# texts, to this table, so a command added in one place and not the
# others fails there.
corteza_commands <- function() {
    rows <- list(
                 .command("quit", "End the session"),
                 .command("exit", same_as = "quit"),
                 .command("q", same_as = "quit"),
                 .command("clear", "Start a new conversation; the transcript stays",
                          room = TRUE, remote = TRUE, talker = "both"),
                 .command("reset", same_as = "clear", room = TRUE, remote = TRUE),
                 .command("new", same_as = "clear", room = TRUE, remote = TRUE),
                 .command("help", "List the commands"),
                 .command("jobs", "List the conversation's delegated jobs, or show one",
                          "[id]"),
                 .command("job", "Continue an incomplete job as a new attempt",
                          "resume <id>", room = TRUE, remote = TRUE, talker = "doer"),
                 .command("cancel", "Cancel a delegated job", "<id>"),
                 .command("copy", "Copy the last reply to the clipboard"),
                 .command("tasks", "Show the task list, or clear it", "[clear]"),
                 .command("tools", "List the session's tools"),
                 .command("model", "List the models, or switch to one",
                          "[name [provider] | N]", room = TRUE, talker = "doer"),
                 .command("effort", "Show or set the reasoning effort",
                          "[level | default]", room = TRUE, talker = "doer"),
                 .command("provider", "Switch provider", "<name>"),
                 .command("spawn", "Start a subagent",
                          "<task> [--model m] [--preset p] [--tools a,b]"),
                 .command("agents", "List the subagents"),
                 .command("ask", "Ask a subagent, and wait up to 60 seconds",
                          "<id> <prompt>"),
                 .command("queue", "Ask a subagent without waiting", "<id> <prompt>"),
                 .command("collect", "Collect a subagent's reply", "<id>"),
                 .command("kill", "End a subagent", "<id>"),
                 .command("sessions", "List this directory's sessions, or every directory's",
                          "[all]"),
                 .command("trace", "Show the last N tool calls", "[N]"),
                 .command("permissions", "Show or switch how tool calls are approved",
                          "[allow | ask | deny | review]"),
                 .command("trust", "Show whether this directory is trusted, or trust it",
                          "[yes]", room = TRUE, talker = "both"),
                 .command("auto", "Work on the task unattended, or on a new goal",
                          "[--exec] [goal] | status | resume", remote = TRUE),
                 .command("max-tokens", "Show or change the reply's output budget",
                          "[N | default]", remote = TRUE),
                 .command("remote-control", paste("Continue this session from a room;",
                "there, off ends it and status reports it"),
                          "[--bot ID] [room] | off | status", remote = TRUE),
                 .command("rc", same_as = "remote-control", remote = TRUE),
                 .command("dryrun", "Turn dry-run mode on or off"),
                 .command("paste", "Take several lines as one message, up to /end",
                          "[text]"),
                 .command("plan", "Turn plan mode on or off", "[task]"),
                 .command("spent", "Show what this run has spent"),
                 .command("cost", same_as = "spent"),
                 .command("context-edit", "Control conversation context editing",
                          "on | off | status", room = TRUE, talker = "both"),
                 .command("status", "Show the model, the directory and the context in use",
                          room = TRUE, talker = "both"),
                 .command("context", same_as = "status"),
                 .command("compact", "Summarize the conversation to free context"),
                 .command("refine", "Turn the conversation into one-line lessons",
                          "[list | rollback <id> | promote <id>]"),
                 .command("flush", "Write durable memories from the conversation"),
                 .command("doctor", "Check the provider, git and the context"),
                 .command("config", "Show the configuration in force"),
                 .command("last", "Show the output of a recent tool call", "[N]"),
                 .command("outputs", "List recent tool outputs"),
                 .command("diff", "Show the git diff against HEAD or a ref", "[ref]"),
                 .command("review", "Review local changes with the current model", "[ref]"),
                 .command("skill", "List, install, remove or test skills",
                          "list | install <path> | remove <name> | test <path>"),
                 .command("skills", same_as = "skill"),
                 .command("r", "Run R here, and stage its output for the next message",
                          "<expr>"),
                 .command("sidebar", "Move the recent turns to a private room (operators)",
                          "[N]", terminal = FALSE, room = TRUE, talker = "talker"),
                 .command("approve-all",
                          "Approve a job's remaining requests (operators)",
                          "[on | off | status] [job]", terminal = FALSE, room = TRUE,
                          talker = "doer"),
                 .command("cd", "Show where the room works, or move it (operators)",
                          "[<dir> | none | default]", terminal = FALSE, room = TRUE,
                          talker = "both"),
                 .command("ls", "List the project roots, or the directories in one (operators)",
                          "[dir]", terminal = FALSE, room = TRUE, talker = "talker"),
                 .command("stop", "Stop at the next tool call", terminal = FALSE,
                          remote = TRUE),
                 .command("approve", "Approve a pending request (operators)",
                          "<request-id>", terminal = FALSE, remote = TRUE),
                 .command("deny", "Refuse a pending request (operators)", "<request-id>",
                          terminal = FALSE, remote = TRUE)
    )
    table <- do.call(rbind, rows)
    # A name another command goes by takes that command's line and
    # argument.
    alias <- nzchar(table$same_as)
    main <- match(table$same_as[alias], table$name)
    table$description[alias] <- table$description[main]
    table$argument[alias] <- table$argument[main]
    table$talker[alias] <- table$talker[main]
    rownames(table) <- NULL
    table
}

.command <- function(name, does = "", argument = "", same_as = "",
                     terminal = TRUE, room = FALSE, remote = FALSE,
                     talker = "") {
    data.frame(name = name, same_as = same_as, argument = argument,
               description = does, terminal = terminal, room = room,
               remote = remote, talker = talker, stringsAsFactors = FALSE)
}
