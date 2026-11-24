# The commands each surface takes (WP20, the command audit). One row a
# name: what it is the same command as (`same_as`, empty for the name
# the command goes by), the argument it takes, a line saying what it
# does, and the surfaces that accept it:
#
# - `terminal`: the CLI and chat(), which share run_repl_loop()'s
#   dispatch;
# - `room`: a bot's room (bot_poll()'s command parsers);
# - `remote`: a room that controls a terminal session (.remote_input()
#   and .remote_bot_route());
# - `acp`: an ACP session (.acp_command(), R/acp-commands.R), which
#   offers its client the rows that go by their own name, and answers
#   /clear and /cd with a sentence: a client starts a new session.
#
# `talker` says what the command acts on in a talker session (R/talker.R):
# the talker's conversation, the doer and its jobs, or both. /model,
# /provider switch the provider's pair; /effort and /max-tokens act on
# the doer (R/model-controls.R). Every row a surface takes declares its
# target when talker mode is enabled. test_repl_talker.R holds the
# terminal's to what each command does.
#
# Nothing dispatches from the table: each surface matches its names as
# before. test_commands.R holds each surface's names, and both help
# texts, to this table, so a command added in one place and not the
# others fails there. ACP's help is made from the table.
corteza_commands <- function() {
    rows <- list(
                 .command("quit", "End the session", talker = "both"),
                 .command("exit", same_as = "quit"),
                 .command("q", same_as = "quit"),
                 .command("clear", "Start a new conversation; the transcript stays",
                          room = TRUE, remote = TRUE, talker = "both"),
                 .command("reset", same_as = "clear", room = TRUE, remote = TRUE),
                 .command("new", same_as = "clear", room = TRUE, remote = TRUE),
                 .command("help", "List the commands", talker = "talker", acp = TRUE),
                 .command("guide", "Explore settings and alternatives", "[topic]", talker = "both"),
                 .command("jobs", "List the conversation's delegated jobs, or show one",
                          "[id]", talker = "doer"),
                 .command("job", "Continue an incomplete job as a new attempt",
                          "resume <id>", room = TRUE, remote = TRUE, talker = "doer"),
                 .command("cancel", "Cancel a delegated job", "<id>", talker = "doer"),
                 .command("copy", "Copy the last reply to the clipboard",
                          talker = "talker"),
                 .command("tasks", "Show the task list, or clear it", "[clear]",
                          talker = "talker", acp = TRUE),
                 .command("tools", "List the session's tools", talker = "talker",
                          acp = TRUE),
                 .command("model", "List the models, or switch to one",
                          "[name [provider] | N]", room = TRUE, talker = "both"),
                 .command("effort", "Show or set the reasoning effort",
                          "[level | default]", room = TRUE, talker = "doer"),
                 .command("provider", "Switch provider", "<name>", talker = "both"),
                 .command("spawn", "Start a subagent",
                          "<task> [--model m] [--preset p] [--tools a,b]",
                          talker = "talker"),
                 .command("agents", "List the subagents", talker = "talker"),
                 .command("ask", "Ask a subagent, and wait up to 60 seconds",
                          "<id> <prompt>", talker = "talker"),
                 .command("queue", "Ask a subagent without waiting", "<id> <prompt>",
                          talker = "talker"),
                 .command("collect", "Collect a subagent's reply", "<id>",
                          talker = "talker"),
                 .command("kill", "End a subagent", "<id>", talker = "talker"),
                 .command("sessions", "List this directory's sessions, or every directory's",
                          "[all]", talker = "talker"),
                 .command("trace", "Show the last N tool calls", "[N]",
                          talker = "talker"),
                 .command("permissions", "Show or switch how tool calls are approved",
                          "[allow | ask | deny | review]", talker = "both"),
                 .command("trust", "Show whether this directory is trusted, or trust it",
                          "[yes]", room = TRUE, talker = "both", acp = TRUE),
                 .command("auto", "Work on the task unattended, or on a new goal",
                          "[--exec] [goal] | status | resume", remote = TRUE,
                          talker = "talker"),
                 .command("max-tokens", "Show or change the reply's output budget",
                          "[N | default]", remote = TRUE, talker = "doer"),
                 .command("remote-control", paste("Continue this session from a room;",
                "there, off ends it and status reports it"),
                          "[--bot ID] [room] | off | status", remote = TRUE,
                          talker = "both"),
                 .command("rc", same_as = "remote-control", remote = TRUE),
                 .command("dryrun", "Turn dry-run mode on or off", talker = "both"),
                 .command("paste", "Take several lines as one message, up to /end",
                          "[text]", talker = "talker"),
                 .command("plan", "Turn plan mode on or off", "[task]", talker = "both"),
                 .command("spent", "Show what this run has spent", talker = "both",
                          acp = TRUE),
                 .command("cost", same_as = "spent", acp = TRUE),
                 .command("context-edit", "Control conversation context editing",
                          "on | off | status", room = TRUE, talker = "both"),
                 .command("status", "Show the model, the directory and the context in use",
                          room = TRUE, talker = "both", acp = TRUE),
                 .command("context", same_as = "status", acp = TRUE),
                 .command("compact", "Summarize the conversation to free context",
                          talker = "talker", acp = TRUE),
                 .command("refine", "Turn the conversation into one-line lessons",
                          "[list | rollback <id> | promote <id>]", talker = "talker"),
                 .command("flush", "Write durable memories from the conversation",
                          talker = "talker"),
                 .command("doctor", "Check the provider, git and the context",
                          talker = "both"),
                 .command("config", "Show the configuration in force", talker = "both"),
                 .command("last", "Show the output of a recent tool call", "[N]",
                          talker = "talker"),
                 .command("outputs", "List recent tool outputs", talker = "talker"),
                 .command("diff", "Show the git diff against HEAD or a ref", "[ref]",
                          talker = "both"),
                 .command("review", "Review local changes with the current model", "[ref]",
                          talker = "doer"),
                 .command("skill", "List, install, remove or test skills",
                          "list | install <path> | remove <name> | test <path>",
                          talker = "both"),
                 .command("skills", same_as = "skill"),
                 .command("r", "Run R here, and stage its output for the next message",
                          "<expr>", talker = "talker"),
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
                     talker = "", acp = FALSE) {
    data.frame(name = name, same_as = same_as, argument = argument,
               description = does, terminal = terminal, room = room,
               remote = remote, talker = talker, acp = acp,
               stringsAsFactors = FALSE)
}
