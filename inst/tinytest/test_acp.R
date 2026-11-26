# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# `corteza acp` (R/acp.R, WP26): the Agent Client Protocol on standard
# input and output. The mappings are tested alone. The loop is driven in
# this process: a scripted client reads each line the agent writes and
# writes its next messages into the agent's input, and the model is a
# script too. No provider is called, and nothing is written outside the
# temporary folders each block makes.

ns <- asNamespace("corteza")

# ---- JSON-RPC and the mappings ---------------------------------------------
local({
    expect_true(isTRUE(ns$.acp_decode("{not json")$.unreadable))
    expect_true(isTRUE(ns$.acp_decode("[1, 2]")$.unreadable))
    expect_null(ns$.acp_decode("   "))
    expect_identical(ns$.acp_kind(list(jsonrpc = "2.0", id = 1L, method = "x")),
                     "request")
    expect_identical(ns$.acp_kind(list(jsonrpc = "2.0", method = "x")),
                     "notification")
    expect_identical(ns$.acp_kind(list(jsonrpc = "2.0", id = 1L, result = list())),
                     "response")
    expect_identical(ns$.acp_encode(ns$.acp_result(1L)),
                     '{"jsonrpc":"2.0","id":1,"result":{}}')
    expect_identical(ns$.acp_encode(ns$.acp_error(NULL, -32700L, "bad")),
                     '{"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"bad"}}')

    # A prompt's text: its text blocks, and a link to a file as its path.
    expect_identical(ns$.acp_prompt_text(list(
        list(type = "text", text = "Read this"),
        list(type = "resource_link", uri = "file:///tmp/a%20b.R", name = "a b.R"))),
        "Read this\n/tmp/a b.R")
    expect_error(ns$.acp_prompt_text(list(list(type = "image", data = "x",
                                               mimeType = "image/png"))),
                 "type 'image'")
    expect_error(ns$.acp_prompt_text(list()), "no content")

    expect_identical(ns$.acp_tool_kind(list(tool = "write_file", class = "write")),
                     "edit")
    expect_identical(ns$.acp_tool_kind(list(tool = "bash", class = "exec")),
                     "execute")
    expect_identical(ns$.acp_tool_kind(list(tool = "read_file", class = "read")),
                     "read")
    expect_identical(ns$.acp_tool_kind(list(tool = "grep_files", class = "read")),
                     "search")
    expect_identical(ns$.acp_tool_kind(list(tool = "fetch_url", class = "read")),
                     "fetch")
    expect_identical(ns$.acp_tool_kind(list(tool = "no_such_tool")), "other")
    # The title names what the call acts on, by the argument's full name,
    # and a path in the session's directory from there, however long
    # the directory.
    expect_identical(ns$.acp_tool_title(list(tool = "write_file",
        args = list(path = "/p/notes.md", content = "hi"))),
        "write_file: /p/notes.md")
    deep <- paste0("/", paste(rep("long-directory-name", 10L), collapse = "/"))
    expect_identical(ns$.acp_tool_title(list(tool = "write_file",
        args = list(path = file.path(deep, "R", "notes.md"))), cwd = deep),
        "write_file: R/notes.md")
    expect_identical(ns$.acp_tool_title(list(tool = "write_file",
        args = list(path = "/elsewhere/notes.md")), cwd = deep),
        "write_file: /elsewhere/notes.md")

    expect_identical(ns$.acp_stop_reason(list(status = "cancelled")), "cancelled")
    expect_identical(ns$.acp_stop_reason(list(status = "incomplete",
                                              reason = "max_turns")),
                     "max_turn_requests")
    expect_identical(ns$.acp_stop_reason(list(status = "incomplete",
                                              reason = "max_tokens")),
                     "max_tokens")
    expect_identical(ns$.acp_stop_reason(list(status = "done")), "end_turn")

    kinds <- function(o) vapply(o, function(x) x$kind, "")
    expect_identical(kinds(ns$.acp_permission_options()),
                     c("allow_once", "allow_always", "reject_once"))
    expect_identical(kinds(ns$.acp_permission_options(always = FALSE)),
                     c("allow_once", "reject_once"))

    # A result is cut for the client, and a diff goes as its own block.
    u <- ns$.acp_tool_done("call-1", TRUE, strrep("x", 5000L),
                           list(lines = c("--- a", "+++ b", "+x")))
    expect_identical(u$status, "completed")
    expect_true(nchar(u$content[[1L]]$content$text) < 4100L)
    expect_true(grepl("[cut here]", u$content[[1L]]$content$text, fixed = TRUE))
    expect_true(startsWith(u$content[[2L]]$content$text, "```diff"))
    expect_identical(ns$.acp_tool_done("c", FALSE, "no")$status, "failed")

    # A location is a file there or one its directory could hold.
    d <- tempfile("acp-loc")
    dir.create(d)
    locs <- ns$.acp_tool_locations(list(paths = c(file.path(d, "new.R"),
                                                  file.path(d, "gone", "x.R"))))
    expect_identical(length(locs), 1L)
    expect_identical(locs[[1L]]$path,
                     normalizePath(file.path(d, "new.R"), winslash = "/",
                                   mustWork = FALSE))
    unlink(d, recursive = TRUE)

    # Why a call asks, for a person: never the policy's bookkeeping.
    expect_identical(ns$.acp_ask_reason(list(reason = "default: random/write/cli")),
                     "")
    expect_true(grepl("credential path", ns$.acp_ask_reason(list(
        rule = "credential", reason = "safety: ~/.ssh/id_rsa is a credential path"))))
    expect_identical(ns$.acp_ask_reason(list(reason = "x",
        gate_reason = "The reviewer passed this to you: unsure")),
        "The reviewer passed this to you: unsure")

    expect_identical(ns$.acp_args(c("--provider", "openai", "--tools", "core,git")),
                     list(provider = "openai", model = NULL,
                          tools = c("core", "git")))
    expect_error(ns$.acp_args("--resume"), "takes --provider")
})

# corteza acp is not offered on Windows yet (R/acp-channel.R), and the
# loop's tests drive it through processx's pipe pair.
local({
    if (.Platform$OS.type == "windows") {
        expect_error(ns$.acp_channel(), "Linux and macOS")
    }
})
if (.Platform$OS.type == "windows") {
    exit_file("corteza acp is not offered on Windows yet")
}

# ---- The harness ----------------------------------------------------------
# Folders of its own for a block: user data and config, state, and a
# project. The provider's key is a stand-in; no request leaves.
acp_fixture <- function() {
    root <- tempfile("acp-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    vars <- c("R_USER_DATA_DIR", "R_USER_CONFIG_DIR", "CORTEZA_STATE_DIR",
              "ANTHROPIC_API_KEY")
    old <- Sys.getenv(vars, unset = NA)
    Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
               R_USER_CONFIG_DIR = file.path(root, "config"),
               CORTEZA_STATE_DIR = file.path(root, "state"),
               ANTHROPIC_API_KEY = "offline-fixture")
    proj <- file.path(root, "proj")
    dir.create(proj)
    list(root = root, proj = proj, restore = function() {
        for (v in vars) {
            if (is.na(old[[v]])) {
                Sys.unsetenv(v)
            } else {
                do.call(Sys.setenv, stats::setNames(list(old[[v]]), v))
            }
        }
        unlink(root, recursive = TRUE)
    })
}

# The model, as a script: each request takes the next step, a tool call
# (`tool`, `args`) or a reply (`text`, streamed as `deltas`). It keeps
# the messages it was sent.
fake_model <- function(steps) {
    st <- new.env()
    st$i <- 0L
    st$seen <- list()
    st$fn <- function(messages, tools, system, model, config = NULL,
                      cache = NULL, thinking_budget_tokens = NULL,
                      on_delta = NULL, thinking = NULL, ...) {
        # What a real request does before it is sent (llm.api's
        # .request_fetch()): its event at phase "start", where a stop
        # asked for ends the turn.
        llm <- asNamespace("llm.api")
        request <- list(id = sprintf("fake-%d", st$i + 1L), attempt = 1L,
                        observer = get(".request_runtime", envir = llm)$current,
                        wire = "messages", model = model)
        get(".request_notify", envir = llm)(request, "start",
                                            serialized_request = "{}")
        st$i <- st$i + 1L
        st$seen[[st$i]] <- messages
        step <- steps[[min(st$i, length(steps))]]
        if (is.function(step)) {
            step <- step()
        }
        usage <- list(input_tokens = 5L, output_tokens = 3L)
        if (!is.null(step$tool)) {
            id <- paste0("t", st$i)
            return(list(text = "", usage = usage,
                        tool_calls = list(list(id = id, name = step$tool,
                                               arguments = step$args)),
                        assistant_message = list(role = "assistant", content = list(
                            list(type = "tool_use", id = id, name = step$tool,
                                 input = step$args)))))
        }
        if (is.function(on_delta)) {
            deltas <- if (is.null(step$deltas)) step$text else step$deltas
            for (d in deltas) {
                on_delta(d)
            }
        }
        list(text = step$text, tool_calls = list(), usage = usage,
             assistant_message = list(role = "assistant", content = list(
                 list(type = "text", text = step$text))))
    }
    st
}

with_model <- function(model, code) {
    llm <- asNamespace("llm.api")
    old <- get(".agent_anthropic", envir = llm)
    utils::assignInNamespace(".agent_anthropic", model$fn, "llm.api")
    on.exit(utils::assignInNamespace(".agent_anthropic", old, "llm.api"),
            add = TRUE)
    force(code)
}

req <- function(id, method, params = ns$.acp_object()) {
    ns$.acp_request(as.integer(id), method, params)
}

# Run the loop with a scripted client. `react(msg, client)` sees each
# message the agent writes; `client$write()` sends one, and
# `client$finish()` closes the agent's input, which ends the loop. Every
# message the agent wrote comes back, in order.
acp_run <- function(first, react = function(msg, client) NULL) {
    p <- processx::conn_create_pipepair()
    out <- new.env()
    out$msgs <- list()
    out$done <- FALSE
    client <- list(
        write = function(msg) {
            line <- if (is.character(msg)) msg else ns$.acp_encode(msg)
            processx::conn_write(p[[2L]], paste0(line, "\n"))
        },
        finish = function() {
            if (!out$done) {
                out$done <- TRUE
                close(p[[2L]])
            }
        })
    send <- function(line) {
        msg <- jsonlite::fromJSON(line, simplifyVector = FALSE)
        out$msgs[[length(out$msgs) + 1L]] <- msg
        # A script that never finishes would hold the loop.
        if (length(out$msgs) > 300L) {
            client$finish()
        }
        react(msg, client)
    }
    for (m in first) {
        client$write(m)
    }
    ns$.acp_main(input = p[[1L]], send = send)
    out$msgs
}

answer <- function(msgs, id) {
    hit <- Filter(function(m) identical(m$id, as.integer(id)) &&
                      is.null(m$method), msgs)
    if (length(hit)) hit[[1L]] else NULL
}
at <- function(msgs, pred) {
    which(vapply(msgs, function(m) isTRUE(pred(m)), logical(1)))
}
is_answer <- function(id) function(m) identical(m$id, as.integer(id)) &&
    is.null(m$method)
is_update <- function(kind) function(m) identical(m$method, "session/update") &&
    identical(m$params$update$sessionUpdate, kind)
is_permission <- function(m) identical(m$method, "session/request_permission")
grant <- function(client, msg, option) {
    client$write(list(jsonrpc = "2.0", id = msg$id, result = list(
        outcome = list(outcome = "selected", optionId = option))))
}
start <- function(proj) {
    list(req(1L, "initialize", list(protocolVersion = 1L)),
         req(2L, "session/new", list(cwd = proj, mcpServers = list())))
}
prompt <- function(id, sid, text) {
    req(id, "session/prompt", list(sessionId = sid,
                                   prompt = list(list(type = "text", text = text))))
}

# ---- Before and around a session --------------------------------------------
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    nowhere <- file.path(fx$root, "nowhere")
    servers <- list(list(name = "files", command = "/usr/bin/files-mcp",
                         args = list(),
                         env = list(list(name = "TOKEN", value = "s3cret-value"))))
    msgs <- acp_run(list(
        req(9L, "session/list"),
        "{not json",
        req(1L, "initialize", list(protocolVersion = 2L)),
        req(2L, "initialize", list(protocolVersion = 1L)),
        req(3L, "session/new", list(cwd = "relative/dir", mcpServers = list())),
        req(4L, "session/new", list(cwd = nowhere, mcpServers = list())),
        req(5L, "session/new", list(cwd = fx$proj, mcpServers = servers)),
        req(6L, "no/such"),
        req(7L, "session/close", list(sessionId = "no-such")),
        req(8L, "session/list", list(cwd = fx$proj))),
        function(msg, client) if (is_answer(8L)(msg)) client$finish())
    expect_identical(answer(msgs, 9L)$error$code, -32600L)
    expect_true(any(vapply(msgs, function(m) {
        is.null(m$id) && identical(m$error$code, -32700L)
    }, logical(1))))
    init <- answer(msgs, 1L)$result
    # The version this agent takes, whatever the client asked for.
    expect_identical(init$protocolVersion, 1L)
    expect_false(init$agentCapabilities$loadSession)
    expect_false(init$agentCapabilities$promptCapabilities$image)
    expect_false(init$agentCapabilities$mcpCapabilities$http)
    expect_identical(sort(names(init$agentCapabilities$sessionCapabilities)),
                     c("close", "list", "resume"))
    expect_identical(init$agentInfo$name, "corteza")
    expect_identical(answer(msgs, 2L)$error$code, -32600L)
    expect_true(grepl("absolute", answer(msgs, 3L)$error$message))
    # A directory that is not there is refused, and none is made.
    expect_true(grepl("no directory", answer(msgs, 4L)$error$message))
    expect_false(dir.exists(nowhere))
    # MCP servers a client names are refused, by name alone.
    refused <- answer(msgs, 5L)$error
    expect_identical(refused$code, -32602L)
    expect_true(grepl("'files'", refused$message, fixed = TRUE))
    expect_false(grepl("s3cret-value", paste(unlist(msgs), collapse = " "),
                       fixed = TRUE))
    expect_identical(answer(msgs, 6L)$error$code, -32601L)
    # Closing a session that is not open answers as a close.
    expect_identical(length(answer(msgs, 7L)$result), 0L)
    expect_identical(length(answer(msgs, 8L)$result$sessions), 0L)
    # Nothing was written in the project.
    expect_identical(list.files(fx$proj, all.files = TRUE, no.. = TRUE),
                     character())
})

# ---- A prompt: a write that asks, answered each way ------------------------
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    target <- file.path(fx$proj, "notes.md")
    write <- list(tool = "write_file", args = list(path = target, content = "hi\n"))
    model <- fake_model(list(write, list(text = "Done.", deltas = c("Do", "ne."))))
    sid <- NULL
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Write the notes."))
        }
        if (is_permission(msg)) {
            grant(client, msg, "allow_once")
        }
        if (is_answer(3L)(msg)) {
            client$finish()
        }
    }))
    expect_true(is.character(sid) && nzchar(sid))
    expect_identical(answer(msgs, 2L)$result$modes$currentModeId, "ask")
    expect_identical(readLines(target), "hi")
    ask <- msgs[[at(msgs, is_permission)[1L]]]
    expect_identical(vapply(ask$params$options, function(o) o$kind, ""),
                     c("allow_once", "allow_always", "reject_once"))
    expect_identical(ask$params$toolCall$kind, "edit")
    expect_identical(ask$params$toolCall$title, "write_file: notes.md")
    # In order: the call announced pending, the question, the run, the
    # result, the reply, and then the answer.
    announced <- at(msgs, is_update("tool_call"))
    expect_identical(msgs[[announced]]$params$update$status, "pending")
    expect_identical(msgs[[announced]]$params$update$locations[[1L]]$path,
                     normalizePath(target, winslash = "/"))
    updates <- at(msgs, is_update("tool_call_update"))
    statuses <- vapply(msgs[updates], function(m) m$params$update$status, "")
    expect_identical(statuses, c("in_progress", "completed"))
    # The reply streams, in more than one chunk. Where the chunks break
    # is the redaction's: it holds back what could begin a secret.
    chunks <- at(msgs, is_update("agent_message_chunk"))
    expect_true(length(chunks) >= 2L)
    expect_identical(paste(vapply(msgs[chunks], function(m) {
        m$params$update$content$text
    }, ""), collapse = ""), "Done.")
    done <- at(msgs, is_answer(3L))
    expect_true(announced < at(msgs, is_permission)[1L])
    expect_true(max(c(updates, chunks)) < done)
    expect_identical(answer(msgs, 3L)$result$stopReason, "end_turn")
    # The prompt is in the transcript as what was typed, the line the
    # reviewer reads as the person's.
    record <- ns$transcript_load_strict(sid)
    users <- Filter(function(e) identical(e$role, "user"), record$entries)
    expect_identical(users[[1L]]$staged, FALSE)
    expect_identical(ns$review_conversation(sid)$lines[[1L]],
                     "Person: Write the notes.")
})

# A secret the session knows (the provider's key here) reaches the
# client hidden, in a tool call and in the reply, however the deltas
# split it.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    key <- Sys.getenv("ANTHROPIC_API_KEY")
    target <- file.path(fx$proj, "k.md")
    model <- fake_model(list(
        list(tool = "write_file", args = list(path = target, content = key)),
        list(text = paste0("The key is ", key, "."),
             deltas = c("The key is off", "line-fix", "ture."))))
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            client$write(prompt(3L, msg$result$sessionId, "Show the key."))
        }
        if (is_permission(msg)) {
            grant(client, msg, "allow_once")
        }
        if (is_answer(3L)(msg)) {
            client$finish()
        }
    }))
    everything <- paste(vapply(msgs, function(m) {
        as.character(jsonlite::toJSON(m, auto_unbox = TRUE, null = "null"))
    }, ""), collapse = "\n")
    expect_false(grepl(key, everything, fixed = TRUE))
    announced <- Filter(is_update("tool_call"), msgs)[[1L]]
    expect_identical(announced$params$update$rawInput$content, "[REDACTED]")
    chunks <- Filter(is_update("agent_message_chunk"), msgs)
    expect_identical(paste(vapply(chunks, function(m) {
        m$params$update$content$text
    }, ""), collapse = ""), "The key is [REDACTED].")
})

local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    target <- function(n) file.path(fx$proj, sprintf("f%d.md", n))
    write <- function(n) list(tool = "write_file",
                              args = list(path = target(n), content = "x\n"))
    model <- fake_model(list(write(1L), list(text = "No."),
                             write(2L), list(text = "Kept."),
                             write(3L), list(text = "Again.")))
    asked <- 0L
    sid <- NULL
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "One."))
        }
        if (is_permission(msg)) {
            asked <<- asked + 1L
            grant(client, msg, c("reject_once", "allow_always")[asked])
        }
        if (is_answer(3L)(msg)) {
            client$write(prompt(4L, sid, "Two."))
        }
        if (is_answer(4L)(msg)) {
            client$write(prompt(5L, sid, "Three."))
        }
        if (is_answer(5L)(msg)) {
            client$finish()
        }
    }))
    # Denied: the call fails and nothing is written; the turn goes on.
    expect_false(file.exists(target(1L)))
    first <- Filter(function(m) is_update("tool_call_update")(m) &&
                        identical(m$params$update$toolCallId, "call-1"), msgs)
    expect_identical(first[[length(first)]]$params$update$status, "failed")
    expect_identical(answer(msgs, 3L)$result$stopReason, "end_turn")
    # "Always" runs it and is kept for the directory, as at the terminal;
    # the next call of the tool asks nothing.
    expect_true(file.exists(target(2L)))
    expect_true("write_file" %in% ns$approvals_saved(fx$proj))
    expect_true(file.exists(target(3L)))
    expect_identical(asked, 2L)
    expect_identical(length(at(msgs, is_permission)), 2L)
})

# The credential rule's ask offers no "always", and a saved "always" for
# the tool does not answer it. Asked of the callback alone, so no call
# is made on a real credential path.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    expect_true(ns$approval_save(fx$proj, "write_file"))
    p <- processx::conn_create_pipepair()
    sent <- list()
    choice <- "allow_always"
    st <- NULL
    st <- ns$.acp_state(ns$.acp_channel(p[[1L]], function(line) {
        msg <- jsonlite::fromJSON(line, simplifyVector = FALSE)
        sent[[length(sent) + 1L]] <<- msg
        if (identical(msg$method, "session/request_permission")) {
            processx::conn_write(p[[2L]], paste0(ns$.acp_encode(list(
                jsonrpc = "2.0", id = msg$id, result = list(outcome = list(
                    outcome = "selected", optionId = choice)))), "\n"))
        }
    }))
    session <- new.env()
    session$.acp_calls <- 0L
    st$live <- new.env()
    st$live$sid <- "s1"
    st$live$session <- session
    st$live$cancelled <- FALSE
    st$live$call <- NULL
    cb <- ns$.acp_approval_cb(st, "s1", fx$proj)
    call <- list(tool = "write_file", args = list(path = "~/.ssh/config"))
    credential <- list(approval = "ask", rule = "credential",
                       reason = "safety: ~/.ssh/config is a credential path")
    # The saved answer covers an ordinary ask of the tool.
    expect_true(cb(call, list(approval = "ask", reason = "default")))
    expect_identical(length(sent), 0L)
    # Not this one: asked, without "always", and an "always" sent anyway
    # is no yes.
    expect_false(cb(call, credential))
    ask <- Filter(function(m) identical(m$method, "session/request_permission"),
                  sent)[[1L]]
    expect_identical(vapply(ask$params$options, function(o) o$kind, ""),
                     c("allow_once", "reject_once"))
    expect_true(grepl("credential path", ask$params$toolCall$title, fixed = TRUE))
    st$live$call <- NULL
    choice <- "allow_once"
    expect_true(cb(call, credential))
    # Leaving plan mode is asked the same way: a saved "always" for the
    # tool does not answer it, and the request offers none.
    expect_true(ns$approval_save(fx$proj, "exit_plan_mode"))
    leave <- list(tool = "exit_plan_mode", args = list(plan = "1. Edit a.R"))
    plan_exit <- ns$check_plan_exit(list(tool = "exit_plan_mode",
                                         context = list(plan_mode = TRUE)))
    expect_true(cb(leave, list(approval = "ask", reason = "default")))
    before <- length(sent)
    st$live$call <- NULL
    choice <- "allow_always"
    expect_false(cb(leave, plan_exit))
    ask <- Filter(function(m) identical(m$method, "session/request_permission"),
                  sent[-seq_len(before)])[[1L]]
    expect_identical(vapply(ask$params$options, function(o) o$kind, ""),
                     c("allow_once", "reject_once"))
    expect_true(grepl("yours to approve", ask$params$toolCall$title, fixed = TRUE))
    st$live$call <- NULL
    choice <- "allow_once"
    expect_true(cb(leave, plan_exit))
    close(p[[2L]])
})

# ---- Cancelling ----------------------------------------------------------
# While the reply streams: the stream stops, and the answer, after every
# update, is `cancelled`. The next prompt runs.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    model <- fake_model(list(list(text = "one two three",
                                  deltas = c("one ", "two ", "three")),
                             list(text = "Next.")))
    sid <- NULL
    sent_cancel <- FALSE
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Count."))
        }
        if (is_update("agent_message_chunk")(msg) && !sent_cancel) {
            sent_cancel <<- TRUE
            client$write(ns$.acp_notification("session/cancel",
                                              list(sessionId = sid)))
        }
        if (is_answer(3L)(msg)) {
            client$write(prompt(4L, sid, "Again."))
        }
        if (is_answer(4L)(msg)) {
            client$finish()
        }
    }))
    expect_identical(answer(msgs, 3L)$result$stopReason, "cancelled")
    first <- at(msgs, is_answer(3L))
    chunks <- at(msgs, is_update("agent_message_chunk"))
    before <- chunks[chunks < first]
    expect_true(length(before) >= 1L && length(before) < 3L)
    expect_identical(answer(msgs, 4L)$result$stopReason, "end_turn")
    # The next request was valid: its history ends with the new prompt.
    last <- model$seen[[length(model$seen)]]
    expect_identical(last[[length(last)]]$role, "user")
})

# While a permission request is open: the call is declined, the turn
# stops, and the call's last update comes before the answer. The
# client's `cancelled` answer that follows matches nothing open, and is
# dropped.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    target <- file.path(fx$proj, "never.md")
    model <- fake_model(list(list(tool = "write_file",
                                  args = list(path = target, content = "x")),
                             list(text = "Should not come.")))
    sid <- NULL
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Write it."))
        }
        if (is_permission(msg)) {
            client$write(ns$.acp_notification("session/cancel",
                                              list(sessionId = sid)))
            client$write(list(jsonrpc = "2.0", id = msg$id, result = list(
                outcome = list(outcome = "cancelled"))))
        }
        if (is_answer(3L)(msg)) {
            client$finish()
        }
    }))
    expect_identical(answer(msgs, 3L)$result$stopReason, "cancelled")
    expect_false(file.exists(target))
    expect_identical(model$i, 1L)
    last <- Filter(is_update("tool_call_update"), msgs)
    expect_identical(last[[length(last)]]$params$update$status, "failed")
    expect_true(max(at(msgs, is_update("tool_call_update"))) <
                    at(msgs, is_answer(3L)))
})

# During a long command: a list is answered while it runs; a cancel ends
# it, its call fails saying so, and the turn ends `cancelled` without
# asking the model again. Starts processes, so not on CRAN.
if (.Platform$OS.type != "windows" &&
    (tinytest::at_home() || identical(Sys.getenv("NOT_CRAN"), "true"))) {
    local({
        fx <- acp_fixture()
        on.exit(fx$restore(), add = TRUE)
        model <- fake_model(list(list(tool = "bash", args = list(
            command = "sleep 30", timeout = 60L)), list(text = "Never.")))
        sid <- NULL
        began <- NULL
        msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
            if (is_answer(2L)(msg)) {
                sid <<- msg$result$sessionId
                client$write(prompt(3L, sid, "Wait."))
            }
            if (is_permission(msg)) {
                grant(client, msg, "allow_once")
            }
            if (is_update("tool_call_update")(msg) &&
                identical(msg$params$update$status, "in_progress")) {
                began <<- Sys.time()
                client$write(req(4L, "session/list", list(cwd = fx$proj)))
            }
            if (is_answer(4L)(msg)) {
                client$write(ns$.acp_notification("session/cancel",
                                                  list(sessionId = sid)))
            }
            if (is_answer(3L)(msg)) {
                client$finish()
            }
        }))
        expect_true(as.numeric(difftime(Sys.time(), began, units = "secs")) < 10)
        expect_true(at(msgs, is_answer(4L)) < at(msgs, is_answer(3L)))
        expect_identical(answer(msgs, 3L)$result$stopReason, "cancelled")
        expect_identical(model$i, 1L)
        ended <- Filter(is_update("tool_call_update"), msgs)
        ended <- ended[[length(ended)]]$params$update
        expect_identical(ended$status, "failed")
        expect_true(grepl("cancelled", ended$content[[1L]]$content$text,
                          fixed = TRUE))
    })
}

# A stop asked for leaves the conversation as it was: no summary is
# asked for, and one asked for before the stop came is not installed.
# The stop is read when the request returns, a failed one too: that read
# is where an ACP session takes the client's cancel (.acp_stop()).
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    stop_now <- FALSE
    asked <- 0L
    seen <- FALSE
    llm <- asNamespace("llm.api")
    old_chat <- get("chat", envir = llm)
    on.exit(utils::assignInNamespace("chat", old_chat, "llm.api"), add = TRUE)
    utils::assignInNamespace("chat", function(...) {
        asked <<- asked + 1L
        if (identical(stop_now, "failing")) {
            stop_now <<- TRUE
            stop("the summary request failed")
        }
        if (identical(stop_now, "during")) {
            stop_now <<- TRUE
        }
        list(content = "A summary.", usage = list(input_tokens = 1L,
                                                  output_tokens = 1L))
    }, "llm.api")
    config <- list(subagents = list(context_compaction = list(
        keep_recent_turns = 1L, keep_recent_tokens = 0L)))
    fresh <- function() {
        s <- corteza::new_session(provider = "anthropic",
                                  model_map = list(cloud = "claude-test"))
        s$stop_requested <- function() {
            if (isTRUE(stop_now)) {
                seen <<- TRUE
            }
            isTRUE(stop_now)
        }
        turns <- c("one", "two", "three")
        s$history <- unlist(lapply(turns, function(t) {
            list(list(role = "user", content = t),
                 list(role = "assistant", content = toupper(t)))
        }), recursive = FALSE)
        s
    }
    compact <- function(s) {
        ns$maybe_compact_turn_session(s, config, threshold = 0,
                                      min_messages = 2L, reason = "manual")
    }
    # Asked before: no summary is asked for.
    stop_now <- TRUE
    s <- fresh()
    before <- s$history
    expect_false(isTRUE(compact(s)))
    expect_identical(asked, 0L)
    expect_identical(s$history, before)
    # Asked while the summary was written: it is not installed.
    stop_now <- "during"
    seen <- FALSE
    s <- fresh()
    expect_false(isTRUE(compact(s)))
    expect_identical(asked, 1L)
    expect_true(seen)
    expect_identical(s$history, before)
    # Asked while a request that failed was made: the stop is read all
    # the same, and the failure recorded as one.
    stop_now <- "failing"
    seen <- FALSE
    s <- fresh()
    expect_false(isTRUE(compact(s)))
    expect_identical(asked, 2L)
    expect_true(seen)
    expect_identical(s$history, before)
    expect_identical(s$last_compaction_failure$reason, "summarizer_error")
    # Not asked: the same conversation compacts.
    stop_now <- FALSE
    s <- fresh()
    expect_true(isTRUE(compact(s)))
    expect_false(identical(s$history, before))
})

# A prompt that cannot be written to the transcript runs nothing: the
# reviewer would judge the turn's calls without the person's latest
# words, which here take back what the prompt before allowed.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    blocked <- "Do not write anything now. Just explain."
    names <- c("transcript_append", "review_ask")
    old <- mget(names, envir = ns)
    on.exit(for (n in names) utils::assignInNamespace(n, old[[n]], "corteza"),
            add = TRUE)
    utils::assignInNamespace("transcript_append", function(session, role,
                                                           content, ...) {
        if (identical(role, "user") && identical(content, blocked)) {
            stop("the disk is full")
        }
        old$transcript_append(session, role, content, ...)
    }, "corteza")
    # A reviewer that follows the conversation it is given.
    utils::assignInNamespace("review_ask", function(session, q) {
        if (any(grepl(blocked, q$conversation, fixed = TRUE))) {
            list(verdict = "refuse", reason = "the latest prompt forbids it")
        } else {
            list(verdict = "approve", reason = "an earlier prompt allows it")
        }
    }, "corteza")
    target <- file.path(fx$proj, "notes.txt")
    model <- fake_model(list(list(text = "Ready."),
                             list(tool = "write_file",
                                  args = list(path = target, content = "changed")),
                             list(text = "Done.")))
    sid <- NULL
    msgs <- NULL
    said <- utils::capture.output(
        msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
            if (is_answer(2L)(msg)) {
                sid <<- msg$result$sessionId
                client$write(req(3L, "session/set_mode",
                                 list(sessionId = sid, modeId = "review")))
            }
            if (is_answer(3L)(msg)) {
                client$write(prompt(4L, sid, "You may write notes.txt."))
            }
            if (is_answer(4L)(msg)) {
                client$write(prompt(5L, sid, blocked))
            }
            if (is_answer(5L)(msg)) {
                client$finish()
            }
        })), type = "message")
    expect_identical(answer(msgs, 4L)$result$stopReason, "end_turn")
    refused <- answer(msgs, 5L)$error
    expect_identical(refused$code, -32603L)
    expect_true(grepl("nothing ran", refused$message, fixed = TRUE))
    expect_true(any(grepl("the disk is full", said, fixed = TRUE)))
    expect_false(file.exists(target))
    expect_identical(model$i, 1L)
})

# A session is built in its own directory: a project skill's R file,
# sourced as the tool table is made, reads a relative path from there,
# not from the launcher's directory; and the caller's directory is the
# working directory again afterwards.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    here <- getwd()
    on.exit(setwd(here), add = TRUE)
    launch <- file.path(fx$root, "launch")
    dir.create(launch)
    writeLines("the launcher's", file.path(launch, "label.txt"))
    writeLines("the project's", file.path(fx$proj, "label.txt"))
    skills <- file.path(fx$proj, ".corteza", "skills")
    dir.create(skills, recursive = TRUE)
    writeLines(c('label <- readLines("label.txt", warn = FALSE)',
                 'register_skill(skill_spec("acp_probe_label", "The label at setup",',
                 '    handler = function(args, ctx) ok(label), class = "read"))'),
               file.path(skills, "probe.R"))
    config <- tools::R_user_dir("corteza", "config")
    dir.create(config, recursive = TRUE)
    jsonlite::write_json(list(trusted_projects = list(fx$proj)),
                         file.path(config, "config.json"), auto_unbox = TRUE)
    setwd(launch)
    session <- ns$.acp_session_build(list(tools = NULL), fx$proj,
                                     list(provider = "anthropic", model = NULL))
    entry <- ns$tool_entry("acp_probe_label", session$tool_table)
    expect_false(is.null(entry))
    read <- entry$handler(list(), list(session = session, cwd = fx$proj))
    expect_identical(read$content[[1L]]$text, "the project's")
    expect_identical(normalizePath(getwd()), normalizePath(launch))
    expect_null(ns$tool_entry("acp_probe_label"))
})

# ---- Messages while a turn runs ---------------------------------------------
# During the permission wait: a list is answered at once; a mode change
# reaches the running session; a session for another directory is made
# at once, and the turn's directory stays; a second prompt for the
# running session is refused; a prompt for the other session waits for
# this one, and a close of the running session is answered after its
# prompt.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    other <- file.path(fx$root, "other")
    dir.create(other)
    target <- file.path(fx$proj, "a.md")
    # The cancelled turn asks the model nothing more, so the other
    # session's prompt takes the second step.
    model <- fake_model(list(list(tool = "write_file",
                                  args = list(path = target, content = "a")),
                             list(text = "Other.")))
    sid <- NULL
    sid2 <- NULL
    wd <- NULL
    asked <- NULL
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Write a."))
        }
        if (is_permission(msg)) {
            asked <<- msg
            client$write(req(10L, "session/list", list(cwd = fx$proj)))
            client$write(req(11L, "session/set_mode",
                             list(sessionId = sid, modeId = "review")))
            client$write(req(12L, "session/new",
                             list(cwd = other, mcpServers = list())))
            client$write(prompt(13L, sid, "Again."))
        }
        if (is_answer(12L)(msg)) {
            sid2 <<- msg$result$sessionId
            wd <<- getwd()
            client$write(prompt(14L, sid2, "Over there."))
            client$write(req(15L, "session/close", list(sessionId = sid)))
            grant(client, asked, "allow_once")
        }
        if (is_answer(14L)(msg)) {
            client$finish()
        }
    }))
    done <- at(msgs, is_answer(3L))
    expect_true(at(msgs, is_answer(10L)) < done)
    expect_identical(answer(msgs, 10L)$result$sessions[[1L]]$sessionId, sid)
    expect_true(at(msgs, is_answer(11L)) < done)
    expect_true(length(at(msgs, is_update("current_mode_update"))) == 1L)
    expect_true(at(msgs, is_answer(12L)) < done)
    expect_identical(normalizePath(wd, winslash = "/"),
                     normalizePath(fx$proj, winslash = "/"))
    expect_identical(answer(msgs, 13L)$error$code, -32600L)
    # The close stopped the running prompt and was answered after it. It
    # came before the client's yes, so the call never ran.
    expect_identical(answer(msgs, 3L)$result$stopReason, "cancelled")
    expect_false(file.exists(target))
    expect_true(at(msgs, is_answer(15L)) > done)
    # The other session's prompt ran once this one ended.
    expect_true(at(msgs, is_answer(14L)) > done)
    expect_identical(answer(msgs, 14L)$result$stopReason, "end_turn")
})

# A cancel of a session whose prompt only waits answers that prompt
# `cancelled` at once, and it never runs.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    other <- file.path(fx$root, "other")
    dir.create(other)
    model <- fake_model(list(list(tool = "write_file", args = list(
        path = file.path(fx$proj, "a.md"), content = "a")),
        list(text = "Done.")))
    asked <- NULL
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            client$write(prompt(3L, msg$result$sessionId, "Write a."))
        }
        if (is_permission(msg)) {
            asked <<- msg
            client$write(req(10L, "session/new",
                             list(cwd = other, mcpServers = list())))
        }
        if (is_answer(10L)(msg)) {
            other_sid <- msg$result$sessionId
            client$write(prompt(11L, other_sid, "Never."))
            client$write(ns$.acp_notification("session/cancel",
                                              list(sessionId = other_sid)))
            grant(client, asked, "allow_once")
        }
        if (is_answer(3L)(msg)) {
            client$finish()
        }
    }))
    expect_identical(answer(msgs, 11L)$result$stopReason, "cancelled")
    expect_true(at(msgs, is_answer(11L)) < at(msgs, is_answer(3L)))
    expect_identical(answer(msgs, 3L)$result$stopReason, "end_turn")
    expect_identical(model$i, 2L)
})

# A client that stops reading: the first write that fails ends the loop,
# quietly, and nothing after it runs.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    p <- processx::conn_create_pipepair()
    writes <- 0L
    send <- function(line) {
        writes <<- writes + 1L
        if (writes >= 2L) {
            stop("the pipe is broken")
        }
    }
    for (m in c(start(fx$proj), list(req(3L, "session/new", list(
        cwd = fx$proj, mcpServers = list()))))) {
        processx::conn_write(p[[2L]], paste0(ns$.acp_encode(m), "\n"))
    }
    close(p[[2L]])
    said <- utils::capture.output(ns$.acp_main(input = p[[1L]], send = send),
                                  type = "message")
    expect_true(any(grepl("the client stopped reading", said, fixed = TRUE)))
    expect_identical(writes, 2L)
    # The first session was made, and its answer failed; the second
    # request was never handled.
    expect_identical(length(ns$session_entries()), 1L)
})

# ---- Modes ------------------------------------------------------------------
# The config's own approval_mode, where it is "allow" or "deny", is a
# mode of its own: shown as the one in force, and one the client can
# come back to. A mode the session does not offer is refused.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    dir <- tools::R_user_dir("corteza", "config")
    dir.create(dir, recursive = TRUE)
    jsonlite::write_json(list(approval_mode = "deny"),
                         file.path(dir, "config.json"), auto_unbox = TRUE)
    msgs <- acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <- msg$result$sessionId
            client$write(req(3L, "session/set_mode",
                             list(sessionId = sid, modeId = "allow")))
            client$write(req(4L, "session/set_mode",
                             list(sessionId = sid, modeId = "ask")))
            client$write(req(5L, "session/set_mode",
                             list(sessionId = sid, modeId = "deny")))
        }
        if (is_answer(5L)(msg)) {
            client$finish()
        }
    })
    modes <- answer(msgs, 2L)$result$modes
    expect_identical(modes$currentModeId, "deny")
    expect_identical(vapply(modes$availableModes, function(m) m$id, ""),
                     c("ask", "review", "deny"))
    expect_identical(answer(msgs, 3L)$error$code, -32602L)
    expect_identical(length(answer(msgs, 4L)$result), 0L)
    expect_identical(length(answer(msgs, 5L)$result), 0L)
    changed <- Filter(is_update("current_mode_update"), msgs)
    expect_identical(vapply(changed, function(m) {
        m$params$update$currentModeId
    }, ""), c("ask", "deny"))
})

# ---- Commands ---------------------------------------------------------------
# A session offers its commands after its answer. A prompt that names
# one runs it in place of a turn: no model is asked, and what it prints
# is the reply. /trust yes trusts the directory for the sessions after;
# /clear says to start a new session; any other word after a slash is
# text for the model.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    model <- fake_model(list(list(text = "Read it.")))
    sid <- NULL
    said <- list()
    script <- c("/help", "/trust", "/trust yes", "/status", "/clear", "/tools",
                "/tasks", "/notes.md says what?")
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(prompt(10L, sid, script[[1L]]))
        }
        if (is.null(msg$method) && is.integer(msg$id) && msg$id >= 10L) {
            n <- msg$id - 9L
            if (n < length(script)) {
                client$write(prompt(msg$id + 1L, sid, script[[n + 1L]]))
            } else {
                client$finish()
            }
        }
    }))
    reply <- function(n) {
        done <- at(msgs, is_answer(9L + n))
        chunks <- Filter(is_update("agent_message_chunk"), msgs[seq_len(done)])
        last <- chunks[[length(chunks)]]
        last$params$update$content$text
    }
    offered <- Filter(is_update("available_commands_update"), msgs)
    expect_identical(length(offered), 1L)
    expect_true(at(msgs, is_update("available_commands_update")) >
                    at(msgs, is_answer(2L)))
    names_offered <- vapply(offered[[1L]]$params$update$availableCommands,
                            function(cmd) cmd$name, "")
    expect_true(all(c("help", "trust", "status", "compact") %in% names_offered))
    expect_false("clear" %in% names_offered)
    expect_true(grepl("/trust [yes]", reply(1L), fixed = TRUE))
    expect_true(grepl("start a new session", reply(1L), fixed = TRUE))
    expect_true(startsWith(reply(2L), "Not trusted:"))
    expect_true(startsWith(reply(3L), "Trusted:"))
    expect_true(grepl("a new session in this directory", reply(3L), fixed = TRUE))
    expect_true(ns$project_trusted(fx$proj))
    # The session's own trust was fixed when it started.
    expect_true(grepl("no for this session, yes from the next", reply(4L),
                      fixed = TRUE))
    expect_true(grepl(as.character(utils::packageVersion("corteza")), reply(4L),
                      fixed = TRUE))
    expect_true(grepl("starts a new session", reply(5L), fixed = TRUE))
    expect_true(grepl("write_file", reply(6L), fixed = TRUE))
    # The task list is off unless the config turns it on, and /tasks
    # says how (task_list_enabled).
    expect_true(startsWith(reply(7L), "The task list is off."))
    # The commands asked no model; the last prompt did.
    expect_identical(model$i, 1L)
    expect_identical(reply(8L), "Read it.")
    expect_identical(answer(msgs, 17L)$result$stopReason, "end_turn")
})

# /compact asks a model for its summary, and a cancel reaches it as it
# reaches a turn. A cancel sent while the summary is asked for ends the
# prompt `cancelled`, the summary uninstalled, whether the request
# failed or answered; a /compact left alone then compacts.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    dir <- tools::R_user_dir("corteza", "config")
    dir.create(dir, recursive = TRUE)
    jsonlite::write_json(list(subagents = list(context_compaction = list(
        keep_recent_tokens = 0L, keep_recent_turns = 1L))),
        file.path(dir, "config.json"), auto_unbox = TRUE)
    llm <- asNamespace("llm.api")
    old_chat <- get("chat", envir = llm)
    on.exit(utils::assignInNamespace("chat", old_chat, "llm.api"), add = TRUE)
    client_now <- NULL
    sid <- NULL
    plan <- c("cancel and fail", "cancel", "answer")
    summaries <- 0L
    utils::assignInNamespace("chat", function(...) {
        summaries <<- summaries + 1L
        step <- plan[[summaries]]
        if (startsWith(step, "cancel")) {
            client_now$write(ns$.acp_notification("session/cancel",
                                                  list(sessionId = sid)))
            Sys.sleep(0.05)
        }
        if (identical(step, "cancel and fail")) {
            stop("the summary request failed")
        }
        list(content = "A summary.", usage = list(input_tokens = 1L,
                                                  output_tokens = 1L))
    }, "llm.api")
    model <- fake_model(list(list(text = "First answer."),
                             list(text = "Second answer.")))
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        client_now <<- client
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "First task."))
        }
        if (is_answer(3L)(msg)) {
            client$write(prompt(4L, sid, "Second task."))
        }
        for (n in 4:6) {
            if (is_answer(n)(msg)) {
                client$write(prompt(n + 1L, sid, "/compact"))
            }
        }
        if (is_answer(7L)(msg)) {
            client$finish()
        }
    }))
    texts <- vapply(Filter(is_update("agent_message_chunk"), msgs),
                    function(m) m$params$update$content$text, "")
    expect_identical(summaries, 3L)
    expect_identical(answer(msgs, 5L)$result$stopReason, "cancelled")
    expect_identical(answer(msgs, 6L)$result$stopReason, "cancelled")
    expect_identical(sum(texts == "Cancelled: the conversation is as it was."), 2L)
    expect_identical(answer(msgs, 7L)$result$stopReason, "end_turn")
    expect_identical(sum(texts == "Compacted."), 1L)
})

# ---- Listing and resuming ---------------------------------------------------
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    old_page <- ns$.ACP_LIST_PAGE
    utils::assignInNamespace(".ACP_LIST_PAGE", 2L, "corteza")
    on.exit(utils::assignInNamespace(".ACP_LIST_PAGE", old_page, "corteza"),
            add = TRUE)
    model <- fake_model(list(list(text = "Remembered.")))
    ids <- character()
    msgs <- with_model(model, acp_run(list(
        req(1L, "initialize", list(protocolVersion = 1L)),
        req(2L, "session/new", list(cwd = fx$proj, mcpServers = list())),
        req(3L, "session/new", list(cwd = fx$proj, mcpServers = list())),
        req(4L, "session/new", list(cwd = fx$proj, mcpServers = list()))),
        function(msg, client) {
            if (is_answer(2L)(msg)) {
                client$write(prompt(5L, msg$result$sessionId, "Remember 42."))
            }
            if (is_answer(5L)(msg)) {
                client$write(req(6L, "session/list", list(cwd = fx$proj)))
            }
            if (is_answer(6L)(msg)) {
                client$write(req(7L, "session/list", list(
                    cwd = fx$proj, cursor = msg$result$nextCursor)))
            }
            if (is_answer(7L)(msg)) {
                client$finish()
            }
        }))
    first <- answer(msgs, 2L)$result$sessionId
    page1 <- answer(msgs, 6L)$result
    page2 <- answer(msgs, 7L)$result
    expect_identical(length(page1$sessions), 2L)
    expect_false(is.null(page1$nextCursor))
    expect_identical(length(page2$sessions), 1L)
    expect_null(page2$nextCursor)
    listed <- c(vapply(page1$sessions, function(s) s$sessionId, ""),
                vapply(page2$sessions, function(s) s$sessionId, ""))
    expect_identical(sort(listed), sort(c(first, answer(msgs, 3L)$result$sessionId,
                                          answer(msgs, 4L)$result$sessionId)))
    expect_true(all(vapply(page1$sessions, function(s) {
        identical(s$cwd, normalizePath(fx$proj, winslash = "/"))
    }, logical(1))))
    expect_true(grepl("^\\d{4}-\\d\\d-\\d\\dT", page1$sessions[[1L]]$updatedAt))
    # The first session's title is its first prompt.
    titled <- Filter(function(s) identical(s$sessionId, first),
                     c(page1$sessions, page2$sessions))[[1L]]
    expect_true(grepl("Remember 42", titled$title, fixed = TRUE))

    # A new process takes the conversation up again: its history goes
    # to the model, and none of it to the client. A resume names the
    # conversation's directory; another is refused, as is an id nobody
    # made.
    elsewhere <- file.path(fx$root, "elsewhere")
    dir.create(elsewhere)
    model2 <- fake_model(list(list(text = "Yes, 42.")))
    msgs <- with_model(model2, acp_run(list(
        req(1L, "initialize", list(protocolVersion = 1L)),
        req(2L, "session/resume", list(sessionId = first, cwd = elsewhere,
                                       mcpServers = list())),
        req(3L, "session/resume", list(sessionId = "no-such", cwd = fx$proj,
                                       mcpServers = list())),
        req(4L, "session/resume", list(sessionId = first, cwd = fx$proj,
                                       mcpServers = list()))),
        function(msg, client) {
            if (is_answer(4L)(msg)) {
                client$write(prompt(5L, first, "What was it?"))
            }
            if (is_answer(5L)(msg)) {
                client$finish()
            }
        }))
    expect_true(grepl("belongs to", answer(msgs, 2L)$error$message))
    expect_identical(answer(msgs, 3L)$error$code, -32002L)
    expect_identical(answer(msgs, 4L)$result$modes$currentModeId, "ask")
    resumed <- at(msgs, is_answer(4L))
    updates <- at(msgs, function(m) identical(m$method, "session/update"))
    expect_identical(sum(updates < resumed), 0L)
    sent <- model2$seen[[1L]]
    texts <- vapply(sent, function(m) {
        paste(unlist(m$content), collapse = " ")
    }, "")
    expect_true(any(grepl("Remember 42.", texts, fixed = TRUE)))
    expect_true(any(grepl("Remembered.", texts, fixed = TRUE)))
    expect_identical(answer(msgs, 5L)$result$stopReason, "end_turn")
})

# ---- Review mode ------------------------------------------------------------
# A call the reviewer approves runs with no permission request; one it
# passes on is asked, with its reason; one it refuses reaches the model.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    rulings <- list(list(verdict = "approve", reason = "part of the task"),
                    list(verdict = "escalate", reason = "unsure"),
                    list(verdict = "refuse", reason = "not asked for"))
    n <- 0L
    old <- ns$review_ask
    utils::assignInNamespace("review_ask", function(session, q) {
        n <<- n + 1L
        rulings[[n]]
    }, "corteza")
    on.exit(utils::assignInNamespace("review_ask", old, "corteza"), add = TRUE)
    write <- function(name) list(tool = "write_file", args = list(
        path = file.path(fx$proj, name), content = "x"))
    model <- fake_model(list(write("a.md"), write("b.md"), write("c.md"),
                             list(text = "Done.")))
    sid <- NULL
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (is_answer(2L)(msg)) {
            sid <<- msg$result$sessionId
            client$write(req(3L, "session/set_mode",
                             list(sessionId = sid, modeId = "review")))
        }
        if (is_answer(3L)(msg)) {
            client$write(prompt(4L, sid, "Write a, b and c."))
        }
        if (is_permission(msg)) {
            grant(client, msg, "allow_once")
        }
        if (is_answer(4L)(msg)) {
            client$finish()
        }
    }))
    expect_identical(length(answer(msgs, 3L)$result), 0L)
    expect_identical(n, 3L)
    asks <- Filter(is_permission, msgs)
    expect_identical(length(asks), 1L)
    expect_true(grepl("unsure", asks[[1L]]$params$toolCall$title, fixed = TRUE))
    expect_true(file.exists(file.path(fx$proj, "a.md")))
    expect_true(file.exists(file.path(fx$proj, "b.md")))
    expect_false(file.exists(file.path(fx$proj, "c.md")))
    expect_identical(answer(msgs, 4L)$result$stopReason, "end_turn")
})

# A reply the transcript lost: the agent asked a question, its write
# failed, and the person's "Yes." answers what the reviewer would not
# see. The reviewer is not asked; the person is, and told why.
local({
    fx <- acp_fixture()
    on.exit(fx$restore(), add = TRUE)
    question <- "Before writing, shall I show you a draft?"
    names <- c("transcript_append", "review_ask")
    old <- mget(names, envir = ns)
    on.exit(for (n in names) utils::assignInNamespace(n, old[[n]], "corteza"),
            add = TRUE)
    utils::assignInNamespace("transcript_append", function(session, role,
                                                           content, ...) {
        if (identical(role, "assistant") && identical(content, question)) {
            stop("the disk is full")
        }
        old$transcript_append(session, role, content, ...)
    }, "corteza")
    reviews <- 0L
    utils::assignInNamespace("review_ask", function(session, q) {
        reviews <<- reviews + 1L
        list(verdict = "approve", reason = "the person said yes")
    }, "corteza")
    target <- file.path(fx$proj, "notes.txt")
    model <- fake_model(list(list(text = question),
                             list(tool = "write_file",
                                  args = list(path = target, content = "x")),
                             list(text = "Done.")))
    sid <- NULL
    msgs <- NULL
    said <- utils::capture.output(
        msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
            if (is_answer(2L)(msg)) {
                sid <<- msg$result$sessionId
                client$write(req(3L, "session/set_mode",
                                 list(sessionId = sid, modeId = "review")))
            }
            if (is_answer(3L)(msg)) {
                client$write(prompt(4L, sid, "Write notes.txt once I confirm."))
            }
            if (is_answer(4L)(msg)) {
                client$write(prompt(5L, sid, "Yes."))
            }
            if (is_permission(msg)) {
                grant(client, msg, "reject_once")
            }
            if (is_answer(5L)(msg)) {
                client$finish()
            }
        })), type = "message")
    expect_true(any(grepl("the disk is full", said, fixed = TRUE)))
    expect_identical(answer(msgs, 4L)$result$stopReason, "end_turn")
    expect_identical(reviews, 0L)
    asks <- Filter(is_permission, msgs)
    expect_identical(length(asks), 1L)
    expect_true(grepl("missing from the transcript",
                      asks[[1L]]$params$toolCall$title, fixed = TRUE))
    expect_false(file.exists(target))
    expect_false(is.null(answer(msgs, 5L)$result))
})

# A cancel ends the wait on the reviewer, and withdraws the request
# (review_cancel()), so the next call finds the reviewer free.
local({
    names <- c("job_monitor_ask", "job_monitor_poll", "job_monitor_close")
    old <- mget(names, envir = ns)
    on.exit(for (n in names) utils::assignInNamespace(n, old[[n]], "corteza"),
            add = TRUE)
    polls <- 0L
    closed <- 0L
    utils::assignInNamespace("job_monitor_ask", function(session, q) {
        invisible(TRUE)
    }, "corteza")
    # A ruling comes at the tenth poll, so a wait that missed the stop
    # ends, and fails below, rather than hanging.
    utils::assignInNamespace("job_monitor_poll", function(session, timeout) {
        polls <<- polls + 1L
        if (polls >= 10L) list(verdict = "approve", reason = "late")
    }, "corteza")
    utils::assignInNamespace("job_monitor_close", function(session) {
        closed <<- closed + 1L
        invisible(TRUE)
    }, "corteza")
    session <- new.env()
    session$stop_requested <- function() polls >= 3L
    stopped <- tryCatch(ns$review_ask(session, list(request_id = "r1")),
                        corteza_stop = function(e) "stopped")
    expect_identical(stopped, "stopped")
    expect_identical(polls, 3L)
    expect_identical(closed, 1L)
})

# ---- Sessions share no R objects --------------------------------------------
# Each session's run_r runs in a worker of its own: an object one session
# makes is not there for another, nor in corteza's process. A config that
# asked for run_r in corteza's own process is told why not, once per
# session, after the session's answer. Starts R workers, so not on CRAN.
if (tinytest::at_home() || identical(Sys.getenv("NOT_CRAN"), "true")) {
    local({
        fx <- acp_fixture()
        on.exit(fx$restore(), add = TRUE)
        dir <- tools::R_user_dir("corteza", "config")
        dir.create(dir, recursive = TRUE)
        jsonlite::write_json(list(run_r_mode = "in_process"),
                             file.path(dir, "config.json"), auto_unbox = TRUE)
        other <- file.path(fx$root, "other")
        dir.create(other)
        run <- function(code) list(tool = "run_r", args = list(code = code))
        model <- fake_model(list(run("acp_kept <- 42; exists('acp_kept')"),
                                 list(text = "Made."),
                                 run("exists('acp_kept')"),
                                 list(text = "Looked.")))
        first <- NULL
        second <- NULL
        msgs <- with_model(model, acp_run(list(
            req(1L, "initialize", list(protocolVersion = 1L)),
            req(2L, "session/new", list(cwd = fx$proj, mcpServers = list())),
            req(3L, "session/new", list(cwd = other, mcpServers = list()))),
            function(msg, client) {
                if (is_answer(2L)(msg)) {
                    first <<- msg$result$sessionId
                }
                if (is_answer(3L)(msg)) {
                    second <<- msg$result$sessionId
                    client$write(prompt(4L, first, "Make it."))
                }
                if (is_permission(msg)) {
                    grant(client, msg, "allow_once")
                }
                if (is_answer(4L)(msg)) {
                    client$write(prompt(5L, second, "Is it there?"))
                }
                if (is_answer(5L)(msg)) {
                    client$finish()
                }
            }))
        result_of <- function(sid) {
            done <- Filter(function(m) {
                is_update("tool_call_update")(m) &&
                    identical(m$params$sessionId, sid) &&
                    identical(m$params$update$status, "completed")
            }, msgs)
            done[[1L]]$params$update$content[[1L]]$content$text
        }
        expect_true(grepl("TRUE", result_of(first), fixed = TRUE))
        expect_true(grepl("FALSE", result_of(second), fixed = TRUE))
        expect_false(exists("acp_kept", envir = globalenv()))
        # The note, once for each session, after its answer.
        notes <- at(msgs, function(m) {
            is_update("agent_message_chunk")(m) &&
                grepl("run_r runs in a worker", m$params$update$content$text,
                      fixed = TRUE)
        })
        expect_identical(length(notes), 2L)
        expect_true(notes[[1L]] > at(msgs, is_answer(2L)))
        expect_identical(msgs[[notes[[1L]]]]$params$sessionId, first)
    })
}

# ---- The command, over pipes ------------------------------------------------
# The agent in a process of its own, as a client starts it: protocol on
# standard output and nothing else there, though the model's code prints
# to it; the end of standard input ends the process, cleanly.
if (.Platform$OS.type != "windows" &&
    (tinytest::at_home() || identical(Sys.getenv("NOT_CRAN"), "true"))) {
    local({
        fx <- acp_fixture()
        on.exit(fx$restore(), add = TRUE)
        script <- file.path(fx$root, "agent.R")
        writeLines(c(
            'utils::assignInNamespace(".agent_anthropic", function(messages,',
            '    tools, system, model, config = NULL, cache = NULL,',
            '    thinking_budget_tokens = NULL, on_delta = NULL,',
            '    thinking = NULL, ...) {',
            '    cat("cat on standard output\\n")',
            '    print("print on standard output")',
            '    on_delta("Hello ")',
            '    on_delta("from the child.")',
            '    text <- "Hello from the child."',
            '    list(text = text, tool_calls = list(),',
            '         usage = list(input_tokens = 1L, output_tokens = 1L),',
            '         assistant_message = list(role = "assistant",',
            '             content = list(list(type = "text", text = text))))',
            '}, "llm.api")',
            'corteza:::.acp_main()'), script)
        p <- processx::process$new(file.path(R.home("bin"), "Rscript"),
                                   c("--vanilla", script),
                                   stdin = "|", stdout = "|", stderr = "|",
                                   env = c("current", R_LIBS = paste(.libPaths(),
                                       collapse = .Platform$path.sep)))
        on.exit(if (p$is_alive()) p$kill(), add = TRUE)
        lines <- character()
        # Read standard output until a message `pred` holds for comes.
        await <- function(pred, seconds = 60) {
            deadline <- Sys.time() + seconds
            while (Sys.time() < deadline) {
                for (m in lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)) {
                    if (isTRUE(pred(m))) {
                        return(m)
                    }
                }
                p$poll_io(200L)
                lines <<- c(lines, p$read_output_lines())
            }
            NULL
        }
        say <- function(msg) p$write_input(paste0(ns$.acp_encode(msg), "\n"))
        say(req(1L, "initialize", list(protocolVersion = 1L)))
        say(req(2L, "session/new", list(cwd = fx$proj, mcpServers = list())))
        made <- await(is_answer(2L))
        expect_true(is.character(made$result$sessionId))
        say(prompt(3L, made$result$sessionId, "Hello?"))
        done <- await(is_answer(3L))
        expect_identical(done$result$stopReason, "end_turn")
        close(p$get_input_connection())
        p$wait(30000L)
        expect_identical(p$get_exit_status(), 0L)
        lines <- c(lines, p$read_all_output_lines())
        # Every line is a JSON-RPC message; what R printed is on
        # standard error.
        parsed <- lapply(lines, function(l) {
            tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE),
                     error = function(e) NULL)
        })
        expect_true(all(vapply(parsed, function(m) identical(m$jsonrpc, "2.0"),
                               logical(1))))
        chunks <- Filter(is_update("agent_message_chunk"), parsed)
        expect_identical(paste(vapply(chunks, function(m) {
            m$params$update$content$text
        }, ""), collapse = ""), "Hello from the child.")
        err <- p$read_all_error()
        expect_true(grepl("cat on standard output", err, fixed = TRUE))
        expect_true(grepl("print on standard output", err, fixed = TRUE))
    })
}
