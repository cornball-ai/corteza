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
    # The title names what the call acts on, by the argument's full name.
    expect_identical(ns$.acp_tool_title(list(tool = "write_file",
        args = list(path = "/p/notes.md", content = "hi"))),
        "write_file: /p/notes.md")

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
            for (d in step$deltas %||% step$text) {
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
        function(msg, client) if (identical(msg$id, 8L)) client$finish())
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
        if (identical(msg$id, 2L)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Write the notes."))
        }
        if (is_permission(msg)) {
            grant(client, msg, "allow_once")
        }
        if (identical(msg$id, 3L) && is.null(msg$method)) {
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
    expect_true(grepl(target, ask$params$toolCall$title, fixed = TRUE))
    # In order: the call announced pending, the question, the run, the
    # result, the reply, and then the answer.
    announced <- at(msgs, is_update("tool_call"))
    expect_identical(msgs[[announced]]$params$update$status, "pending")
    expect_identical(msgs[[announced]]$params$update$locations[[1L]]$path,
                     normalizePath(target, winslash = "/"))
    updates <- at(msgs, is_update("tool_call_update"))
    statuses <- vapply(msgs[updates], function(m) m$params$update$status, "")
    expect_identical(statuses, c("in_progress", "completed"))
    chunks <- at(msgs, is_update("agent_message_chunk"))
    expect_identical(vapply(msgs[chunks], function(m) m$params$update$content$text, ""),
                     c("Do", "ne."))
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
        if (identical(msg$id, 2L)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "One."))
        }
        if (is_permission(msg)) {
            asked <<- asked + 1L
            grant(client, msg, c("reject_once", "allow_always")[asked])
        }
        if (identical(msg$id, 3L) && is.null(msg$method)) {
            client$write(prompt(4L, sid, "Two."))
        }
        if (identical(msg$id, 4L) && is.null(msg$method)) {
            client$write(prompt(5L, sid, "Three."))
        }
        if (identical(msg$id, 5L) && is.null(msg$method)) {
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
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (identical(msg$id, 2L)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Count."))
        }
        if (is_update("agent_message_chunk")(msg) && is.null(sent_cancel)) {
            sent_cancel <<- TRUE
            client$write(ns$.acp_notification("session/cancel",
                                              list(sessionId = sid)))
        }
        if (identical(msg$id, 3L) && is.null(msg$method)) {
            client$write(prompt(4L, sid, "Again."))
        }
        if (identical(msg$id, 4L) && is.null(msg$method)) {
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
sent_cancel <- NULL

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
        if (identical(msg$id, 2L)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Write it."))
        }
        if (is_permission(msg)) {
            client$write(ns$.acp_notification("session/cancel",
                                              list(sessionId = sid)))
            client$write(list(jsonrpc = "2.0", id = msg$id, result = list(
                outcome = list(outcome = "cancelled"))))
        }
        if (identical(msg$id, 3L) && is.null(msg$method)) {
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
    model <- fake_model(list(list(tool = "write_file",
                                  args = list(path = target, content = "a")),
                             list(text = "Done."), list(text = "Other.")))
    sid <- NULL
    sid2 <- NULL
    wd <- NULL
    msgs <- with_model(model, acp_run(start(fx$proj), function(msg, client) {
        if (identical(msg$id, 2L)) {
            sid <<- msg$result$sessionId
            client$write(prompt(3L, sid, "Write a."))
        }
        if (is_permission(msg)) {
            client$write(req(10L, "session/list", list(cwd = fx$proj)))
            client$write(req(11L, "session/set_mode",
                             list(sessionId = sid, modeId = "review")))
            client$write(req(12L, "session/new",
                             list(cwd = other, mcpServers = list())))
            client$write(prompt(13L, sid, "Again."))
        }
        if (identical(msg$id, 12L) && is.null(msg$method)) {
            sid2 <<- msg$result$sessionId
            wd <<- getwd()
            client$write(prompt(14L, sid2, "Over there."))
            client$write(req(15L, "session/close", list(sessionId = sid)))
            client$write(list(jsonrpc = "2.0", id = 1L, result = list(
                outcome = list(outcome = "selected", optionId = "reject_once"))))
        }
        if (identical(msg$id, 14L) && is.null(msg$method)) {
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
    # The close stopped the running prompt and was answered after it.
    expect_identical(answer(msgs, 3L)$result$stopReason, "cancelled")
    expect_true(at(msgs, is_answer(15L)) > done)
    # The other session's prompt ran once this one ended.
    expect_true(at(msgs, is_answer(14L)) > done)
    expect_identical(answer(msgs, 14L)$result$stopReason, "end_turn")
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
            if (identical(msg$id, 2L)) {
                client$write(prompt(5L, msg$result$sessionId, "Remember 42."))
            }
            if (identical(msg$id, 5L) && is.null(msg$method)) {
                client$write(req(6L, "session/list", list(cwd = fx$proj)))
            }
            if (identical(msg$id, 6L)) {
                client$write(req(7L, "session/list", list(
                    cwd = fx$proj, cursor = msg$result$nextCursor)))
            }
            if (identical(msg$id, 7L)) {
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
    expect_identical(answer(msgs, 7L)$result$sessions[[1L]]$cwd %||% "",
                     page2$sessions[[1L]]$cwd)

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
            if (identical(msg$id, 4L)) {
                client$write(prompt(5L, first, "What was it?"))
            }
            if (identical(msg$id, 5L) && is.null(msg$method)) {
                client$finish()
            }
        }))
    expect_true(grepl("belongs to", answer(msgs, 2L)$error$message))
    expect_identical(answer(msgs, 3L)$error$code, -32002L)
    expect_identical(answer(msgs, 4L)$result$modes$currentModeId, "ask")
    resumed <- at(msgs, is_answer(4L))
    expect_identical(length(at(msgs, function(m) {
        identical(m$method, "session/update")
    })[at(msgs, function(m) identical(m$method, "session/update")) < resumed]), 0L)
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
        if (identical(msg$id, 2L)) {
            sid <<- msg$result$sessionId
            client$write(req(3L, "session/set_mode",
                             list(sessionId = sid, modeId = "review")))
        }
        if (identical(msg$id, 3L)) {
            client$write(prompt(4L, sid, "Write a, b and c."))
        }
        if (is_permission(msg)) {
            grant(client, msg, "allow_once")
        }
        if (identical(msg$id, 4L) && is.null(msg$method)) {
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
