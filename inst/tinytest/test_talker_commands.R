# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# A bot's room is a talker by default (WP20b), and each command a room
# takes acts, in a talker session, on what the command table's `talker`
# column says (R/commands.R). /clear, /job resume, /trust, /sidebar and
# /approve-all are held to it by their own files, which build talker
# rooms now; /model, /effort, /status and /context-edit are here.
# Nothing here reaches a homeserver or a model: the sessions use ollama,
# which needs no key, and no turn runs.

ns <- asNamespace("corteza")
room <- "!room:example.invalid"
base_cfg <- function(...) {
    utils::modifyList(list(user = "bot", user_id = "@bot:example.invalid",
                           operators = "@human:example.invalid",
                           provider = "ollama", model = "fixture-model"),
                      list(...))
}
# A room's session, built for real. With `cwd` the room works there;
# without one it has no directory (WP10).
room_session <- function(cfg, tools_filter = NULL, cwd = NULL) {
    if (is.null(cwd)) {
        return(ns$bot_get_or_create_session(ns$bot_new_session_registry(),
                                            room, cfg,
                                            tools_filter = tools_filter))
    }
    dir.create(cwd, showWarnings = FALSE)
    saved <- get("bot_room_cwd", envir = ns)
    assignInNamespace("bot_room_cwd", function(...) cwd, "corteza")
    on.exit(assignInNamespace("bot_room_cwd", saved, "corteza"), add = TRUE)
    ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg,
                                 tools_filter = tools_filter)
}
in_project <- function(cfg, ...) {
    room_session(cfg, cwd = tempfile("talker-room-"), ...)
}
model_cmd <- function(s, text, cfg = base_cfg(), ...) {
    ns$bot_apply_model_command(s, ns$bot_parse_model_command(text), cfg = cfg,
                               ...)
}

# ---- The default -------------------------------------------------------------
local({
    # A room with a directory and no `talker` entry is a talker, on the
    # bot's own model, which its doer keeps.
    s <- in_project(base_cfg())
    expect_true(isTRUE(s$talker))
    expect_false(isTRUE(s$unbound))
    expect_identical(s$tools_filter, ns$TALKER_TOOLS)
    expect_identical(s$model_map$cloud, "fixture-model")
    expect_identical(s$doer_model, "fixture-model")
    expect_identical(s$doer_provider, "ollama")
    expect_identical(s$default_model, "fixture-model")
    # Turned off by the config.
    off <- in_project(base_cfg(talker = list(enabled = FALSE)))
    expect_false(isTRUE(off$talker))
    expect_false(identical(off$tools_filter, ns$TALKER_TOOLS))
    # Restricted tools keep it off, from the config or from the run, and
    # the room keeps the tools it was given.
    r1 <- in_project(base_cfg(tools_filter = c("read_file", "grep_files")))
    expect_false(isTRUE(r1$talker))
    expect_identical(r1$tools_filter, c("read_file", "grep_files"))
    r2 <- in_project(base_cfg(), tools_filter = "read_file")
    expect_false(isTRUE(r2$talker))
    # On where the config asks, as before.
    r3 <- in_project(base_cfg(tools_filter = "read_file",
                              talker = list(enabled = TRUE)))
    expect_true(isTRUE(r3$talker))
    # A room with no directory hands no work on, so it is no talker
    # unless the config asks.
    u <- room_session(base_cfg())
    expect_true(isTRUE(u$unbound))
    expect_false(isTRUE(u$talker))
    expect_true(isTRUE(room_session(base_cfg(
        talker = list(enabled = TRUE)))$talker))
})

# ---- /model: the doer's, and the talker's where it follows -------------------
local({
    cfg <- base_cfg(model_pairs = list(
        ollama = list(talker = "reply-local", doer = "work-local"),
        openai_codex = list(talker = "reply-codex", doer = "work-codex")))
    s <- in_project(cfg)
    expect_identical(s$model_map$cloud, "reply-local")
    expect_identical(s$doer_model, "work-local")
    model_cmd(s, "/model work-codex openai_codex", cfg)
    expect_identical(s$model_map$cloud, "reply-codex")
    expect_identical(s$doer_model, "work-codex")
    expect_identical(s$doer_provider, "openai_codex")
})

local({
    s <- in_project(base_cfg())
    ack <- model_cmd(s, "/model claude-opus-5-5 anthropic")
    expect_identical(s$doer_model, "claude-opus-5-5")
    expect_identical(s$doer_provider, "anthropic")
    expect_identical(s$model_map$cloud, "claude-opus-5-5")
    expect_identical(s$provider, "anthropic")
    expect_true(grepl("Effective on the next reply and the next job.", ack,
                      fixed = TRUE))
    # The next job runs on it.
    spec <- ns$job_worker_spec(s)
    expect_identical(spec$model, "claude-opus-5-5")
    expect_identical(spec$provider, "anthropic")
    # The menu marks it as current.
    menu <- ns$bot_render_model_menu(list(
        list(model = "fixture-model", provider = "ollama"),
        list(model = "claude-opus-5-5", provider = "anthropic")), s)
    expect_true(grepl("Current: claude-opus-5-5 (anthropic)", menu, fixed = TRUE))
    expect_true(grepl("claude-opus-5-5  (anthropic)  <- current", menu,
                      fixed = TRUE))
    # A menu number does the same.
    model_cmd(s, "/model 1", available = list(
        list(model = "fixture-model", provider = "ollama")))
    expect_identical(s$doer_model, "fixture-model")
    expect_identical(s$model_map$cloud, "fixture-model")
    expect_identical(s$doer_provider, "ollama")
})
local({
    # A talker with a model of its own keeps it; the work moves.
    s <- in_project(base_cfg(talker = list(model = "fixture-fast")))
    expect_identical(s$model_map$cloud, "fixture-fast")
    expect_identical(s$doer_model, "fixture-model")
    s$doer_thinking <- "adaptive"
    ack <- model_cmd(s, "/model fixture-big")
    expect_identical(s$doer_model, "fixture-big")
    expect_identical(s$model_map$cloud, "fixture-fast")
    expect_identical(ns$job_worker_spec(s)$model, "fixture-big")
    # A thinking type belongs to the model it was set for.
    expect_null(s$doer_thinking)
    expect_true(grepl("Replies: fixture-fast", ack, fixed = TRUE))
    # /status names both.
    st <- ns$bot_status_text(s)
    expect_true(grepl("model: fixture-big\nprovider: ollama\n", st, fixed = TRUE))
    expect_true(grepl("replies: fixture-fast (ollama)", st, fixed = TRUE))
    # The terminal's /model keeps the work's model as the session's, the
    # one --resume starts from.
    ctx <- new.env()
    ctx$session <- s
    ctx$config <- list()
    ns$.repl_model_command(ctx, "/model fixture-huge")
    expect_identical(ctx$model, "fixture-huge")
    expect_identical(ctx$provider, "ollama")
    expect_identical(s$model_map$cloud, "fixture-fast")
})
local({
    # A room that is no talker: /status as before, with no replies line.
    s <- in_project(base_cfg(talker = list(enabled = FALSE)))
    model_cmd(s, "/model fixture-big")
    expect_identical(s$model_map$cloud, "fixture-big")
    st <- ns$bot_status_text(s)
    expect_true(startsWith(st, "model: fixture-big\nprovider: ollama\ncwd: "))
    expect_false(grepl("replies:", st, fixed = TRUE))
})

# ---- /effort: the doer's; the talker's comes from the talker block ------------
local({
    s <- in_project(base_cfg(reasoning_effort = "medium",
                             talker = list(reasoning_effort = "low")))
    expect_identical(s$doer_reasoning_effort, "medium")
    expect_identical(s$reasoning_effort, "low")
    effort <- function(text) {
        ns$.effort_command(s, ns$.effort_parse_command(text), cfg = base_cfg())
    }
    # Ollama takes no effort setting: the doer's provider says so.
    expect_true(grepl("not supported", effort("/effort high"), fixed = TRUE))
    expect_identical(s$doer_reasoning_effort, "medium")
    model_cmd(s, "/model claude-opus-5-5 anthropic")
    out <- effort("/effort high")
    expect_identical(s$doer_reasoning_effort, "high")
    expect_identical(s$reasoning_effort, "low")
    expect_true(grepl("high (session override)", out, fixed = TRUE))
    expect_identical(ns$job_worker_spec(s)$reasoning_effort, "high")
    # default goes back to the configured one, the doer's at start.
    out <- effort("/effort default")
    expect_identical(s$doer_reasoning_effort, "medium")
    expect_identical(s$reasoning_effort, "low")
    expect_true(grepl("medium (configuration)", out, fixed = TRUE))
})

# ---- The jobs block: what /model and /effort change, a job runs with ----------
# The corteza config's `jobs` block names the doer's model and effort.
# /model and /effort change them, /status shows them, and the next job
# runs with them: talker_enable() reads the block once, and
# job_worker_spec() reads the doer's fields alone. The room reads the
# corteza config from its folder, here a temporary one, so the user's
# own config stays out of it.
local({
    old <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
    Sys.setenv(R_USER_CONFIG_DIR = tempfile("talker-config-"))
    on.exit(if (is.na(old)) Sys.unsetenv("R_USER_CONFIG_DIR") else
        Sys.setenv(R_USER_CONFIG_DIR = old), add = TRUE)
    dir <- tools::R_user_dir("corteza", "config")
    dir.create(dir, recursive = TRUE)
    writeLines(jsonlite::toJSON(list(jobs = list(model = "fixture-worker",
                                                 reasoning_effort = "low")),
                                auto_unbox = TRUE),
               file.path(dir, "config.json"))
    s <- in_project(base_cfg())
    expect_true(isTRUE(s$talker))
    # The talker stays on the bot's model; the doer takes the block's.
    expect_identical(s$model_map$cloud, "fixture-model")
    spec <- ns$job_worker_spec(s)
    expect_identical(spec$model, "fixture-worker")
    expect_identical(spec$reasoning_effort, "low")
    st <- ns$bot_status_text(s)
    expect_true(grepl("model: fixture-worker\nprovider: ollama\n", st,
                      fixed = TRUE))
    expect_true(grepl("replies: fixture-model (ollama)", st, fixed = TRUE))
    # /model moves the work, and the next job runs on it.
    model_cmd(s, "/model claude-opus-5-5 anthropic")
    spec <- ns$job_worker_spec(s)
    expect_identical(spec$model, "claude-opus-5-5")
    expect_identical(spec$provider, "anthropic")
    expect_true(grepl("model: claude-opus-5-5\nprovider: anthropic\n",
                      ns$bot_status_text(s), fixed = TRUE))
    # /effort likewise; default goes back to the block's.
    effort <- function(text) {
        ns$.effort_command(s, ns$.effort_parse_command(text), cfg = base_cfg())
    }
    expect_true(grepl("high (session override)", effort("/effort high"),
                      fixed = TRUE))
    expect_identical(ns$job_worker_spec(s)$reasoning_effort, "high")
    expect_true(grepl("low (configuration)", effort("/effort default"),
                      fixed = TRUE))
    expect_identical(ns$job_worker_spec(s)$reasoning_effort, "low")
})

# ---- /context-edit: the conversation's, which the doer inherits ---------------
local({
    s <- in_project(base_cfg())
    # Set both ways here: the user's own config may have it either way.
    ns$.context_edit_command(s, ns$.context_edit_parse_command("/context-edit off"))
    expect_false(isTRUE(ns$job_worker_spec(s)$context_edit$enabled))
    ns$.context_edit_command(s, ns$.context_edit_parse_command("/context-edit on"))
    expect_true(isTRUE(s$context_edit$enabled))
    expect_true(isTRUE(ns$job_worker_spec(s)$context_edit$enabled))
    # Unless the doer's own setting says otherwise.
    s$config$jobs <- list(doer = list(context_edit = list(enabled = FALSE)))
    expect_false(isTRUE(ns$job_worker_spec(s)$context_edit$enabled))
})

# ---- The table says the same ----------------------------------------------------
local({
    tbl <- ns$corteza_commands()
    said <- stats::setNames(tbl$talker, tbl$name)
    expect_identical(unname(said[c("model", "effort")]), c("both", "doer"))
    expect_identical(unname(said[c("status", "context-edit")]), c("both", "both"))
})
