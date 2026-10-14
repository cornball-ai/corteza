# Session journal (R/journal.R). Offline throughout: snapshots, segments,
# the de-duplicated record, turn slices across mid-turn rewrites,
# rollover of an earlier live snapshot, and the fail-closed rewrite
# through maybe_compact_turn_session() and turn()'s checkpoint.

journal_tmp <- function() {
    d <- tempfile("corteza_journal_")
    dir.create(d, recursive = TRUE)
    d
}

journal_session <- function(dir = NULL, history = list(),
                            provider = "anthropic") {
    s <- corteza::new_session(channel = "cli", provider = provider,
                              model_map = list(cloud = "claude-sonnet-4-6",
                                               local = NULL))
    s$history <- history
    if (!is.null(dir)) {
        corteza:::journal_install(s, dir = dir)
    }
    s
}

user_msg <- function(text) list(role = "user", content = text)

summary_msg <- function(text, role = "user") {
    list(role = role, content = paste0("[compacted history]\n\n", text))
}

anthropic_round <- function(i) {
    list(
        list(role = "assistant", content = list(list(
            type = "tool_use", id = sprintf("t%d", i), name = "run_r",
            input = list(code = sprintf("x <- %d", i))))),
        list(role = "user", content = list(list(
            type = "tool_result", tool_use_id = sprintf("t%d", i),
            content = sprintf("[1] %d", i))))
    )
}

openai_round <- function(i) {
    list(
        list(role = "assistant", content = "", tool_calls = list(list(
            id = sprintf("c%d", i), type = "function",
            `function` = list(name = "run_r", arguments = "{}")))),
        list(role = "tool", tool_call_id = sprintf("c%d", i), name = "run_r",
             content = sprintf("ok %d", i))
    )
}

# A session with no directory has no journal, and nothing changes --------

bare <- journal_session()
expect_null(corteza:::journal_dir(bare))
expect_false(corteza:::journal_snapshot(bare))
expect_null(corteza:::journal_rewrite(bare, list(), list(), "threshold"))
expect_null(corteza:::journal_mark(bare))
expect_false(corteza:::journal_rewritten_since(bare, NULL))
bare$history <- c(list(user_msg("a")), anthropic_round(1))
expect_identical(corteza:::journal_turn_slice(bare, NULL, 1L),
                 anthropic_round(1))

# Snapshot: both provider shapes round-trip exactly --------

h <- c(list(user_msg("find foo")), anthropic_round(1), openai_round(2),
       list(list(role = "assistant", content = list(list(
           type = "text", text = "pi is 3.14159265358979")))))
dir <- journal_tmp()
s <- journal_session(dir, h)
expect_equal(corteza:::journal_dir(s), dir)
expect_true(corteza:::journal_snapshot(s))
live <- corteza:::journal_load(dir)$live
expect_identical(live$history, h)
expect_equal(live$inherited, 0L)
expect_equal(live$seq, 0L)
expect_identical(corteza:::journal_history(dir), h)

# Rewrite: a compaction-shaped replacement leaves a segment, and the
# record stays the original history, once, without the summary --------

after1 <- c(list(summary_msg("one")), h[4:6])
seg_path <- corteza:::journal_rewrite(s, h, after1, reason = "threshold",
                                      summary = "one")
expect_equal(basename(seg_path), "segment-0001.json")
j <- corteza:::journal_load(dir)
expect_equal(length(j$segments), 1L)
seg <- j$segments[[1]]
expect_equal(seg$seq, 1L)
expect_equal(seg$reason, "threshold")
expect_equal(seg$inherited, 0L)
expect_equal(seg$kept, 4L)
expect_equal(seg$summary, "one")
expect_true(nzchar(seg$timestamp))
expect_identical(seg$history, h)
expect_identical(seg$after, after1)
expect_identical(j$live$history, after1)
expect_equal(j$live$inherited, 4L)
expect_identical(corteza:::journal_history(dir), h)

# The conversation goes on from the compacted history.
s$history <- c(after1, anthropic_round(3), anthropic_round(4))
expect_true(corteza:::journal_snapshot(s))
expect_identical(corteza:::journal_history(dir),
                 c(h, anthropic_round(3), anthropic_round(4)))

# A second compaction keeps only the last round.
before2 <- s$history
after2 <- c(list(summary_msg("two")), anthropic_round(4))
corteza:::journal_rewrite(s, before2, after2, reason = "checkpoint",
                          summary = "two")
j <- corteza:::journal_load(dir)
expect_equal(length(j$segments), 2L)
expect_equal(j$segments[[2]]$inherited, 4L)
expect_equal(j$segments[[2]]$kept, 3L)
expect_identical(corteza:::journal_history(dir),
                 c(h, anthropic_round(3), anthropic_round(4)))

# Turn slice: what a turn added, across zero, one, two rewrites --------

dir <- journal_tmp()
s <- journal_session(dir, c(list(user_msg("start")), anthropic_round(1)))
corteza:::journal_snapshot(s)
mark <- corteza:::journal_mark(s)
expect_equal(mark$seq, 0L)
expect_equal(mark$live_len, 3L)
expect_false(corteza:::journal_rewritten_since(s, mark))

s$history <- c(s$history, anthropic_round(2))
corteza:::journal_snapshot(s)
expect_identical(corteza:::journal_turn_slice(s, mark, 3L), anthropic_round(2))

a <- s$history
after_a <- c(list(summary_msg("s1")), anthropic_round(2))
corteza:::journal_rewrite(s, a, after_a, "checkpoint", summary = "s1")
s$history <- c(after_a, anthropic_round(3))
corteza:::journal_snapshot(s)
expect_true(corteza:::journal_rewritten_since(s, mark))
expect_identical(corteza:::journal_entries_since(s, mark),
                 c(anthropic_round(2), anthropic_round(3)))

b <- s$history
after_b <- c(list(summary_msg("s2")), anthropic_round(3))
corteza:::journal_rewrite(s, b, after_b, "checkpoint", summary = "s2")
s$history <- c(after_b, anthropic_round(4))
corteza:::journal_snapshot(s)
expect_identical(corteza:::journal_turn_slice(s, mark, 3L),
                 c(anthropic_round(2), anthropic_round(3), anthropic_round(4)))
# pre_len is ignored once a journal is present: the mark knows better.
expect_identical(corteza:::journal_turn_slice(s, mark, 0L),
                 c(anthropic_round(2), anthropic_round(3), anthropic_round(4)))

# Rollover: an earlier live snapshot becomes the first segment --------

dir <- journal_tmp()
s1 <- journal_session(dir, h)
corteza:::journal_snapshot(s1)
resumed <- list(user_msg("resumed"),
                list(role = "assistant", content = "welcome back"))
s2 <- journal_session(dir, resumed)
j <- corteza:::journal_load(dir)
expect_equal(length(j$segments), 1L)
expect_equal(j$segments[[1]]$reason, "rollover")
expect_identical(j$segments[[1]]$history, h)
expect_identical(j$segments[[1]]$after, resumed)
expect_identical(j$live$history, resumed)
expect_equal(j$live$inherited, 2L)
expect_identical(corteza:::journal_history(dir), h)
expect_equal(corteza:::journal_mark(s2)$seq, 1L)
s2$history <- c(resumed, anthropic_round(9))
corteza:::journal_snapshot(s2)
expect_identical(corteza:::journal_history(dir), c(h, anthropic_round(9)))

# apply_exit_marker leaves a snapshot that includes the repair --------

dir <- journal_tmp()
s <- journal_session(dir, list(user_msg("go"), anthropic_round(1)[[1]]))
corteza:::apply_exit_marker(s, "go", 0L, "[interrupted]")
expect_equal(length(s$history), 4L)
expect_identical(corteza:::journal_load(dir)$live$history, s$history)

# Atomic write replaces an existing file and leaves no temp behind --------

p <- file.path(journal_tmp(), "x.json")
corteza:::.journal_write_json(list(a = 1), p)
corteza:::.journal_write_json(list(a = 2), p)
expect_equal(jsonlite::fromJSON(p)$a, 2)
expect_false(file.exists(paste0(p, ".tmp")))

# Derived directory follows sessionId, and config turns it off --------

run_derived_dir_test <- function() {
    tmp_data <- tempfile("corteza_journal_data_")
    dir.create(tmp_data, recursive = TRUE)
    on.exit(unlink(tmp_data, recursive = TRUE), add = TRUE)
    old <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
    Sys.setenv(R_USER_DATA_DIR = tmp_data)
    on.exit({
        if (is.na(old)) Sys.unsetenv("R_USER_DATA_DIR") else {
            Sys.setenv(R_USER_DATA_DIR = old)
        }
    }, add = TRUE)
    sessions <- file.path(tools::R_user_dir("corteza", "data"), "agents",
                          "main", "sessions")

    s <- journal_session(NULL, list(user_msg("hi")))
    expect_null(corteza:::journal_install(s, agent_id = "main"))
    s$sessionId <- "brave_turing"
    first <- file.path(sessions, "brave_turing.journal")
    expect_equal(corteza:::journal_dir(s), first)
    expect_true(corteza:::journal_snapshot(s))
    expect_true(file.exists(file.path(first, "live.json")))

    # /clear gives the session a new id: a new journal, fresh state.
    s$sessionId <- "calm_hopper"
    s$history <- list(user_msg("fresh"))
    expect_true(corteza:::journal_snapshot(s))
    second <- file.path(sessions, "calm_hopper.journal")
    expect_identical(corteza:::journal_load(second)$live$history,
                     list(user_msg("fresh")))
    expect_identical(corteza:::journal_load(first)$live$history,
                     list(user_msg("hi")))
    expect_equal(corteza:::journal_mark(s)$seq, 0L)

    s$config <- list(journal_enabled = FALSE)
    expect_null(corteza:::journal_dir(s))
    expect_false(corteza:::journal_snapshot(s))
}
run_derived_dir_test()

# maybe_compact_turn_session: the segment is written before the rewrite,
# and a journal that cannot be written blocks the rewrite --------

compact_journal_config <- list(
    context_compact_pct = 0,
    subagents = list(context_compaction = list(
        mode = "inherit_strict", compact_pct = 0, keep_recent_turns = 1L,
        keep_recent_tokens = 1L, min_messages = 2L, timeout_seconds = 1L
    ))
)

run_compaction_journal_test <- function() {
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice", function(...) {
        x <- "earlier work"
        attr(x, "usage") <- list()
        x
    }, ns = "corteza")
    history <- list(user_msg("one"), list(role = "assistant", content = "a"),
                    user_msg("two"), list(role = "assistant", content = "b"))

    dir <- journal_tmp()
    s <- journal_session(dir, history)
    s$config <- compact_journal_config
    s$context_window <- 1000L
    seen <- NULL
    s$on_compaction <- function(event) seen <<- event
    expect_true(corteza:::maybe_compact_turn_session(
        s, compact_journal_config, tools = list(), system = "sys",
        reason = "checkpoint"))
    expect_false(is.null(seen))
    j <- corteza:::journal_load(dir)
    expect_equal(length(j$segments), 1L)
    expect_equal(j$segments[[1]]$reason, "checkpoint")
    expect_equal(j$segments[[1]]$summary, "earlier work")
    expect_identical(j$segments[[1]]$history, history)
    expect_identical(j$live$history, s$history)
    expect_equal(length(s$history), 3L)
    expect_identical(corteza:::journal_history(dir), history)

    # Fail closed. chmod means nothing to root, and Windows has no mode.
    if (.Platform$OS.type != "windows" &&
        !identical(Sys.info()[["effective_user"]], "root")) {
        dir2 <- journal_tmp()
        s2 <- journal_session(dir2, history)
        s2$config <- compact_journal_config
        s2$context_window <- 1000L
        Sys.chmod(dir2, mode = "0500")
        on.exit(Sys.chmod(dir2, mode = "0700"), add = TRUE)
        expect_error(corteza:::maybe_compact_turn_session(
            s2, compact_journal_config, tools = list(), system = "sys"),
            "journal")
        expect_identical(s2$history, history)
        expect_equal(s2$compaction_count %||% 0L, 0L)
        Sys.chmod(dir2, mode = "0700")
        expect_equal(length(list.files(dir2, pattern = "^segment-")), 0L)
    }
}
run_compaction_journal_test()

# turn(): the history_callback snapshots, the checkpoint compaction
# leaves a segment, and the record is the whole trajectory --------

run_turn_journal_test <- function() {
    original_agent <- get("agent", envir = asNamespace("llm.api"))
    original_summary <- get("compact_summarize_slice",
                            envir = asNamespace("corteza"))
    on.exit(assignInNamespace("agent", original_agent, ns = "llm.api"),
            add = TRUE)
    on.exit(assignInNamespace("compact_summarize_slice", original_summary,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice", function(...) {
        x <- "earlier work"
        attr(x, "usage") <- list()
        x
    }, ns = "corteza")

    snapshots <- 0L
    stub_agent <- function(prompt, tools = list(), tool_handler = NULL,
                           system = NULL, model = NULL,
                           provider = "anthropic", max_turns = 20L,
                           verbose = TRUE, history = NULL,
                           history_callback = NULL,
                           checkpoint_callback = NULL, ...) {
        h <- c(history %||% list(), list(user_msg(prompt)),
               anthropic_round(1))
        history_callback(h)
        h <- c(h, anthropic_round(2))
        history_callback(h)
        replacement <- checkpoint_callback(h, list(
            agent_turn = 2L, provider = provider, model = model,
            assistant_text = "", tool_call_count = 1L,
            usage = list(input_tokens = 10, output_tokens = 2)
        ))
        if (!is.null(replacement)) h <- replacement$history
        h <- c(h, list(list(role = "assistant", content = "done")))
        history_callback(h)
        list(content = "done", model = model, provider = provider,
             turns = 3L, history = h,
             usage = list(input_tokens = 10, output_tokens = 2))
    }
    assignInNamespace("agent", stub_agent, ns = "llm.api")

    dir <- journal_tmp()
    session <- journal_session(dir)
    session$config <- compact_journal_config
    session$context_window <- 1000L
    mark <- corteza:::journal_mark(session)
    result <- corteza::turn("play", session, tools = list())
    expect_equal(result$reply, "done")

    j <- corteza:::journal_load(dir)
    expect_equal(length(j$segments), 1L)
    expect_equal(j$segments[[1]]$reason, "checkpoint")
    expect_identical(j$live$history, session$history)
    whole <- c(list(user_msg("play")), anthropic_round(1), anthropic_round(2),
               list(list(role = "assistant", content = "done")))
    expect_identical(corteza:::journal_history(dir), whole)
    expect_true(corteza:::journal_rewritten_since(session, mark))
    expect_identical(corteza:::journal_turn_slice(session, mark, 0L), whole)
}
run_turn_journal_test()

# Commit protocol: a segment counts only once the live snapshot that
# follows it acknowledges it, so a failure at either write leaves the
# record and the turn slice exactly as they were --------

run_commit_boundary_tests <- function() {
    # chmod means nothing to root, and Windows has no mode.
    if (.Platform$OS.type == "windows" ||
        identical(Sys.info()[["effective_user"]], "root")) {
        return(invisible())
    }
    h <- c(list(user_msg("one")), anthropic_round(1), anthropic_round(2))
    after <- c(list(summary_msg("s")), anthropic_round(2))

    # The segment write fails: nothing on disk or in state changes, and
    # the retry is clean.
    dir <- journal_tmp()
    s <- journal_session(dir, h)
    corteza:::journal_snapshot(s)
    mark <- corteza:::journal_mark(s)
    Sys.chmod(dir, mode = "0500")
    on.exit(Sys.chmod(dir, mode = "0700"), add = TRUE)
    expect_error(corteza:::journal_rewrite(s, h, after, "threshold"), "journal")
    Sys.chmod(dir, mode = "0700")
    expect_equal(corteza:::journal_mark(s)$seq, 0L)
    expect_equal(length(list.files(dir, pattern = "^segment-")), 0L)
    expect_identical(corteza:::journal_history(dir), h)
    expect_identical(corteza:::journal_turn_slice(s, mark, 5L), list())
    expect_false(corteza:::journal_rewritten_since(s, mark))
    corteza:::journal_rewrite(s, h, after, "threshold")
    expect_equal(corteza:::journal_mark(s)$seq, 1L)
    expect_equal(length(corteza:::journal_load(dir)$segments), 1L)
    expect_identical(corteza:::journal_history(dir), h)

    # The live write fails after the segment is on disk: the segment is
    # not part of the record, the state has not moved, and the retry
    # overwrites it instead of adding a duplicate.
    dir <- journal_tmp()
    s <- journal_session(dir, h)
    corteza:::journal_snapshot(s)
    mark <- corteza:::journal_mark(s)
    blocker <- file.path(dir, "live.json.tmp")
    dir.create(blocker)
    expect_error(corteza:::journal_rewrite(s, h, after, "threshold"), "journal")
    expect_true(file.exists(file.path(dir, "segment-0001.json")))
    expect_equal(corteza:::journal_mark(s)$seq, 0L)
    expect_equal(length(corteza:::journal_load(dir)$segments), 0L)
    expect_identical(corteza:::journal_history(dir), h)
    expect_identical(corteza:::journal_turn_slice(s, mark, 5L), list())
    expect_false(corteza:::journal_rewritten_since(s, mark))
    expect_false(corteza:::journal_snapshot(s))
    unlink(blocker, recursive = TRUE)
    corteza:::journal_rewrite(s, h, after, "threshold")
    s$history <- after # as the caller does once the rewrite is recorded
    expect_equal(corteza:::journal_mark(s)$seq, 1L)
    expect_equal(length(corteza:::journal_load(dir)$segments), 1L)
    expect_identical(corteza:::journal_history(dir), h)
    expect_identical(corteza:::journal_turn_slice(s, mark, 5L), list())
    expect_true(corteza:::journal_rewritten_since(s, mark))

    # A restart after such a failure: the committed count comes from the
    # live snapshot, the stray segment is ignored and then overwritten by
    # the rollover, and the record is intact throughout.
    dir <- journal_tmp()
    s <- journal_session(dir, h)
    corteza:::journal_snapshot(s)
    corteza:::journal_rewrite(s, h, after, "threshold")
    s$history <- c(after, anthropic_round(3))
    corteza:::journal_snapshot(s)
    blocker <- file.path(dir, "live.json.tmp")
    dir.create(blocker)
    after2 <- c(list(summary_msg("t")), anthropic_round(3))
    expect_error(corteza:::journal_rewrite(s, s$history, after2, "checkpoint"),
                 "journal")
    unlink(blocker, recursive = TRUE)
    expect_true(file.exists(file.path(dir, "segment-0002.json")))
    expect_equal(length(corteza:::journal_load(dir)$segments), 1L)
    expect_identical(corteza:::journal_history(dir), c(h, anthropic_round(3)))
    s2 <- journal_session(dir, s$history)
    expect_equal(corteza:::journal_mark(s2)$seq, 2L)
    j <- corteza:::journal_load(dir)
    expect_equal(length(j$segments), 2L)
    expect_equal(j$segments[[2]]$reason, "rollover")
    expect_identical(corteza:::journal_history(dir), c(h, anthropic_round(3)))
}
run_commit_boundary_tests()

# Room journals are owned by the bot as well as the room --------

cfg_a <- list(user_id = "@cornelius:cornball.ai")
cfg_b <- list(user_id = "@padre:cornball.ai")
room <- "!abc:cornball.ai"
expect_false(identical(corteza:::bot_journal_dir(cfg_a, room),
                       corteza:::bot_journal_dir(cfg_b, room)))
expect_identical(corteza:::bot_journal_dir(cfg_a, room),
                 corteza:::bot_journal_dir(cfg_a, room))
expect_false(identical(corteza:::bot_journal_dir(cfg_a, room),
                       corteza:::bot_journal_dir(cfg_a, paste0(room, "|thread"))))
expect_true(grepl("[.]journal$", corteza:::bot_journal_dir(cfg_a, room)))

# A job worker gets a journal beside its checkpoint --------

run_job_worker_journal_test <- function() {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    state <- corteza:::.subagent_state
    old_session <- state$session
    on.exit(assign("session", old_session, envir = state), add = TRUE)
    cwd <- tempfile("corteza_worker_cwd_")
    dir.create(cwd, recursive = TRUE)
    state_dir <- tempfile("corteza_worker_state_")
    spec <- list(
        init_fn = function(spec) {
            corteza::subagent_turn_init(provider = "anthropic",
                                        model = "claude-sonnet-4-6",
                                        tools_filter = "read_file",
                                        system = "worker")
        },
        identity = list(owner = "@bot:x", key = "!r:x", workspace = cwd,
                        role = "doer")
    )
    restored <- corteza:::.job_worker_child_init(cwd, spec, state_dir)
    expect_false(isTRUE(restored$restored))
    expect_equal(corteza:::journal_dir(state$session),
                 file.path(state_dir, "journal"))
    expect_true(dir.exists(file.path(state_dir, "journal")))
    state$session$history <- list(user_msg("job one"))
    expect_true(corteza:::journal_snapshot(state$session))
    expect_identical(
        corteza:::journal_load(file.path(state_dir, "journal"))$live$history,
        list(user_msg("job one")))
}
run_job_worker_journal_test()

# Replacing an existing file where rename does not replace: the old file
# is parked, never deleted, and readers fall back to it --------

run_rename_fallback_tests <- function() {
    original <- get(".journal_rename", envir = asNamespace("corteza"))
    on.exit(assignInNamespace(".journal_rename", original, ns = "corteza"),
            add = TRUE)
    h <- c(list(user_msg("one")), anthropic_round(1), anthropic_round(2))
    after <- c(list(summary_msg("s")), anthropic_round(2))

    # The platform refuses to rename over an existing target, once per
    # target: the park-and-retry path lands the new file and clears the
    # parked copy.
    dir <- journal_tmp()
    s <- journal_session(dir, h)
    corteza:::journal_snapshot(s)
    refused <- character()
    assignInNamespace(".journal_rename", function(from, to) {
        if (file.exists(to) && !to %in% refused) {
            refused <<- c(refused, to)
            return(FALSE)
        }
        original(from, to)
    }, ns = "corteza")
    s$history <- c(h, anthropic_round(3))
    expect_true(corteza:::journal_snapshot(s))
    expect_identical(corteza:::journal_load(dir)$live$history, s$history)
    expect_false(file.exists(file.path(dir, "live.json.prev")))
    expect_false(file.exists(file.path(dir, "live.json.tmp")))
    assignInNamespace(".journal_rename", original, ns = "corteza")

    # The retry fails too (or the process stops between the park and the
    # retry): the parked copy still carries the commit marker, so the
    # committed segment stays committed, and a restart repairs it.
    dir <- journal_tmp()
    s <- journal_session(dir, h)
    corteza:::journal_snapshot(s)
    corteza:::journal_rewrite(s, h, after, "threshold")
    s$history <- c(after, anthropic_round(3))
    corteza:::journal_snapshot(s)
    assignInNamespace(".journal_rename", function(from, to) {
        if (grepl("live[.]json$", to)) {
            return(FALSE)
        }
        original(from, to)
    }, ns = "corteza")
    s$history <- c(s$history, anthropic_round(4))
    expect_false(corteza:::journal_snapshot(s))
    expect_false(file.exists(file.path(dir, "live.json")))
    expect_true(file.exists(file.path(dir, "live.json.prev")))
    j <- corteza:::journal_load(dir)
    expect_equal(j$live$seq, 1L)
    expect_equal(length(j$segments), 1L)
    expect_identical(corteza:::journal_history(dir), c(h, anthropic_round(3)))
    assignInNamespace(".journal_rename", original, ns = "corteza")
    s2 <- journal_session(dir, s$history)
    expect_true(file.exists(file.path(dir, "live.json")))
    expect_false(file.exists(file.path(dir, "live.json.prev")))
    expect_equal(corteza:::journal_mark(s2)$seq, 2L)
    expect_identical(corteza:::journal_history(dir), c(h, anthropic_round(3)))

    # A parked copy beside a live file is a leftover: the live file wins
    # and the repair removes it.
    dir <- journal_tmp()
    s <- journal_session(dir, h)
    corteza:::journal_snapshot(s)
    file.copy(file.path(dir, "live.json"), file.path(dir, "live.json.prev"))
    s$history <- c(h, anthropic_round(3))
    corteza:::journal_snapshot(s)
    s2 <- journal_session(dir, list())
    expect_false(file.exists(file.path(dir, "live.json.prev")))
    expect_identical(corteza:::journal_history(dir), c(h, anthropic_round(3)))
}
run_rename_fallback_tests()

# A monitor's scope change is a rewrite: the previous job's conversation
# is a segment, not an overwritten snapshot --------

run_monitor_scope_journal_test <- function() {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    sub_state <- corteza:::.subagent_state
    worker_state <- corteza:::.job_worker_state
    old_session <- sub_state$session
    old_scope <- worker_state$monitor_scope
    old_run <- worker_state$monitor_run_fn
    on.exit({
        assign("session", old_session, envir = sub_state)
        assign("monitor_scope", old_scope, envir = worker_state)
        assign("monitor_run_fn", old_run, envir = worker_state)
    }, add = TRUE)
    cwd <- tempfile("corteza_monitor_cwd_")
    dir.create(cwd, recursive = TRUE)
    state_dir <- tempfile("corteza_monitor_state_")
    # A stand-in for the monitor's model: one exchange per question,
    # mirrored into the journal the way turn()'s history_callback does.
    spec <- list(
        role = "monitor",
        init_fn = function(spec) {
            corteza::subagent_turn_init(provider = "anthropic",
                                        model = "claude-sonnet-4-6",
                                        tools_filter = "read_file",
                                        system = "monitor")
        },
        run_fn = function(prompt) {
            s <- corteza:::.subagent_state$session
            s$history <- c(s$history, list(user_msg(prompt),
                                           list(role = "assistant",
                                                content = "approve")))
            corteza:::journal_snapshot(s)
            list(reply = "approve", usage = list())
        },
        identity = list(owner = "@bot:x", key = "!r:x", workspace = cwd,
                        role = "monitor")
    )
    corteza:::.job_worker_child_init(cwd, spec, state_dir)
    jdir <- file.path(state_dir, "journal")

    a1 <- corteza:::.job_monitor_child_ask("q1", "job-1")
    expect_equal(a1$reply, "approve")
    a2 <- corteza:::.job_monitor_child_ask("q2", "job-1")
    expect_equal(length(sub_state$session$history), 4L)
    expect_equal(length(corteza:::journal_load(jdir)$segments), 0L)

    a3 <- corteza:::.job_monitor_child_ask("q3", "job-2")
    expect_equal(a3$reply, "approve")
    expect_equal(length(sub_state$session$history), 2L)
    j <- corteza:::journal_load(jdir)
    expect_equal(length(j$segments), 1L)
    expect_equal(j$segments[[1]]$reason, "scope")
    expect_equal(length(j$segments[[1]]$history), 4L)
    expect_equal(j$segments[[1]]$kept, 0L)
    record <- corteza:::journal_history(jdir)
    expect_equal(length(record), 6L)
    expect_equal(record[[1]]$content, "q1")
    expect_equal(record[[5]]$content, "q3")

    # A journal that cannot record the change keeps the old scope and
    # its history, and the answer is an error for the parent to act on.
    if (.Platform$OS.type != "windows" &&
        !identical(Sys.info()[["effective_user"]], "root")) {
        Sys.chmod(jdir, mode = "0500")
        on.exit(Sys.chmod(jdir, mode = "0700"), add = TRUE)
        a4 <- corteza:::.job_monitor_child_ask("q4", "job-3")
        Sys.chmod(jdir, mode = "0700")
        expect_true(grepl("journal", a4$error %||% ""))
        expect_equal(worker_state$monitor_scope, "job-2")
        expect_equal(length(sub_state$session$history), 2L)
    }
}
run_monitor_scope_journal_test()
