# After a compaction the model is told which R objects remain
# (R/r-objects.R). Offline: the summarizer is replaced.
source("isolate.R", local = TRUE)

local({
    ns <- asNamespace("corteza")
    # A session whose first run_r found nothing in the environment, unless
    # told otherwise.
    session <- function(channel = "cli", ..., before = character()) {
        s <- corteza::new_session(channel = channel, provider = "anthropic",
                                  model_map = list(cloud = "claude-test",
                                                   local = NULL),
                                  web_search = FALSE)
        s$r_objects_before <- before
        extra <- list(...)
        for (n in names(extra)) assign(n, extra[[n]], envir = s)
        s
    }
    env <- new.env()
    env$df <- data.frame(a = 1:3, b = letters[1:3])
    env$x <- c(1.5, 2.5, 3.5, 4.5, 5.5)
    env$f <- function(y) y + 1
    env$e <- new.env()
    env$.hidden <- 1

    # ---- The names, sorted, hidden ones left out ----------------------------
    expect_identical(ns$r_objects_note(session(), envir = env),
                     "R objects that run_r made, still in the session: df, e, f, x.")

    # ---- Nothing of an object's own code runs --------------------------------
    # The list reads names only. An active binding's function and a
    # delayed binding's expression do not run, and no method of a class
    # is called. A name that is not syntactic is in backticks.
    # Each records through a function of this block: an expression that
    # delayedAssign() defers runs in this environment itself, where `<<-`
    # would reach past it.
    ran <- character()
    record <- function(what) ran <<- c(ran, what)
    own <- new.env()
    makeActiveBinding("act", function() {
        record("active")
        1
    }, own)
    delayedAssign("lazy", {
        record("delayed")
        2
    }, assign.env = own)
    methods_made <- c("format.wp25probe", "print.wp25probe",
                      "length.wp25probe", "as.character.wp25probe")
    on.exit(suppressWarnings(rm(list = methods_made, envir = globalenv())),
            add = TRUE)
    for (m in methods_made) {
        assign(m, local({
            name <- m
            function(x, ...) {
                record(name)
                "probe"
            }
        }), envir = globalenv())
    }
    own$probe <- structure(list(1), class = "wp25probe")
    assign("a, b\nc", 3, envir = own)
    note <- ns$r_objects_note(session(), envir = own)
    expect_identical(ran, character())
    expect_identical(note, paste0("R objects that run_r made, still in the ",
                                  "session: `a, b\\nc`, act, lazy, probe."))
    # The bindings are as they were: each still runs its code when read,
    # so the test above can fail.
    expect_true(bindingIsActive("act", own))
    expect_identical(get("lazy", envir = own), 2)
    expect_identical(get("act", envir = own), 1)
    expect_identical(ran, c("delayed", "active"))

    # ---- Whose objects --------------------------------------------------------
    # The terminal, chat() and a child own theirs. A room shares the bot's
    # global environment, and run_r in a worker keeps its objects there.
    expect_true(nzchar(ns$r_objects_note(session("console"), envir = env)))
    expect_identical(ns$r_objects_note(session("matrix"), envir = env), "")
    expect_true(nzchar(ns$r_objects_note(session("matrix", is_subagent = TRUE),
                                         envir = env)))
    worker <- session(config = list(run_r_mode = "worker"))
    expect_identical(ns$r_objects_note(worker, envir = env), "")
    odd <- session(config = list(run_r_mode = "elsewhere"))
    expect_identical(ns$r_objects_note(odd, envir = env), "")

    # ---- Only what run_r made ---------------------------------------------------
    # What the environment held before the session's first run_r is left
    # out, and before any run_r there is nothing of the session's.
    expect_identical(ns$r_objects_note(session(before = c("df", "f")),
                                       envir = env),
                     "R objects that run_r made, still in the session: e, x.")
    expect_identical(ns$r_objects_note(session(before = NULL), envir = env), "")
    expect_identical(ns$r_objects_note(session(), envir = new.env()), "")
    # The mark is taken once, at the first call.
    s <- session(before = NULL)
    ns$r_objects_mark(s, envir = env)
    expect_identical(s$r_objects_before, c("df", "e", "f", "x"))
    ns$r_objects_mark(s, envir = new.env())
    expect_identical(s$r_objects_before, c("df", "e", "f", "x"))

    # ---- The caps ---------------------------------------------------------------
    many <- new.env()
    for (i in 1:40) assign(sprintf("obj%02d", i), i, envir = many)
    note <- ns$r_objects_note(session(), envir = many)
    expect_identical(lengths(regmatches(note, gregexpr("obj[0-9]+", note))), 30L)
    expect_true(endsWith(note, "obj30, and 10 more; ls() lists them."))
    note <- ns$r_objects_note(session(), envir = many, chars = 30L)
    expect_identical(note, paste("R objects that run_r made, still in the",
                                 "session: obj01, obj02, obj03, obj04, and 36",
                                 "more; ls() lists them."))
    note <- ns$r_objects_note(session(), envir = many, chars = 3L)
    expect_identical(note, paste("R objects that run_r made, still in the",
                                 "session: 40, which ls() lists."))

    # ---- Open handles ------------------------------------------------------------
    held <- new.env()
    store <- ns$handle_store_for(held)
    assign(".h_002", list(value = 2), envir = store)
    assign(".h_001", list(value = 1), envir = store)
    expect_identical(ns$r_objects_note(session(), envir = held),
                     "Open handles, which read_handle reads: .h_001, .h_002.")

    # ---- Through a compaction ------------------------------------------------------
    # The summary the history keeps ends with the list. A room's does not.
    original <- get("compact_summarize_slice", envir = ns)
    on.exit(assignInNamespace("compact_summarize_slice", original,
                              ns = "corteza"), add = TRUE)
    assignInNamespace("compact_summarize_slice",
                      function(...) "the summary", ns = "corteza")
    # The object is made by run_r, as a model would make it. One already
    # in the global environment at the session's first run_r is left out.
    # Both are named to sort first, ahead of whatever else is there.
    made <- c("a_wp25_made_before", "a_wp25_was_there")
    on.exit(suppressWarnings(rm(list = made, envir = globalenv())), add = TRUE)
    assign("a_wp25_was_there", 1, envir = globalenv())
    run_r <- function(s, code) {
        ns$.tool_run_r_session(code, ctx = list(session = s))
    }
    turns <- list(list(role = "user", content = "q1"),
                  list(role = "assistant", content = "a1"),
                  list(role = "user", content = "q2"),
                  list(role = "assistant", content = "a2"),
                  list(role = "user", content = "q3"),
                  list(role = "assistant", content = "a3"))
    config <- list(context_compact_pct = 90L,
                   subagents = list(context_compaction = list(
                       keep_recent_turns = 1L, timeout_seconds = 1L)))
    compact <- function(s) {
        s$history <- turns
        done <- ns$maybe_compact_turn_session(s, config, tools = list(),
                                              system = "x", threshold = 0,
                                              min_messages = 2L)
        list(done = isTRUE(done), first = s$history[[1]]$content)
    }
    s <- session(before = NULL)
    run_r(s, "a_wp25_made_before <- data.frame(id = 1:4)")
    got <- compact(s)
    expect_true(got$done)
    expect_true(startsWith(got$first, "[compacted history]\n\nthe summary\n\n"))
    expect_true(grepl(paste("R objects that run_r made, still in the session:",
                            "a_wp25_made_before"), got$first, fixed = TRUE))
    expect_false(grepl("a_wp25_was_there", got$first, fixed = TRUE))
    # The compaction leaves the object itself in place.
    expect_identical(get("a_wp25_made_before", envir = globalenv()),
                     data.frame(id = 1:4))
    # A session that never ran R lists nothing, and neither does a room.
    got <- compact(session(before = NULL))
    expect_identical(got$first, "[compacted history]\n\nthe summary")
    room <- session("matrix", before = NULL)
    run_r(room, "a_wp25_made_before <- 2")
    got <- compact(room)
    expect_true(got$done)
    expect_identical(got$first, "[compacted history]\n\nthe summary")
})
