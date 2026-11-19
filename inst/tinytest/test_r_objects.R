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

    # ---- Each object: name, class, shape and size --------------------------
    note <- ns$r_objects_note(session(), envir = env)
    lines <- strsplit(note, "\n", fixed = TRUE)[[1]]
    expect_identical(lines[[1]],
                     "R objects that run_r made, still in the session:")
    expect_true(any(grepl("^- df: data.frame, 3 x 2, [0-9.]+ [kM]?B$", lines)))
    expect_true(any(grepl("^- x: numeric, length 5, [0-9.]+ [kM]?B$", lines)))
    expect_true(any(grepl("^- f: function, [0-9.]+ [kM]?B$", lines)))
    expect_true("- e: environment" %in% lines)
    expect_false(any(grepl("hidden", lines, fixed = TRUE)))
    # Sorted by name, so the list reads the same each time.
    expect_identical(sub(":.*", "", lines[-1]), c("- df", "- e", "- f", "- x"))

    # ---- Nothing of an object's own code runs ------------------------------------
    # An active binding is named, not read. A shape comes from attributes
    # and storage, so a class's dim() and length() methods are not called.
    # A list with a class has no length, and a vector with one keeps its
    # own.
    ran <- character()
    methods_made <- c("dim.wp25probe", "length.wp25probe")
    on.exit(suppressWarnings(rm(list = methods_made, envir = globalenv())),
            add = TRUE)
    assign("dim.wp25probe", function(x) {
        ran <<- c(ran, "dim")
        c(9L, 9L)
    }, envir = globalenv())
    assign("length.wp25probe", function(x) {
        ran <<- c(ran, "length")
        99L
    }, envir = globalenv())
    own <- new.env()
    makeActiveBinding("act", function() {
        ran <<- c(ran, "active")
        1
    }, own)
    own$vec <- structure(1:4, class = "wp25probe")
    own$rec <- structure(list(1, 2), class = "wp25probe")
    own$when <- as.POSIXlt("2026-01-01", tz = "UTC")
    own$m <- matrix(1:6, 2L)
    lines <- strsplit(ns$r_objects_note(session(), envir = own), "\n")[[1]]
    expect_identical(ran, character())
    expect_true("- act: an active binding, not read" %in% lines)
    expect_true(any(grepl("^- vec: wp25probe, length 4, [0-9.]+ [kM]?B$", lines)))
    expect_true(any(grepl("^- rec: wp25probe, [0-9.]+ [kM]?B$", lines)))
    expect_true(any(grepl("^- when: POSIXlt/POSIXt, [0-9.]+ [kM]?B$", lines)))
    expect_true(any(grepl("^- m: matrix/array, 2 x 3, [0-9.]+ [kM]?B$", lines)))
    # The probe's methods do run when called, so the test above can fail.
    expect_identical(dim(own$vec), c(9L, 9L))
    expect_identical(ran, "dim")

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
    lines <- strsplit(ns$r_objects_note(session(before = c("df", "f")),
                                        envir = env), "\n")[[1]]
    expect_identical(sub(":.*", "", lines[-1]), c("- e", "- x"))
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
    lines <- strsplit(ns$r_objects_note(session(), envir = many), "\n")[[1]]
    expect_identical(length(grep("^- obj", lines)), 30L)
    expect_identical(lines[[length(lines)]], "And 10 more; ls() lists them.")
    lines <- strsplit(ns$r_objects_note(session(), envir = many, chars = 100L),
                      "\n")[[1]]
    shown <- length(grep("^- obj", lines))
    expect_true(shown >= 1L && shown < 10L)
    expect_identical(lines[[length(lines)]],
                     sprintf("And %d more; ls() lists them.", 40L - shown))

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
    expect_true(grepl("- a_wp25_made_before: data.frame, 4 x 1,", got$first,
                      fixed = TRUE))
    expect_false(grepl("a_wp25_was_there", got$first, fixed = TRUE))
    # A session that never ran R lists nothing, and neither does a room.
    got <- compact(session(before = NULL))
    expect_identical(got$first, "[compacted history]\n\nthe summary")
    room <- session("matrix", before = NULL)
    run_r(room, "a_wp25_made_before <- 2")
    got <- compact(room)
    expect_true(got$done)
    expect_identical(got$first, "[compacted history]\n\nthe summary")
})
