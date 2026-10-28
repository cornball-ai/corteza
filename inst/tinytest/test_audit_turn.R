# Real provider loop with a fake adapter: duplicate tool names and a denied
# call exercise IDs before policy; no model or live state is used.
local({
    ns <- asNamespace("corteza")
    root <- tempfile("audit-turn-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
    on.exit(if (is.na(key)) Sys.unsetenv("ANTHROPIC_API_KEY") else
        Sys.setenv(ANTHROPIC_API_KEY = key), add = TRUE)
    original <- get(".agent_anthropic", asNamespace("llm.api"))
    on.exit(utils::assignInNamespace(".agent_anthropic", original, "llm.api"), add = TRUE)
    requests <- executions <- 0L
    mode <- "tools"
    utils::assignInNamespace(".agent_anthropic", function(...) {
        requests <<- requests + 1L
        if (mode == "error") stop(structure(list(message = "fixture error", call = NULL),
            class = c("fixture_error", "error", "condition")))
        if (mode == "interrupt") stop(structure(list(message = "fixture interrupt", call = NULL),
            class = c("interrupt", "condition")))
        calls <- if (requests == 1L) lapply(c("allow", "deny"), function(path)
            list(id = path, name = "read_file", arguments = list(path = path))) else list()
        blocks <- if (length(calls)) lapply(calls, function(x)
            list(type = "tool_use", id = x$id, name = x$name, input = x$arguments)) else
                list(list(type = "text", text = "finished"))
        list(text = "finished", tool_calls = calls,
            assistant_message = list(role = "assistant", content = blocks),
            usage = list(input_tokens = 7L, output_tokens = 3L))
    }, "llm.api")
    seen_ids <- character()
    opt <- options(corteza.policy = function(call) {
        seen_ids <<- c(seen_ids, call$call_id)
        list(model = "cloud", approval = if (call$args$path == "deny") "deny" else "allow",
            reason = "fixture")
    })
    on.exit(options(opt), add = TRUE)
    s <- corteza::new_session(provider = "anthropic", model_map = list(cloud = "fixture"),
        web_search = FALSE)
    s$audit_dir <- root
    s$config <- list(context_edit = list(enabled = FALSE), context_compact_pct = Inf)
    s$journal_dir <- file.path(root, "journal")
    corteza:::journal_install(s, dir = s$journal_dir)
    set.seed(42)
    seed <- .Random.seed
    answer <- corteza:::.audit_with_origin(s, list(kind = "matrix", event_id = "fixture-event"),
        corteza::turn("test", s, tools = list(), tool_executor = function(...) {
            executions <<- executions + 1L
            "result"
        }))
    expect_identical(.Random.seed, seed)
    expect_equal(requests, 2L)
    expect_equal(executions, 1L)
    expect_equal(length(unique(seen_ids)), 2L)
    expect_true(all(startsWith(seen_ids, paste0(answer$audit$turn_id, ":c"))))
    expect_null(s$audit_origin)
    records <- corteza:::auto_log_read(answer$audit$stream)
    by_type <- function(type) Filter(function(x) identical(x$type, type),
        corteza:::auto_log_read(answer$audit$stream)$records)
    expect_equal(records$n_malformed, 0L)
    expect_equal(by_type("turn_start")[[1]]$origin$event_id, "fixture-event")
    end <- by_type("turn_end")[[1]]
    expect_equal(end$usage$total_tokens, 20)
    expect_identical(end$usage_source, "canonical")
    expect_identical(end$termination$status, "done")
    expect_equal(end$journal$seq, 0)
    expect_equal(length(by_type("policy")), 2L)
    expect_true(any(vapply(by_type("tool_event"), function(x) identical(x$outcome, "deny"), TRUE)))
    mode <- "error"
    caught <- tryCatch(corteza::turn("fail", s, tools = list()), error = identity)
    expect_inherits(caught, "fixture_error")
    end <- tail(by_type("turn_end"), 1)[[1]]
    expect_identical(end$termination$status, "error")
    expect_true("fixture_error" %in% unlist(end$condition_classes))
    expect_null(end$usage)
    expect_identical(tail(by_type("turn_start"), 1)[[1]]$origin$kind, "unknown")
    mode <- "interrupt"
    caught <- tryCatch(corteza::turn("stop", s, tools = list()), interrupt = identity)
    expect_inherits(caught, "interrupt")
    expect_identical(tail(by_type("turn_end"), 1)[[1]]$termination$status, "interrupted")
    expect_equal(length(by_type("turn_start")), 3L)
    expect_equal(length(by_type("turn_end")), 3L)
    expect_null(s$.audit_turn)
    if (.Platform$OS.type == "unix") {
        expect_equal(as.integer(file.info(answer$audit$stream)$mode), strtoi("600", 8L))
    }
})

# Failed observational writes leave one notice and a persisted gap on recovery.
local({
    root <- tempfile("audit-gap-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    s <- new.env(parent = emptyenv())
    s$audit_dir <- root
    state <- corteza:::.audit_state(s)
    runtime <- corteza:::.audit_runtime
    old_notice <- runtime$noticed
    runtime$noticed <- FALSE
    on.exit(runtime$noticed <- old_notice, add = TRUE)
    writer <- get(".audit_write", asNamespace("corteza"))
    on.exit(utils::assignInNamespace(".audit_write", writer, "corteza"), add = TRUE)
    utils::assignInNamespace(".audit_write", function(...) stop("disk full"), "corteza")
    notices <- character()
    withCallingHandlers({
        expect_false(corteza:::.audit_append(state, "turn_start", turn_id = "lost"))
        expect_false(corteza:::.audit_append(state, "turn_end", turn_id = "lost"))
    }, message = function(m) {
        notices <<- c(notices, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    expect_equal(length(notices), 1L)
    expect_equal(state$lost, 2)
    utils::assignInNamespace(".audit_write", writer, "corteza")
    expect_true(corteza:::.audit_append(state, "turn_start", turn_id = "next"))
    records <- corteza:::auto_log_read(state$path)$records
    expect_equal(vapply(records, `[[`, "", "type"), c("gap", "turn_start"))
    expect_equal(records[[1]]$lost_records, 2)
    expect_true(nzchar(records[[1]]$first_failure))
    expect_equal(state$lost, 0)
    # A restart reserves another stream even when its counter was reset.
    old_counter <- runtime$counter
    runtime$counter <- old_counter - 1
    on.exit(runtime$counter <- max(runtime$counter, old_counter), add = TRUE)
    other <- new.env(parent = emptyenv())
    other$audit_dir <- root
    second <- corteza:::.audit_state(other)
    expect_false(identical(state$id, second$id))
    expect_true(corteza:::.audit_append(second, "turn_start", turn_id = "restart"))
    expect_equal(length(corteza:::auto_log_read(state$path)$records), 2L)
    cat('{"broken":', file = state$path, append = TRUE)
    damaged <- corteza:::auto_log_read(state$path)
    expect_equal(damaged$n_malformed, 1L)
    expect_true(damaged$torn_tail)
    expect_true(corteza:::.audit_append(state, "turn_end", turn_id = "next"))
    damaged <- corteza:::auto_log_read(state$path)
    expect_equal(damaged$malformed_lines, 3L)
    expect_true(damaged$interior_damage)
    expect_equal(length(damaged$records), 3L)
})

local({
    root <- tempfile("audit-null-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    old <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
    Sys.setenv(R_USER_DATA_DIR = root)
    on.exit(if (is.na(old)) Sys.unsetenv("R_USER_DATA_DIR") else Sys.setenv(R_USER_DATA_DIR = old), add = TRUE)
    s <- list(sessionId = "audit-fixture", cwd = root)
    path <- corteza:::transcript_append(s, "assistant", "unknown")
    corteza:::transcript_append(s, "assistant", "known", usage = list(input_tokens = 7, output_tokens = 3))
    rows <- lapply(readLines(path), jsonlite::fromJSON, simplifyVector = FALSE)
    expect_true(grepl('"usage":null', tail(readLines(path), 2)[[1]], fixed = TRUE))
    expect_null(tail(rows, 2)[[1]]$usage)
    expect_equal(tail(rows, 1)[[1]]$usage$input_tokens, 7)
    s <- new.env(parent = emptyenv())
    s$journal_enabled <- FALSE
    s$journal_dir <- file.path(root, "history")
    expect_equal(corteza:::.audit_root(s), file.path(s$journal_dir, "audit"))
    s$config <- list(audit = list(enabled = FALSE))
    expect_null(corteza:::.audit_state(s))
})
