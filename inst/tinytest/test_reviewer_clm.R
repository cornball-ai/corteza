source("isolate.R", local = TRUE)

# The monitor reads a complete record on first use. A second ruling can
# cross the cold-start ceiling only from exactly the same retained inputs.
local({
    ns <- asNamespace("corteza")
    state <- ns$.subagent_state
    worker <- ns$.job_worker_state
    keys <- c("review_record", "monitor_scope", "monitor_run_fn")
    old <- mget(keys, worker, ifnotfound = rep(list(NULL), length(keys)))
    old_session <- state$session
    on.exit({
        state$session <- old_session
        list2env(old, worker)
    }, add = TRUE)
    s <- state$session <- corteza::new_session()
    calls <- character()
    worker$monitor_run_fn <- function(prompt) {
        calls <<- c(calls, prompt)
        s$history <- c(s$history, list(list(role = "user", content = prompt),
                                       list(role = "assistant", content = "ruling")))
        list(reply = "ruling")
    }
    first <- paste("Person: edit only a.R", strrep("a", 58000))
    last <- paste("Person: stop editing a.R", strrep("b", 58000))
    ask <- function(lines, record = "conversation-A") {
        q <- list(request_id = "r000001", tool = "write_file",
                  args = list(path = "a.R"), record = record, conversation = lines)
        ns$.job_monitor_child_ask(q, record)
    }
    expect_null(ask(first)$error)
    expect_null(ask(c(first, last))$error)
    expect_true(grepl("stop editing a.R", calls[[2L]], fixed = TRUE))
    expect_false(grepl("edit only a.R", calls[[2L]], fixed = TRUE))
    expect_equal(length(ns$.review_inputs(s$history)), 2L)
    expect_error(ns$history_replace(s, s$history[-1L], "compaction"), "instructions")
    expect_true(nzchar(ask(c(first, last), record = "conversation-B")$error))
    expect_true(nzchar(ask(c(first, paste0(last, " changed")))$error))
    s$history <- s$history[-1L]
    expect_true(nzchar(ask(c(first, last))$error))
    expect_equal(length(calls), 2L)
    expect_error(ns$job_monitor_question(list(args = list(command = strrep("a", 2100L)))),
                 "evidence limit")
    s$.review_limit <- 10000L
    s$.review_prefix <- list(system = strrep("a", 2000L))
    expect_error(ns$.review_check_size(s, list(), strrep("b", 5000L)), "budget")
})
