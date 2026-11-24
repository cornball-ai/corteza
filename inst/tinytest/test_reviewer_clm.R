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
    ask <- function(lines, record = "conversation-A", args = list(path = "a.R")) {
        q <- list(request_id = "r000001", tool = "write_file",
                  args = args, record = record, conversation = lines)
        ns$.job_monitor_child_ask(q, record)
    }
    expect_null(ask(first)$error)
    expect_null(ask(c(first, last))$error)
    expect_true(grepl("stop editing a.R", calls[[2L]], fixed = TRUE))
    expect_false(grepl("edit only a.R", calls[[2L]], fixed = TRUE))
    expect_equal(length(ns$.review_inputs(s$history)), 2L)
    expect_true(grepl("NEW LINES", calls[[2L]], fixed = TRUE))
    original <- worker$review_record
    oversized <- list(content = strrep("a", 2100L))
    parent <- corteza::new_session()
    parent$.review_window <- list(id = "conversation-A", chars = 116100L)
    expect_error(ns$job_monitor_ask(parent, list(args = oversized)), "evidence limit")
    expect_identical(parent$.review_window$id, "conversation-A")
    expect_null(parent$.job_monitor)
    expect_true(grepl("100,000 new characters", ns$.review_record_limit_reason(216100)))
    expect_error(ask(c(first, last), args = oversized), "evidence limit")
    expect_identical(worker$review_record, original)
    expect_null(ask(c(first, last))$error)
    expect_equal(length(calls), 3L)
    expect_error(ns$history_replace(s, s$history[-1L], "compaction"), "instructions")
    expect_true(nzchar(ask(c(first, last), record = "conversation-B")$error))
    expect_true(nzchar(ask(c(first, paste0(last, " changed")))$error))
    s$history <- s$history[-1L]
    expect_true(nzchar(ask(c(first, last))$error))
    expect_equal(length(calls), 3L)
    expect_error(ns$job_monitor_question(list(args = list(command = strrep("a", 2100L)))),
                 "evidence limit")
    explanation <- strrep("a", 1500L)
    goal <- strrep("b", 4000L)
    evidence <- ns$.review_evidence(list(narration = explanation, goal = goal))
    expect_identical(evidence$narration, explanation)
    expect_identical(evidence$goal, goal)
    expect_true(grepl(goal, ns$job_monitor_question(list(goal = goal)), fixed = TRUE))
    expect_error(ns$.review_evidence(list(narration = paste0(explanation, "x"))),
                 "explanation exceeds 1,500 characters")
    expect_error(ns$.review_evidence(list(goal = paste0(goal, "x"))),
                 "delegated task exceeds 4,000 characters")
    question <- list(goal = paste0(goal, "x"), args = list(path = "a.R"))
    response <- ns$job_monitor_ask_wait(parent, question)
    expect_identical(response$verdict, "escalate")
    expect_true(grepl("delegated task exceeds 4,000", response$reason, fixed = TRUE))
    expect_null(parent$.job_monitor)
    s$.review_limit <- 10000L
    s$.review_prefix <- list(system = strrep("a", 2000L))
    expect_error(ns$.review_check_size(s, list(), strrep("b", 5000L)), "budget")
})

# A real provider count applies only to the unchanged request prefix that it covered.
local({
    ns <- asNamespace("corteza")
    s <- new.env(parent = emptyenv())
    s$.protect_review_inputs <- TRUE
    s$.review_limit <- 15000
    s$.review_prefix <- list(system = "review")
    args <- list(provider = "openai_codex", model = "m", prompt = strrep("word ", 3500))
    s$.review_route <- args[c("provider", "model")]
    history <- list(list(role = "user", content = args$prompt))
    expect_error(ns$.review_check_size(s, history), "budget")
    ns$.review_measure_request(s, args, list(total_tokens = 3700, input_tokens = 3580))
    expect_silent(ns$.review_check_size(s, history, "Person: stop"))
    expect_error(ns$.review_check_size(s, history, strrep("x", 15000)), "budget")
    s$.review_route$model <- "other"
    expect_error(ns$.review_check_size(s, history), "budget")
    s$.review_route$model <- "m"
    history[[1]]$content <- paste0(history[[1]]$content, " changed")
    expect_error(ns$.review_check_size(s, history), "budget")
})
