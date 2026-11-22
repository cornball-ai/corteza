source("isolate.R", local = TRUE)

local({
    ns <- asNamespace("corteza")
    make <- function() {
        s <- corteza::new_session(provider = "anthropic_claude", system = "fixture",
            model_map = list(cloud = "work-claude"), web_search = FALSE)
        s$config <- list(fallback = "work-codex openai_codex")
        ns$talker_enable(s, list(model = "reply-claude", thinking = "between_tools"))
        s
    }
    set <- function(s, text) ns$bot_apply_model_command(s, ns$bot_parse_model_command(text))
    s <- make()
    ack <- set(s, "/model work-codex OpenAI_codex")
    expect_identical(s$provider, "openai_codex")
    expect_identical(s$doer_provider, "openai_codex")
    expect_identical(s$model_map$cloud, "work-codex")
    expect_identical(ns$job_worker_spec(s)$model, "work-codex")
    expect_identical(ns$job_worker_spec(s)$provider, "openai_codex")
    expect_null(s$thinking)
    expect_true(grepl("next reply and the next job", ack, fixed = TRUE))
    set(s, "/model work-claude anthropic_claude")
    expect_identical(s$model_map$cloud, "reply-claude")
    expect_identical(s$doer_model, "work-claude")
    expect_identical(s$thinking, "between_tools")
    before <- list(s$provider, s$doer_provider, s$model_map$cloud, s$doer_model)
    expect_true(grepl("Model unchanged", set(s, "/model whatever missing-provider")))
    expect_identical(list(s$provider, s$doer_provider, s$model_map$cloud, s$doer_model), before)

    # Configured pairs may choose different models and reasoning for each
    # role. Restoring a pair restores its own settings, not the last route's.
    s$config$model_pairs <- list(openai_codex = list(
        talker = list(model = "reply-codex", reasoning_effort = "low"),
        doer = list(model = "work-codex", reasoning_effort = "high")))
    s$.model_pairs$openai_codex <- NULL
    ack <- set(s, "/model work-codex openai_codex")
    expect_identical(s$model_map$cloud, "reply-codex")
    expect_identical(s$reasoning_effort, "low")
    expect_identical(ns$job_worker_spec(s)$reasoning_effort, "high")
    expect_true(grepl("Both use openai_codex", ack, fixed = TRUE))
    s$doer_reasoning_effort <- "xhigh"
    set(s, "/model work-claude anthropic_claude")
    set(s, "/model work-codex openai_codex")
    expect_identical(s$doer_reasoning_effort, "xhigh")
    expect_identical(s$doer_reasoning_default, "high")
    set(s, "/model other-work-codex")
    expect_identical(s$doer_reasoning_effort, "xhigh")
    expect_identical(s$doer_reasoning_default, "high")
    expect_identical(s$model_map$cloud, "reply-codex")

    menu <- ns$bot_available_models(list(provider = "anthropic_claude",
        model = "work-claude", fallback = "work-codex openai_codex"),
        ollama_models = character())
    expect_true(any(vapply(menu, function(x) identical(x$provider, "openai_codex"), logical(1))))

    # The actual fallback executor sees the selected talker, while jobs
    # dispatched during or after that request see its matching doer.
    s <- make()
    s$config$model_pairs <- list(openai_codex = list(
        talker = "reply-codex", doer = "work-codex"))
    ns$.fallback_reset()
    on.exit(ns$.fallback_reset(), add = TRUE)
    calls <- list()
    result <- ns$.agent_with_fallback(list(provider = s$provider,
        model = s$model_map$cloud, system = "fixture", history = list()), s,
        .call = function(args) {
            calls[[length(calls) + 1L]] <<- list(provider = args$provider, model = args$model,
                doer = ns$job_worker_spec(s)[c("provider", "model")])
            if (args$provider == "anthropic_claude") stop("HTTP 429 rate limit")
            list(content = "done", history = args$history)
        })
    expect_identical(calls[[2L]]$model, "reply-codex")
    expect_identical(calls[[2L]]$doer, list(provider = "openai_codex", model = "work-codex"))
    expect_identical(ns$job_worker_spec(s)$provider, "openai_codex")
    expect_identical(s$provider, "anthropic_claude")
    ns$.fallback_reset()
    ns$.agent_with_fallback(list(provider = s$provider, model = s$model_map$cloud,
        system = "fixture", history = list()), s, .call = function(args) {
            expect_identical(args$model, "reply-claude")
            expect_identical(ns$job_worker_spec(s)$model, "work-claude")
            expect_identical(ns$job_worker_spec(s)$provider, "anthropic_claude")
            list(content = "back", history = args$history)
        })
})

# A completed run can compact without making its history longer. A
# quota fallback resumes that history instead of submitting the task again.
local({
    ns <- asNamespace("corteza")
    ns$.fallback_reset()
    on.exit(ns$.fallback_reset(), add = TRUE)
    s <- corteza::new_session(provider = "anthropic", system = "fixture",
        model_map = list(cloud = "fixture"), web_search = FALSE)
    s$config <- list(fallback = "fixture openai_codex")
    s$history <- list(list(role = "user", content = "Old request"),
                     list(role = "assistant", content = "Old answer"))
    attempts <- 0L
    ns$.agent_with_fallback(list(provider = "anthropic", model = "fixture",
        prompt = "Write the fixture file", history = s$history), s,
        .call = function(args) {
            attempts <<- attempts + 1L
            if (args$provider == "anthropic") {
                ns$history_replace(s, list(
                    list(role = "user", content = "Summary: fixture file already written."),
                    list(role = "assistant", content = "Only the report remains.")),
                    "test_compaction")
                stop("HTTP 429 rate limit")
            }
            expect_false(identical(args$prompt, "Write the fixture file"))
            expect_true(grepl("Do not repeat any completed tool call", args$prompt, fixed = TRUE))
            expect_true(any(vapply(args$history, function(m) {
                grepl("already written", ns$message_text(m), fixed = TRUE)
            }, logical(1))))
            list(content = "done", history = args$history)
        })
    expect_identical(attempts, 2L)
})
