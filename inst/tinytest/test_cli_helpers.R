library(tinytest)

# format_status_summary is a pure formatter — it should produce
# something printable from a tame fake session even if /context
# helpers haven't been wired yet.
status <- corteza:::format_status_summary(
                                          session = list(sessionKey = "abc123"),
                                          provider = "anthropic",
                                          display_model = "claude-sonnet-4-6",
                                          tools = list(list(name = "read_file"),
                                                       list(name = "write_file")),
                                          opts = list(dry_run = FALSE),
                                          config = list(approval_mode = "ask"),
                                          session_tokens = 1234,
                                          context_limit = 200000,
                                          context_files = c("a.md", "b.md"),
                                          skill_docs = c("s1")
)
expect_true(grepl("Session: abc123", status, fixed = TRUE))
expect_true(grepl("anthropic", status, fixed = TRUE))
expect_true(grepl("Tools: 2", status, fixed = TRUE))
expect_true(grepl("Dry-run: off", status, fixed = TRUE))
expect_true(grepl("Approval mode: ask", status, fixed = TRUE))

config_text <- corteza:::format_config_summary(
                                               config = list(approval_mode = "ask",
                                                             dangerous_tools = c("bash", "write_file")),
                                               provider = "openai",
                                               display_model = "gpt-4o",
                                               opts = list(port = 7850, tools = NULL)
)
expect_true(grepl("provider: openai", config_text, fixed = TRUE))
expect_true(grepl("model: gpt-4o", config_text, fixed = TRUE))
expect_true(grepl("port: 7850", config_text, fixed = TRUE))
expect_true(grepl("tools: all", config_text, fixed = TRUE))
expect_true(grepl("dangerous tools: bash, write_file", config_text,
                  fixed = TRUE))

# format_doctor_report exposes seams for the network-touching
# provider_status() and the process-touching in_git_repo() so tests
# can stub both. Confirm the doctor report renders cleanly without
# hitting the network or shelling out to git.
doctor <- corteza:::format_doctor_report(
                                         cwd = "/tmp/proj",
                                         session = list(sessionKey = "k1", model = "x"),
                                         provider = "anthropic",
                                         display_model = "claude-sonnet-4-6",
                                         tools = list(list(name = "read_file")),
                                         config = list(approval_mode = "ask"),
                                         context_files = character(),
                                         skill_docs = character(),
                                         provider_check_fn = function(provider, model = NULL) {
                                             list(ok = TRUE, message = "stubbed reachable")
                                         },
                                         git_check_fn = function() TRUE
)
expect_true(grepl("corteza doctor", doctor, fixed = TRUE))
expect_true(grepl("provider: anthropic", doctor, fixed = TRUE))
expect_true(grepl("stubbed reachable", doctor, fixed = TRUE))
expect_true(grepl("git: repository detected", doctor, fixed = TRUE))
expect_true(grepl("project approvals: none", doctor, fixed = TRUE))

# provider_status: env-var sniff without hitting the cloud. Use
# Sys.setenv inside a local() to avoid leaking out.
local({
    old <- Sys.getenv("ANTHROPIC_API_KEY")
    on.exit(Sys.setenv(ANTHROPIC_API_KEY = old), add = TRUE)
    Sys.setenv(ANTHROPIC_API_KEY = "fake-test-key")
    p <- corteza:::provider_status("anthropic")
    expect_true(isTRUE(p$ok))
    expect_true(grepl("ANTHROPIC_API_KEY", p$message, fixed = TRUE))

    Sys.unsetenv("ANTHROPIC_API_KEY")
    p2 <- corteza:::provider_status("anthropic")
    expect_false(isTRUE(p2$ok))
    expect_true(grepl("missing", p2$message, fixed = TRUE))
})

# resolve_provider_model: moonshot kimi-k2 → kimi-k2.6 rewrite.
expect_identical(corteza:::resolve_provider_model("moonshot", "kimi-k2"),
                 "kimi-k2.6")
expect_identical(corteza:::resolve_provider_model("moonshot", "kimi-k2.6"),
                 "kimi-k2.6")
# A NULL model falls back via default_provider_model (which delegates
# to llm.api's table). Assert it tracks that, not a pinned string, so
# the test survives llm.api's model updates.
expect_identical(corteza:::resolve_provider_model("anthropic"),
                 corteza::default_provider_model("anthropic"))
expect_identical(corteza:::resolve_provider_model("moonshot"),
                 corteza::default_provider_model("moonshot"))
expect_true(nzchar(corteza:::resolve_provider_model("moonshot")))
# Unknown provider: NULL passes through so chat() can keep its
# "(provider default)" fallback for the display string.
expect_null(corteza:::resolve_provider_model("nonesuch"))

# preferred_chat_temperature: moonshot forces 1.
expect_equal(corteza:::preferred_chat_temperature("moonshot", 0.2), 1)
expect_equal(corteza:::preferred_chat_temperature("anthropic", 0.3), 0.3)

# truncate_output: line + char limits each kick in.
big <- paste(sprintf("line %03d", 1:500), collapse = "\n")
clipped <- corteza:::truncate_output(big, max_lines = 50L,
                                     max_chars = 10000L)
expect_true(nchar(clipped) < nchar(big))
expect_true(grepl("[truncated at 50 lines]", clipped, fixed = TRUE))

short <- "tiny"
expect_identical(corteza:::truncate_output(short), short)

empty <- corteza:::truncate_output("")
expect_identical(empty, "")

# run_review with a stubbed chat_fn proves it assembles the prompt
# correctly without hitting the network.
captured <- NULL
fake_chat <- function(prompt, provider, model, system, temperature, ...) {
    captured <<- list(prompt = prompt, provider = provider, model = model)
    list(content = "No findings.")
}
out <- corteza:::run_review(
                            provider = "anthropic",
                            model = NULL,
                            diff_target = "HEAD",
                            diff_status = " M R/foo.R",
                            diff_text = "diff body",
                            chat_fn = fake_chat
)
expect_identical(out$content, "No findings.")
expect_true(grepl("Git diff target: HEAD", captured$prompt, fixed = TRUE))
expect_true(grepl(" M R/foo.R", captured$prompt, fixed = TRUE))
expect_true(grepl("diff body", captured$prompt, fixed = TRUE))
expect_identical(captured$model, "claude-sonnet-4-6")

# do_compact with a stubbed chat_fn proves it assembles the summary
# prompt and returns the structured result without hitting the
# network.
captured2 <- NULL
fake_chat2 <- function(prompt, provider, model, system, temperature, ...) {
    captured2 <<- prompt
    list(content = "summary text here")
}
sess <- list(
    messages = list(
        list(role = "user", content = "hello"),
        list(role = "assistant",
             content = list(list(type = "text", text = "hi there")))
    )
)
emitted <- character()
fake_emit <- function(msg) emitted <<- c(emitted, msg)
result <- corteza:::do_compact(sess, "anthropic", NULL,
                               chat_fn = fake_chat2,
                               emit = fake_emit)
expect_identical(result$summary, "summary text here")
expect_true(result$tokens > 0L)
expect_true(any(grepl("Auto-compacting", emitted)))
expect_true(any(grepl("Compacted to", emitted)))
expect_true(grepl("[user]: hello", captured2, fixed = TRUE))
expect_true(grepl("[assistant]: hi there", captured2, fixed = TRUE))

# do_compact returns NULL when the chat call errors.
fail_result <- corteza:::do_compact(
                                    sess, "anthropic", NULL,
                                    chat_fn = function(...) stop("simulated failure"),
                                    emit = function(...) invisible()
)
expect_null(fail_result)

# compact_message_text: short bodies unchanged, huge ones elided.
expect_equal(corteza:::compact_message_text("short"), "short")
local({
    big <- strrep("y", 20000L)
    out <- corteza:::compact_message_text(big)
    expect_true(grepl("large message elided", out, fixed = TRUE))
    expect_true(nchar(out) < nchar(big))
})

# Emergency compaction: a giant tool-result body already in history must
# not blow the summary prompt. do_compact() elides it to a marker so
# /compact can recover an already-wedged session.
local({
    huge <- paste(sprintf("row %d", 1:50000), collapse = "\n")
    sess_big <- list(messages = list(
        list(role = "user", content = "run the thing"),
        # stand-in for a huge tool_result already mirrored into history
        list(role = "user", content = huge)
    ))
    captured_big <- NULL
    fake_chat_big <- function(prompt, provider, model, system, temperature, ...) {
        captured_big <<- prompt
        list(content = "ok")
    }
    res <- corteza:::do_compact(sess_big, "anthropic", NULL,
                                chat_fn = fake_chat_big,
                                emit = function(...) invisible())
    expect_false(is.null(res))
    expect_true(nchar(captured_big) < nchar(huge))
    expect_true(grepl("large message elided", captured_big, fixed = TRUE))
    # the small message survives verbatim
    expect_true(grepl("[user]: run the thing", captured_big, fixed = TRUE))
})

# .compact_trim_total: drops oldest rendered messages when the aggregate
# overflows the total budget, keeping the most recent + a note.
local({
    msgs <- vapply(1:20, function(i) strrep(sprintf("m%02d-", i), 2000L),
                   character(1L))
    trimmed <- corteza:::.compact_trim_total(msgs, max_total = 50000L)
    expect_true(length(trimmed) < length(msgs))
    expect_true(grepl("earlier message", trimmed[1], fixed = TRUE))
    # the newest message is retained
    expect_true(any(grepl("m20-", trimmed)))
})
