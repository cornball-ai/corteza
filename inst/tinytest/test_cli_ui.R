library(tinytest)

call <- list(
    tool = "write_file",
    args = list(path = "R/chat.R", content = "x"),
    channel = "cli"
)
decision <- list(model = "cloud", reason = "default: code/write/cli")
lines <- corteza:::cli_approval_lines(
    call,
    decision,
    gate_reason = "Config requires approval for write_file.",
    cwd = "/tmp/project",
    persistent_label = "Allow always for this session"
)

expect_true(any(grepl("Write file", lines, fixed = TRUE)))
expect_true(any(grepl("Write access to local files", lines, fixed = TRUE)))
expect_true(any(grepl("Path: R/chat.R", lines, fixed = TRUE)))
expect_true(any(grepl("Allow always for this session", lines, fixed = TRUE)))
expect_true(any(grepl("Policy: default: code/write/cli", lines, fixed = TRUE)))

summary_start <- corteza:::cli_event_summary(list(
    event = "tool_call",
    tool = "bash",
    args = list(command = "git status\nls")
))
expect_equal(summary_start$kind, "start")
expect_true(grepl("Bash\\(git status\\)", summary_start$title))
expect_true(any(grepl("git status", summary_start$detail_lines, fixed = TRUE)))

summary_result <- corteza:::cli_event_summary(list(
    event = "tool_result",
    tool = "bash",
    success = TRUE,
    result_lines = 3L,
    elapsed_ms = 15
))
expect_equal(summary_result$kind, "ok")
expect_true(any(grepl("3 lines in 15ms", summary_result$detail_lines,
                      fixed = TRUE)))

pretty_call <- tryCatch(
    capture.output(corteza:::.cli_render_event(list(
        event = "tool_call",
        tool = "read_file",
        args = list(path = "/tmp/x")
    ), pretty = TRUE)),
    error = function(e) e
)
expect_false(inherits(pretty_call, "error"))

pretty_result <- tryCatch(
    capture.output(corteza:::.cli_render_event(list(
        event = "tool_result",
        tool = "read_file",
        success = TRUE,
        result_lines = 2L,
        elapsed_ms = 4
    ), pretty = TRUE)),
    error = function(e) e
)
expect_false(inherits(pretty_result, "error"))
