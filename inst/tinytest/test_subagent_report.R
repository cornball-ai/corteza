# Cursor-inspired structured completion reports are optional. Existing callers
# still get the exact character reply; report callers get rendered metadata and
# a machine-readable attribute.

plain <- corteza:::.extract_subagent_report("ordinary reply", requested = FALSE)
expect_equal(plain$reply, "ordinary reply")
expect_null(plain$report)
expect_null(plain$report_error)
expect_identical(corteza:::.format_subagent_reply(list(reply = "ordinary reply")),
                 "ordinary reply")

payload <- paste0(
    "Implemented the requested change.\n\n",
    "<corteza_handoff>",
    '{"status":"partial","notes":["Tests pass"],',
    '"findings":["One legacy caller remains"],',
    '"concerns":["Migration needs review"],',
    '"deviations":["Used a smaller fixture"],',
    '"open_questions":["Retire the alias?"],',
    '"feedback":["Pass explicit context"]}',
    "</corteza_handoff>"
)
parsed <- corteza:::.extract_subagent_report(payload, requested = TRUE)
expect_equal(parsed$reply, "Implemented the requested change.")
expect_equal(parsed$report$status, "partial")
expect_equal(parsed$report$concerns, "Migration needs review")
expect_null(parsed$report_error)

formatted <- corteza:::.format_subagent_reply(parsed)
expect_true(is.character(formatted))
expect_equal(length(formatted), 1L)
expect_true(grepl("Structured handoff", formatted, fixed = TRUE))
expect_true(grepl("Concerns:\n- Migration needs review", formatted,
                  fixed = TRUE))
expect_equal(attr(formatted, "corteza_report")$status, "partial")

tool_result <- corteza:::.subagent_tool_result(formatted)
expect_equal(tool_result$content[[1L]]$text, as.character(formatted))
expect_equal(tool_result$structuredContent$report$status, "partial")
expect_equal(tool_result$structuredContent$report$findings,
             "One legacy caller remains")

missing <- corteza:::.extract_subagent_report("No trailer", requested = TRUE)
expect_null(missing$report)
expect_true(grepl("omitted", missing$report_error, fixed = TRUE))
missing_text <- corteza:::.format_subagent_reply(missing)
expect_true(grepl("Structured handoff warning", missing_text, fixed = TRUE))

invalid <- corteza:::.extract_subagent_report(
    '<corteza_handoff>{"status":"mystery"}</corteza_handoff>',
    requested = TRUE
)
expect_null(invalid$report)
expect_true(grepl("status must be", invalid$report_error, fixed = TRUE))

# Positional compatibility: return_name remains argument five and report is
# appended as argument six.
expect_equal(names(formals(corteza::subagent_query))[5L], "return_name")
expect_equal(names(formals(corteza::subagent_query))[6L], "report")
expect_false(formals(corteza::subagent_query)$report)
expect_equal(names(formals(corteza::tool_query_subagent))[4L], "return_name")
expect_equal(names(formals(corteza::tool_query_subagent))[5L], "timeout")
expect_equal(names(formals(corteza::tool_query_subagent))[6L], "report")
