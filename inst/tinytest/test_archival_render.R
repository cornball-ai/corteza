# Transcript rendering for the summarization prompt. Offline.

render <- corteza:::archival_render_transcript
entry_to_text <- corteza:::archival_history_entry_to_text
validate <- corteza:::archival_validate_structured
first_line <- corteza:::archival_first_line

# Empty slice -> empty string.
expect_equal(render(list()), "")

# String content -> "## role\n<content>".
slice <- list(
    list(role = "user", content = "hello world"),
    list(role = "assistant", content = "ack")
)
out <- render(slice)
expect_true(grepl("## user\\nhello world", out))
expect_true(grepl("## assistant\\nack", out))

# Tool blocks render as [tool_use: name(input)] / [tool_result: text].
slice_with_blocks <- list(
    list(role = "assistant", content = list(
        list(type = "text", text = "let me check"),
        list(type = "tool_use", id = "t1", name = "read_file",
             input = list(path = "src/foo.R"))
    )),
    list(role = "user", content = list(
        list(type = "tool_result", tool_use_id = "t1",
             content = list(list(type = "text", text = "file contents")))
    ))
)
rendered <- render(slice_with_blocks)
expect_true(grepl("\\[tool_use: read_file", rendered))
expect_true(grepl("\\[tool_result: file contents", rendered))

# entry_to_text produces a flat string from list-of-blocks content.
flat <- entry_to_text(slice_with_blocks[[1]])
expect_true(is.character(flat) && length(flat) == 1L)
expect_true(grepl("let me check", flat))

# Structured validator: valid JSON returns text unchanged.
valid_json <- '{"outcome":"ok","key_findings":[],"files_touched":[],"tools_used":[],"open_questions":[]}'
expect_equal(validate(valid_json), valid_json)

# Invalid JSON gets [unparsed] prefix so the parent slot still has data.
bad_json <- "{ outcome: ok, but not valid json }"
out_bad <- validate(bad_json)
expect_true(startsWith(out_bad, "[unparsed]"))

# first_line: trims, caps at 80 chars.
expect_equal(first_line("first line\nsecond line"), "first line")
expect_equal(first_line(""), "(no prompt)")
expect_equal(first_line("   "), "(no prompt)")
long <- paste(rep("a", 200L), collapse = "")
out_long <- first_line(long)
expect_true(nchar(out_long) <= 80L)
expect_true(endsWith(out_long, "..."))
