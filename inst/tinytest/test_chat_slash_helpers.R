# parse_spawn_flags + chat_help_text + chat_format_tools_list. All
# offline; no chat() invocation.

parse <- corteza:::parse_spawn_flags

# 1. Bare task, no flags.
r <- parse("audit the auth code")
expect_equal(r$task, "audit the auth code")
expect_null(r$model)
expect_null(r$preset)
expect_null(r$tools)

# 2. --model at the end.
r <- parse("audit auth --model claude-haiku")
expect_equal(r$task, "audit auth")
expect_equal(r$model, "claude-haiku")

# 3. --preset in the middle.
r <- parse("audit --preset work the auth code")
expect_equal(r$task, "audit the auth code")
expect_equal(r$preset, "work")

# 4. All three flags, mixed order.
r <- parse("audit --tools read_file,grep_files --preset minimal auth --model gpt-4")
expect_equal(r$task, "audit auth")
expect_equal(r$model, "gpt-4")
expect_equal(r$preset, "minimal")
expect_equal(r$tools, c("read_file", "grep_files"))

# 5. Help text mentions key surfaces.
help_text <- corteza:::chat_help_text()
expect_true(grepl("/spawn", help_text))
expect_true(grepl("/agents", help_text))
expect_true(grepl("/ask", help_text))
expect_true(grepl("/kill", help_text))
expect_true(grepl("/help", help_text))
expect_true(grepl("/quit", help_text))
expect_true(grepl("--preset", help_text))
expect_true(grepl("--tools", help_text))
