# Tests for heartbeat reminder system

# hb_init ----

llamaR:::hb_init()
status <- llamaR:::hb_status()
expect_true(status$enabled)
expect_equal(status$turn_count, 0L)
expect_equal(status$consecutive_failures, 0L)

# Custom config
llamaR:::hb_init(list(heartbeat = list(failure_threshold = 5L)))
expect_equal(llamaR:::.heartbeat$failure_threshold, 5L)

# Disable via config
llamaR:::hb_init(list(heartbeat = list(enabled = FALSE)))
expect_false(llamaR:::hb_status()$enabled)
expect_null(llamaR:::hb_check())

# hb_record_tool ----

llamaR:::hb_init()

# Successful tool calls reset failure counter
llamaR:::hb_record_tool("read_file", list(path = "foo.R"), "contents", TRUE)
expect_equal(llamaR:::.heartbeat$consecutive_failures, 0L)
expect_equal(length(llamaR:::.heartbeat$tool_history), 1)

# Failed tool calls increment failure counter
llamaR:::hb_record_tool("read_file", list(path = "bad.R"), "Error: not found",
                        FALSE)
expect_equal(llamaR:::.heartbeat$consecutive_failures, 1L)

llamaR:::hb_record_tool("write_file", list(path = "x.R"), "Error: permission",
                        FALSE)
expect_equal(llamaR:::.heartbeat$consecutive_failures, 2L)

# Success resets the counter
llamaR:::hb_record_tool("bash", list(command = "ls"), "file1\nfile2", TRUE)
expect_equal(llamaR:::.heartbeat$consecutive_failures, 0L)

# hb_record_turn ----

llamaR:::hb_init()
llamaR:::hb_record_turn()
llamaR:::hb_record_turn()
expect_equal(llamaR:::hb_status()$turn_count, 2L)

# hb_detect_failure_streak ----

llamaR:::hb_init(list(heartbeat = list(failure_threshold = 3L)))

# No reminder below threshold
llamaR:::hb_record_tool("a", list(), "err", FALSE)
llamaR:::hb_record_tool("b", list(), "err", FALSE)
expect_null(llamaR:::hb_detect_failure_streak())

# Fires at threshold
llamaR:::hb_record_tool("c", list(), "err", FALSE)
reminder <- llamaR:::hb_detect_failure_streak()
expect_true(!is.null(reminder))
expect_true(grepl("3 consecutive tool failures", reminder))

# hb_detect_doom_loop ----

llamaR:::hb_init(list(heartbeat = list(doom_threshold = 2L)))

# Different tools: no doom loop
llamaR:::hb_record_tool("read_file", list(path = "a.R"), "ok", TRUE)
llamaR:::hb_record_tool("write_file", list(path = "b.R"), "ok", TRUE)
expect_null(llamaR:::hb_detect_doom_loop())

# Same tool + same args repeated
llamaR:::hb_init(list(heartbeat = list(doom_threshold = 2L)))
llamaR:::hb_record_tool("read_file", list(path = "foo.R"), "ok", TRUE)
llamaR:::hb_record_tool("read_file", list(path = "foo.R"), "ok", TRUE)
reminder <- llamaR:::hb_detect_doom_loop()
expect_true(!is.null(reminder))
expect_true(grepl("same arguments", reminder))

# Same tool, different args: no doom loop
llamaR:::hb_init(list(heartbeat = list(doom_threshold = 2L)))
llamaR:::hb_record_tool("read_file", list(path = "a.R"), "ok", TRUE)
llamaR:::hb_record_tool("read_file", list(path = "b.R"), "ok", TRUE)
expect_null(llamaR:::hb_detect_doom_loop())

# hb_detect_high_context ----

llamaR:::hb_init()

expect_null(llamaR:::hb_detect_high_context(50))
expect_null(llamaR:::hb_detect_high_context(79))

reminder <- llamaR:::hb_detect_high_context(85)
expect_true(!is.null(reminder))
expect_true(grepl("85%", reminder))

# hb_detect_periodic ----

llamaR:::hb_init(list(heartbeat = list(periodic_interval = 3L)))

# No rules: never fires
for (i in seq_len(5)) llamaR:::hb_record_turn()
expect_null(llamaR:::hb_detect_periodic(NULL))
expect_null(llamaR:::hb_detect_periodic(""))

# With rules: fires at interval
llamaR:::hb_init(list(heartbeat = list(periodic_interval = 3L)))
for (i in seq_len(3)) llamaR:::hb_record_turn()
reminder <- llamaR:::hb_detect_periodic("Use base R, not tidyverse")
expect_true(!is.null(reminder))
expect_true(grepl("base R", reminder))

# Doesn't fire again until next interval
expect_null(llamaR:::hb_detect_periodic("Use base R, not tidyverse"))
llamaR:::hb_record_turn()
expect_null(llamaR:::hb_detect_periodic("Use base R, not tidyverse"))

# Suppression ----

llamaR:::hb_init(list(heartbeat = list(failure_threshold = 1L)))

# First fire: works
llamaR:::hb_record_tool("a", list(), "err", FALSE)
r1 <- llamaR:::hb_detect_failure_streak()
expect_true(!is.null(r1))

# Second fire: works
r2 <- llamaR:::hb_detect_failure_streak()
expect_true(!is.null(r2))

# Third fire: suppressed after this
r3 <- llamaR:::hb_detect_failure_streak()
expect_true(!is.null(r3))

# Fourth fire: suppressed
r4 <- llamaR:::hb_detect_failure_streak()
expect_null(r4)

# Clear suppression: fires again
llamaR:::hb_clear_suppression("failure_streak")
r5 <- llamaR:::hb_detect_failure_streak()
expect_true(!is.null(r5))

# hb_hash_args ----

h1 <- llamaR:::hb_hash_args("read_file", list(path = "a.R"))
h2 <- llamaR:::hb_hash_args("read_file", list(path = "a.R"))
h3 <- llamaR:::hb_hash_args("read_file", list(path = "b.R"))
h4 <- llamaR:::hb_hash_args("write_file", list(path = "a.R"))

expect_equal(h1, h2)
expect_true(h1 != h3)
expect_true(h1 != h4)

# Null/empty args
h5 <- llamaR:::hb_hash_args("bash", NULL)
h6 <- llamaR:::hb_hash_args("bash", list())
expect_equal(h5, h6)

# hb_check integration ----

llamaR:::hb_init(list(heartbeat = list(failure_threshold = 2L)))

# No issues: no reminder
expect_null(llamaR:::hb_check(token_pct = 50))

# Failure streak triggers
llamaR:::hb_record_tool("a", list(), "err", FALSE)
llamaR:::hb_record_tool("b", list(), "err", FALSE)
reminder <- llamaR:::hb_check(token_pct = 50)
expect_true(!is.null(reminder))
expect_true(grepl("consecutive tool failures", reminder))

# Disabled: no reminders
llamaR:::hb_init(list(heartbeat = list(enabled = FALSE)))
llamaR:::hb_record_tool("a", list(), "err", FALSE)
llamaR:::hb_record_tool("b", list(), "err", FALSE)
expect_null(llamaR:::hb_check(token_pct = 95))
