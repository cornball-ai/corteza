# Tests for heartbeat reminder system

# hb_init ----

corteza:::hb_init()
status <- corteza:::hb_status()
expect_true(status$enabled)
expect_equal(status$turn_count, 0L)
expect_equal(status$consecutive_failures, 0L)

# Custom config
corteza:::hb_init(list(heartbeat = list(failure_threshold = 5L)))
expect_equal(corteza:::.heartbeat$failure_threshold, 5L)

# Disable via config
corteza:::hb_init(list(heartbeat = list(enabled = FALSE)))
expect_false(corteza:::hb_status()$enabled)
expect_null(corteza:::hb_check())

# hb_record_tool ----

corteza:::hb_init()

# Successful tool calls reset failure counter
corteza:::hb_record_tool("read_file", list(path = "foo.R"), "contents", TRUE)
expect_equal(corteza:::.heartbeat$consecutive_failures, 0L)
expect_equal(length(corteza:::.heartbeat$tool_history), 1)

# Failed tool calls increment failure counter
corteza:::hb_record_tool("read_file", list(path = "bad.R"), "Error: not found",
                        FALSE)
expect_equal(corteza:::.heartbeat$consecutive_failures, 1L)

corteza:::hb_record_tool("write_file", list(path = "x.R"), "Error: permission",
                        FALSE)
expect_equal(corteza:::.heartbeat$consecutive_failures, 2L)

# Success resets the counter
corteza:::hb_record_tool("bash", list(command = "ls"), "file1\nfile2", TRUE)
expect_equal(corteza:::.heartbeat$consecutive_failures, 0L)

# hb_record_turn ----

corteza:::hb_init()
corteza:::hb_record_turn()
corteza:::hb_record_turn()
expect_equal(corteza:::hb_status()$turn_count, 2L)

# hb_detect_failure_streak ----

corteza:::hb_init(list(heartbeat = list(failure_threshold = 3L)))

# No reminder below threshold
corteza:::hb_record_tool("a", list(), "err", FALSE)
corteza:::hb_record_tool("b", list(), "err", FALSE)
expect_null(corteza:::hb_detect_failure_streak())

# Fires at threshold
corteza:::hb_record_tool("c", list(), "err", FALSE)
reminder <- corteza:::hb_detect_failure_streak()
expect_true(!is.null(reminder))
expect_true(grepl("3 consecutive tool failures", reminder))

# hb_detect_doom_loop ----

corteza:::hb_init(list(heartbeat = list(doom_threshold = 2L)))

# Different tools: no doom loop
corteza:::hb_record_tool("read_file", list(path = "a.R"), "ok", TRUE)
corteza:::hb_record_tool("write_file", list(path = "b.R"), "ok", TRUE)
expect_null(corteza:::hb_detect_doom_loop())

# Same tool + same args repeated
corteza:::hb_init(list(heartbeat = list(doom_threshold = 2L)))
corteza:::hb_record_tool("read_file", list(path = "foo.R"), "ok", TRUE)
corteza:::hb_record_tool("read_file", list(path = "foo.R"), "ok", TRUE)
reminder <- corteza:::hb_detect_doom_loop()
expect_true(!is.null(reminder))
expect_true(grepl("same arguments", reminder))

# Same tool, different args: no doom loop
corteza:::hb_init(list(heartbeat = list(doom_threshold = 2L)))
corteza:::hb_record_tool("read_file", list(path = "a.R"), "ok", TRUE)
corteza:::hb_record_tool("read_file", list(path = "b.R"), "ok", TRUE)
expect_null(corteza:::hb_detect_doom_loop())

# hb_detect_high_context ----

corteza:::hb_init()

expect_null(corteza:::hb_detect_high_context(50))
expect_null(corteza:::hb_detect_high_context(79))

reminder <- corteza:::hb_detect_high_context(85)
expect_true(!is.null(reminder))
expect_true(grepl("85%", reminder))

# hb_detect_periodic ----

corteza:::hb_init(list(heartbeat = list(periodic_interval = 3L)))

# No rules: never fires
for (i in seq_len(5)) corteza:::hb_record_turn()
expect_null(corteza:::hb_detect_periodic(NULL))
expect_null(corteza:::hb_detect_periodic(""))

# With rules: fires at interval
corteza:::hb_init(list(heartbeat = list(periodic_interval = 3L)))
for (i in seq_len(3)) corteza:::hb_record_turn()
reminder <- corteza:::hb_detect_periodic("Use base R, not tidyverse")
expect_true(!is.null(reminder))
expect_true(grepl("base R", reminder))

# Doesn't fire again until next interval
expect_null(corteza:::hb_detect_periodic("Use base R, not tidyverse"))
corteza:::hb_record_turn()
expect_null(corteza:::hb_detect_periodic("Use base R, not tidyverse"))

# Suppression ----

corteza:::hb_init(list(heartbeat = list(failure_threshold = 1L)))

# First fire: works
corteza:::hb_record_tool("a", list(), "err", FALSE)
r1 <- corteza:::hb_detect_failure_streak()
expect_true(!is.null(r1))

# Second fire: works
r2 <- corteza:::hb_detect_failure_streak()
expect_true(!is.null(r2))

# Third fire: suppressed after this
r3 <- corteza:::hb_detect_failure_streak()
expect_true(!is.null(r3))

# Fourth fire: suppressed
r4 <- corteza:::hb_detect_failure_streak()
expect_null(r4)

# Clear suppression: fires again
corteza:::hb_clear_suppression("failure_streak")
r5 <- corteza:::hb_detect_failure_streak()
expect_true(!is.null(r5))

# hb_hash_args ----

h1 <- corteza:::hb_hash_args("read_file", list(path = "a.R"))
h2 <- corteza:::hb_hash_args("read_file", list(path = "a.R"))
h3 <- corteza:::hb_hash_args("read_file", list(path = "b.R"))
h4 <- corteza:::hb_hash_args("write_file", list(path = "a.R"))

expect_equal(h1, h2)
expect_true(h1 != h3)
expect_true(h1 != h4)

# Null/empty args
h5 <- corteza:::hb_hash_args("bash", NULL)
h6 <- corteza:::hb_hash_args("bash", list())
expect_equal(h5, h6)

# hb_check integration ----

corteza:::hb_init(list(heartbeat = list(failure_threshold = 2L)))

# No issues: no reminder
expect_null(corteza:::hb_check(token_pct = 50))

# Failure streak triggers
corteza:::hb_record_tool("a", list(), "err", FALSE)
corteza:::hb_record_tool("b", list(), "err", FALSE)
reminder <- corteza:::hb_check(token_pct = 50)
expect_true(!is.null(reminder))
expect_true(grepl("consecutive tool failures", reminder))

# Disabled: no reminders
corteza:::hb_init(list(heartbeat = list(enabled = FALSE)))
corteza:::hb_record_tool("a", list(), "err", FALSE)
corteza:::hb_record_tool("b", list(), "err", FALSE)
expect_null(corteza:::hb_check(token_pct = 95))
