# User-deny condition: shape and propagation.

# user_deny_condition() builds a condition object whose class list
# is exactly the one we promise. "error" must NOT appear so the
# inner tryCatch(error = function(e) FALSE) in .make_tool_handler
# does not swallow it before chat() can catch it.
cnd <- corteza:::user_deny_condition("write_file")
expect_inherits(cnd, "corteza_user_deny")
expect_inherits(cnd, "interrupt")
expect_inherits(cnd, "condition")
expect_false(inherits(cnd, "error"))
expect_equal(cnd$tool, "write_file")
expect_true(grepl("write_file", cnd$message))

# Empty / NULL tool -> "?" fallback.
expect_equal(corteza:::user_deny_condition()$tool, "?")
expect_equal(corteza:::user_deny_condition("")$tool, "?")
expect_equal(corteza:::user_deny_condition(NULL)$tool, "?")

# Marker text mentions the tool and tells the LLM to stop.
mk <- corteza:::user_deny_marker("write_file")
expect_true(grepl("write_file", mk))
expect_true(grepl("Stop", mk))
expect_true(grepl("ask", mk))

# The interrupt marker carries the SAME "stop and ask the user"
# directive as the deny marker, so Ctrl+C/Esc and an explicit deny
# leave the LLM with the same next-turn instruction (issue #104).
im <- corteza:::user_interrupt_marker()
expect_true(grepl("Interrupted by user", im, fixed = TRUE))
expect_true(grepl(corteza:::.user_abort_directive, im, fixed = TRUE))
expect_true(grepl(corteza:::.user_abort_directive, mk, fixed = TRUE))

# Critical: a defensive tryCatch(error = ...) around the approval_cb
# MUST NOT swallow the deny. This is the bug we're fixing -- the
# old approval_cb returned FALSE for option "3", which then got fed
# back to the LLM as a "[user declined]" tool result. We replaced
# that with stop(user_deny_condition(...)) which should propagate
# THROUGH the inner error handler and out to the outer chat()
# tryCatch.
fake_cb <- function() {
    stop(corteza:::user_deny_condition("write_file"))
}
outcome <- tryCatch(
    tryCatch(
        fake_cb(),
        error = function(e) "ERR-SWALLOWED"
    ),
    corteza_user_deny = function(c) c$tool
)
expect_equal(outcome, "write_file")

# And the same condition is also catchable as "interrupt" -- so a
# surface that forgot to register a corteza_user_deny handler still
# falls through to the existing interrupt-marker path instead of
# bubbling out as an uncaught error.
outcome2 <- tryCatch(
    fake_cb(),
    interrupt = function(c) "caught-as-interrupt"
)
expect_equal(outcome2, "caught-as-interrupt")
