library(tinytest)

# ---- check_plan_mode: deny gate ----

# Outside plan mode: NULL (pass-through).
expect_null(corteza:::check_plan_mode(
    list(tool = "write_file", context = list(plan_mode = FALSE))
))
expect_null(corteza:::check_plan_mode(
    list(tool = "write_file", context = list())
))

# Inside plan mode: writes denied.
d <- corteza:::check_plan_mode(
    list(tool = "write_file", context = list(plan_mode = TRUE))
)
expect_equal(d$approval, "deny")
expect_true(grepl("plan mode", d$reason, fixed = TRUE))

# Inside plan mode: exec denied (bash, run_r).
expect_equal(
    corteza:::check_plan_mode(
        list(tool = "bash", context = list(plan_mode = TRUE))
    )$approval,
    "deny"
)
expect_equal(
    corteza:::check_plan_mode(
        list(tool = "run_r", context = list(plan_mode = TRUE))
    )$approval,
    "deny"
)

# Inside plan mode: reads pass through.
expect_null(corteza:::check_plan_mode(
    list(tool = "read_file", context = list(plan_mode = TRUE))
))
expect_null(corteza:::check_plan_mode(
    list(tool = "grep_files", context = list(plan_mode = TRUE))
))

# Inside plan mode: exit_plan_mode is whitelisted (the escape hatch).
expect_null(corteza:::check_plan_mode(
    list(tool = "exit_plan_mode", context = list(plan_mode = TRUE))
))

# ---- policy() integration ----

# In plan mode, policy() short-circuits to deny for writes.
expect_equal(
    corteza::policy(list(
        tool = "write_file",
        args = list(path = "/tmp/x", content = "y"),
        context = list(plan_mode = TRUE)
    ))$approval,
    "deny"
)
# Reads still flow through the normal decision path.
expect_true(
    corteza::policy(list(
        tool = "read_file",
        args = list(path = "/tmp/x"),
        context = list(plan_mode = TRUE)
    ))$approval %in% c("allow", "ask")
)

# Plan mode wins over the default tensor: even a path classified as
# "personal" (which the tensor would route to ask) is denied for writes.
local({
    op <- options(corteza.personal_paths = "~/Documents")
    on.exit(options(op), add = TRUE)
    expect_equal(
        corteza::policy(list(
            tool = "write_file",
            args = list(path = "~/Documents/notes.md", content = "x"),
            context = list(plan_mode = TRUE)
        ))$approval,
        "deny"
    )
})

# ---- new_session() carries plan_mode ----

s <- corteza::new_session(channel = "cli", provider = "anthropic",
                          plan_mode = TRUE)
expect_true(isTRUE(s$plan_mode))

s2 <- corteza::new_session(channel = "cli", provider = "anthropic")
expect_false(isTRUE(s2$plan_mode))

# ---- .plan_mode_compose_system ----

# Off: returns the base system prompt verbatim.
expect_identical(
    corteza:::.plan_mode_compose_system("base", FALSE),
    "base"
)
# On with base: appends the addendum.
out <- corteza:::.plan_mode_compose_system("base", TRUE)
expect_true(grepl("# Plan mode", out, fixed = TRUE))
expect_true(grepl("^base", out))
# On with null base: addendum only, still mentions plan mode.
out_null <- corteza:::.plan_mode_compose_system(NULL, TRUE)
expect_true(grepl("# Plan mode", out_null, fixed = TRUE))

# ---- .plan_mode_filter_tools ----

# Set up a fake tool list with one normal tool. The skill registry
# already holds exit_plan_mode (registered by register_builtin_skills).
corteza:::ensure_skills()
fake_tools <- list(list(name = "read_file", description = "r",
                        input_schema = list()))

# Out of plan mode: exit_plan_mode is not injected.
out_off <- corteza:::.plan_mode_filter_tools(fake_tools, FALSE)
names_off <- vapply(out_off, function(t) t$name %||% "", character(1))
expect_false("exit_plan_mode" %in% names_off)

# In plan mode: exit_plan_mode is injected.
out_on <- corteza:::.plan_mode_filter_tools(fake_tools, TRUE)
names_on <- vapply(out_on, function(t) t$name %||% "", character(1))
expect_true("exit_plan_mode" %in% names_on)

# Idempotent in plan mode: if exit_plan_mode is already in the list, it
# isn't duplicated.
twice <- corteza:::.plan_mode_filter_tools(out_on, TRUE)
expect_equal(sum(vapply(twice, function(t) {
    identical(t$name %||% "", "exit_plan_mode")
}, logical(1))), 1L)

# Out of plan mode: existing exit_plan_mode is stripped.
stripped <- corteza:::.plan_mode_filter_tools(out_on, FALSE)
expect_false("exit_plan_mode" %in%
                 vapply(stripped, function(t) t$name %||% "", character(1)))

# ---- tool_exit_plan_mode handler ----

# Empty plan rejected.
res_bad <- corteza::tool_exit_plan_mode(plan = "")
expect_true(isTRUE(res_bad$isError))

# Non-empty plan returns ok and signals proceed.
res_ok <- corteza::tool_exit_plan_mode(plan = "1. read a file\n2. edit it")
expect_false(isTRUE(res_ok$isError))
expect_true(grepl("Plan approved", res_ok$content[[1]]$text))

# ---- .make_tool_handler flips plan_mode on successful exit ----

s_flip <- corteza::new_session(channel = "console", provider = "anthropic",
                               plan_mode = TRUE,
                               approval_cb = function(call, decision) TRUE)
handler <- corteza:::.make_tool_handler(
    s_flip,
    tool_executor = function(name, args) {
        list(content = list(list(type = "text", text = "Plan approved.")),
             isError = FALSE)
    }
)
expect_true(isTRUE(s_flip$plan_mode))
handler("exit_plan_mode", list(plan = "do X"))
expect_false(isTRUE(s_flip$plan_mode))
