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

# ---- .plan_mode_note ----
# Plan mode reaches the model as a note at the head of the next prompt;
# the system text stays as it is (WP14, test_request_prefix.R).

# On: what to produce, and how to leave.
on <- corteza:::.plan_mode_note(TRUE)
expect_true(grepl("^\\[Plan mode is on\\.", on))
expect_true(grepl("exit_plan_mode", on, fixed = TRUE))
expect_true(grepl("\\]$", on))
# Off: says so, and nothing about exit_plan_mode.
off <- corteza:::.plan_mode_note(FALSE)
expect_true(grepl("^\\[Plan mode is off\\.", off))
expect_false(grepl("exit_plan_mode", off, fixed = TRUE))

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

# ---- Leaving plan mode asks a person ----
# exit_plan_mode is a control tool, which runs unasked, and the tool
# answers "Plan approved": before 0.8.1 the model left plan mode on its
# own word. policy() asks (rule "plan_exit"), and nothing after the
# mode answers it for the person.
local({
    leave <- function(config = NULL, channel = "cli", plan_mode = TRUE) {
        corteza::policy(list(tool = "exit_plan_mode",
                             args = list(plan = "1. edit a.R"),
                             channel = channel,
                             context = list(plan_mode = plan_mode)), config)
    }
    d <- leave()
    expect_equal(d$approval, "ask")
    expect_identical(d$rule, "plan_exit")
    expect_true(corteza:::.person_rule(d))
    expect_true(corteza:::.person_rule(list(rule = "credential")))
    expect_false(corteza:::.person_rule(list(rule = "plan_mode")))
    expect_false(corteza:::.person_rule(list()))
    for (config in list(list(approval_mode = "allow"),
                        list(approval_mode = "deny"),
                        list(permissions = list(exit_plan_mode = "allow")))) {
        expect_identical(leave(config)$rule, "plan_exit")
        expect_equal(leave(config)$approval, "ask")
    }
    op <- options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "anything goes")
    })
    on.exit(options(op), add = TRUE)
    expect_identical(leave()$rule, "plan_exit")
    expect_identical(leave(channel = "matrix")$rule, "plan_exit")
    expect_identical(leave(channel = "console")$rule, "plan_exit")
    # Outside plan mode the rule says nothing: turn()'s handler refuses
    # the call by name before policy() is asked.
    expect_null(corteza:::check_plan_exit(list(tool = "exit_plan_mode",
                                               context = list(plan_mode = FALSE))))
})

# The handler: a no keeps plan mode, and the model is told; a yes ends
# it. The approval callback is shown the rule.
local({
    seen <- list()
    answer <- FALSE
    s <- corteza::new_session(channel = "console", provider = "anthropic",
                              plan_mode = TRUE,
                              approval_cb = function(call, decision) {
                                  seen[[length(seen) + 1L]] <<- decision
                                  answer
                              })
    ran <- 0L
    handler <- corteza:::.make_tool_handler(s, tool_executor = function(name, args) {
        ran <<- ran + 1L
        corteza::tool_exit_plan_mode(args$plan)
    })
    out <- handler("exit_plan_mode", list(plan = "1. edit a.R"))
    expect_true(isTRUE(s$plan_mode))
    expect_equal(ran, 0L)
    expect_identical(seen[[1L]]$rule, "plan_exit")
    expect_false(grepl("Plan approved", paste(unlist(out), collapse = " ")))
    answer <- TRUE
    handler("exit_plan_mode", list(plan = "1. edit a.R"))
    expect_false(isTRUE(s$plan_mode))
    expect_equal(ran, 1L)
})

# Unattended gates send it to a person: a room's supervisor, and the
# hall monitor of an /auto run, which ends the run there.
local({
    d <- corteza:::check_plan_exit(list(tool = "exit_plan_mode",
                                        context = list(plan_mode = TRUE)))
    call <- list(tool = "exit_plan_mode", args = list(plan = "1. edit a.R"),
                 channel = "matrix", class = "control")
    root <- tempfile("plan-exit-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    v <- corteza:::supervisor_route(call, d, cwd = root, root = root)
    expect_identical(v$route, "human")
    expect_true(any(grepl("would end plan mode", v$flags, fixed = TRUE)))
    m <- corteza:::monitor_in_envelope(call, d)
    expect_false(m$ok)
    expect_true(grepl("plan mode", m$reason, fixed = TRUE))
})

# chat()'s callback: an "always" held for the tool does not answer it.
local({
    cb <- corteza:::chat_approval_cb(tempdir())
    answered <- attr(cb, "answered")
    attr(cb, "always")(list(tool = "exit_plan_mode"))
    expect_true(isTRUE(answered(list(tool = "exit_plan_mode"),
                                list(approval = "ask"))))
    expect_false(isTRUE(answered(list(tool = "exit_plan_mode"),
        corteza:::check_plan_exit(list(tool = "exit_plan_mode",
                                       context = list(plan_mode = TRUE))))))
})
