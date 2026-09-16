library(tinytest)

# Per-tool output caps from config. A host whose own tool returns a large
# structured result the model must see whole (a game board, a table) raises
# the cap for that tool alone; every other tool keeps the universal cap.

# ---- .tool_output_caps_for ----

expect_null(corteza:::.tool_output_caps_for(NULL, "bash"))
expect_null(corteza:::.tool_output_caps_for(list(), "bash"))
cfg <- list(tool_output_caps = list(
    game_action = list(max_chars = 400000, max_lines = 20000),
    read_file = list(max_lines = 5000)))
caps <- corteza:::.tool_output_caps_for(cfg, "game_action")
expect_equal(caps$max_chars, 400000L)
expect_equal(caps$max_lines, 20000L)
expect_null(corteza:::.tool_output_caps_for(cfg, "bash"))
# A field left out keeps the tool's default budget, which is the read
# budget for the content-read tools.
caps <- corteza:::.tool_output_caps_for(cfg, "read_file")
expect_equal(caps$max_chars, corteza:::.tool_output_read_max_chars)
expect_equal(caps$max_lines, 5000L)
caps <- corteza:::.tool_output_caps_for(
    list(tool_output_caps = list(bash = list(max_lines = 200))), "bash")
expect_equal(caps$max_chars, corteza:::.tool_output_max_chars)
expect_equal(caps$max_lines, 200L)
expect_error(corteza:::.tool_output_caps_for(
    list(tool_output_caps = list(bash = list(max_lines = 0))), "bash"),
    "positive number")
expect_error(corteza:::.tool_output_caps_for(
    list(tool_output_caps = list(bash = list(max_chars = "many"))), "bash"),
    "positive number")
expect_error(corteza:::.tool_output_caps_for(
    list(tool_output_caps = list(bash = 10)), "bash"), "must be a list")

# ---- .validate_tool_output_caps ----

expect_null(corteza:::.validate_tool_output_caps(NULL))
expect_null(corteza:::.validate_tool_output_caps(list()))
expect_null(corteza:::.validate_tool_output_caps(cfg))
expect_error(corteza:::.validate_tool_output_caps(
    list(tool_output_caps = list(list(max_lines = 1)))), "named list")
expect_error(corteza:::.validate_tool_output_caps(
    list(tool_output_caps = list(bash = list(max_lines = -1)))),
    "positive number")

# ---- handler integration ----

# The configured tool passes its 50k lines through unchanged, a sibling
# tool with no entry is still capped, the dry-run branch honors the same
# cap, and a malformed config fails before any tool runs.
local({
    on.exit({
        options(corteza.policy = NULL)
        corteza:::clear_handles()
    }, add = TRUE)
    options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "test allow")
    })
    body <- paste(sprintf("L%d", 1:50000), collapse = "\n")
    fake <- function(name, args) {
        list(content = list(list(type = "text", text = body)))
    }
    s <- corteza::new_session("cli",
                              approval_cb = function(call, decision) TRUE)
    s$config$tool_output_caps <- list(
        grep_files = list(max_chars = 1e6, max_lines = 1e5))
    h <- corteza:::.make_tool_handler(s, tool_executor = fake)
    expect_equal(h("grep_files", list(pattern = "x")), body)
    out <- h("bash", list(command = "x"))
    expect_true(grepl("truncated", out))
    expect_true(nchar(out) < 5000L)
    s$dry_run <- TRUE
    expect_equal(h("grep_files", list(pattern = "x")), body)
    out <- h("bash", list(command = "x"))
    expect_true(grepl("truncated", out))
    s$dry_run <- FALSE
    # Past even the raised cap, the result still stashes to a handle.
    s$config$tool_output_caps <- list(grep_files = list(max_lines = 100))
    h <- corteza:::.make_tool_handler(s, tool_executor = fake)
    out <- h("grep_files", list(pattern = "x"))
    expect_true(grepl("truncated", out))
    expect_true(grepl("50000 lines", out))
    s$config$tool_output_caps <- list(grep_files = list(max_lines = -1))
    expect_error(corteza:::.make_tool_handler(s, tool_executor = fake),
                 "positive number")
})
