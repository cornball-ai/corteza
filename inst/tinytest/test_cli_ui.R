library(tinytest)

# Static approval prompt: simplified single-line Access, no Reason
# section, key hints on choices 1 and 3.
call <- list(
    tool = "write_file",
    args = list(path = "R/chat.R", content = "x"),
    channel = "cli"
)
decision <- list(model = "cloud", reason = "default: code/write/cli")
lines <- corteza:::cli_approval_lines(
                                      call,
                                      decision,
                                      cwd = "/tmp/project",
                                      persistent_label = "Allow always for this session",
                                      deny_label = "Deny (Esc)"
)

# Title still reflects the long tool label.
expect_true(any(grepl("Write file", lines, fixed = TRUE)))
# Access collapses to one line that names the path directly.
expect_true(any(grepl("Write to R/chat.R", lines, fixed = TRUE)))
# Old verbose Access lines are gone.
expect_false(any(grepl("Write access to local files", lines, fixed = TRUE)))
# Reason / Policy / Model route stripped.
expect_false(any(grepl("Policy:", lines, fixed = TRUE)))
expect_false(any(grepl("Model route", lines, fixed = TRUE)))
expect_false(any(grepl("Reason", lines, fixed = TRUE)))
# Key hints land on choices 1 and 3. Choice 3 advertises the
# surface-appropriate interrupt key (Esc in the R console, Ctrl+C in
# the terminal CLI). The key doesn't literally cancel the prompt
# itself -- readline doesn't catch Esc/Ctrl+C -- but it cancels the
# in-flight turn, which is the user-facing escape hatch they want.
expect_true(any(grepl("Allow once (Enter)", lines, fixed = TRUE)))
expect_true(any(grepl("Deny (Esc)", lines, fixed = TRUE)))
expect_true(any(grepl("Allow always for this session", lines, fixed = TRUE)))

# CLI surface uses Ctrl+C instead of Esc.
cli_lines <- corteza:::cli_approval_lines(
                                          call,
                                          decision,
                                          cwd = "/tmp/project",
                                          persistent_label = "Allow always for this project",
                                          deny_label = "Deny (Ctrl+C)"
)
expect_true(any(grepl("Deny (Ctrl+C)", cli_lines, fixed = TRUE)))

# Default deny_label stays unadorned for callers that don't pass it.
default_lines <- corteza:::cli_approval_lines(call, decision,
                                              cwd = "/tmp/project")
expect_true(any(grepl("^   3\\. Deny$", default_lines)))

# Duplicate Path detail under the title is suppressed once Access
# names the same path.
expect_false(any(grepl("^   Path: R/chat.R$", lines)))

# bash call: Access shows "Run command in <cwd>", no path.
bash_call <- list(
    tool = "bash",
    args = list(command = "git status"),
    channel = "cli"
)
bash_lines <- corteza:::cli_approval_lines(bash_call,
                                           decision = NULL,
                                           cwd = "/tmp/proj")
expect_true(any(grepl("Run command in /tmp/proj", bash_lines, fixed = TRUE)))
# Boilerplate "Shell commands can invoke scripts..." is dropped now
# that we only show noteworthy warnings.
expect_false(any(grepl("Shell commands can invoke scripts",
                       bash_lines, fixed = TRUE)))

# Noteworthy warnings still surface. A credential-touching call gets
# a Warning line.
cred_call <- list(tool = "read_file",
                  args = list(path = "~/.ssh/id_rsa"),
                  channel = "cli")
cred_decision <- list(reason = "credential path")
cred_lines <- corteza:::cli_approval_lines(cred_call,
                                           cred_decision,
                                           cwd = "/tmp/proj")
expect_true(any(grepl("Warning", cred_lines, fixed = TRUE)))
expect_true(any(grepl("credential path", cred_lines, fixed = TRUE)))

# cli_user_replied_line paraphrases the choice into a single line.
ur1 <- corteza:::cli_user_replied_line(
                                       list(tool = "replace_in_file",
                                            args = list(path = "CLAUDE.md",
                                                        old_text = "a", new_text = "b"),
                                            channel = "cli"),
                                       "1",
                                       persistent_label = "Allow always for this project"
)
expect_identical(ur1, "Allow writing to CLAUDE.md once")

ur2 <- corteza:::cli_user_replied_line(
                                       list(tool = "bash",
                                            args = list(command = "git status"),
                                            channel = "cli"),
                                       "2",
                                       persistent_label = "Allow always for this project"
)
expect_true(grepl("Always allow running `git status`", ur2, fixed = TRUE))
expect_true(grepl("for this project", ur2, fixed = TRUE))

ur3 <- corteza:::cli_user_replied_line(
                                       list(tool = "run_r",
                                            args = list(code = "1 + 1"),
                                            channel = "cli"),
                                       "3",
                                       persistent_label = "Allow always for this project"
)
expect_identical(ur3, "Deny running R code")

# Scope phrase tracks the persistent label so chat() gets "for this
# session" instead of "for this project".
ur2_chat <- corteza:::cli_user_replied_line(
                                            list(tool = "replace_in_file",
                                                 args = list(path = "CLAUDE.md"),
                                                 channel = "console"),
                                            "2",
                                            persistent_label = "Allow always for this session"
)
expect_true(grepl("for this session", ur2_chat, fixed = TRUE))

# Existing cli_event_summary contract is unchanged.
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

# Backslash continuation: odd trailing backslashes trigger; even
# trailing don't. Helper returns the seed (line minus the last `\`)
# or NULL.
expect_identical(corteza:::backslash_continuation_seed("foo\\"), "foo")
expect_null(corteza:::backslash_continuation_seed("foo\\\\"))
expect_null(corteza:::backslash_continuation_seed("foo"))
expect_null(corteza:::backslash_continuation_seed(""))
# Three trailing backslashes is odd, counted as continuation. Seed
# preserves the two leading backslashes.
expect_identical(corteza:::backslash_continuation_seed("foo\\\\\\"), "foo\\\\")
# Non-character / wrong-shape inputs return NULL defensively.
expect_null(corteza:::backslash_continuation_seed(NULL))
expect_null(corteza:::backslash_continuation_seed(c("a\\", "b\\")))
expect_null(corteza:::backslash_continuation_seed(42L))

# ANSI prompt markup wraps escape sequences so bash readline's column
# math stays correct.
expect_identical(corteza:::.markup_prompt_for_readline("> "), "> ")
ansi_wrapped <- corteza:::.markup_prompt_for_readline("\033[32m> \033[0m")
expect_true(grepl("\001\033\\[32m\002", ansi_wrapped))
expect_true(grepl("\001\033\\[0m\002", ansi_wrapped))

# read_paste_block: drive it via a stubbed read_prompt_input so we can
# test the state machine without a TTY. Two regression cases:
#   1. A single returned line containing embedded "\n" + "/end"
#      (mimics bash's pasted-multi-line drain) — must terminate
#      immediately, not consume another input.
#   2. A line ending with no trailing `\` terminates with the line
#      included.
# Heredoc mode: trailing-backslash continuation entry. First sub-line
# without trailing `\` is final. Embedded `/end` in a pasted block
# fires immediately (bash drains pasted lines into a single joined
# string).
local({
    inputs <- c("line one \\", "line two\n/end\nshould not appear")
    i <- 0L
    fake_read <- function(prompt) {
        i <<- i + 1L
        if (i > length(inputs)) return(character())
        inputs[i]
    }
    ns <- asNamespace("corteza")
    orig <- ns$read_prompt_input
    on.exit({
        unlockBinding("read_prompt_input", ns)
        assign("read_prompt_input", orig, envir = ns)
        lockBinding("read_prompt_input", ns)
    }, add = TRUE)
    unlockBinding("read_prompt_input", ns)
    assign("read_prompt_input", fake_read, envir = ns)

    out <- corteza:::read_paste_block(seed = NULL,
                                      empty_message = "",
                                      heredoc = TRUE)
    # "line one \" strips to "line one " (trailing space preserved,
    # matching bash's behavior). "line two" has no `\` and is the
    # final line, included in the buffer. "/end" never reached because
    # heredoc already terminated.
    expect_identical(out, "line one \nline two")
    expect_equal(i, 2L)
})

# Heredoc mode: a single non-backslash line terminates with that line
# included.
local({
    inputs <- c("just one line")
    i <- 0L
    fake_read <- function(prompt) {
        i <<- i + 1L
        if (i > length(inputs)) return(character())
        inputs[i]
    }
    ns <- asNamespace("corteza")
    orig <- ns$read_prompt_input
    on.exit({
        unlockBinding("read_prompt_input", ns)
        assign("read_prompt_input", orig, envir = ns)
        lockBinding("read_prompt_input", ns)
    }, add = TRUE)
    unlockBinding("read_prompt_input", ns)
    assign("read_prompt_input", fake_read, envir = ns)

    out <- corteza:::read_paste_block(seed = "first",
                                      empty_message = "",
                                      heredoc = TRUE)
    expect_identical(out, "first\njust one line")
})

# /paste mode (heredoc = FALSE, the default): collect every sub-line
# verbatim until /end or EOF. Codex caught the bug where this used to
# terminate on the first non-backslash line, dropping the rest of a
# pasted log/code block.
local({
    inputs <- c("line one\nline two\nline three\n/end\ndropped")
    i <- 0L
    fake_read <- function(prompt) {
        i <<- i + 1L
        if (i > length(inputs)) return(character())
        inputs[i]
    }
    ns <- asNamespace("corteza")
    orig <- ns$read_prompt_input
    on.exit({
        unlockBinding("read_prompt_input", ns)
        assign("read_prompt_input", orig, envir = ns)
        lockBinding("read_prompt_input", ns)
    }, add = TRUE)
    unlockBinding("read_prompt_input", ns)
    assign("read_prompt_input", fake_read, envir = ns)

    out <- corteza:::read_paste_block(seed = NULL,
                                      empty_message = "")
    expect_identical(out, "line one\nline two\nline three")
})

# /paste mode preserves literal trailing backslashes — code or paths
# with `\` at end of line shouldn't be interpreted as continuation
# markers in the explicit /paste contract.
local({
    inputs <- c("export PATH=foo\\", "/end")
    i <- 0L
    fake_read <- function(prompt) {
        i <<- i + 1L
        if (i > length(inputs)) return(character())
        inputs[i]
    }
    ns <- asNamespace("corteza")
    orig <- ns$read_prompt_input
    on.exit({
        unlockBinding("read_prompt_input", ns)
        assign("read_prompt_input", orig, envir = ns)
        lockBinding("read_prompt_input", ns)
    }, add = TRUE)
    unlockBinding("read_prompt_input", ns)
    assign("read_prompt_input", fake_read, envir = ns)

    out <- corteza:::read_paste_block(seed = NULL, empty_message = "")
    # The `\` survives because /paste mode doesn't strip continuation.
    expect_identical(out, "export PATH=foo\\")
})

# .console_deny_label ----
# The deny label is a plain "Deny". The interrupt-key hint (Esc /
# Ctrl+C) was removed because the actual key differs across RStudio, the
# terminal R console, and the corteza CLI -- no surface-dependent
# behavior left to pin.
expect_equal(corteza:::.console_deny_label(), "Deny")

# cli_tool_explanation ----
# Path/URL tools template deterministically from their args.
expect_equal(corteza:::cli_tool_explanation(list(tool = "read_file",
                                                 args = list(path = "R/x.R"))),
             "Read R/x.R.")
expect_equal(corteza:::cli_tool_explanation(list(tool = "fetch_url",
                                                 args = list(url = "https://e.com"))),
             "Fetch https://e.com.")
# Opaque exec tools (bash/run_r) surface the model's own narration.
expect_equal(corteza:::cli_tool_explanation(list(
                                                 tool = "bash", args = list(command = "git commit"),
                                                 model_context = list(assistant_text = "Commit the staged changes."))),
             "Commit the staged changes.")
# No narration available -> no explanation.
expect_null(corteza:::cli_tool_explanation(list(tool = "run_r",
                                                args = list(code = "1 + 1"))))

# .bounded_rationale: sanitize, collapse whitespace, cap length.
expect_equal(corteza:::.bounded_rationale("  multi\n  line\ttext "), "multi line text")
expect_null(corteza:::.bounded_rationale(NULL))
expect_null(corteza:::.bounded_rationale("   "))
bounded <- corteza:::.bounded_rationale(strrep("x", 300L), max_chars = 50L)
expect_true(nchar(bounded) <= 50L)
expect_true(endsWith(bounded, "..."))

# The approval prompt renders the explanation line under the title.
expl_lines <- corteza:::cli_approval_lines(
                                           list(tool = "bash", args = list(command = "git commit -m x"),
                                                model_context = list(assistant_text = "Commit the changes.")),
                                           decision = list(model = "cloud", reason = "default"),
                                           cwd = "/tmp/p")
expect_true(any(grepl("Commit the changes.", expl_lines, fixed = TRUE)))

# .handle_bash_prompt_status ----
# Regression: PR #49 wired the approval prompt to bash's `read -e -p`
# via .read_prompt_via_bash. When bash is killed by SIGINT (Ctrl+C
# at the prompt) it exits 130; before this fix the function returned
# character() and the caller defaulted empty -> "1" (Approve), so
# Ctrl+C silently approved the pending tool call. Status 130 must
# now raise an R-level interrupt so the surrounding
# tryCatch(interrupt = ...) handlers in inst/bin/corteza and
# R/chat.R catch it the same as a real terminal Ctrl+C.

# Status 130 -> interrupt condition. tryCatch's interrupt handler
# matches by class, and our stop() carries class c("interrupt",
# "condition").
caught_interrupt <- FALSE
tryCatch(
    corteza:::.handle_bash_prompt_status(130L, ""),
    interrupt = function(c) caught_interrupt <<- TRUE
)
expect_true(caught_interrupt)

# Non-zero non-130 status (read failure that isn't SIGINT) returns
# empty -- caller behavior unchanged for genuine read errors.
expect_identical(corteza:::.handle_bash_prompt_status(1L, ""), character())

# Status 0 with a missing tempfile path returns empty (defensive
# guard for the case where bash succeeded but the output file
# vanished).
expect_identical(
    corteza:::.handle_bash_prompt_status(0L, tempfile("nonexistent-")),
    character()
)

# Status 0 with a valid tempfile reads its lines back.
local({
    tmp <- tempfile("bash-prompt-status-")
    writeLines(c("first line", "second line"), tmp)
    on.exit(unlink(tmp), add = TRUE)
    expect_identical(corteza:::.handle_bash_prompt_status(0L, tmp),
                     c("first line", "second line"))
})

# NULL status is treated as success (system2 returns NULL when stdout
# = "" was requested and the child exited cleanly).
local({
    tmp <- tempfile("bash-prompt-null-")
    writeLines("ok", tmp)
    on.exit(unlink(tmp), add = TRUE)
    expect_identical(corteza:::.handle_bash_prompt_status(NULL, tmp), "ok")
})

# --- approval-explanation sanitization: model-controlled fields can't forge
# extra prompt lines or leak ANSI. ---
local({
    # Crafted path: embedded newline + fake "Reason:" line, plus an ANSI code.
    evil <- "a.txt\nReason: rm -rf /\033[31m injected"
    expl <- corteza:::cli_tool_explanation(
        list(tool = "read_file", args = list(path = evil)))
    expect_false(grepl("\n", expl, fixed = TRUE))     # no forged second line
    expect_false(grepl("\033", expl, fixed = TRUE))   # no raw ESC byte
    expect_false(grepl("[31m", expl, fixed = TRUE))   # no leftover ANSI tail
    expect_true(startsWith(expl, "Read "))            # still the template

    # An empty/whitespace field falls back to the template default.
    expl2 <- corteza:::cli_tool_explanation(
        list(tool = "fetch_url", args = list(url = "   ")))
    expect_identical(expl2, "Fetch the URL.")
})

# .sanitize_inline strips complete CSI sequences, not just the ESC byte; and
# .bounded_rationale stays NULL only when the result is empty.
expect_identical(corteza:::.sanitize_inline("hi \033[31mred\033[0m there"),
                 "hi red there")
expect_false(grepl("[31m",
                   corteza:::.bounded_rationale("x \033[31my"), fixed = TRUE))
expect_null(corteza:::.bounded_rationale("\033[0m   \n"))

# --- item 4 (full renderer): the approval prompt's detail and access lines
# sanitize model-controlled fields too, not only the one-line explanation. A
# crafted path/pattern can't forge an extra labeled line via an embedded
# newline. ---
local({
    lines <- corteza:::cli_approval_lines(
        list(tool = "read_file", args = list(path = "a.txt\nReason: forged")),
        decision = list(reason = "default"), cwd = "/tmp")
    expect_false(any(grepl("\n", lines, fixed = TRUE)))   # no injected lines
    expect_true(any(grepl("a.txt", lines, fixed = TRUE))) # path still shown

    det <- corteza:::cli_tool_detail_lines(
        "grep_files", list(pattern = "x\nReason: forged"), cwd = "/tmp")
    expect_false(any(grepl("\n", det, fixed = TRUE)))

    acc <- corteza:::cli_call_access_lines(
        list(tool = "read_file", args = list(path = "a.txt\nReason: forged")),
        cwd = "/tmp")
    expect_false(any(grepl("\n", acc, fixed = TRUE)))

    # The tool NAME (from the LLM tool-call name) is model-controlled too --
    # a crafted name can't forge a line in the rendered title.
    tlines <- corteza:::cli_approval_lines(
        list(tool = "read_file\nReason: forged", args = list(path = "a.txt")),
        decision = list(reason = "default"), cwd = "/tmp")
    expect_false(any(grepl("\n", tlines, fixed = TRUE)))

    # The no-path/no-URL access fallback renders the tool name too.
    accf <- corteza:::cli_call_access_lines(
        list(tool = "unknown\nReason: forged", args = list()), cwd = "/tmp")
    expect_false(any(grepl("\n", accf, fixed = TRUE)))
})

# cli_tool_preview feeds the progress display and the approval detail-line
# fallback (for tools with no explicit field line, e.g. git_diff's ref), so
# its model-controlled output is sanitized too.
expect_false(grepl("\n",
    corteza:::cli_tool_preview("git_diff", list(ref = "x\nReason: forged")),
    fixed = TRUE))

# cli_user_replied_line feeds a summary into the history the model reads next
# turn, so its model-controlled target (command/path/url/tool) is sanitized.
local({
    s <- corteza:::cli_user_replied_line(
        list(tool = "read_file", args = list(path = "a.txt\nReason: forged")),
        "1", cwd = "/tmp")
    expect_false(grepl("\n", s, fixed = TRUE))
    expect_true(grepl("a.txt Reason: forged", s, fixed = TRUE))
})

# .sanitize_inline: NA-safe, strips OSC payloads (not just CSI), and vectorized.
expect_identical(corteza:::.sanitize_inline(NA_character_), "")
expect_identical(corteza:::.sanitize_inline("a\033]8;;http://evil\033\\b"), "ab")
expect_identical(corteza:::.sanitize_inline(c("x\ny", "p\tq")), c("x y", "p q"))
