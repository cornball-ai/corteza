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
                                      persistent_label = "Allow always for this session"
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
# Key hints land on choices 1 and 3.
expect_true(any(grepl("Allow once (Enter)", lines, fixed = TRUE)))
expect_true(any(grepl("Deny (Esc)", lines, fixed = TRUE)))
expect_true(any(grepl("Allow always for this session", lines, fixed = TRUE)))

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
