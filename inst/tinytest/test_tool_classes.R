# Every tool has a class, and one gate reads it (WP18, the
# approval-engine note).

# ---- Every built-in tool names its class -------------------------------
# One table for the tools corteza registers and the ones a room's host
# adds. A built-in tool registered without a class, or missing here,
# fails the suite: its class is decided where it is added.
expected <- c(
              read_file = "read", skill_instructions = "read", list_files = "read",
              grep_files = "read", read_handle = "read", r_help = "read",
              installed_packages = "read", web_search = "read", fetch_url = "read",
              git_status = "read", git_diff = "read", git_log = "read",
              bg_status = "read", collect_subagent = "read",
              list_subagents = "read", job_status = "read",
              write_file = "write", replace_in_file = "write", harness_note = "write",
              run_r = "exec", run_r_script = "exec", bash = "exec", cmd = "exec",
              spawn_subagent = "control", kill_subagent = "control",
              bg_kill = "control", job_cancel = "control",
              exit_plan_mode = "control", task_create = "control",
              task_update = "control",
              query_subagent = "continue", delegate = "continue",
              job_resume = "continue",
              send_room_file = "write", ask_room = "continue",
              delegate_to = "continue", edit_context = "control"
)

corteza::ensure_skills()
registry <- corteza:::.skill_registry
builtin <- Filter(function(n) isTRUE(registry[[n]]$builtin), ls(registry))
expect_true(length(builtin) >= 30L)
expect_identical(setdiff(builtin, names(expected)), character())
known <- intersect(names(expected),
                   c(builtin, names(corteza:::.session_tool_classes)))
for (name in known) {
    expect_identical(corteza:::classify_op(name), expected[[name]], info = name)
}

# ---- What each mode does with each class ------------------------------
# The table the gate reads, through policy(), for every tool above. A
# call with no path, in the terminal, with the user's default settings.
verdicts <- list(
                 default = c(read = "allow", write = "ask", exec = "ask",
                             control = "allow", continue = "allow", unknown = "ask"),
                 plan = c(read = "allow", write = "deny", exec = "deny",
                          control = "allow", continue = "deny", unknown = "deny")
)
expect_identical(corteza:::.mode_verdicts, verdicts)
local({
    op <- options(corteza.policy = NULL)
    on.exit(options(op), add = TRUE)
    cfg <- list(approval_mode = "ask",
                dangerous_tools = corteza:::default_dangerous_tools(),
                permissions = list())
    for (name in c(known, "no_such_tool")) {
        class <- corteza:::classify_op(name)
        for (mode in names(verdicts)) {
            d <- corteza::policy(list(tool = name, args = list(), channel = "cli",
                                      context = list(plan_mode = mode == "plan")),
                                 cfg)
            expect_identical(d$approval, verdicts[[mode]][[class]],
                             info = paste(name, mode))
        }
    }
})

# ---- A package tool, with a class and without --------------------------
local({
    on.exit(suppressWarnings(rm(list = c("tools::file_ext",
                                         "tools::file_path_sans_ext"),
                                envir = corteza:::.skill_registry)), add = TRUE)
    corteza:::package_as_skills("tools", functions = "file_ext")
    corteza:::package_as_skills("tools", functions = "file_path_sans_ext",
                                class = "read")
    expect_identical(corteza:::classify_op("tools::file_ext"), "unknown")
    expect_identical(corteza:::classify_op("tools::file_path_sans_ext"), "read")
    p <- function(tool, plan = FALSE) {
        corteza::policy(list(tool = tool, args = list(x = "a.R"), channel = "cli",
                             context = list(plan_mode = plan)), list())$approval
    }
    expect_identical(p("tools::file_ext"), "ask")
    expect_identical(p("tools::file_ext", plan = TRUE), "deny")
    expect_identical(p("tools::file_path_sans_ext"), "allow")
    expect_identical(p("tools::file_path_sans_ext", plan = TRUE), "allow")
    # A class nobody defined is refused when the entry loads.
    expect_message(corteza:::load_skill_packages(list(skill_packages = list(
                list(package = "tools", functions = "file_ext",
                     class = "harmless")))), "class must be one of")
    expect_identical(corteza:::classify_op("tools::file_ext"), "unknown")
})

# ---- Package tools a config lists ---------------------------------------
# None by default: a function becomes a tool when a config lists it,
# with its class. Listed as writes, file.copy and friends ask as
# write_file does, and plan mode refuses them. In a room with a monitor,
# a copy inside the checkout goes to the monitor, and one outside it
# goes to a person.
local({
    Sys.setenv(R_USER_CONFIG_DIR = tempfile("corteza_config_"))
    proj <- tempfile("classes-proj")
    dir.create(file.path(proj, ".git"), recursive = TRUE)
    proj <- normalizePath(proj)
    on.exit(unlink(proj, recursive = TRUE), add = TRUE)
    cfg <- corteza:::load_config(proj)
    expect_identical(cfg$skill_packages, list())
    before <- ls(corteza:::.skill_registry)
    corteza:::load_skill_packages(cfg)
    expect_identical(ls(corteza:::.skill_registry), before)
    cfg$skill_packages <- list(
        list(package = "base", functions = c("file.exists", "Sys.glob"),
             class = "read"),
        list(package = "base", functions = c("file.copy", "file.remove",
                                             "dir.create"), class = "write"),
        list(package = "utils", functions = c("read.csv", "head", "tail"),
             class = "read"))
    corteza:::load_skill_packages(cfg)
    on.exit(suppressWarnings(rm(list = grep("^(base|utils)::", ls(corteza:::.skill_registry),
                                            value = TRUE),
                                envir = corteza:::.skill_registry)), add = TRUE)
    for (name in c("base::file.copy", "base::file.remove", "base::dir.create")) {
        expect_identical(corteza:::classify_op(name), "write", info = name)
    }
    for (name in c("base::file.exists", "base::Sys.glob", "utils::read.csv",
                   "utils::head", "utils::tail")) {
        expect_identical(corteza:::classify_op(name), "read", info = name)
    }
    copy <- list(tool = "base::file.copy", channel = "matrix",
                 args = list(from = file.path(proj, "a.R"),
                             to = file.path(proj, "b.R")))
    d <- corteza::policy(copy, cfg)
    expect_identical(d$approval, "ask")
    expect_identical(corteza:::supervisor_route(copy, d, cwd = proj, root = proj)$route,
                     "monitor")
    copy$args$to <- "~/b.R"
    d <- corteza::policy(copy, cfg)
    expect_identical(corteza:::supervisor_route(copy, d, cwd = proj, root = proj)$route,
                     "human")
    copy$context <- list(plan_mode = TRUE)
    expect_identical(corteza::policy(copy, cfg)$approval, "deny")
    # The room's file sender asks, as any write does: corteza writes no
    # permission for it, so approval_mode and dangerous_tools reach it.
    # A person who wants it unasked says so in the config.
    expect_null(cfg$permissions$send_room_file)
    send <- list(tool = "send_room_file", channel = "matrix",
                 args = list(path = file.path(proj, "plot.png")))
    expect_identical(corteza::policy(send, cfg)$approval, "ask")
    expect_identical(corteza::policy(send, utils::modifyList(cfg,
                     list(approval_mode = "deny")))$approval, "deny")
    cfg$permissions$send_room_file <- "allow"
    expect_identical(corteza::policy(send, cfg)$approval, "allow")
})

# ---- A session in plan mode: the job and subagent tools ----------------
# Work that continues in a child, a worker or another room runs in a mode
# set elsewhere, so plan mode refuses it; starting a child, which takes
# the session's mode, and reading or stopping work pass.
local({
    s <- corteza::new_session("cli", approval_cb = function(...) TRUE,
                              plan_mode = TRUE)
    ran <- character()
    h <- corteza:::.make_tool_handler(s, tool_executor = function(name, args) {
        ran <<- c(ran, name)
        list(content = list(list(type = "text", text = "ok")))
    })
    for (tool in c("query_subagent", "delegate", "job_resume", "harness_note")) {
        out <- h(tool, list(id = "1", prompt = "x", task = "x"))
        expect_true(grepl("plan mode", out), info = tool)
    }
    passes <- c("spawn_subagent", "list_subagents", "collect_subagent",
                "kill_subagent", "job_status", "job_cancel")
    for (tool in passes) {
        h(tool, list(id = "1", task = "x"))
    }
    expect_identical(ran, passes)
})

# ---- chat()'s callback and the credential rule --------------------------
# A saved "always" answers by the tool's name. It never answers the
# credential rule's ask, whose prompt offers no "always"; a "2" there
# counts as once and is not kept.
local({
    ns <- asNamespace("corteza")
    replies <- character()
    prompts <- 0L
    fake <- function(prompt_str = "> ", use_readline = TRUE) {
        prompts <<- prompts + 1L
        if (!length(replies)) {
            return("1")
        }
        reply <- replies[[1L]]
        replies <<- replies[-1L]
        reply
    }
    orig <- ns$read_prompt_input
    unlockBinding("read_prompt_input", ns)
    assign("read_prompt_input", fake, envir = ns)
    lockBinding("read_prompt_input", ns)
    on.exit({
        unlockBinding("read_prompt_input", ns)
        assign("read_prompt_input", orig, envir = ns)
        lockBinding("read_prompt_input", ns)
    }, add = TRUE)

    cb <- corteza:::chat_approval_cb(tempdir())
    ask <- function(call, decision, reply = character()) {
        replies <<- reply
        before <- prompts
        said <- utils::capture.output(ok <- cb(call, decision))
        list(ok = ok, asked = prompts - before, said = said)
    }
    plain <- list(approval = "ask", reason = "config: needs approval")
    key <- list(approval = "ask", rule = "credential",
                reason = "safety: ~/.ssh/id_rsa is a credential path")
    notes <- list(tool = "read_file", args = list(path = "notes.md"))
    secret <- list(tool = "read_file", args = list(path = "~/.ssh/id_rsa"))

    got <- ask(notes, plain, "2")
    expect_true(got$ok)
    expect_equal(got$asked, 1L)
    expect_true(any(grepl("Allow always for this session", got$said, fixed = TRUE)))
    expect_equal(ask(notes, plain)$asked, 0L)

    got <- ask(secret, key, "1")
    expect_true(got$ok)
    expect_equal(got$asked, 1L)
    expect_false(any(grepl("Allow always", got$said, fixed = TRUE)))
    expect_true(any(grepl("3. ", got$said, fixed = TRUE)))
    got <- ask(secret, key, "2")
    expect_true(got$ok)
    expect_equal(got$asked, 1L)
    expect_equal(ask(secret, key, "1")$asked, 1L)
    expect_equal(ask(notes, plain)$asked, 0L)
    # Nor is a "2" there kept for the tool's ordinary calls.
    expect_equal(ask(list(tool = "write_file",
                          args = list(path = "~/.ssh/config", content = "x")),
                     key, "2")$asked, 1L)
    expect_equal(ask(list(tool = "write_file",
                          args = list(path = "out.md", content = "x")),
                     plain, "1")$asked, 1L)
})
