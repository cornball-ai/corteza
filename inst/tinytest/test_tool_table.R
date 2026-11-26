# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# A session takes its tools when it starts (WP19, R/tool-table.R). Each
# block names what it pins.
source("isolate.R", local = TRUE)

local({
    ns <- asNamespace("corteza")
    root <- tempfile("tooltable-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    old_config <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA_character_)
    old_wd <- getwd()
    old_tools <- getOption("corteza.tools")
    ns$ensure_skills()
    reg <- ns$.skill_registry
    builtin_read <- get("read_file", envir = reg)
    on.exit({
        setwd(old_wd)
        # base:: on purpose: tinytest masks options() inside a test file.
        base::options(corteza.tools = old_tools)
        assign("read_file", builtin_read, envir = reg)
        suppressWarnings(rm(list = intersect(c("late_tool", "proj_tool",
                                               "user_tool"), ls(reg)),
                            envir = reg))
        if (is.na(old_config)) {
            Sys.unsetenv("R_USER_CONFIG_DIR")
        } else {
            Sys.setenv(R_USER_CONFIG_DIR = old_config)
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config-home"))

    put <- function(path, lines) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        writeLines(lines, path)
    }
    skill <- function(name, says, class) {
        sprintf(paste0("register_skill(skill_spec(%s, %s, params = list(),",
                       " handler = function(args, ctx) ok(%s), class = %s))"),
                deparse(name), deparse(paste("says", says)), deparse(says),
                deparse(class))
    }
    a <- file.path(root, "a")
    b <- file.path(root, "b")
    put(file.path(a, ".corteza", "skills", "tools.R"),
        c(skill("proj_tool", "from a", "read"),
          skill("read_file", "a's read_file", "read")))
    put(file.path(b, ".corteza", "skills", "tools.R"),
        skill("proj_tool", "from b", "write"))
    put(file.path(ns$corteza_data_path("skills"), "mine.R"),
        c(skill("user_tool", "from the user", "read"),
          skill("web_search", "the user's web_search", "read")))
    put(ns$corteza_config_path("config.json"),
        jsonlite::toJSON(list(trusted_projects = list(a, b),
                              skill_packages = list(list(package = "tools",
                                                         functions = "file_ext",
                                                         class = "read"))),
                         auto_unbox = TRUE))
    expect_true(ns$project_trusted(a))
    expect_true(ns$project_trusted(b))
    had_pkg <- exists("tools::file_ext", envir = reg, inherits = FALSE)

    said <- character()
    build <- function(dir) {
        said <<- character()
        withCallingHandlers(ns$session_tool_table(dir, ns$load_config(dir)),
                            message = function(m) {
            said <<- c(said, conditionMessage(m))
            invokeRestart("muffleMessage")
        })
    }
    text <- function(res) {
        paste(vapply(res$content %||% list(), function(x) x$text %||% "", ""),
              collapse = "")
    }
    run <- function(name, table) {
        text(ns$call_skill(name, list(), table = table))
    }

    # ---- Two projects in one process ---------------------------------------
    # Each session sees its own project's tool, runs its own handler and
    # reads its own class. Neither reaches the registry.
    ta <- build(a)
    said_a <- said
    tb <- build(b)
    expect_identical(run("proj_tool", ta), "from a")
    expect_identical(run("proj_tool", tb), "from b")
    expect_identical(ns$classify_op("proj_tool", ta), "read")
    expect_identical(ns$classify_op("proj_tool", tb), "write")
    expect_identical(ta$proj_tool$origin, list(kind = "project", dir = a))
    expect_identical(tb$proj_tool$origin, list(kind = "project", dir = b))
    expect_null(ns$get_skill("proj_tool"))
    expect_null(ns$get_skill("user_tool"))
    offered <- function(table) {
        vapply(ns$skills_as_api_tools(NULL, table), function(t) t$name, "")
    }
    expect_true("proj_tool" %in% offered(ta))
    expect_false("proj_tool" %in% offered(NULL))
    # The user's skill is in both, from the user, and so is the config's
    # package tool, with its class.
    expect_identical(run("user_tool", ta), "from the user")
    expect_identical(tb$user_tool$origin, list(kind = "user"))
    expect_identical(ta[["tools::file_ext"]]$origin, list(kind = "package"))
    expect_identical(ns$classify_op("tools::file_ext", tb), "read")
    expect_identical(exists("tools::file_ext", envir = reg, inherits = FALSE),
                     had_pkg)

    # A filter by package name selects from the same table: the package's
    # category is built from the session's tools, not the registry's.
    by_pkg <- function(filter, table) {
        vapply(ns$skills_as_api_tools(filter, table), function(t) t$name, "")
    }
    expect_identical(by_pkg("tools", ta), "tools__file_ext")
    expect_identical(by_pkg("tools::file_ext", ta), "tools__file_ext")
    if (!had_pkg) {
        expect_identical(by_pkg("tools", NULL), character())
    }

    # ---- A name is taken once ----------------------------------------------
    # A project's read_file and a user's web_search are skipped, and the
    # person is told which source lost the name to which.
    expect_true(ta$read_file$builtin)
    expect_identical(ta$read_file$origin, list(kind = "builtin"))
    expect_false(grepl("a's read_file", run("read_file", ta), fixed = TRUE))
    expect_true(any(grepl(sprintf(
        "Skipping the tool read_file from the project %s: a built-in tool", a),
        said_a, fixed = TRUE)))
    expect_true(any(grepl(
        "Skipping the tool web_search from your skills: a built-in tool",
        said_a, fixed = TRUE)))
    expect_identical(ta$web_search$origin, list(kind = "builtin"))
    # The registry's own read_file is untouched.
    expect_identical(get("read_file", envir = reg), builtin_read)

    # ---- A registration after the session started ---------------------------
    # It changes neither what the session offers nor what it runs: a new
    # tool, or a built-in replaced in the registry.
    ns$register_skill(ns$skill_spec("late_tool", "late", params = list(),
                                    handler = function(args, ctx) ns$ok("late"),
                                    class = "read"))
    ns$register_skill(ns$skill_spec("read_file", "replaced", params = list(),
                                    handler = function(args, ctx) ns$ok("replaced"),
                                    class = "read"))
    expect_false("late_tool" %in% offered(ta))
    expect_true(grepl("Unknown skill", run("late_tool", ta), fixed = TRUE))
    expect_identical(ns$classify_op("late_tool", ta), "unknown")
    expect_false(identical(run("read_file", ta), "replaced"))
    expect_identical(run("read_file", NULL), "replaced")
    assign("read_file", builtin_read, envir = reg)
    # A session started now takes what the registry holds, as a host's.
    tc <- build(a)
    expect_identical(tc$late_tool$origin, list(kind = "host"))
    rm("late_tool", envir = reg)

    # ---- Through turn()'s handler -------------------------------------------
    # The call the gate reads carries the class and origin of the entry
    # that runs, and the default executor runs the table's handler.
    seen <- NULL
    s <- ns$new_session("console", approval_cb = function(call, decision, ...) {
        seen <<- call
        TRUE
    })
    s$cwd <- b
    s$tool_table <- tb
    h <- ns$.make_tool_handler(s)
    out <- h("proj_tool", list())
    expect_true(grepl("from b", out, fixed = TRUE))
    expect_identical(seen$class, "write")
    expect_identical(seen$origin, "project")
    s$tool_table <- ta
    seen <- NULL
    out <- h("proj_tool", list())
    expect_true(grepl("from a", out, fixed = TRUE))
    expect_null(seen)
    # A name the table does not hold runs nothing, though the registry
    # has it.
    ns$register_skill(ns$skill_spec("late_tool", "late", params = list(),
                                    handler = function(args, ctx) ns$ok("late"),
                                    class = "read"))
    out <- h("late_tool", list())
    expect_true(grepl("Unknown skill: late_tool", out, fixed = TRUE))
    expect_identical(seen$class, "unknown")
    rm("late_tool", envir = reg)

    # A worker's approval request keeps the call's class and origin, so the
    # process that answers it reads the entry the worker runs, not the
    # registry's.
    job <- ns$job_create("fixture", workspace = b)
    ns$job_approval_request(job, list(tool = "proj_tool", args = list(),
                                      class = "write", origin = "project"),
                            list(reason = "ask"))
    req <- ns$job_approval_pending(job)[[1]]
    call <- ns$job_request_call(req)
    expect_identical(call$class, "write")
    expect_identical(call$origin, "project")
    expect_identical(ns$call_class(call), "write")
    expect_identical(ns$call_class(list(tool = "proj_tool")), "unknown")

    # ---- What turn() offers the model -----------------------------------------
    # The model call is stubbed, so this stays offline. A session offers
    # its own project's tool, and /auto's rule reads the table's class:
    # a's read may run in a run the monitor brokers, and b's write that
    # names no target is listed (WP14) and refused at the call.
    local({
        lns <- asNamespace("llm.api")
        orig <- get("agent", envir = lns, inherits = FALSE)
        sent <- NULL
        stub <- function(...) {
            sent <<- vapply(list(...)$tools, function(t) t$name, "")
            list(content = "ok", history = list(),
                 usage = list(input_tokens = 1L, output_tokens = 1L))
        }
        assignInNamespace("agent", stub, ns = "llm.api")
        on.exit(assignInNamespace("agent", orig, ns = "llm.api"), add = TRUE)
        s <- ns$new_session(channel = "console", provider = "anthropic",
                            model_map = list(cloud = "claude-test", local = NULL),
                            web_search = FALSE)
        corteza::turn("hi", s)
        expect_false("proj_tool" %in% sent)
        s$tool_table <- tb
        corteza::turn("hi", s)
        expect_true("proj_tool" %in% sent)
        expect_true("read_file" %in% sent)
        s$tool_table <- ta
        s$auto_config <- list()
        corteza::turn("hi", s)
        expect_true("proj_tool" %in% sent)
        # b's tool is a write that names no target: listed, as the table
        # gives it, and refused at the call.
        s$tool_table <- tb
        corteza::turn("hi", s)
        expect_true("proj_tool" %in% sent)
        handler <- ns$.make_tool_handler(s, function(...) stop("ran in an auto run"))
        expect_true(grepl("refused during this /auto run: proj_tool",
                          handler("proj_tool", list()), fixed = TRUE))
        s$tool_table <- ta
        handler <- ns$.make_tool_handler(s, function(...) ns$ok("read ran"))
        expect_false(grepl("refused during this /auto run",
                           handler("proj_tool", list()), fixed = TRUE))
        s$tool_table <- tb
        # The memory flush's session offers its parent's tools.
        s$auto_config <- NULL
        sent <- NULL
        ns$run_memory_flush(list(session = s, provider = "anthropic",
                                 model = "claude-test", cwd = b,
                                 config = list(memory_flush_prompt = "flush")))
        expect_true("proj_tool" %in% sent)
    })

    # ---- The gate reads the call's class --------------------------------------
    p <- function(call) ns$policy(call, list())$approval
    call <- list(tool = "proj_tool", args = list(), channel = "cli")
    expect_identical(p(call), "ask")
    expect_identical(p(c(call, class = "read")), "allow")
    expect_identical(p(c(call, list(class = "read",
                                    context = list(plan_mode = TRUE)))), "allow")
    expect_identical(p(c(call, list(class = "write",
                                    context = list(plan_mode = TRUE)))), "deny")
    # A class nobody defined is not taken.
    expect_identical(p(c(call, class = "harmless")), "ask")
    # A project's tool reads every string as a path, whatever its
    # arguments are called; corteza's own tools by name.
    named <- list(tool = "proj_tool", args = list(note = "~/x"), origin = "project")
    expect_identical(ns$resolve_paths(named), "~/x")
    named$origin <- "builtin"
    expect_identical(ns$resolve_paths(named), character())

    # ---- Each surface builds its own -----------------------------------------
    setup <- function(dir) {
        suppressMessages(ns$session_setup("console", cwd = dir,
                                          provider = "anthropic",
                                          model = "claude-test",
                                          validate_api_key = FALSE,
                                          load_project_context = FALSE))
    }
    sa <- setup(a)
    sb <- setup(b)
    expect_identical(run("proj_tool", sa$tool_table), "from a")
    expect_identical(run("proj_tool", sb$tool_table), "from b")
    expect_null(ns$get_skill("proj_tool"))
    # serve()'s handler lists and runs its own.
    mcp <- ns$.mcp_state
    old_mcp <- mcp$table
    assign("table", tb, envir = mcp)
    listed <- vapply(ns$mcp_visible_tools(), function(t) t$name, "")
    got <- ns$handle_request(list(jsonrpc = "2.0", id = 1L, method = "tools/call",
                                  params = list(name = "proj_tool",
                                                arguments = list())))
    assign("table", old_mcp, envir = mcp)
    expect_true("proj_tool" %in% listed)
    expect_true(grepl("from b", jsonlite::toJSON(got, auto_unbox = TRUE),
                      fixed = TRUE))
    # A child process builds its own from the same inputs.
    child <- callr::r(function(cwd) {
        corteza::worker_init(cwd)
        corteza::subagent_turn_init()
        s <- get(".subagent_state", envir = asNamespace("corteza"))$session
        list(origin = s$tool_table$proj_tool$origin,
             package = s$tool_table[["tools::file_ext"]]$origin,
             registry = is.null(corteza:::get_skill("proj_tool")))
    }, list(cwd = b))
    expect_identical(child$origin, list(kind = "project", dir = b))
    expect_identical(child$package, list(kind = "package"))
    expect_true(child$registry)

    # ---- A change of trust applies to the next session ----------------------
    put(ns$corteza_config_path("config.json"),
        jsonlite::toJSON(list(trusted_projects = list(a)), auto_unbox = TRUE))
    expect_false(ns$project_trusted(b))
    expect_null(build(b)$proj_tool)
    expect_identical(run("proj_tool", tb), "from b")
    expect_identical(run("proj_tool", sb$tool_table), "from b")
})
