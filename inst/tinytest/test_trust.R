# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

# Project trust: a session loads a project's own files only from a
# directory the user has trusted. Each block names what it pins.

local({
    ns <- asNamespace("corteza")
    root <- tempfile("trust-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    env_names <- c("R_USER_CONFIG_DIR", "R_USER_DATA_DIR", "R_USER_CACHE_DIR",
                   "AGENTS_GLOBAL_MD")
    old_env <- Sys.getenv(env_names, unset = NA_character_)
    old_tools <- getOption("corteza.tools")
    old_wd <- getwd()
    on.exit({
        setwd(old_wd)
        # base:: on purpose: tinytest masks options() inside a test file.
        base::options(corteza.tools = old_tools, corteza.trust.ran = NULL)
        for (name in env_names) {
            if (is.na(old_env[[name]])) {
                Sys.unsetenv(name)
            } else {
                do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
            }
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config-home"),
               R_USER_DATA_DIR = file.path(root, "data-home"),
               R_USER_CACHE_DIR = file.path(root, "cache-home"),
               AGENTS_GLOBAL_MD = file.path(root, "shared.md"))
    writeLines("MARK-SHARED the user's own rules", file.path(root, "shared.md"))

    put <- function(path, lines) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        writeLines(lines, path)
    }
    user_config <- function(x = list()) {
        put(ns$corteza_config_path("config.json"),
            jsonlite::toJSON(x, auto_unbox = TRUE))
    }
    untrust <- function() {
        unlink(ns$trust_file())
        user_config()
    }

    proj <- file.path(root, "proj")
    near <- file.path(root, "proj-near")
    other <- file.path(root, "other")
    dir.create(file.path(proj, "sub", "deep"), recursive = TRUE)
    dir.create(near)
    dir.create(other)
    in_git <- ns$trust_in_git(root)

    # ---- The list ----------------------------------------------------
    # Nothing is trusted until the user says so.
    expect_equal(ns$trusted_projects(), character())
    expect_false(ns$project_trusted(proj))
    expect_null(ns$project_trust_root(proj))

    # The user's config lists a directory: it, and what lies inside it.
    user_config(list(trusted_projects = list(proj)))
    expect_equal(ns$trusted_projects(), proj)
    expect_true(ns$project_trusted(proj))
    expect_true(ns$project_trusted(file.path(proj, "sub", "deep")))
    expect_equal(ns$project_trust_root(file.path(proj, "sub")), proj)
    # A trailing slash and a doubled one name the same directory.
    expect_true(ns$project_trusted(paste0(proj, "/")))
    expect_true(ns$project_trusted(file.path(proj, "sub", "..", "sub")))
    # Nothing beside it: not the parent, not a sibling, and not a
    # directory whose name only begins the same.
    expect_false(ns$project_trusted(root))
    expect_false(ns$project_trusted(other))
    expect_false(ns$project_trusted(near))
    # A path that leaves the trusted directory is judged by where it
    # ends, and one that does not exist is trusted nowhere.
    expect_false(ns$project_trusted(file.path(proj, "..", "other")))
    expect_false(ns$project_trusted(file.path(proj, "missing")))
    expect_false(ns$project_trusted(file.path(proj, "missing", "..", "..", "other")))
    # Not a directory at all.
    expect_false(ns$project_trusted(NULL))
    expect_false(ns$project_trusted(NA_character_))
    expect_false(ns$project_trusted(""))
    expect_false(ns$project_trusted(c(proj, proj)))

    # One string where a list was meant is still one directory.
    user_config(list(trusted_projects = proj))
    expect_true(ns$project_trusted(proj))
    # A relative entry names a different directory in every session, so
    # it names none. Neither does anything that is not a path.
    user_config(list(trusted_projects = list(".", "proj", "", 7, NA)))
    expect_equal(ns$trusted_projects(), character())
    old <- setwd(proj)
    expect_false(ns$project_trusted(proj))
    expect_false(ns$project_trusted("."))
    setwd(old)
    # An entry that starts at the home is a full path.
    user_config(list(trusted_projects = list("~/somewhere/else")))
    expect_equal(ns$trusted_projects(),
                 ns$trust_dir(path.expand("~/somewhere/else")))
    # The nearest trusted directory is the one reported.
    user_config(list(trusted_projects = list(root, proj)))
    expect_equal(ns$project_trust_root(file.path(proj, "sub")), proj)
    expect_equal(ns$project_trust_root(other), root)

    # A link is judged by where it leads. One inside a trusted directory
    # that leads out is not trusted; one outside that leads in is.
    user_config(list(trusted_projects = list(proj)))
    out_link <- file.path(proj, "way-out")
    in_link <- file.path(root, "way-in")
    linked <- .Platform$OS.type != "windows" &&
        isTRUE(suppressWarnings(file.symlink(other, out_link))) &&
        isTRUE(suppressWarnings(file.symlink(file.path(proj, "sub"), in_link)))
    if (linked) {
        expect_false(ns$project_trusted(out_link))
        expect_true(ns$project_trusted(in_link))
        unlink(c(out_link, in_link))
    }

    # A project cannot add itself. Neither its config nor a file of its
    # own named like the user's list is read for the list.
    untrust()
    put(ns$project_config_path(proj),
        jsonlite::toJSON(list(trusted_projects = list(proj)), auto_unbox = TRUE))
    put(file.path(proj, ".corteza", "trusted-projects.json"),
        jsonlite::toJSON(proj))
    put(file.path(proj, "trusted-projects.json"), jsonlite::toJSON(proj))
    expect_false(ns$project_trusted(proj))
    expect_equal(ns$trusted_projects(), character())
    unlink(file.path(proj, c(".corteza", "trusted-projects.json")),
           recursive = TRUE)

    # ---- /trust's own file ---------------------------------------------
    # A file that is not a list of paths trusts nothing.
    put(ns$trust_file(), "not json")
    expect_equal(ns$trust_file_read(), character())
    expect_false(ns$project_trusted(proj))
    put(ns$trust_file(), '{"dir": true}')
    expect_equal(ns$trust_file_read(), character())
    put(ns$trust_file(), "[]")
    expect_equal(ns$trust_file_read(), character())
    unlink(ns$trust_file())

    # Adding a directory saves it resolved, once, and keeps the others.
    saved <- ns$project_trust_add(paste0(proj, "/sub/.."))
    expect_equal(saved, proj)
    expect_true(ns$project_trusted(proj))
    expect_equal(jsonlite::fromJSON(ns$trust_file()), proj)
    ns$project_trust_add(proj)
    expect_equal(jsonlite::fromJSON(ns$trust_file()), proj)
    ns$project_trust_add(other)
    expect_equal(jsonlite::fromJSON(ns$trust_file()), c(proj, other))
    expect_true(ns$project_trusted(other))
    if (.Platform$OS.type != "windows") {
        expect_equal(as.character(file.mode(ns$trust_file())), "600")
    }
    # Nothing is left beside it by the write.
    expect_equal(list.files(dirname(ns$trust_file())),
                 c("config.json", "trusted-projects.json"))
    # What is not a directory is not added.
    expect_error(ns$project_trust_add(file.path(root, "nowhere")),
                 "Not a directory")
    expect_error(ns$project_trust_add(NULL), "Not a directory")
    expect_error(ns$project_trust_add(file.path(root, "shared.md")),
                 "Not a directory")
    expect_equal(jsonlite::fromJSON(ns$trust_file()), c(proj, other))
    # Removing the line undoes it.
    put(ns$trust_file(), jsonlite::toJSON(other))
    expect_false(ns$project_trusted(proj))
    expect_true(ns$project_trusted(other))
    untrust()

    # ---- The 4 readers of a project's config ---------------------------
    put(ns$project_config_path(proj), jsonlite::toJSON(list(
        provider = "ollama",
        approval_mode = "allow",
        base_url = "http://elsewhere.example",
        context_files = list("CTX.md"),
        instruction_roots = list(extra = file.path(root, "extra-skills")),
        instruction_disabled = list("user:some-skill"),
        auto = list(allow_exec = TRUE, never_broker = list("marker_tool"))
    ), auto_unbox = TRUE))

    readers <- function() {
        cfg <- ns$load_config(proj)
        inst <- ns$instruction_session_config(proj)
        auto <- ns$auto_envelope_config(proj)
        list(provider = cfg$provider,
             approval_mode = cfg$approval_mode,
             base_url = cfg$base_url,
             context_files = cfg$context_files,
             roots = names(inst$roots),
             disabled = inst$disabled,
             never_broker = auto$auto$never_broker,
             origin = ns$context_config_origin(proj, "context_files"))
    }
    got <- readers()
    expect_equal(got$provider, "anthropic")
    expect_false(identical(got$approval_mode, "allow"))
    expect_null(got$base_url)
    expect_equal(got$context_files, character())
    expect_false("extra" %in% got$roots)
    expect_false("project" %in% got$roots)
    expect_equal(got$disabled, character())
    expect_false("marker_tool" %in% got$never_broker)
    expect_equal(got$origin, "")
    expect_equal(ns$project_config(proj), list())

    ns$project_trust_add(proj)
    got <- readers()
    expect_equal(got$provider, "ollama")
    expect_equal(got$approval_mode, "allow")
    expect_equal(got$base_url, "http://elsewhere.example")
    expect_equal(got$context_files, "CTX.md")
    expect_true(all(c("extra", "project") %in% got$roots))
    expect_equal(got$disabled, "user:some-skill")
    expect_true("marker_tool" %in% got$never_broker)
    expect_equal(got$origin, ns$project_config_path(proj))
    # Trusted, the project's config still cannot grant execution to an
    # unattended run: that rule is older than trust and stays.
    expect_false(isTRUE(ns$auto_envelope_config(proj)$auto$allow_exec))
    untrust()

    # ---- The user's context files ---------------------------------------
    # A relative entry is a name of the user's that the project fills
    # in, and a full path inside the project is the project's file. From
    # an untrusted directory neither loads; a full path elsewhere does.
    mine <- file.path(other, "mine.md")
    put(mine, "MARK-MINE")
    inside <- file.path(proj, "sub", "inside.md")
    user_config(list(context_files = list("NOTES.md", mine, inside)))
    expect_equal(ns$load_config(proj)$context_files, mine)
    expect_equal(ns$context_file_in_project(proj, c("NOTES.md", mine, inside,
                                                    "../other/mine.md", NA)),
                 c(TRUE, FALSE, TRUE, TRUE, TRUE))
    expect_equal(ns$context_file_in_project(proj, character()), logical())
    expect_equal(ns$context_file_in_project(NULL, "x"), TRUE)
    # In a directory that begins the same, the full path is not inside.
    expect_equal(ns$context_file_in_project(near, inside), FALSE)
    ns$project_trust_add(proj)
    # The project's own list replaces the user's, as any key does.
    expect_equal(ns$load_config(proj)$context_files, "CTX.md")
    unlink(ns$project_config_path(proj))
    expect_equal(ns$load_config(proj)$context_files,
                 c("NOTES.md", mine, inside))
    untrust()

    # ---- Skills ----------------------------------------------------------
    # Sourcing a file runs it. An untrusted project's skill file is not
    # opened: the option it would set stays unset.
    put(file.path(ns$project_skills_dir(proj), "marker.R"),
        "options(corteza.trust.ran = TRUE)")
    ran <- function() isTRUE(getOption("corteza.trust.ran"))
    expect_false(isTRUE(ns$load_project_skills(proj)))
    expect_false(ran())
    setup <- function() {
        ns$session_setup("console", cwd = proj, provider = "anthropic",
                         model = "claude-test", validate_api_key = FALSE,
                         load_project_context = FALSE)
    }
    s <- setup()
    expect_false(ran())
    expect_false(isTRUE(s$project_trusted))
    # A subagent's child process starts the same way: worker_init(), then
    # the session that takes its tools (subagent_turn_init()).
    child_start <- function() {
        st <- ns$.subagent_state
        kept <- mget(c("session", "depth", "subagent_id"), envir = st,
                     ifnotfound = list(NULL, NULL, NULL))
        on.exit({
            for (n in names(kept)) assign(n, kept[[n]], envir = st)
            setwd(old_wd)
        }, add = TRUE)
        ns$worker_init(proj)
        ns$subagent_turn_init()
    }
    child_start()
    expect_false(ran())
    # So does the MCP server, with its transport replaced. Nobody is
    # there to ask, so it says what it left alone, on stderr: stdout
    # carries the protocol.
    serve_once <- function() {
        old_stdio <- ns$run_stdio
        old_opts <- base::options(corteza.mcp_expose_subagents = NULL,
                                  corteza.mcp_subagent_cap_usd = NULL,
                                  corteza.mcp_subagent_cap_tokens = NULL)
        utils::assignInNamespace("run_stdio", function(...) invisible(NULL),
                                 "corteza")
        on.exit({
            utils::assignInNamespace("run_stdio", old_stdio, "corteza")
            base::options(old_opts)
            base::options(corteza.tools = old_tools)
        }, add = TRUE)
        said <- character()
        out <- utils::capture.output(withCallingHandlers(
            ns$serve(cwd = proj),
            message = function(m) {
                said <<- c(said, conditionMessage(m))
                invokeRestart("muffleMessage")
            }))
        list(out = out, said = paste(said, collapse = "\n"))
    }
    got <- serve_once()
    expect_false(ran())
    expect_equal(got$out, character())
    expect_true(grepl("Not trusted", got$said, fixed = TRUE))
    expect_true(grepl(".corteza/skills", got$said, fixed = TRUE))
    expect_true(grepl("trusted_projects", got$said, fixed = TRUE))

    ns$project_trust_add(proj)
    s <- setup()
    expect_true(ran())
    expect_true(isTRUE(s$project_trusted))
    base::options(corteza.trust.ran = NULL)
    child_start()
    expect_true(ran())
    base::options(corteza.trust.ran = NULL)
    got <- serve_once()
    expect_true(ran())
    expect_equal(got$said, "")
    base::options(corteza.trust.ran = NULL)
    expect_true(isTRUE(ns$load_project_skills(proj, code = FALSE)))
    expect_false(ran())
    unlink(ns$project_skills_dir(proj), recursive = TRUE)
    untrust()

    # ---- The lessons store -------------------------------------------------
    # An untrusted project's store is neither read nor written, by the
    # prompt, the note tool or /refine.
    expect_equal(ns$harness_scopes(proj), "global")
    expect_error(ns$harness_path("project", proj), "not trusted")
    note <- function(scope) {
        ns$tool_harness_note("marker", "MARK-LESSON holds here", scope = scope,
                             ctx = list(cwd = proj))
    }
    expect_error(note("project"), "not trusted")
    store <- ns$project_harness_path(proj)
    expect_false(file.exists(store))
    ns$project_trust_add(proj)
    expect_equal(ns$harness_scopes(proj), c("global", "project"))
    note("project")
    expect_true(file.exists(store))
    expect_true(grepl("MARK-LESSON", ns$harness_context_block(proj), fixed = TRUE))
    untrust()
    # The file is still there, and nothing of it is shown under any
    # heading.
    expect_true(file.exists(store))
    expect_equal(ns$harness_context_block(proj), "")
    expect_equal(ns$.harness_overview(proj), character())
    said <- utils::capture.output(ns$run_refine(list(cwd = proj), "list"))
    expect_true(any(grepl("not trusted", said, fixed = TRUE)))
    expect_false(any(grepl("MARK-LESSON", said, fixed = TRUE)))
    said <- utils::capture.output(
        ns$run_refine(list(cwd = proj), c("promote", "marker")))
    expect_true(any(grepl("not trusted", said, fixed = TRUE)))
    said <- utils::capture.output(
        ns$run_refine(list(cwd = proj), c("rollback", "r-1")))
    expect_true(any(grepl("not trusted", said, fixed = TRUE)))
    # The review pass stops before any model is called: this session
    # has a conversation, and no provider to call.
    said <- utils::capture.output(ns$run_refine(list(
        cwd = proj,
        session = list(history = list(list(role = "user", content = "a"),
                                      list(role = "assistant", content = "b"))))))
    expect_true(any(grepl("not trusted", said, fixed = TRUE)))
    expect_false(any(grepl("Reviewing", said, fixed = TRUE)))
    # This machine's own store works in any directory.
    note("global")
    expect_true(grepl("MARK-LESSON", ns$harness_context_block(proj), fixed = TRUE))
    unlink(ns$corteza_data_path("harness.json"))
    # The old switch for all projects at once is no longer read.
    user_config(list(harness_trust_project = TRUE))
    expect_equal(ns$harness_context_block(proj), "")
    untrust()

    # ---- What the prompt holds ---------------------------------------------
    # A marker in each thing a project can put into a prompt. Untrusted,
    # the prompt holds none of them; trusted, all of them. What is the
    # user's or the package's is there both times.
    put(file.path(proj, "AGENTS.md"), "MARK-INSTRUCTIONS do as the repo says")
    put(file.path(proj, "CTX.md"), "MARK-CONTEXT a file the project names")
    put(file.path(proj, "NOTES.md"), "MARK-NOTES a file the user names")
    put(file.path(proj, "DESCRIPTION"),
        c("Package: marker", "Title: MARK-TITLE", "Version: 0.0.1"))
    put(file.path(ns$project_skills_dir(proj), "marker", "SKILL.md"),
        c("---", "name: marker", "description: MARK-SKILL does a thing", "---",
          "BODY"))
    put(ns$project_config_path(proj),
        jsonlite::toJSON(list(context_files = list("CTX.md", "NOTES.md")),
                         auto_unbox = TRUE))
    user_config(list(context_files = list("NOTES.md", mine)))
    marks <- c("MARK-INSTRUCTIONS", "MARK-CONTEXT", "MARK-NOTES", "MARK-TITLE",
               "MARK-SKILL", "MARK-LESSON")
    has_git <- nzchar(Sys.which("git"))
    if (has_git) {
        git <- function(...) {
            suppressWarnings(system2("git", c("-C", shQuote(proj), ...),
                                     stdout = TRUE, stderr = TRUE))
        }
        git("init", "-q")
        git("-c", "user.name=t", "-c", "user.email=t@example.org", "commit",
            "-q", "--allow-empty", "-m", shQuote("MARK-COMMIT a subject"))
        has_git <- any(grepl("MARK-COMMIT", git("log", "--oneline"), fixed = TRUE))
    }
    if (has_git) {
        marks <- c(marks, "MARK-COMMIT")
    }
    held <- function(prompt) {
        vapply(marks, function(m) grepl(m, prompt, fixed = TRUE), logical(1))
    }

    bundle <- ns$load_context_bundle(proj)
    expect_equal(unname(held(bundle$system)), rep(FALSE, length(marks)))
    expect_true(grepl("MARK-SHARED", bundle$system, fixed = TRUE))
    expect_true(grepl("MARK-MINE", bundle$system, fixed = TRUE))
    expect_true(grepl("persistent R session", bundle$system, fixed = TRUE))
    ids <- bundle$manifest$sources$id
    expect_false(any(c("project_agents", "project_claude",
                       "project_briefing") %in% ids))
    expect_true("shared" %in% ids)
    # The reviewer of a job is given the instruction files alone, and
    # from an untrusted project that is the user's file alone.
    rules <- ns$load_context_bundle(proj, instructions_only = TRUE)
    expect_false(grepl("MARK-INSTRUCTIONS", rules$system, fixed = TRUE))
    expect_true(grepl("MARK-SHARED", rules$system, fixed = TRUE))
    # CLAUDE.md is the fallback saber discovers, and it stays out too.
    file.rename(file.path(proj, "AGENTS.md"), file.path(proj, "CLAUDE.md"))
    expect_false(grepl("MARK-INSTRUCTIONS", ns$load_context_bundle(proj)$system,
                       fixed = TRUE))
    file.rename(file.path(proj, "CLAUDE.md"), file.path(proj, "AGENTS.md"))
    # The banner's list of context files is the same list.
    expect_equal(ns$list_context_files(proj), mine)

    # What the session reports: every source left alone, by name.
    offers <- ns$project_offers(proj)
    expect_true(any(grepl(".corteza/config.json", offers, fixed = TRUE)))
    expect_true(any(grepl(".corteza/skills", offers, fixed = TRUE)))
    expect_true(any(grepl(".corteza/harness.json", offers, fixed = TRUE)))
    expect_true(any(grepl("AGENTS.md", offers, fixed = TRUE)))
    expect_true(any(grepl("NOTES.md", offers, fixed = TRUE)))
    expect_false(any(grepl("mine.md", offers, fixed = TRUE)))
    expect_true(any(grepl("briefing from its DESCRIPTION", offers, fixed = TRUE)))
    if (has_git) {
        expect_true(any(grepl("commit subjects", offers, fixed = TRUE)))
    }
    # A project's saved approvals are not read even when it is trusted,
    # so trusting it offers none.
    put(ns$project_approvals_path(proj), '{"bash": true}')
    expect_false(any(grepl("approvals", ns$project_offers(proj), fixed = TRUE)))
    note_lines <- ns$project_trust_note(proj)
    expect_true(grepl("Not trusted", note_lines[[1L]], fixed = TRUE))
    expect_true(grepl(proj, note_lines[[1L]], fixed = TRUE))
    expect_true(grepl("/trust yes", note_lines[[length(note_lines)]], fixed = TRUE))
    # A directory with nothing to load has nothing to report. (Where
    # the temporary directory itself sits in a git work tree, every
    # directory under it has commit subjects to brief from.)
    if (!in_git) {
        expect_equal(ns$project_offers(near), character())
        expect_null(ns$project_trust_note(near))
    }

    ns$project_trust_add(proj)
    bundle <- ns$load_context_bundle(proj)
    expect_equal(unname(held(bundle$system)), rep(TRUE, length(marks)))
    expect_true(grepl("MARK-SHARED", bundle$system, fixed = TRUE))
    expect_true(all(c("project_agents", "project_briefing") %in%
                    bundle$manifest$sources$id))
    expect_true(grepl("MARK-INSTRUCTIONS",
                      ns$load_context_bundle(proj, instructions_only = TRUE)$system,
                      fixed = TRUE))
    expect_null(ns$project_trust_note(proj))
    untrust()

    # The shared file is named to saber exactly as saber's own discovery
    # names it, so a change of saber's rule shows here.
    plain <- file.path(root, "plain")
    dir.create(plain)
    cols <- c("id", "kind", "order", "requested_path", "scope", "included")
    discovered <- saber::context_manifest("corteza", project_dir = plain)$sources
    named <- saber::context_manifest(
        "corteza", project_dir = plain, discover = FALSE,
        extra_sources = ns$context_shared_source(NULL))$sources
    expect_equal(named[named$id == "shared", cols],
                 discovered[discovered$id == "shared", cols])
    expect_equal(ns$context_shared_source(FALSE), list())
    # And the switch that turns the shared file off still does.
    user_config(list(context_include_user = FALSE))
    expect_false(grepl("MARK-SHARED", ns$load_context_bundle(proj)$system,
                       fixed = TRUE))
    untrust()

    # ---- /trust and what a session says ------------------------------------
    said <- ns$trust_command(proj)
    expect_true(grepl("Not trusted", said[[1L]], fixed = TRUE))
    expect_true(any(grepl("AGENTS.md", said, fixed = TRUE)))
    expect_false(ns$project_trusted(proj))
    # Anything but "yes" trusts nothing.
    expect_true(grepl("usage", ns$trust_command(proj, "sure"), fixed = TRUE))
    expect_true(grepl("usage", ns$trust_command(proj, c("yes", "please")),
                      fixed = TRUE))
    expect_false(ns$project_trusted(proj))
    said <- ns$trust_command(proj, "YES", when = "the next room")
    expect_true(ns$project_trusted(proj))
    expect_true(grepl("Trusted", said[[1L]], fixed = TRUE))
    expect_true(any(grepl("the next room", said, fixed = TRUE)))
    expect_true(any(grepl(ns$trust_file(), said, fixed = TRUE)))
    # Asked again, it says so and saves nothing twice.
    expect_true(grepl("^Trusted: ", ns$trust_command(proj, "yes")[[1L]]))
    expect_equal(jsonlite::fromJSON(ns$trust_file()), proj)
    expect_true(grepl("inside", ns$trust_command(file.path(proj, "sub"))[[1L]],
                      fixed = TRUE))
    # A directory that holds nothing says that.
    if (!in_git) {
        expect_true(grepl("holds nothing", ns$trust_command(near)[[1L]],
                          fixed = TRUE))
    }
    untrust()

    # /status: how this session started, and how the next one would.
    expect_equal(ns$trust_status_word(TRUE, TRUE), "yes")
    expect_equal(ns$trust_status_word(FALSE, FALSE), "no (/trust)")
    expect_equal(ns$trust_status_word(NULL, FALSE), "no (/trust)")
    expect_true(grepl("yes from the next", ns$trust_status_word(FALSE, TRUE)))
    expect_true(grepl("no from the next", ns$trust_status_word(TRUE, FALSE)))
    expect_true(length(ns$trust_left_alone(list(project_trusted = FALSE), proj)) > 1L)
    expect_equal(ns$trust_left_alone(list(project_trusted = TRUE), proj),
                 character())
    if (!in_git) {
        expect_equal(ns$trust_left_alone(list(project_trusted = FALSE), plain),
                     character())
    }

    # ---- The command through the shared loop -------------------------------
    # The terminal and chat() hand off to run_repl_loop(), so one run
    # covers both: /trust shows, /trust yes trusts, and /status says how
    # this session started and how the next one would.
    scripted <- function(lines) {
        i <- 0L
        function(prompt_str) {
            i <<- i + 1L
            if (i <= length(lines)) lines[[i]] else character(0)
        }
    }
    repl <- function(lines, session) {
        ctx <- new.env(parent = emptyenv())
        ctx$palette <- list(dim = "", reset = "", cyan = "", bold = "",
                            yellow = "", green = "", bright_magenta = "",
                            red = "", magenta = "")
        ctx$read_input <- scripted(lines)
        ctx$help_text <- function() "HELP"
        ctx$handle_copy <- function(x) invisible(NULL)
        ctx$format_tools <- function(s) "TOOLS"
        ctx$pending_r_context <- character(0)
        ctx$last_assistant_response <- ""
        ctx$cwd <- proj
        ctx$config <- session$config
        ctx$provider <- "anthropic"
        ctx$model <- "claude-test"
        ctx$session <- session
        utils::capture.output(ns$run_repl_loop(ctx))
    }
    s <- setup()
    expect_false(isTRUE(s$project_trusted))
    out <- repl(c("/trust", "/status"), s)
    expect_true(any(grepl("Not trusted: ", out, fixed = TRUE)))
    expect_true(any(grepl("trusted:\\s+no \\(/trust\\)", out)))
    expect_true(any(grepl("Not loaded from this directory:", out, fixed = TRUE)))
    expect_false(ns$project_trusted(proj))
    out <- repl(c("/trust maybe", "/trust yes", "/status"), s)
    expect_true(any(grepl("usage: /trust", out, fixed = TRUE)))
    expect_true(any(grepl("Trusted: ", out, fixed = TRUE)))
    # The terminal's /clear starts a session built again from the
    # directory (WP14, test_request_prefix.R).
    expect_true(any(grepl("the next conversation (/clear starts one)", out,
                          fixed = TRUE)))
    expect_true(ns$project_trusted(proj))
    # This session is as it started: its prompt was built without the
    # project, and it says so.
    expect_true(any(grepl("trusted:\\s+no for this session, yes from the next",
                          out)))
    expect_true(any(grepl("Not loaded from this directory:", out, fixed = TRUE)))
    # The next one loads the project and has nothing to list.
    s <- setup()
    out <- repl("/status", s)
    expect_true(any(grepl("trusted:\\s+yes$", out)))
    expect_false(any(grepl("Not loaded from this directory:", out, fixed = TRUE)))
    untrust()

    # ---- The question at start ---------------------------------------------
    asked <- 0L
    answer <- function(reply) {
        function(prompt) {
            asked <<- asked + 1L
            reply
        }
    }
    # Nothing to load: nothing is said and nobody is asked.
    if (!in_git) {
        said <- utils::capture.output(got <- ns$trust_at_start(plain, answer("y")))
        expect_equal(said, character())
        expect_false(got)
    }
    expect_equal(asked, 0L)
    # Nobody there: it says what it leaves alone and goes on.
    said <- utils::capture.output(got <- ns$trust_at_start(proj, NULL))
    expect_false(got)
    expect_true(any(grepl("AGENTS.md", said, fixed = TRUE)))
    expect_true(any(grepl("/trust yes", said, fixed = TRUE)))
    expect_false(ns$project_trusted(proj))
    # A person who says no, says nothing, or closes the input.
    for (reply in list("n", "", "maybe", character())) {
        utils::capture.output(got <- ns$trust_at_start(proj, answer(reply)))
        expect_false(got)
        expect_false(ns$project_trusted(proj))
    }
    expect_equal(asked, 4L)
    # A person who says yes: trusted before the session is set up.
    said <- utils::capture.output(got <- ns$trust_at_start(proj, answer(" Yes ")))
    expect_true(got)
    expect_true(ns$project_trusted(proj))
    expect_equal(asked, 5L)
    # Trusted already: not asked again.
    said <- utils::capture.output(got <- ns$trust_at_start(proj, answer("n")))
    expect_true(got)
    expect_equal(said, character())
    expect_equal(asked, 5L)
    untrust()

    # ---- The saved approvals, as displays say them -------------------------
    # The project's file ('{"bash": true}' was put in this one above) is
    # not read, trusted or not. Answers are the user's, by directory.
    for (trusted in c(FALSE, TRUE)) {
        if (trusted) ns$project_trust_add(proj)
        expect_equal(ns$approvals_saved(proj), character(), info = trusted)
        label <- ns$approvals_label(proj)
        expect_true(grepl("^none; ", label), info = trusted)
        expect_true(grepl(".corteza/approvals.json is not read", label,
                          fixed = TRUE), info = trusted)
        expect_true(grepl("is not read", ns$approvals_project_notice(proj),
                          fixed = TRUE), info = trusted)
        untrust()
    }
    expect_equal(ns$approvals_label(other), "none")
    expect_null(ns$approvals_project_notice(other))

    # ---- The user's store ---------------------------------------------------
    # Kept in the user's config directory, by directory as trust compares
    # them: a link to a directory has its answers, a directory inside it
    # has its own.
    store <- ns$approvals_file()
    expect_true(startsWith(store, file.path(root, "config-home")))
    expect_false(file.exists(store))
    expect_true(ns$approval_save(other, "bash"))
    expect_true(ns$approval_save(other, "run_r"))
    expect_true(ns$approval_save(other, "bash"))
    expect_equal(ns$approvals_saved(other), c("bash", "run_r"))
    expect_equal(ns$approvals_saved(file.path(other, ".")), c("bash", "run_r"))
    expect_equal(ns$approvals_saved(proj), character())
    expect_equal(ns$approvals_saved(file.path(proj, "sub")), character())
    if (.Platform$OS.type != "windows") {
        alias <- file.path(root, "other-link")
        stopifnot(file.symlink(other, alias))
        expect_equal(ns$approvals_saved(alias), c("bash", "run_r"))
        unlink(alias)
    }
    expect_true(grepl("bash, run_r (kept in", ns$approvals_label(other),
                      fixed = TRUE))
    expect_false(dir.exists(file.path(other, ".corteza")))
    # A save reads, changes and writes the file under one lock, so two at
    # once each keep their answer (test_approvals.R runs 2 processes).
    # One that cannot get the lock in time saves nothing.
    lock <- paste0(store, ".lock")
    dir.create(lock)
    expect_false(ns$approval_save(other, "git_status", timeout = 0.2))
    expect_equal(ns$approvals_saved(other), c("bash", "run_r"))
    unlink(lock, recursive = TRUE)
    expect_true(ns$approval_save(other, "git_status", timeout = 0.2))
    expect_equal(ns$approvals_saved(other), c("bash", "git_status", "run_r"))
    expect_equal(list.files(dirname(store), pattern = "tmp$"), character())
    # A store that cannot be read gives none and is set aside, not
    # overwritten by the next answer.
    writeLines("{ not json", store)
    expect_equal(ns$approvals_saved(other), character())
    expect_true(file.exists(paste0(store, ".unreadable")))
    expect_true(ns$approval_save(other, "bash"))
    expect_equal(ns$approvals_saved(other), "bash")
    expect_equal(readLines(paste0(store, ".unreadable")), "{ not json")
    unlink(c(store, paste0(store, ".unreadable")))
    # A store that cannot be written saves nothing, and says so.
    blocker <- file.path(root, "not-a-dir")
    writeLines("x", blocker)
    Sys.setenv(R_USER_CONFIG_DIR = file.path(blocker, "config"))
    expect_false(ns$approval_save(other, "bash"))
    Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config-home"))
    expect_equal(ns$approvals_saved(other), character())

    # ---- The saved approvals, as the terminal's prompt reads them ----------
    # The callback is the installed command's own, with the read of the
    # terminal replaced. A project's approvals.json arrives with the
    # repository: it answers nothing, trusted or not. "Always" is saved
    # in the user's store for the directory.
    bin <- system.file("bin", "corteza", package = "corteza")
    if (nzchar(bin)) {
        wanted <- "cli_approval_cb"
        defs <- Filter(function(e) {
            is.call(e) && identical(e[[1L]], as.name("<-")) &&
                is.name(e[[2L]]) && as.character(e[[2L]]) %in% wanted
        }, as.list(parse(bin, keep.source = FALSE)))
        expect_equal(length(defs), length(wanted))
        cli <- new.env(parent = globalenv())
        for (def in defs) {
            eval(def, envir = cli)
        }
        cli$color <- list(yellow = "", reset = "", cyan = "", dim = "")
        prompts <- 0L
        replies <- character()
        cli$cli_read_line <- function(prompt_str = "> ") {
            prompts <<- prompts + 1L
            if (!length(replies)) {
                # Asked when no answer was expected: deny, and let the
                # count of prompts say so.
                return("3")
            }
            reply <- replies[[1L]]
            replies <<- replies[-1L]
            reply
        }
        calls <- list(bash = list(command = "git status"),
                      run_r = list(code = "1 + 1"))
        ask <- function(cb, tool, reply = character()) {
            replies <<- reply
            before <- prompts
            said <- utils::capture.output(ok <- cb(
                list(tool = tool, args = calls[[tool]]),
                list(approval = "ask", reason = "config: needs approval")))
            list(ok = ok, asked = prompts - before, said = said)
        }
        saved <- ns$project_approvals_path(proj)
        before <- readLines(saved)

        for (trusted in c(FALSE, TRUE)) {
            if (trusted) ns$project_trust_add(proj)
            # The project's '{"bash": true}' answers nothing.
            got <- ask(cli$cli_approval_cb(proj), "bash", "1")
            expect_true(got$ok)
            expect_equal(got$asked, 1L, info = trusted)
            expect_true(any(grepl("Allow always for this project", got$said,
                                  fixed = TRUE)), info = trusted)
            untrust()
        }
        cb <- cli$cli_approval_cb(proj)
        # Once is once.
        expect_equal(ask(cb, "bash", "1")$asked, 1L)
        # "Always" is saved for the directory, in the user's store: this
        # session and the next skip the prompt, and the project's file is
        # as it was.
        expect_equal(ask(cb, "bash", "2")$asked, 1L)
        got <- ask(cb, "bash")
        expect_true(got$ok)
        expect_equal(got$asked, 0L)
        expect_equal(ns$approvals_saved(proj), "bash")
        expect_equal(ask(cli$cli_approval_cb(proj), "bash")$asked, 0L)
        expect_equal(ask(cb, "run_r", "1")$asked, 1L)
        # The credential rule's ask is answered each time: the session's
        # "always" for the tool does not answer it, and its prompt
        # offers none. A "2" there counts as once and is not kept.
        key <- function(cb, reply, tool = "bash") {
            replies <<- reply
            before_n <- prompts
            said <- utils::capture.output(ok <- cb(
                list(tool = tool, args = calls[[tool]]),
                list(approval = "ask", rule = "credential",
                     reason = "safety: ~/.ssh/id_rsa is a credential path")))
            list(ok = ok, asked = prompts - before_n, said = said)
        }
        got <- key(cb, "1")
        expect_true(got$ok)
        expect_equal(got$asked, 1L)
        expect_false(any(grepl("Allow always", got$said, fixed = TRUE)))
        got <- key(cb, "2", "run_r")
        expect_true(got$ok)
        expect_equal(got$asked, 1L)
        expect_equal(key(cb, "1", "run_r")$asked, 1L)
        expect_equal(ask(cb, "run_r", "1")$asked, 1L)
        expect_equal(ns$approvals_saved(proj), "bash")
        # Nor does the saved one in a new session.
        expect_equal(key(cli$cli_approval_cb(proj), "1")$asked, 1L)
        expect_equal(ask(cb, "bash")$asked, 0L)
        expect_equal(readLines(saved), before)
        # Leaving plan mode is asked the same way, each time: the prompt
        # shows the plan and offers no "always", a "2" counts as once,
        # and an "always" the tool has does not answer it.
        plan <- "1. Edit a.R\n2. Run the tests"
        leave <- function(cb, reply) {
            replies <<- reply
            before_n <- prompts
            said <- utils::capture.output(ok <- cb(
                list(tool = "exit_plan_mode", args = list(plan = plan)),
                ns$check_plan_exit(list(tool = "exit_plan_mode",
                                        context = list(plan_mode = TRUE)))))
            list(ok = ok, asked = prompts - before_n, said = said)
        }
        got <- leave(cb, "1")
        expect_true(got$ok)
        expect_equal(got$asked, 1L)
        expect_true(any(grepl("Run the tests", got$said, fixed = TRUE)))
        expect_false(any(grepl("Allow always", got$said, fixed = TRUE)))
        got <- leave(cb, "2")
        expect_true(got$ok)
        # A "3" denies as for any call: it stops the turn, and plan mode
        # stays, since the tool did not run.
        denied <- tryCatch(leave(cb, "3"), corteza_user_deny = function(e) e)
        expect_true(inherits(denied, "corteza_user_deny"))
        expect_equal(ns$approvals_saved(proj), "bash")
        expect_false(isTRUE(attr(cb, "answered")(list(tool = "bash"),
            list(approval = "ask", rule = "plan_exit"))))
        # Where the project has no folder, "always" makes none.
        cb <- cli$cli_approval_cb(other)
        expect_equal(ask(cb, "run_r", "2")$asked, 1L)
        expect_equal(ask(cb, "run_r")$asked, 0L)
        expect_false(dir.exists(file.path(other, ".corteza")))
        # An answer that cannot be saved holds for this session, and the
        # person is told.
        Sys.setenv(R_USER_CONFIG_DIR = file.path(blocker, "config"))
        cb <- cli$cli_approval_cb(proj)
        got <- ask(cb, "run_r", "2")
        expect_equal(got$asked, 1L)
        expect_true(any(grepl("Could not save it", got$said, fixed = TRUE)))
        expect_equal(ask(cb, "run_r")$asked, 0L)
        expect_equal(ask(cli$cli_approval_cb(proj), "run_r", "1")$asked, 1L)
        Sys.setenv(R_USER_CONFIG_DIR = file.path(root, "config-home"))
        unlink(ns$approvals_file())
    }

    # ---- The command's entry point -----------------------------------------
    # The last thing the installed command does: ask about the directory,
    # then read its options, which take their defaults from the config,
    # and start. Run here as written, with the 2 calls after the question
    # replaced and the arguments and the terminal answered for.
    if (nzchar(bin)) {
        entry <- Filter(function(e) {
            is.call(e) && identical(e[[1L]], as.name("if")) &&
                identical(e[[2L]], quote(!interactive()))
        }, as.list(parse(bin, keep.source = FALSE)))
        expect_equal(length(entry), 1L)
        start <- function(args, tty, reply = "") {
            run <- new.env(parent = globalenv())
            run$commandArgs <- function(...) args
            run$getwd <- function() proj
            run$isatty <- function(con) tty
            run$cli_read_line <- function(prompt_str = "> ") {
                cat(prompt_str, "\n")
                reply
            }
            run$parse_args <- function() {
                cat("OPTIONS-READ trusted:", ns$project_trusted(proj), "\n")
                list()
            }
            run$run_agent <- function(opts) cat("STARTED\n")
            utils::capture.output(eval(entry[[1L]][[3L]], envir = run))
        }
        at <- function(said, text) which(grepl(text, said, fixed = TRUE))[1L]
        # Nobody at a terminal: it says what it leaves alone, asks
        # nothing, and goes on untrusted.
        said <- start(character(), tty = FALSE, reply = "y")
        expect_true(isTRUE(at(said, "Not trusted: ") <
                           at(said, "OPTIONS-READ trusted: FALSE")))
        expect_true(isTRUE(at(said, "OPTIONS-READ") < at(said, "STARTED")))
        expect_false(any(grepl("[y/N]", said, fixed = TRUE)))
        expect_false(ns$project_trusted(proj))
        # A person who says no.
        said <- start(c("--model", "x"), tty = TRUE, reply = "n")
        expect_true(any(grepl("[y/N]", said, fixed = TRUE)))
        expect_true(any(grepl("OPTIONS-READ trusted: FALSE", said, fixed = TRUE)))
        # --help and --list load nothing of the project's, so they ask
        # nothing, a person there or not.
        for (flag in c("--help", "-h", "--list")) {
            said <- start(flag, tty = TRUE, reply = "y")
            expect_equal(trimws(said), c("OPTIONS-READ trusted: FALSE", "STARTED"),
                         info = flag)
        }
        expect_false(ns$project_trusted(proj))
        # A person who says yes: the directory is trusted by the time the
        # options, and with them the project's config, are read.
        said <- start(character(), tty = TRUE, reply = "y")
        expect_true(isTRUE(at(said, "Trusted. Saved in") <
                           at(said, "OPTIONS-READ trusted: TRUE")))
        expect_true(ns$project_trusted(proj))
        # Trusted: nothing is said or asked.
        said <- start(character(), tty = TRUE, reply = "n")
        expect_equal(trimws(said), c("OPTIONS-READ trusted: TRUE", "STARTED"))
        untrust()
    }
})

# ---- No reader of a project's control folder outside trust ----------------
# Every function of the installed namespace, and of the terminal command,
# is searched for a string that names the folder. Only the functions
# below may: the 4 that build its paths, and the rules that refuse to
# touch it. A new reader fails here until it goes through one of the 4.
local({
    ns <- asNamespace("corteza")
    strings_of <- function(x) {
        if (is.character(x)) {
            return(x)
        }
        if (is.function(x)) {
            return(c(strings_of(formals(x)), strings_of(body(x))))
        }
        if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
            return(unlist(lapply(as.list(x), strings_of), use.names = FALSE))
        }
        character()
    }
    # The folder as a part of a path: at the start of the string or
    # after a separator, and ending there or going on into the folder.
    # A sentence that mentions ".corteza/config.json" has a space or a
    # bracket before it, and a field named ".corteza_type" goes on.
    names_folder <- function(x) {
        any(grepl("(^|[/\\\\])\\.corteza($|[/(])", strings_of(x)))
    }
    expect_true(names_folder(quote(file.path(cwd, ".corteza", "config.json"))))
    expect_true(names_folder(quote(paste0(cwd, "/.corteza/skills"))))
    expect_true(names_folder(quote(function(d = "~/.corteza") d)))
    expect_false(names_folder(quote(stop("set it in .corteza/config.json or"))))
    expect_false(names_folder(quote(x[[".corteza_type"]])))
    fns <- Filter(function(n) is.function(get(n, ns)), ls(ns, all.names = TRUE))
    naming <- sort(fns[vapply(fns, function(n) names_folder(get(n, ns)), NA)])
    builders <- c("project_approvals_path", "project_config_path",
                  "project_harness_path", "project_skills_dir")
    # The room file sender and the policy scanner also recognize this directory.
    rules <- c(".room_media_send", "r_code_scan", "scan_sensitive", "scan_zone")
    expect_equal(naming, sort(c(builders, rules)))

    # Who calls each builder. The reads are in the first of each line.
    callers <- function(builder) {
        sort(fns[vapply(fns, function(n) {
            builder %in% all.names(body(get(n, ns)))
        }, NA)])
    }
    expect_equal(callers("project_config_path"),
                 sort(c("project_config", "context_config_origin",
                        "instruction_session_config", "project_offers")))
    expect_equal(callers("project_skills_dir"),
                 sort(c("load_project_skills", "instruction_session_config",
                        "project_offers")))
    expect_equal(callers("project_harness_path"),
                 sort(c("harness_path", "project_offers")))
    # Only to report that the file is there: nothing reads it.
    expect_equal(callers("project_approvals_path"),
                 sort(c("approvals_label", "approvals_project_notice")))
    # And each read is behind the decision.
    for (reader in c("project_config", "load_project_skills", "harness_path")) {
        expect_true("project_trusted" %in% all.names(body(get(reader, ns))))
    }

    # The terminal command builds no path into the folder itself.
    bin <- system.file("bin", "corteza", package = "corteza")
    if (nzchar(bin)) {
        code <- parse(bin, keep.source = FALSE)
        expect_false(names_folder(code))
        expect_false("project_approvals_path" %in% all.names(code))
        expect_true(all(c("approvals_saved", "approval_save",
                          "approvals_project_notice") %in% all.names(code)))
        expect_true("trust_at_start" %in% all.names(code))
        # It builds its session by hand, so it records how the session
        # started itself: /status reads that.
        expect_true(any(grepl("turn_session$project_trusted <- ", deparse(code),
                              fixed = TRUE)))
    }
    # chat() cannot be run here: it asks as the command does, before it
    # reads anything, and every other session is built by session_setup().
    expect_true("trust_at_start" %in% all.names(body(ns$chat)))
    expect_true("project_trusted" %in% all.names(body(ns$session_setup)))
})
