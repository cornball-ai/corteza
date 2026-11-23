# Exercise the three persistent worker constructors with a fresh process each.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("worker startup integration runs at home and in CI")
}
source("isolate.R", local = TRUE)
Sys.setenv(R_USER_CONFIG_DIR = tempfile("worker-config"))

local({
    project <- tempfile("worker-project")
    dir.create(project)
    previous <- setwd(project)
    on.exit({
        setwd(previous)
        unlink(project, recursive = TRUE)
    }, add = TRUE)
    writeLines("options(corteza.test_project_profile = TRUE)", ".Rprofile")
    profile_loaded <- function() isTRUE(getOption("corteza.test_project_profile"))
    expect_false(corteza:::project_trusted(project))

    s <- corteza::new_session("cli", model_map = list(cloud = "test-model"))
    s$cwd <- project
    s$config <- list()
    s$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE))

    run_worker <- corteza:::.run_r_worker(s)
    on.exit(corteza:::.run_r_worker_close(s), add = TRUE, after = FALSE)
    expect_false(run_worker$run(profile_loaded))

    job <- corteza:::job_worker_process(s, "doer")
    on.exit(job$worker$close(), add = TRUE, after = FALSE)
    expect_false(job$worker$run(profile_loaded))

    id <- corteza::subagent_spawn("Check startup", parent_session = s, config = s$config)
    on.exit(corteza::subagent_kill(id), add = TRUE, after = FALSE)
    child <- corteza:::.subagent_registry[[id]]$session
    expect_false(child$run(profile_loaded))

    # The same fixture is observable when a host explicitly enables it.
    corteza:::.run_r_worker_close(s)
    s$config$run_r_worker_options <- list(user_profile = "project")
    opted_in <- corteza:::.run_r_worker(s)
    expect_true(opted_in$run(profile_loaded))
})
