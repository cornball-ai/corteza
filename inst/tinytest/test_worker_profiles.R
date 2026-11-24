# Exercise worker startup in a project with its own R profile.
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
    writeLines(c('file.create("profile-loaded")',
                 "options(corteza.test_project_profile = TRUE)"), ".Rprofile")
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

    # An unknown provider fails locally. The background archive runs the
    # foreground summarizer in another child, then appends its result.
    slice <- list(list(role = "user", content = "Archive this fixture."))
    archive_id <- basename(tempfile("archive"))
    transcript <- corteza:::session_transcript_path(archive_id, paste0("subagent-", archive_id))
    corteza:::archival_summarize_bg(archive_id, slice, "paragraph", "fixture-provider", "fixture")
    deadline <- Sys.time() + 30
    repeat {
        done <- file.exists(transcript) &&
            any(grepl("[summary unavailable]", readLines(transcript, warn = FALSE), fixed = TRUE))
        if (done || Sys.time() >= deadline) break
        Sys.sleep(0.05)
    }
    expect_true(done, info = "background archive recorded the foreground child's result")
    expect_false(file.exists("profile-loaded"), info = "archive workers skipped the profile")

    job_id <- corteza:::job_create("Check consultation startup", role = "consultation",
                                  backend = "consultation", workspace = project)
    snapshot <- list(args = list(prompt = "Fixture", provider = "fixture-provider"))
    saveRDS(snapshot, file.path(corteza:::job_dir(job_id), "consultation.rds"))
    consult <- corteza:::.room_consult_start(job_id)
    on.exit(if (consult$is_alive()) consult$kill(), add = TRUE, after = FALSE)
    consult$wait(30000)
    expect_false(consult$is_alive())
    expect_error(consult$get_result(), "anthropic")
    expect_false(file.exists("profile-loaded"), info = "consultation skipped the profile")

    # The same fixture is observable when a host explicitly enables it.
    corteza:::.run_r_worker_close(s)
    s$config$run_r_worker_options <- list(user_profile = "project")
    opted_in <- corteza:::.run_r_worker(s)
    expect_true(opted_in$run(profile_loaded))
    expect_true(file.exists("profile-loaded"))
})
