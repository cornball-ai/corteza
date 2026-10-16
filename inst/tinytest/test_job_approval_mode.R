if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("needs chat.api")
local({
    ns <- asNamespace("corteza")
    root <- tempfile("approval-mode-")
    dir.create(root)
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = root)
    on.exit({
        if (is.na(old)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- list(server = "https://example.invalid", user_id = "@bot:ex", token = "fixture-token", device_id = "FIXTURE",
                operators = "@operator:ex", auto_approve_asks = TRUE)
    members <- c("@bot:ex", "@operator:ex")
    chat <- ns$bot_chat_client(cfg, save_cursor = FALSE,
        .members = function(...) members, .send = function(...) "$sent")
    s <- corteza::new_session("matrix")
    s$job_key <- s$room_id <- "!room:ex"
    s$job_owner <- "@bot:ex"
    s$config <- list()
    create <- function() ns$job_create("Run fixture checks", workspace = root,
        owner = s$job_owner, origin = list(session_key = s$job_key, room = s$room_id))
    id <- create()
    j <- ns$job_read(id)
    request <- function() ns$job_approval_request(id,
        list(tool = "bash", args = list(command = "fixture command")),
        list(reason = "outside project"), extra = list(route = "human"))
    req <- request()
    cmd <- ns$bot_parse_approval_mode("/approve-all")
    expect_identical(cmd$mode, "on")
    expect_null(ns$bot_parse_approval_mode("approve all the files in this essay"))
    expect_identical(ns$bot_parse_approval_mode(paste("/auto off", id))$job, id)
    expect_true(grepl("Only a configured operator", ns$bot_approval_mode_command(
        chat, cfg, s, cmd, "@stranger:ex"), fixed = TRUE))
    expect_null(ns$bot_job_auto_grant(chat, cfg, s, j))
    ack <- ns$bot_approval_mode_command(chat, cfg, s, cmd, "@operator:ex")
    expect_true(grepl("until this job ends", ack, fixed = TRUE))
    answer <- ns$job_read_file(ns$job_approval_path(id, req, "answer"))
    expect_true(answer$approved)
    expect_identical(answer$by, "@operator:ex")
    req2 <- request()
    ns$bot_job_approval(chat, cfg, s, list(job = j, request = list(id = req2)))
    expect_true(ns$job_read_file(ns$job_approval_path(id, req2, "answer"))$approved)
    expect_null(ns$bot_job_auto_grant(chat, utils::modifyList(cfg, list(operators = "@other:ex")), s, j))
    members <- "@bot:ex"
    expect_null(ns$bot_job_auto_grant(chat, cfg, s, j))
    members <- c("@bot:ex", "@operator:ex")
    s$job_owner <- "@another-bot:ex"
    expect_null(ns$bot_job_auto_grant(chat, cfg, s, j))
    s$job_owner <- "@bot:ex"
    ns$bot_approval_mode_command(chat, cfg, s, ns$bot_parse_approval_mode("/auto off"), "@operator:ex")
    expect_null(ns$bot_job_auto_grant(chat, cfg, s, j))
    id2 <- create()
    expect_true(grepl("Choose a job", ns$bot_approval_mode_command(chat, cfg, s, cmd, "@operator:ex"), fixed = TRUE))
    cmd$job <- id
    ns$bot_approval_mode_command(chat, cfg, s, cmd, "@operator:ex")
    expect_null(ns$bot_job_auto_grant(chat, cfg, s, ns$job_read(id2)))
    ns$job_settle(id, "done", result = "done")
    expect_null(ns$bot_job_auto_grant(chat, cfg, s, j))
    gate <- ns$supervisor_gate(function(...) stop("must not be asked"), cwd = root)
    expect_identical(gate(list(tool = "bash"), list(approval = "deny", reason = "fixture"))$action, "deny")
})
