# Async query/collect — pure-function checks.
# These exercise the registry-state guards (busy, no-pending,
# unknown-id) without spinning up a callr child. The full async
# round-trip is covered in test_subagent_callr.R (at_home-gated).

reg <- corteza:::.subagent_registry

# Snapshot + restore so we don't leak stub entries across tests.
prior <- as.list(reg)
on.exit({
    rm(list = ls(reg), envir = reg)
    for (nm in names(prior)) reg[[nm]] <- prior[[nm]]
}, add = TRUE)
rm(list = ls(reg), envir = reg)

# Stub a registry entry with no real callr session. Guard paths
# should fire before any session method is reached.
stub_id <- "stub-12345678"
reg[[stub_id]] <- list(
    id = stub_id,
    seq = 1L,
    task = "stub",
    started_at = Sys.time(),
    timeout = Sys.time() + 600,
    pending = NULL,
    pending_started_at = NULL,
    session = NULL
)

# subagent_collect on idle agent → error "No pending query".
err <- tryCatch(corteza::subagent_collect(stub_id),
                error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("No pending query", conditionMessage(err)))

# Now flip the stub to busy and verify subagent_query(wait=FALSE)
# refuses to stack a second call.
reg[[stub_id]]$pending <- "in-flight prompt"
reg[[stub_id]]$pending_started_at <- Sys.time()

err <- tryCatch(
    corteza::subagent_query(stub_id, "second prompt", wait = FALSE),
    error = function(e) e
)
expect_inherits(err, "error")
expect_true(grepl("is busy with", conditionMessage(err)))

# Unknown id: both surfaces raise.
err <- tryCatch(corteza::subagent_collect("does-not-exist"),
                error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("Subagent not found", conditionMessage(err)))

err <- tryCatch(
    corteza::subagent_query("does-not-exist", "x", wait = FALSE),
    error = function(e) e
)
expect_inherits(err, "error")
expect_true(grepl("Subagent not found", conditionMessage(err)))

# format_subagent_list distinguishes idle vs busy.
reg[[stub_id]]$pending <- NULL
idle_listing <- corteza:::format_subagent_list(corteza::subagent_list())
expect_true(grepl("idle", idle_listing))

reg[[stub_id]]$pending <- "checking the deploy log"
busy_listing <- corteza:::format_subagent_list(corteza::subagent_list())
expect_true(grepl("busy:", busy_listing))
expect_true(grepl("checking the deploy log", busy_listing))
