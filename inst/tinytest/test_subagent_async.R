# Async query/collect — pure-function checks.
# These exercise the registry-state guards (busy, no-pending,
# unknown-id) without spinning up a callr child. The full async
# round-trip is covered in test_subagent_callr.R (at_home-gated).

reg <- corteza:::.subagent_registry

# Snapshot any prior registry state, then start clean. We restore at
# the bottom rather than via on.exit() because top-level on.exit in
# tinytest files attaches to the global frame and isn't reliable.
prior <- as.list(reg)
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

# Flip the stub to busy and verify both wait paths refuse to stack a
# second call. The session=NULL stub means any call past the busy
# guard would NPE on info$session$..., so reaching the guard message
# is itself proof that the guard fires before the session is touched.
reg[[stub_id]]$pending <- "in-flight prompt"
reg[[stub_id]]$pending_started_at <- Sys.time()

err <- tryCatch(
    corteza::subagent_query(stub_id, "second prompt", wait = FALSE),
    error = function(e) e
)
expect_inherits(err, "error")
expect_true(grepl("is busy with", conditionMessage(err)))

# Same guard must apply to the sync path: r_session can only carry one
# in-flight call.
err <- tryCatch(
    corteza::subagent_query(stub_id, "second prompt", wait = TRUE),
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

# Cleanup: drop stub, restore prior entries.
rm(list = ls(reg), envir = reg)
for (nm in names(prior)) reg[[nm]] <- prior[[nm]]
