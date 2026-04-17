library(tinytest)

expect_true(is.function(llamaR::matrix_configure))
expect_true(is.function(llamaR::matrix_send))
expect_true(is.function(llamaR::matrix_poll))
expect_true(is.function(llamaR::matrix_run))

# Config persistence round-trip (no network, isolated HOME).
local({
  orig_home <- Sys.getenv("HOME")
  tmp_home <- tempfile("home-")
  dir.create(tmp_home)
  Sys.setenv(HOME = tmp_home)
  on.exit({
    Sys.setenv(HOME = orig_home)
    unlink(tmp_home, recursive = TRUE)
  }, add = TRUE)

  cfg <- list(
    server = "https://example",
    user = "bot",
    password = "pw",
    token = "tok",
    user_id = "@bot:example",
    device_id = "DEV",
    room_id = "!abc:example",
    sync_token = NULL
  )
  llamaR:::matrix_save_config(cfg)
  loaded <- llamaR:::matrix_load_config()
  expect_equal(loaded$user_id, "@bot:example")
  expect_equal(loaded$room_id, "!abc:example")
  expect_equal(file.mode(llamaR:::matrix_config_path()),
               as.octmode("0600"))
})

# matrix_new_session wires config into a turn session.
local({
  cfg <- list(
    server = "https://example",
    user = "bot",
    user_id = "@bot:example",
    room_id = "!abc:example",
    model = "kimi-k2.5",
    provider = "moonshot",
    tools_filter = NULL,
    auto_approve_asks = FALSE
  )
  s <- llamaR:::matrix_new_session(cfg)
  expect_true(is.environment(s))
  expect_equal(s$channel, "matrix")
  expect_equal(s$provider, "moonshot")
  expect_equal(s$model_map$cloud, "kimi-k2.5")
  # Default approval_cb declines (auto_approve_asks = FALSE)
  expect_false(s$approval_cb(list(), list()))

  cfg$auto_approve_asks <- TRUE
  s2 <- llamaR:::matrix_new_session(cfg)
  expect_true(s2$approval_cb(list(), list()))
})

if (at_home() && nzchar(Sys.getenv("MX_TEST_SERVER"))) {
  # Live round-trip would configure, send, and poll here. Skipped in
  # package check.
}
