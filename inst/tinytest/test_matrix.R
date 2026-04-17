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

# matrix_extract_messages walks all joined rooms.
local({
  sync <- list(rooms = list(join = list(
    `!dm:ex` = list(timeline = list(events = list(
      list(type = "m.room.message", sender = "@troy:ex",
           event_id = "$e1",
           content = list(msgtype = "m.text", body = "hi from dm"))
    ))),
    `!vault:ex` = list(timeline = list(events = list(
      list(type = "m.room.message", sender = "@troy:ex",
           event_id = "$e2",
           content = list(msgtype = "m.text", body = "hi from vault")),
      list(type = "m.room.message", sender = "@bot:ex",
           event_id = "$e3",
           content = list(msgtype = "m.text", body = "bot echo"))
    )))
  )))
  out <- llamaR:::matrix_extract_messages(sync, "@bot:ex")
  expect_equal(length(out), 2L)
  rooms <- vapply(out, function(m) m$room_id, character(1L))
  expect_true(all(c("!dm:ex", "!vault:ex") %in% rooms))
  # Bot's own echo is filtered out.
  bodies <- vapply(out, function(m) m$body, character(1L))
  expect_false("bot echo" %in% bodies)
})

# matrix_extract_invites returns the room IDs pending acceptance.
local({
  sync <- list(rooms = list(invite = list(
    `!newroom:ex` = list(invite_state = list()),
    `!another:ex` = list(invite_state = list())
  )))
  invites <- llamaR:::matrix_extract_invites(sync)
  expect_equal(sort(invites), c("!another:ex", "!newroom:ex"))

  # No pending invites -> character(0)
  expect_equal(
    llamaR:::matrix_extract_invites(list(rooms = list(join = list()))),
    character()
  )
})

# The session registry hands out the same session for the same room.
local({
  cfg <- list(server = "https://example", user = "bot",
              user_id = "@bot:ex", room_id = "!dm:ex",
              model = "kimi-k2.5", provider = "moonshot",
              tools_filter = NULL, auto_approve_asks = TRUE)
  reg <- llamaR:::matrix_new_session_registry()
  s1 <- llamaR:::matrix_get_or_create_session(reg, "!dm:ex", cfg)
  s2 <- llamaR:::matrix_get_or_create_session(reg, "!dm:ex", cfg)
  expect_identical(s1, s2)
  s3 <- llamaR:::matrix_get_or_create_session(reg, "!vault:ex", cfg)
  expect_false(identical(s1, s3))
  expect_equal(s1$room_id, "!dm:ex")
  expect_equal(s3$room_id, "!vault:ex")
})

if (at_home() && nzchar(Sys.getenv("MX_TEST_SERVER"))) {
  # Live round-trip would configure, send, and poll here. Skipped in
  # package check.
}
