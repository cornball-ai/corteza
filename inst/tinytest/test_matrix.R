library(tinytest)

expect_true(is.function(corteza::matrix_configure))
expect_true(is.function(corteza::matrix_send))
expect_true(is.function(corteza::matrix_poll))
expect_true(is.function(corteza::matrix_run))

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
  corteza:::matrix_save_config(cfg)
  loaded <- corteza:::matrix_load_config()
  expect_equal(loaded$user_id, "@bot:example")
  expect_equal(loaded$room_id, "!abc:example")
  # POSIX file modes don't apply on Windows; skip there.
  if (.Platform$OS.type != "windows") {
    expect_equal(file.mode(corteza:::matrix_config_path()),
                 as.octmode("0600"))
  }
})

# matrix_new_session wires config into a turn session.
# Skipped in R CMD check: needs MOONSHOT_API_KEY via new_session().
if (at_home()) local({
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
  s <- corteza:::matrix_new_session(cfg)
  expect_true(is.environment(s))
  expect_equal(s$channel, "matrix")
  expect_equal(s$provider, "moonshot")
  expect_equal(s$model_map$cloud, "kimi-k2.5")
  # Default approval_cb declines (auto_approve_asks = FALSE)
  expect_false(s$approval_cb(list(), list()))

  cfg$auto_approve_asks <- TRUE
  s2 <- corteza:::matrix_new_session(cfg)
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
  out <- corteza:::matrix_extract_messages(sync, "@bot:ex")
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
  invites <- corteza:::matrix_extract_invites(sync)
  expect_equal(sort(invites), c("!another:ex", "!newroom:ex"))

  # No pending invites -> character(0)
  expect_equal(
    corteza:::matrix_extract_invites(list(rooms = list(join = list()))),
    character()
  )
})

# The session registry hands out the same session for the same room.
# Skipped in R CMD check: creates sessions that need MOONSHOT_API_KEY.
if (at_home()) local({
  cfg <- list(server = "https://example", user = "bot",
              user_id = "@bot:ex", room_id = "!dm:ex",
              model = "kimi-k2.5", provider = "moonshot",
              tools_filter = NULL, auto_approve_asks = TRUE)
  reg <- corteza:::matrix_new_session_registry()
  s1 <- corteza:::matrix_get_or_create_session(reg, "!dm:ex", cfg)
  s2 <- corteza:::matrix_get_or_create_session(reg, "!dm:ex", cfg)
  expect_identical(s1, s2)
  s3 <- corteza:::matrix_get_or_create_session(reg, "!vault:ex", cfg)
  expect_false(identical(s1, s3))
  expect_equal(s1$room_id, "!dm:ex")
  expect_equal(s3$room_id, "!vault:ex")
})

# Mention detection: explicit m.mentions takes precedence.
local({
  msg <- list(body = "hey",
              mentions = list("@cornelius:cornball.ai"))
  expect_true(corteza:::matrix_message_mentions_self(
    msg, "@cornelius:cornball.ai"))
  expect_false(corteza:::matrix_message_mentions_self(
    msg, "@other:cornball.ai"))
})

# Mention detection: fallback to @localpart substring.
local({
  expect_true(corteza:::matrix_message_mentions_self(
    list(body = "hey @cornelius what do you think?"),
    "@cornelius:cornball.ai"))
  expect_true(corteza:::matrix_message_mentions_self(
    list(body = "@CORNELIUS help"),
    "@cornelius:cornball.ai"))
  expect_true(corteza:::matrix_message_mentions_self(
    list(body = "please @cornelius:cornball.ai"),
    "@cornelius:cornball.ai"))
  # Bare "cornelius" without @ is not a mention.
  expect_false(corteza:::matrix_message_mentions_self(
    list(body = "cornelius is great"),
    "@cornelius:cornball.ai"))
  expect_false(corteza:::matrix_message_mentions_self(
    list(body = "hello world"),
    "@cornelius:cornball.ai"))
  expect_false(corteza:::matrix_message_mentions_self(
    list(body = ""), "@cornelius:cornball.ai"))
})

# matrix_should_respond: DM always -> TRUE regardless of mention.
local({
  s <- new.env(parent = emptyenv())
  s$is_dm <- TRUE
  expect_true(corteza:::matrix_should_respond(
    list(body = "hi"), s, "@cornelius:cornball.ai"))
})

# matrix_should_respond: group room requires mention.
local({
  s <- new.env(parent = emptyenv())
  s$is_dm <- FALSE
  expect_false(corteza:::matrix_should_respond(
    list(body = "chatter among humans"), s,
    "@cornelius:cornball.ai"))
  expect_true(corteza:::matrix_should_respond(
    list(body = "@cornelius what?"), s,
    "@cornelius:cornball.ai"))
})

# Agent name capitalization.
expect_equal(
  corteza:::matrix_agent_name(list(user_id = "@cornelius:cornball.ai")),
  "Cornelius"
)
expect_equal(
  corteza:::matrix_agent_name(list(user_id = "@cloptimus:example")),
  "Cloptimus"
)
expect_equal(
  corteza:::matrix_agent_name(list(user_id = "")),
  "agent"
)

# Topic parser.
expect_equal(
  corteza:::matrix_parse_topic("~/To_Do | todo management"),
  list(cwd = "~/To_Do", description = "todo management")
)
expect_equal(
  corteza:::matrix_parse_topic("/tmp/scratch | quick stuff"),
  list(cwd = "/tmp/scratch", description = "quick stuff")
)
expect_equal(
  corteza:::matrix_parse_topic("./relative | works"),
  list(cwd = "./relative", description = "works")
)
# Description-only topic (no leading path).
expect_equal(
  corteza:::matrix_parse_topic("Discussing the wiki contents"),
  list(cwd = NULL, description = "Discussing the wiki contents")
)
# Pipe without leading path — treated as description containing a pipe.
expect_equal(
  corteza:::matrix_parse_topic("a | b | c"),
  list(cwd = NULL, description = "a | b | c")
)
# Empty / NULL topic.
expect_equal(
  corteza:::matrix_parse_topic(NULL),
  list(cwd = NULL, description = NULL)
)
expect_equal(
  corteza:::matrix_parse_topic("   "),
  list(cwd = NULL, description = NULL)
)

if (at_home() && nzchar(Sys.getenv("MX_TEST_SERVER"))) {
  # Live round-trip would configure, send, and poll here. Skipped in
  # package check.
}

# matrix_session_to_markdown: format only the unseen tail.
local({
    s <- new.env(parent = emptyenv())
    s$history <- list(
        list(role = "user",      content = "hello"),
        list(role = "assistant", content = "hi back"),
        list(role = "user",      content = "and now this")
    )
    s$ingested_through <- 1L
    md <- corteza:::matrix_session_to_markdown(s, "!room:s.c", "Test Room")
    expect_true(grepl("# Test Room", md, fixed = TRUE))
    expect_false(grepl("hello", md, fixed = TRUE))      # already ingested
    expect_true(grepl("hi back", md, fixed = TRUE))
    expect_true(grepl("and now this", md, fixed = TRUE))

    # Nothing new -> NULL.
    s$ingested_through <- 3L
    expect_null(corteza:::matrix_session_to_markdown(s, "!room:s.c"))
})

# matrix_archive_session round-trip: ingest, dedupe, ingest tail only.
# Sessions in real use are environments, so test with environments.
if (requireNamespace("pensar", quietly = TRUE)) {
    local({
        v <- tempfile("vault-")
        on.exit(unlink(v, recursive = TRUE), add = TRUE)
        pensar::init_vault(v)
        op <- options(pensar.vault = v)
        on.exit(options(op), add = TRUE)

        s <- new.env(parent = emptyenv())
        s$history <- list(
            list(role = "user",      content = "first"),
            list(role = "assistant", content = "ok")
        )
        out1 <- corteza:::matrix_archive_session(s, "!t:s.c")
        expect_true(file.exists(out1))
        expect_equal(s$ingested_through, 2L)

        # No new turns -> no-op.
        out2 <- corteza:::matrix_archive_session(s, "!t:s.c")
        expect_null(out2)
        expect_equal(s$ingested_through, 2L)

        # New turn -> only that turn lands in the file.
        s$history[[3]] <- list(role = "user", content = "third")
        out3 <- corteza:::matrix_archive_session(s, "!t:s.c")
        body <- paste(readLines(out3), collapse = "\n")
        expect_false(grepl("first", body, fixed = TRUE))
        expect_true(grepl("third", body, fixed = TRUE))
        expect_equal(s$ingested_through, 3L)
    })
}

# matrix_archive_session: silent no-op when pensar isn't installed.
if (!requireNamespace("pensar", quietly = TRUE)) {
    s <- new.env(parent = emptyenv())
    s$history <- list(list(role = "user", content = "x"))
    out <- corteza:::matrix_archive_session(s, "!r:s.c")
    expect_null(out)
    expect_null(s$ingested_through)
}

# matrix_handle_flush_signal: no signal -> no-op.
local({
    sig <- tempfile("nosig-")
    expect_equal(corteza:::matrix_handle_flush_signal(sig, new.env()), 0L)
})

# matrix_handle_flush_signal: signal present -> flush + remove file.
if (requireNamespace("pensar", quietly = TRUE)) {
    local({
        v <- tempfile("vault-")
        on.exit(unlink(v, recursive = TRUE), add = TRUE)
        pensar::init_vault(v)
        op <- options(pensar.vault = v)
        on.exit(options(op), add = TRUE)

        sig <- tempfile("sig-")
        file.create(sig)
        on.exit(if (file.exists(sig)) file.remove(sig), add = TRUE)

        reg <- new.env(parent = emptyenv())
        s <- new.env(parent = emptyenv())
        s$history <- list(list(role = "user", content = "hi"))
        assign("!r:s.c", s, envir = reg)

        n <- corteza:::matrix_handle_flush_signal(sig, reg)
        expect_equal(n, 1L)
        expect_false(file.exists(sig))    # signal consumed
        expect_equal(s$ingested_through, 1L)
    })
}

# matrix_archive_all: walks the registry and counts archived rooms.
if (requireNamespace("pensar", quietly = TRUE)) {
    local({
        v <- tempfile("vault-")
        on.exit(unlink(v, recursive = TRUE), add = TRUE)
        pensar::init_vault(v)
        op <- options(pensar.vault = v)
        on.exit(options(op), add = TRUE)

        reg <- new.env(parent = emptyenv())
        s1 <- new.env(parent = emptyenv())
        s1$history <- list(list(role = "user", content = "a"))
        assign("!r1:s.c", s1, envir = reg)
        s2 <- new.env(parent = emptyenv())
        s2$history <- list(list(role = "user", content = "b"))
        assign("!r2:s.c", s2, envir = reg)

        expect_equal(corteza:::matrix_archive_all(reg), 2L)
        # Second flush is a no-op for both rooms.
        expect_equal(corteza:::matrix_archive_all(reg), 0L)
    })
}

# matrix_is_clear_command: recognize /clear, /reset, /new alone or
# after an @-mention; reject bare prose that happens to contain /clear.
expect_true(corteza:::matrix_is_clear_command("/clear"))
expect_true(corteza:::matrix_is_clear_command("  /clear  "))
expect_true(corteza:::matrix_is_clear_command("/reset"))
expect_true(corteza:::matrix_is_clear_command("/new"))
expect_true(corteza:::matrix_is_clear_command("@cornelius /clear"))
expect_true(corteza:::matrix_is_clear_command("@cornelius:s.c /reset"))
expect_false(corteza:::matrix_is_clear_command("/clear the room please"))
expect_false(corteza:::matrix_is_clear_command("don't /clear yet"))
expect_false(corteza:::matrix_is_clear_command("hello"))
expect_false(corteza:::matrix_is_clear_command(""))
expect_false(corteza:::matrix_is_clear_command(NULL))
