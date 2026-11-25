library(tinytest)

# A bot answers to its operators. Up to 0.8.0 a bot with none accepted
# every invite, and bot_configure() had no way to name one: the
# documented setup made an open bot. The invite gate itself is tested in
# test_rooms.R, beside the reply gate.

# --- The argument as it is saved ---
local({
    check <- corteza:::bot_check_operators
    expect_null(check(NULL))
    expect_null(check(character()))
    expect_identical(check("@troy:example.org"), "@troy:example.org")
    # Named twice is named once.
    expect_identical(check(c("@a:ex", "@b:ex", "@a:ex")), c("@a:ex", "@b:ex"))
    # A list, as a config read from JSON holds them.
    expect_identical(check(list("@a:ex", "@b:ex")), c("@a:ex", "@b:ex"))

    # A name without its server matches nobody. It is refused here, and
    # the error says which one.
    expect_error(check("troy"), "full Matrix IDs")
    expect_error(check(c("@ok:ex", "troy", "@also")), "troy, @also")
    expect_error(check("@troy"), "full Matrix IDs")
    expect_error(check(""), "full Matrix IDs")
    expect_error(check(NA_character_), "full Matrix IDs")
})

# --- bot_configure() checks it before anything asks for the transport ---
# So these need no chat.api and reach no server.
local({
    expect_error(
        corteza::bot_configure("https://ex.invalid", "bot", "pw", "!r:ex",
                               operators = "troy"),
        "operators must be full Matrix IDs")
    expect_error(suppressWarnings(
        corteza::matrix_configure("https://ex.invalid", "bot", "pw", "!r:ex",
                                  operators = "troy")),
        "operators must be full Matrix IDs")

    # After `cwd`, which is after everything 0.8.0 had: a call written
    # by position against 0.8.0 still matches. Only `project_roots`
    # comes after it (test_room_binding.R).
    for (name in c("bot_configure", "matrix_configure")) {
        f <- formals(getExportedValue("corteza", name))
        expect_identical(utils::tail(names(f), 3L),
                         c("cwd", "operators", "project_roots"), info = name)
        expect_null(f$operators, info = name)
    }
})

# --- What bot_configure() saves and says ---
# The transport's configure is replaced, so nothing logs in and nothing
# is written.
if (requireNamespace("chat.api", quietly = TRUE) &&
    requireNamespace("mx.client", quietly = TRUE)) local({
    real <- get("chat_matrix_configure", asNamespace("chat.api"))
    on.exit(assignInNamespace("chat_matrix_configure", real, "chat.api"))
    calls <- list()
    assignInNamespace("chat_matrix_configure",
                      function(server, user, password, room, ...) {
        calls[[length(calls) + 1L]] <<- list(...)
        list(user_id = "@bot:example.org", room_id = room)
    }, "chat.api")
    configure <- function(...) {
        said <- capture.output(
            cfg <- corteza::bot_configure("https://ex.invalid", "bot", "pw",
                                          "!r:example.org", ...),
            type = "message")
        list(cfg = cfg, said = said, extra = calls[[length(calls)]]$extra)
    }

    # Operators are saved as given, once each, and nothing more is said.
    got <- configure(operators = c("@troy:example.org", "@troy:example.org",
                                   "@ana:example.org"))
    expect_identical(got$extra$operators,
                     c("@troy:example.org", "@ana:example.org"))
    expect_equal(length(got$said), 1L)
    expect_true(grepl("Configured @bot:example.org", got$said, fixed = TRUE))

    # None given: none is saved, and the person is told what that means
    # while they can still do something about it.
    got <- configure()
    expect_null(got$extra$operators)
    note <- got$said[-1L]
    expect_equal(length(note), 1L)
    expect_true(any(grepl("no operators", note, fixed = TRUE)))
    expect_true(any(grepl("accepts no invites", note, fixed = TRUE)))
    expect_true(any(grepl("bot_configure(operators = ", note, fixed = TRUE)))

    # The deprecated name forwards it.
    before <- length(calls)
    suppressMessages(suppressWarnings(
        corteza::matrix_configure("https://ex.invalid", "bot", "pw",
                                  "!r:example.org",
                                  operators = "@troy:example.org")))
    expect_equal(length(calls), before + 1L)
    expect_identical(calls[[length(calls)]]$extra$operators,
                     "@troy:example.org")
})

# --- What a bot with no operators still does ---
# A room it is already in works as it did: its one human is answered and
# answers the approval prompts.
local({
    none <- corteza:::bot_operators(list())
    expect_identical(none, character())
    solo <- c("@bot:ex", "@ana:ex")
    expect_true(corteza:::bot_should_respond(
        list(body = "hi", sender = "@ana:ex"), "@bot:ex", solo,
        bots = "@bot:ex", operators = none))
    expect_identical(corteza:::bot_approvers(list(), solo, bots = "@bot:ex"),
                     "@ana:ex")
    # And nobody from outside can add it to another.
    expect_identical(suppressMessages(corteza:::bot_allowed_invites(
        list(list(channel = "!new:ex", inviter = "@ana:ex")), none)),
        character())
})
