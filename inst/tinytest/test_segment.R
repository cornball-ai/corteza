library(tinytest)

# /clear-time segmentation: title derivation, vault pointer form, and
# the segment room a cleared conversation becomes. The loopback adapter
# is the transport double: it supports channel_create and set_state,
# which is exactly the pair segmentation is gated on.

if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}
if (!isTRUE(chat.api::chat_capabilities(chat.api::chat_loopback())$set_state)) {
    exit_file("chat.api too old: loopback has no set_state")
}

fake_session <- function(transcript) {
    s <- new.env(parent = emptyenv())
    s$transcript <- transcript
    s
}

# ---- Title ----
# The first human line names the segment; assistant chatter and blank
# user turns do not.
local({
    s <- fake_session(list(
        list(role = "assistant", content = "Morning briefing text."),
        list(role = "user", content = ""),
        list(role = "user", content = "Fix the fold formula\nand more"),
        list(role = "user", content = "second topic")
    ))
    title <- corteza:::bot_segment_title(s)
    expect_true(startsWith(title, "Fix the fold formula ("))
    # The date suffix makes recurring topics tell apart in a room list.
    expect_true(grepl(format(Sys.Date()), title, fixed = TRUE))
})

# Truncation is the sanitizer's, applied to the line and not the date.
local({
    long <- paste(rep("word", 40L), collapse = " ")
    s <- fake_session(list(list(role = "user", content = long)))
    title <- corteza:::bot_segment_title(s, max_chars = 20L)
    expect_true(nchar(title) < nchar(long))
    expect_true(grepl(format(Sys.Date()), title, fixed = TRUE))
})

# No human line at all still names the segment something.
local({
    title <- corteza:::bot_segment_title(fake_session(list()))
    expect_true(is.character(title) && nzchar(title))
})

# ---- Commands are not what the user said ----
# The fork auto-prepends a bare "/clear" as its own message, so a
# conversation opening with one was titled "/clear (2026-08-20)": the
# tool's own vocabulary, on a permanent room, saying nothing about what
# is inside it.
local({
    s <- fake_session(list(
        list(role = "user", content = "/clear"),
        list(role = "user", content = "How do I proof sourdough?")
    ))
    title <- corteza:::bot_segment_title(s)
    expect_true(startsWith(title, "How do I proof sourdough? ("))
    expect_false(grepl("/clear", title, fixed = TRUE))
})

# A command with arguments is still a command.
expect_true(corteza:::.bot_is_command_line("/model sonnet"))
expect_true(corteza:::.bot_is_command_line("/clear"))
# A path is not, and neither is arithmetic or a bare slash. Listed
# because a rule that ate these would silently drop real content: the
# title falls back and the segment stops being worth keeping.
expect_false(corteza:::.bot_is_command_line("/home/troy/cerebro"))
expect_false(corteza:::.bot_is_command_line("/ hello"))
expect_false(corteza:::.bot_is_command_line("and/or"))
expect_false(corteza:::.bot_is_command_line("/"))

# ---- Worth keeping ----
# A /clear straight after a /clear ends a conversation in which the user
# said nothing. It archives fine, and used to become a permanent room
# named after the command that ended it.
expect_false(corteza:::bot_segment_worth_keeping(
    fake_session(list(list(role = "user", content = "/clear")))))
expect_false(corteza:::bot_segment_worth_keeping(fake_session(list())))
# Assistant output alone is not the user saying something either: a
# scheduled briefing posted into the room and never answered.
expect_false(corteza:::bot_segment_worth_keeping(
    fake_session(list(list(role = "assistant", content = "Briefing.")))))
# One real line is enough. The bar is "said anything", not a turn count:
# a threshold would be a number nobody chose.
expect_true(corteza:::bot_segment_worth_keeping(
    fake_session(list(list(role = "user", content = "/clear"),
                      list(role = "user", content = "thanks")))))

# ---- Vault pointer ----
expect_null(corteza:::bot_vault_ref(NULL))
expect_null(corteza:::bot_vault_ref(""))
# A path outside any vault passes through unchanged.
local({
    p <- file.path(tempdir(), "not-a-vault", "x.md")
    expect_identical(corteza:::bot_vault_ref(p),
                     normalizePath(p, mustWork = FALSE))
})
# A path under the pensar vault comes back vault-relative, which is the
# form that survives the vault moving hosts.
if (requireNamespace("pensar", quietly = TRUE)) {
    root <- tryCatch(pensar::default_vault(), error = function(e) NULL)
    if (!is.null(root) && nzchar(root)) {
        p <- file.path(normalizePath(root, mustWork = FALSE),
                       "raw", "matrix", "seg.md")
        expect_identical(corteza:::bot_vault_ref(p),
                         file.path("raw", "matrix", "seg.md"))
    }
}

# ---- The segment room ----
local({
    lo <- chat.api::chat_loopback()
    seg <- corteza:::bot_segment_from_clear(lo, "home", "Fold formula (2026-08-19)",
                                            "raw/matrix/seg.md")
    expect_identical(seg$id, "Fold formula (2026-08-19)")
    expect_identical(seg$name, "Fold formula (2026-08-19)")
    # The summary went to the new room as a notice carrying the pointer.
    msg <- chat.api::chat_poll(lo)$messages[[1L]]
    expect_identical(msg$channel, seg$id)
    expect_identical(msg$kind, "notice")
    expect_true(grepl("raw/matrix/seg.md", msg$body, fixed = TRUE))
    # And the lifecycle state event marks it a segment of the home room.
    key <- paste(seg$id, "ai.cornball.lifecycle", "", sep = "\r")
    st <- lo$env$state[[key]]
    expect_identical(st$state, "segment")
    expect_identical(st$segment_of, "home")
    expect_identical(st$vault, "raw/matrix/seg.md")
    expect_true(is.character(st$since) && nzchar(st$since))
})

# No archive pointer: the state event and summary simply omit it.
local({
    lo <- chat.api::chat_loopback()
    seg <- corteza:::bot_segment_from_clear(lo, "home", "Untitled (x)", NULL)
    key <- paste(seg$id, "ai.cornball.lifecycle", "", sep = "\r")
    expect_false("vault" %in% names(lo$env$state[[key]]))
    expect_false(grepl("Archived transcript",
                       chat.api::chat_poll(lo)$messages[[1L]]$body,
                       fixed = TRUE))
})

# A transport that cannot write state skips with a message rather than
# erroring: archive-and-reset is the critical path, not the segment.
local({
    irc <- structure(list(env = new.env(parent = emptyenv()), nick = "bot"),
                     class = c("chat_irc", "chat_client"))
    expect_false(isTRUE(chat.api::chat_capabilities(irc)$set_state))
    expect_message(res <- corteza:::bot_segment_from_clear(irc, "#lab",
                                                           "t", NULL),
                   "skipping")
    expect_null(res)
})
