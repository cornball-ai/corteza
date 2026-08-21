# Images arriving in a room: which ones get sent, and what they become.
#
# The fetch is seamed through a fake chat client rather than mocked at
# the chat.api level, because the thing under test is the decision --
# which attachment is worth a download and what the model ends up
# holding -- not the transport.

if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}

png_bytes <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))

att <- function(id = "mxc://ex/abc", name = "IMG_0942.png",
                mime = "image/png", bytes = 4096L) {
    chat.api::chat_attachment(id = id, name = name, mime = mime,
                              bytes = bytes, url = id)
}

msg <- function(..., body = "look at this") {
    atts <- list(...)
    list(channel = "!room:ex", id = "$1", sender = "@alice:ex", body = body,
         attachments = if (length(atts)) atts else NULL)
}

# A chat client that answers chat_capabilities() and chat_download().
# S3 dispatch is on the class, so the methods below are the whole of
# what corteza can reach.
fake_chat <- function(attachments = TRUE, on_download = NULL) {
    structure(list(attachments = attachments,
                   on_download = on_download %||% function(a, dest) {
                       writeBin(png_bytes, dest)
                       dest
                   }),
              class = c("chat_fake", "chat_client"))
}
chat_capabilities.chat_fake <- function(client, ...) {
    list(attachments = isTRUE(client$attachments))
}
chat_download.chat_fake <- function(client, attachment, dest = NULL, ...) {
    dest <- dest %||% tempfile(fileext = ".png")
    client$on_download(attachment, dest)
}
# Registered into chat.api's namespace, where the generics live: a
# method defined only here is invisible to UseMethod() called from
# inside corteza.
registerS3method("chat_capabilities", "chat_fake", chat_capabilities.chat_fake,
                 envir = asNamespace("chat.api"))
registerS3method("chat_download", "chat_fake", chat_download.chat_fake,
                 envir = asNamespace("chat.api"))

# ---- Which provider gets pictures ----

expect_true(corteza:::bot_images_enabled("openai_codex"))
expect_true(corteza:::bot_images_enabled("anthropic"))
expect_true(corteza:::bot_images_enabled("anthropic_claude"))
expect_true(corteza:::bot_images_enabled("moonshot"))
# A local build is as likely to be a text-only 9b as a vision model, so
# ollama is off unless the config says otherwise.
expect_false(corteza:::bot_images_enabled("ollama"))
expect_false(corteza:::bot_images_enabled(NULL))
# The override goes both ways: on for a vision-capable local model, off
# for a bot that should stay text.
expect_true(corteza:::bot_images_enabled("ollama", list(images = TRUE)))
expect_false(corteza:::bot_images_enabled("openai_codex", list(images = FALSE)))

# ---- Which attachments are pictures worth fetching ----

expect_identical(length(corteza:::bot_image_attachments(msg(att()))), 1L)
expect_identical(length(corteza:::bot_image_attachments(msg())), 0L)

# A PDF or a voice note is left alone. Handing a model a spreadsheet as
# an image would be worse than not handing it anything.
expect_identical(length(corteza:::bot_image_attachments(
    msg(att(mime = "application/pdf", name = "notes.pdf")))), 0L)
expect_identical(length(corteza:::bot_image_attachments(
    msg(att(mime = NA_character_)))), 0L)

# Too big to send is worth saying so about before spending the fetch.
local({
    big <- msg(att(bytes = 20L * 1024L * 1024L))
    expect_message(out <- corteza:::bot_image_attachments(big), "over the")
    expect_identical(length(out), 0L)
})

# An unknown size is not a reason to refuse: the server did not say, and
# the post-download check catches an actual oversize file.
expect_identical(length(corteza:::bot_image_attachments(
    msg(att(bytes = NA_integer_)))), 1L)

# The limit is configurable.
# The ceiling is per provider, because they differ by 4x. One
# conservative number chosen from the strictest provider refused the
# first real photograph anyone sent to a bot running gpt-5.5.
expect_identical(corteza:::bot_image_max_bytes(provider = "openai_codex"),
                 corteza:::.BOT_IMAGE_MAX_BYTES[["openai_codex"]])
expect_identical(corteza:::bot_image_max_bytes(provider = "anthropic"),
                 corteza:::.BOT_IMAGE_MAX_BYTES[["anthropic"]])
expect_true(corteza:::bot_image_max_bytes(provider = "openai_codex") >
            corteza:::bot_image_max_bytes(provider = "anthropic"))

# The regression, by the number that actually failed. cornball2.png was
# 5,584,988 bytes -- 6.5% over the old flat 5 MB cap -- sent to a bot on
# openai_codex, which takes four times that.
expect_true(5584988 <= corteza:::bot_image_max_bytes(provider = "openai_codex"))
# ... and it would still be refused on Anthropic, correctly: that is a
# real 5 MB limit, not a guess. The two must not collapse to one number.
expect_false(5584988 <= corteza:::bot_image_max_bytes(provider = "anthropic"))

# An unnamed or unknown provider takes the conservative default rather
# than erroring: [[ on a named atomic vector raises "subscript out of
# bounds" for a name that is not there, which would cost the poll every
# picture in it rather than one.
expect_identical(corteza:::bot_image_max_bytes(),
                 corteza:::.BOT_IMAGE_MAX_DEFAULT)
expect_identical(corteza:::bot_image_max_bytes(provider = "some_gateway"),
                 corteza:::.BOT_IMAGE_MAX_DEFAULT)
expect_identical(corteza:::bot_image_max_bytes(provider = NA_character_),
                 corteza:::.BOT_IMAGE_MAX_DEFAULT)

# The config override still wins over both.
expect_identical(corteza:::bot_image_max_bytes(list(image_max_bytes = 1024),
                                               provider = "openai_codex"),
                 1024)
expect_identical(corteza:::bot_image_max_bytes(list(image_max_bytes = "nope"),
                                               provider = "anthropic"),
                 corteza:::.BOT_IMAGE_MAX_BYTES[["anthropic"]])

# ---- What the model ends up holding ----

if (!corteza:::bot_llm_multimodal()) {
    exit_file("llm.api has no llm_content(): image content tests skipped")
}

# A message with no attachment comes back as the string it went in as.
# This is the assertion that keeps every room that never sees a picture
# behaving exactly as it did.
expect_identical(
    corteza:::bot_message_content(fake_chat(), msg(), "hello there",
                                  "openai_codex"),
    "hello there")

# A message with one comes back as llm_content: the same text first,
# then the picture.
local({
    out <- corteza:::bot_message_content(fake_chat(), msg(att()),
                                         "[@alice:ex] look at this",
                                         "openai_codex")
    expect_inherits(out, "llm_content")
    expect_identical(length(out), 2L)
    expect_identical(out[[1L]]$text, "[@alice:ex] look at this")
    expect_inherits(out[[2L]], "llm_image")
    expect_identical(out[[2L]]$mime, "image/png")
    # The bytes are the ones the transport handed over, by value.
    expect_identical(out[[2L]]$data, jsonlite::base64_enc(png_bytes))
})

# Two pictures in one message become two parts, after the text.
local({
    out <- corteza:::bot_message_content(
        fake_chat(), msg(att(), att(id = "mxc://ex/two", name = "b.png")),
        "these", "anthropic")
    expect_identical(length(out), 3L)
    expect_true(all(vapply(out[2:3], inherits, logical(1), "llm_image")))
})

# A provider with no vision keeps its text, and says why rather than
# dropping the picture silently.
local({
    expect_message(
        out <- corteza:::bot_message_content(fake_chat(), msg(att()),
                                             "hello", "ollama"),
        "not configured for vision")
    expect_identical(out, "hello")
})

# A transport that reports no inbound media is not asked to download.
local({
    fetched <- FALSE
    cl <- fake_chat(attachments = FALSE,
                    on_download = function(a, dest) {
                        fetched <<- TRUE
                        writeBin(png_bytes, dest)
                        dest
                    })
    expect_identical(
        corteza:::bot_message_content(cl, msg(att()), "hello", "anthropic"),
        "hello")
    expect_false(fetched)
})

# ---- One bad picture must not cost the room its message ----

# An encrypted attachment is the common case: chat.api refuses those,
# because the bytes behind the URL are ciphertext it cannot decrypt.
local({
    cl <- fake_chat(on_download = function(a, dest) {
        stop("chat.api: attachment ", a$id, " is encrypted")
    })
    expect_message(
        out <- corteza:::bot_message_content(cl, msg(att()), "hello",
                                             "anthropic"),
        "could not fetch")
    expect_identical(out, "hello")
})

# A fetch that returns a path with nothing behind it is the same answer.
local({
    cl <- fake_chat(on_download = function(a, dest) dest)
    expect_identical(
        corteza:::bot_message_content(cl, msg(att()), "hello", "anthropic"),
        "hello")
})

# A file that is oversized only after the download is caught there. The
# `bytes` on the record is what the sender's client claimed, and nothing
# verified it -- so the declared size here passes the pre-check and the
# real one does not.
local({
    cl <- fake_chat(on_download = function(a, dest) {
        writeBin(as.raw(rep(0L, 4096)), dest)
        dest
    })
    expect_message(
        out <- corteza:::bot_message_content(cl, msg(att(bytes = 50L)),
                                             "hello", "anthropic",
                                             cfg = list(image_max_bytes = 100)),
        "after download")
    expect_identical(out, "hello")
})

# A 5.5 MB photo goes through on openai_codex and is refused on
# anthropic, from one call with nothing but the provider differing.
# This is the end-to-end form of the regression above: the unit test
# pins the number, this pins that the number is actually consulted.
local({
    big <- as.raw(rep(0L, 6L * 1024L * 1024L))
    cl <- fake_chat(on_download = function(a, dest) {
        writeBin(big, dest)
        dest
    })
    m <- msg(att(bytes = NA_integer_))
    out <- corteza:::bot_message_content(cl, m, "what is this",
                                         "openai_codex")
    expect_inherits(out, "llm_content")
    expect_true(corteza:::bot_llm_multimodal())

    expect_message(
        small <- corteza:::bot_message_content(cl, m, "what is this",
                                               "anthropic"),
        "over the")
    expect_identical(small, "what is this")
})

# One good picture and one bad one leaves the good one in place.
local({
    n <- 0L
    cl <- fake_chat(on_download = function(a, dest) {
        n <<- n + 1L
        if (n == 1L) {
            stop("gone")
        }
        writeBin(png_bytes, dest)
        dest
    })
    expect_message(
        out <- corteza:::bot_message_content(
            cl, msg(att(), att(id = "mxc://ex/two")), "hello", "anthropic"),
        "could not fetch")
    expect_inherits(out, "llm_content")
    expect_identical(length(out), 2L)
})

# The downloaded file does not survive the encode. A room that sees a
# lot of pictures would otherwise fill the temp directory, and the
# bytes are in the llm_image by then.
local({
    kept <- NULL
    cl <- fake_chat(on_download = function(a, dest) {
        kept <<- dest
        writeBin(png_bytes, dest)
        dest
    })
    corteza:::bot_message_content(cl, msg(att()), "hello", "anthropic")
    expect_false(file.exists(kept))
})
