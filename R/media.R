# Inbound images: what a picture posted in a room becomes for the model.
#
# A picture arrives as its own message with its own event id, carrying a
# chat_attachment that names where the bytes live rather than holding
# them. Turning that into something a model can look at is three steps:
# fetch through the transport (the mxc URL wants this client's
# credentials, so nothing else can), encode, and hand it to llm.api in
# its provider-neutral form.
#
# Fetching is the transport's verb, chat.api::chat_download(), rather
# than mx.api::mx_download(): corteza stopped calling mx.api directly in
# 0.7.0 and the contract is what a non-Matrix transport would implement.

# Providers whose models take images. Deliberately a provider list and
# not a model list: a model whitelist goes stale every time a vendor
# ships, and the failure it prevents (a 400 on an unsupported input) is
# both loud and rare compared to the failure it causes (a current
# vision model refusing to look at a picture because a constant here
# was not updated).
#
# ollama is the exception and is off by default, because a local build
# is as likely to be a text-only 9b as a vision model, and the local
# case is the one where a config knob is easy to set.
.BOT_VISION_PROVIDERS <- c("anthropic", "anthropic_claude", "openai",
                           "openai_codex", "moonshot")

# 5 MB. Anthropic's per-image ceiling is the lowest of the providers
# here, and base64 adds a third on top of whatever this is, so the
# conservative number is the one that does not depend on which room a
# picture landed in.
.BOT_IMAGE_MAX_BYTES <- 5L * 1024L * 1024L

# Whether to send images at all for this provider. cfg$images overrides,
# either way: TRUE for a vision-capable ollama build, FALSE to turn the
# whole thing off for a bot that should stay text.
bot_images_enabled <- function(provider, cfg = list()) {
    override <- cfg$images
    if (!is.null(override)) {
        return(isTRUE(override))
    }
    isTRUE((provider %||% "") %in% .BOT_VISION_PROVIDERS)
}

# Whether the installed llm.api can carry an image. Feature-detected
# rather than pinned to a version floor, the treatment history_callback
# and web_search already get here: llm.api is an Imports, so a floor
# above what CRAN has makes corteza uninstallable for everyone, to buy
# a check that a one-line exists() makes anyway. CI installs llm.api
# from its default branch and fails loudly if this is FALSE, so the
# tests below cannot skip themselves into a green tick.
bot_llm_multimodal <- function() {
    all(c("llm_content", "llm_image") %in% getNamespaceExports("llm.api"))
}

bot_image_max_bytes <- function(cfg = list()) {
    n <- suppressWarnings(as.numeric(cfg$image_max_bytes %||% NA))
    if (is.na(n) || n <= 0) {
        return(.BOT_IMAGE_MAX_BYTES)
    }
    n
}

# The image attachments on one message. Non-image attachments -- a PDF,
# a voice note -- are left alone: this understands pictures, and
# silently handing a model a spreadsheet as an image would be worse
# than not handing it anything.
bot_image_attachments <- function(m, max_bytes = .BOT_IMAGE_MAX_BYTES) {
    atts <- m$attachments
    if (!length(atts)) {
        return(list())
    }
    Filter(Negate(is.null), lapply(atts, function(a) {
        mime <- a$mime
        if (!is.character(mime) || length(mime) != 1L || is.na(mime) ||
                                   !grepl("^image/", mime)) {
            return(NULL)
        }
        # The declared size, when there is one. A picture too big to
        # send is worth saying so about before spending the download.
        size <- suppressWarnings(as.numeric(a$bytes %||% NA))
        if (!is.na(size) && size > max_bytes) {
            message("corteza: skipping ", a$name %||% a$id, " (",
                    format(size, big.mark = ","), " bytes, over the ",
                    format(max_bytes, big.mark = ","), " byte limit)")
            return(NULL)
        }
        a
    }))
}

# Fetch one attachment and encode it. NULL when it cannot be had, with
# the reason on stderr rather than an error: one unreadable picture
# must not cost the room its message. An encrypted attachment is the
# common case here -- chat.api refuses those, because the bytes behind
# the URL are ciphertext it has no way to decrypt.
bot_fetch_image <- function(chat, attachment,
                            max_bytes = .BOT_IMAGE_MAX_BYTES) {
    path <- tryCatch(chat.api::chat_download(chat, attachment),
                     error = function(e) {
        message("corteza: could not fetch ",
                attachment$name %||% attachment$id, ": ", conditionMessage(e))
        NULL
    })
    if (is.null(path) || !file.exists(path)) {
        return(NULL)
    }
    on.exit(unlink(path), add = TRUE)
    # Checked again after the fetch, because `bytes` on the record is
    # what the sender's client claimed and nothing verified it.
    size <- file.size(path)
    if (is.na(size) || size <= 0 || size > max_bytes) {
        message("corteza: skipping ", attachment$name %||% attachment$id,
                " (", format(size, big.mark = ","), " bytes after download)")
        return(NULL)
    }
    tryCatch(llm.api::llm_image(path, mime = attachment$mime),
             error = function(e) {
        message("corteza: could not encode ",
                attachment$name %||% attachment$id, ": ",
                conditionMessage(e))
        NULL
    })
}

# The content for one incoming message: the text as before, or that
# text plus the pictures that came with it.
#
# Returns the plain string whenever there is no image to add, so a room
# that never sees one behaves exactly as it did. That matters more than
# it looks: `text` also feeds the reply gate and the Matrix transcript
# ledger, and only the model's copy needs to be anything else.
bot_message_content <- function(chat, m, text, provider, cfg = list()) {
    max_bytes <- bot_image_max_bytes(cfg)
    atts <- bot_image_attachments(m, max_bytes)
    if (!length(atts)) {
        return(text)
    }
    if (!bot_images_enabled(provider, cfg)) {
        message("corteza: ", length(atts), " image(s) not sent: provider '",
                provider %||% "(unset)", "' is not configured for vision. ",
                "Set images: true in the Matrix config to override.")
        return(text)
    }
    if (!isTRUE(chat.api::chat_capabilities(chat)$attachments)) {
        return(text)
    }
    if (!bot_llm_multimodal()) {
        message("corteza: ", length(atts), " image(s) not sent: this llm.api ",
                "build has no llm_content(). Update llm.api.")
        return(text)
    }
    imgs <- Filter(Negate(is.null),
                   lapply(atts, function(a) bot_fetch_image(chat, a, max_bytes)))
    if (!length(imgs)) {
        return(text)
    }
    do.call(llm.api::llm_content, c(list(text), imgs))
}
