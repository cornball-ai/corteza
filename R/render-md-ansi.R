# Render a markdown response string to ANSI-styled plain text for
# terminal display. The raw markdown source is preserved when the
# terminal doesn't support ANSI (NO_COLOR, piped to file, etc.) so
# /copy and stdout redirection always see the source the LLM emitted.

#' ANSI escape pair for italic. Not part of `ansi_colors()` because
#' italic support is less universal than bold/color; modern terminals
#' (iTerm2, kitty, alacritty, gnome-terminal, modern xterm,
#' xterm.js / RStudio Server) render it, older ones may ignore it or
#' fall back to inverse video. Acceptable risk for chat-response
#' display.
#' @noRd
md_italic <- function() {
    list(on = "\033[3m", off = "\033[23m")
}

#' ANSI escape pair for bold. We use the explicit `\033[22m` to *exit*
#' bold rather than a full `\033[0m` reset so styles nest cleanly --
#' a `\033[0m` inside a colored heading would also drop the color.
#' @noRd
md_bold <- function() {
    list(on = "\033[1m", off = "\033[22m")
}

#' Apply inline markdown transforms (bold, italic, inline code) to a
#' single line of text. Inline code is masked first so its contents
#' aren't re-interpreted by the bold/italic regex passes.
#' @noRd
render_md_inline <- function(text, palette) {
    italic <- md_italic()
    bold <- md_bold()

    # Mask inline `code` spans so ** and _ inside them stay literal.
    matches <- gregexpr("`[^`\n]+`", text, perl = TRUE)[[1]]
    placeholders <- list()
    if (matches[1] != -1L) {
        lens <- attr(matches, "match.length")
        for (j in rev(seq_along(matches))) {
            start <- matches[j]
            len <- lens[j]
            inner <- substring(text, start + 1L, start + len - 2L)
            ph <- sprintf("\001I%d\001", j)
            placeholders[[ph]] <- sprintf("%s%s%s", palette$bright_cyan,
                inner, palette$reset)
            text <- paste0(
                           substring(text, 1L, start - 1L),
                           ph,
                           substring(text, start + len)
            )
        }
    }

    # Markdown links: [text](url) -> blue text, dim (url).
    text <- gsub("\\[([^]\n]+)\\]\\(([^)\n]+)\\)",
                 sprintf("%s\\1%s %s(\\2)%s",
                         palette$bright_blue, palette$reset,
                         palette$dim, palette$reset),
                 text)

    # Bold first (** has higher precedence than * for italic).
    text <- gsub("\\*\\*([^*\n]+)\\*\\*",
                 sprintf("%s\\1%s", bold$on, bold$off),
                 text, perl = TRUE)

    # Italic via *...*: only when the asterisks aren't adjacent to
    # other asterisks (would be bold) and aren't bare math like a*b.
    text <- gsub("(?<![*[:alnum:]])\\*([^*\n]+?)\\*(?![*[:alnum:]])",
                 sprintf("%s\\1%s", italic$on, italic$off),
                 text, perl = TRUE)

    # Italic via _..._: only when the underscores are at word
    # boundaries, so `my_var_name` and snake_case identifiers don't
    # get italicized mid-token.
    text <- gsub("(?<![[:alnum:]_])_([^_\n]+?)_(?![[:alnum:]_])",
                 sprintf("%s\\1%s", italic$on, italic$off),
                 text, perl = TRUE)

    # Restore inline code spans.
    for (ph in names(placeholders)) {
        text <- sub(ph, placeholders[[ph]], text, fixed = TRUE)
    }
    text
}

#' Render the response of a chat turn as ANSI-styled text for the
#' terminal. Returns `text` unchanged when the terminal doesn't
#' support ANSI or the user has opted out via
#' `options(corteza.markdown = FALSE)`.
#'
#' Transforms handled:
#' * `# H1` / `## H2` / `### H3` headings (bold, with color for H1/H2)
#' * `**bold**`, `*italic*` / `_italic_`, `` `inline code` ``
#' * Fenced code blocks ```` ``` `` ```` (dim, 2-space indent)
#' * `- ` and `* ` bullets become `\u2022` (green)
#' * `> blockquote` lines get a dim `|` prefix
#' * `[text](url)` links: blue text, dim url
#' @noRd
render_md_ansi <- function(text, palette = ansi_colors()) {
    if (!is.character(text) || length(text) != 1L || !nzchar(text)) {
        return(text)
    }
    if (!isTRUE(getOption("corteza.markdown", TRUE))) {
        return(text)
    }
    # When the terminal doesn't support ANSI, palette entries are
    # empty strings. Skip the work entirely so we don't strip
    # markdown syntax markers that the user might want to see.
    if (!nzchar(palette$reset)) {
        return(text)
    }

    bold <- md_bold()
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    out <- character(length(lines))
    in_code <- FALSE

    for (i in seq_along(lines)) {
        ln <- lines[i]
        # Fenced code block boundaries: hide the fence line itself
        # (matching the look of glow / Claude Code) and indent the
        # body. NA marks the line for removal so the surrounding
        # blank lines collapse naturally.
        if (grepl("^```", ln)) {
            in_code <- !in_code
            out[i] <- NA_character_
            next
        }
        if (in_code) {
            out[i] <- sprintf("  %s%s%s", palette$dim, ln, palette$reset)
            next
        }
        # Headings - strip the leading `# ` markers and style the
        # body. Palette tracks the existing inst/bin/corteza
        # render_markdown(): H1 bright_magenta, H2 bright_blue,
        # H3 bright_blue without bold.
        if (grepl("^### ", ln)) {
            body <- sub("^### ", "", ln)
            out[i] <- sprintf("%s%s%s", palette$bright_blue,
                              render_md_inline(body, palette), palette$reset)
            next
        }
        if (grepl("^## ", ln)) {
            body <- sub("^## ", "", ln)
            out[i] <- sprintf("%s%s%s%s",
                              bold$on, palette$bright_blue,
                              render_md_inline(body, palette),
                              palette$reset)
            next
        }
        if (grepl("^# ", ln)) {
            body <- sub("^# ", "", ln)
            out[i] <- sprintf("%s%s%s%s",
                              bold$on, palette$bright_magenta,
                              render_md_inline(body, palette),
                              palette$reset)
            next
        }
        # Blockquote prefix.
        if (grepl("^> ", ln)) {
            body <- sub("^> ", "", ln)
            out[i] <- sprintf("%s|%s %s",
                              palette$dim, palette$reset,
                              render_md_inline(body, palette))
            next
        }
        # Bullet list: replace leading `- ` or `* ` with a green
        # Unicode bullet, preserving any indent.
        bullet_m <- regexpr("^(\\s*)[-*] ", ln, perl = TRUE)
        if (bullet_m != -1L) {
            indent_len <- attr(bullet_m, "capture.length")[1]
            indent <- substring(ln, 1L, indent_len)
            rest <- substring(ln, indent_len + 3L)
            out[i] <- sprintf("%s%s\u2022%s %s",
                              indent, palette$green, palette$reset,
                              render_md_inline(rest, palette))
            next
        }
        out[i] <- render_md_inline(ln, palette)
    }
    paste(out[!is.na(out)], collapse = "\n")
}

