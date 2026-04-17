library(tinytest)

# --- matrix_extract_reaction_verdict ---

# No events -> NULL.
expect_null(
    llamaR:::matrix_extract_reaction_verdict(
        list(rooms = list(join = setNames(list(), character()))),
        "!r:ex", "@bot:ex", "$target"
    )
)

# Thumbs up from a non-self sender -> TRUE.
local({
    sync <- list(rooms = list(join = list(
        `!r:ex` = list(timeline = list(events = list(
            list(
                type = "m.reaction",
                sender = "@troy:ex",
                content = list(
                    `m.relates_to` = list(
                        rel_type = "m.annotation",
                        event_id = "$target",
                        key = "\U0001F44D"
                    )
                )
            )
        )))
    )))
    expect_equal(
        llamaR:::matrix_extract_reaction_verdict(
            sync, "!r:ex", "@bot:ex", "$target"
        ),
        TRUE
    )
})

# Thumbs down -> FALSE.
local({
    sync <- list(rooms = list(join = list(
        `!r:ex` = list(timeline = list(events = list(
            list(
                type = "m.reaction",
                sender = "@troy:ex",
                content = list(
                    `m.relates_to` = list(
                        rel_type = "m.annotation",
                        event_id = "$target",
                        key = "\U0001F44E"
                    )
                )
            )
        )))
    )))
    expect_equal(
        llamaR:::matrix_extract_reaction_verdict(
            sync, "!r:ex", "@bot:ex", "$target"
        ),
        FALSE
    )
})

# Reaction from the bot itself is ignored (it posts its own 👍 / 👎
# buttons, which must not count as user approval).
local({
    sync <- list(rooms = list(join = list(
        `!r:ex` = list(timeline = list(events = list(
            list(
                type = "m.reaction",
                sender = "@bot:ex",
                content = list(
                    `m.relates_to` = list(
                        rel_type = "m.annotation",
                        event_id = "$target",
                        key = "\U0001F44D"
                    )
                )
            )
        )))
    )))
    expect_null(
        llamaR:::matrix_extract_reaction_verdict(
            sync, "!r:ex", "@bot:ex", "$target"
        )
    )
})

# Reaction on a different event -> NULL.
local({
    sync <- list(rooms = list(join = list(
        `!r:ex` = list(timeline = list(events = list(
            list(
                type = "m.reaction",
                sender = "@troy:ex",
                content = list(
                    `m.relates_to` = list(
                        rel_type = "m.annotation",
                        event_id = "$other",
                        key = "\U0001F44D"
                    )
                )
            )
        )))
    )))
    expect_null(
        llamaR:::matrix_extract_reaction_verdict(
            sync, "!r:ex", "@bot:ex", "$target"
        )
    )
})

# matrix_approval_prompt renders a short readable string.
local({
    call <- list(tool = "bash", args = list(command = "rm -rf /tmp/x"))
    decision <- list(reason = "default: random/exec/matrix")
    out <- llamaR:::matrix_approval_prompt(call, decision, 60L)
    expect_true(grepl("bash", out))
    expect_true(grepl("command=rm -rf /tmp/x", out))
    expect_true(grepl("60s", out))
})

# Long arg gets truncated in the prompt.
local({
    big <- paste(rep("a", 200), collapse = "")
    out <- llamaR:::matrix_approval_prompt(
        list(tool = "write_file", args = list(content = big)),
        list(reason = "test"), 30L
    )
    expect_true(nchar(out) < 500L)
    expect_true(grepl("\\.\\.\\.", out))
})

# matrix_approval_cb: auto mode always approves.
local({
    cb <- llamaR:::matrix_approval_cb(list(auto_approve_asks = TRUE))
    expect_true(cb(list(), list()))
})
