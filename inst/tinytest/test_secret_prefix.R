local({
    ns <- asNamespace("corteza")
    session <- new.env(parent = emptyenv())
    session$secret_config <- session$secret_values <- character()
    tokens <- c("syt_fixture123456789", "hf_fixture987654321")
    quoted <- c(sprintf('export MATRIX_TOKEN="%s"; ./whoami.sh', tokens[1L]),
                sprintf("HF_TOKEN='%s' python upload.py", tokens[2L]))
    unquoted <- sprintf("HF_TOKEN=%s python upload.py", tokens[2L])

    expect_identical(ns$.secret_literals(quoted), tokens)
    ns$.secret_observe(session, quoted)
    expect_identical(session$secret_values, tokens)
    expect_false(any(vapply(tokens, function(x) any(grepl(x,
        ns$.redact_text(quoted), fixed = TRUE)), TRUE)))

    # Neither code expressions nor pagination cursors become learned secrets.
    code <- c('MATRIX_TOKEN = Sys.getenv("MATRIX_TOKEN")',
              'SERVICE_API_KEY = .get_key("openai")', 'APP_PASSWORD: required')
    cursors <- c('sync_token="cursor123456789"',
                 '{"pagination_token":"cursor123456789"}')
    expect_identical(ns$.secret_literals(code), character())
    expect_identical(ns$.redact_text(code), code)
    expect_identical(ns$.secret_literals(cursors), character())
    expect_identical(ns$.redact_text(cursors), cursors)
    for (text in cursors) {
        expect_null(ns$.approval_remote_problem(list(tool = "bash", args = list(command = text))))
    }

    # No registry values are supplied: each refusal must come from the label.
    calls <- c(quoted, unquoted,
               'TEAM_SERVICE_API_KEY="fixtureKey1234567" command',
               'team_matrix_token="fixtureToken1234567" command',
               '{"MATRIX_TOKEN":"syt_fixture123456789"}')
    for (command in calls) {
        expect_true(grepl("local CLI", ns$.approval_remote_problem(
            list(tool = "bash", args = list(command = command))), fixed = TRUE))
    }
    expect_null(ns$.approval_remote_problem(list(tool = "bash", args = list(command = "git status"))))

    # Exercise remote review itself. Fakes accept an ordinary call, exposing
    # any missed credential gate without writing a mailbox or sending a message.
    saved <- list()
    patch <- function(name, fn) {
        saved[[name]] <<- get(name, ns)
        utils::assignInNamespace(name, fn, "corteza")
    }
    on.exit(for (name in names(saved)) {
        utils::assignInNamespace(name, saved[[name]], "corteza")
    }, add = TRUE)
    sent <- character()
    writes <- 0L
    patch(".remote_emit", function(ctx, text) sent <<- c(sent, text))
    patch(".remote_write", function(...) { writes <<- writes + 1L; TRUE })
    patch(".remote_wait", function(...) list(approved = TRUE))
    session$secret_values <- character()
    ctx <- list(session = session, remote_dir = tempfile("prefix-mailbox-"))
    expect_true(ns$.remote_approve(ctx, list(tool = "bash", args = list(command = "git status")), list(reason = "ask")))
    expect_true(writes > 0L)
    for (command in c(quoted, unquoted)) {
        sent <- character()
        writes <- 0L
        expect_false(ns$.remote_approve(ctx, list(tool = "bash", args = list(command = command)), list(reason = "ask")))
        expect_identical(writes, 0L)
        expect_length(sent, 1L)
        expect_true(grepl("local CLI", sent, fixed = TRUE))
        expect_false(any(vapply(tokens, function(x) any(grepl(x, sent, fixed = TRUE)), TRUE)))
    }
})
