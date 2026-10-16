local({
    ns <- asNamespace("corteza")
    old_models <- ns$.model_discover
    old_agent <- llm.api::agent
    on.exit({
        assignInNamespace(".model_discover", old_models, "corteza")
        assignInNamespace("agent", old_agent, "llm.api")
    }, add = TRUE)
    queried <- 0L
    assignInNamespace(".model_discover", function(provider, base_url = NULL) {
        queried <<- queried + 1L
        expect_equal(provider, "openai")
        structure(c("gpt-existing", "gpt-6-astra"), source = "provider")
    }, "corteza")
    ctx <- new.env(parent = emptyenv())
    ctx$session <- corteza::new_session(provider = "openai",
        model_map = list(cloud = "gpt-existing"), system = "fixture",
        web_search = FALSE)
    ctx$config <- list(provider = "openai", model = "gpt-existing", reasoning_effort = "medium")
    ctx$session$config <- ctx$config
    ctx$session$thinking <- "between_tools"
    ctx$disk_session <- list(session = list(model = "gpt-existing", provider = "openai"))
    ctx$ws_enabled <- FALSE
    ctx$palette <- list(dim = "", reset = "")
    input <- c("/model", "/model 2", "/effort high", "/effort")
    index <- 0L
    ctx$read_input <- function(...) {
        index <<- index + 1L
        if (index <= length(input)) input[index] else character()
    }
    output <- capture.output(ns$run_repl_loop(ctx))
    expect_equal(queried, 1L)
    expect_true(any(grepl("gpt-6-astra", output, fixed = TRUE)))
    expect_true(any(grepl("high (session override)", output, fixed = TRUE)))
    expect_equal(ctx$session$model_map$cloud, "gpt-6-astra")
    expect_equal(ctx$model, "gpt-6-astra")
    expect_equal(ctx$disk_session$session$model, "gpt-6-astra")
    expect_null(ctx$session$thinking)
    expect_equal(ctx$session$reasoning_effort, "high")
    # Selection is from the displayed menu even if a later discovery differs.
    assignInNamespace(".model_discover", function(...) stop("must use displayed menu"), "corteza")
    ns$.repl_model_command(ctx, "/model 1")
    expect_equal(ctx$session$model_map$cloud, "gpt-existing")
    bad <- ns$.repl_model_command(ctx, "/model 999999999999999999999")
    expect_true(grepl("No menu entry", bad, fixed = TRUE))
    expect_equal(ctx$session$model_map$cloud, "gpt-existing")
    expect_true(grepl("Usage:", ns$.repl_model_command(ctx, "/model a b c"), fixed = TRUE))
    # A failed discovery still shows the current and configured models.
    menu <- ns$.repl_model_command(ctx, "/model")
    expect_true(grepl("Catalog unavailable", menu, fixed = TRUE))
    expect_true(grepl("gpt-existing", menu, fixed = TRUE))
    ns$.repl_model_command(ctx, "/model named-model openai_codex")
    expect_equal(ctx$provider, "openai_codex")
    expect_equal(ctx$disk_session$session$provider, "openai_codex")
    expect_null(ns$.effort_parse_command("please discuss effort"))
    expect_null(ns$.effort_parse_command("/effortful high"))
    expect_equal(ns$.effort_parse_command("@bot /effort HIGH")$value, "high")
    invalid <- ns$.effort_command(ctx$session, ns$.effort_parse_command("/effort high extra"))
    expect_true(grepl("Usage:", invalid, fixed = TRUE))
    expect_equal(ctx$session$reasoning_effort, "high")
    status <- ns$.effort_command(ctx$session, ns$.effort_parse_command("/effort default"))
    expect_null(ctx$session$reasoning_effort)
    expect_true(grepl("medium (configuration)", status, fixed = TRUE))
    # Split talkers never change the worker's saved effort, including reset.
    ctx$session$talker <- TRUE
    ctx$session$doer_reasoning_effort <- "xhigh"
    ns$.effort_command(ctx$session, ns$.effort_parse_command("/effort low"))
    expect_equal(ctx$session$doer_reasoning_effort, "xhigh")
    status <- ns$.effort_command(ctx$session, ns$.effort_parse_command("/effort default"))
    expect_true(grepl("default (provider default)", status, fixed = TRUE))
    expect_equal(ctx$session$doer_reasoning_effort, "xhigh")
    ns$.effort_command(ctx$session, ns$.effort_parse_command("/effort default"),
        cfg = list(reasoning_effort = "max", talker = list(reasoning_effort = "medium")))
    expect_equal(ctx$session$reasoning_effort, "medium")
    expect_equal(ctx$session$doer_reasoning_effort, "xhigh")
    ctx$session$reasoning_effort <- NULL
    ctx$session$provider <- "ollama"
    status <- ns$.effort_command(ctx$session, ns$.effort_parse_command("/effort high"))
    expect_true(grepl("not supported", status, fixed = TRUE))
    expect_null(ctx$session$reasoning_effort)
    # The same session override reaches turn(), not just its display text.
    captured <- NULL
    assignInNamespace("agent", function(...) {
        captured <<- list(...)
        list(content = "fixture", history = list(), usage = list(input_tokens = 1L, output_tokens = 1L))
    }, "llm.api")
    for (provider in c("openai", "openai_codex", "anthropic")) {
        s <- corteza::new_session(provider = provider, system = "fixture",
            model_map = list(cloud = "fixture-model"), tools_filter = character(),
            web_search = FALSE)
        ns$.effort_command(s, ns$.effort_parse_command("/effort high"))
        corteza::turn("hello", s)
        if (provider == "anthropic") expect_equal(captured$output_config$effort, "high") else
            expect_equal(captured$reasoning_effort, "high")
    }
})

# Exercise the real discovery path, replacing only the HTTP transport.
# A gateway URL retained after switching provider must not redirect a cloud
# catalog request (including its Authorization header) to that gateway.
if ("list_models" %in% getNamespaceExports("llm.api")) local({
    original <- get(".models_get", asNamespace("llm.api"))
    opts <- options(llm.api_base = NULL, llm.api.api_base = NULL,
                    llm.api_key = "fixture-key")
    on.exit({
        options(opts)
        assignInNamespace(".models_get", original, "llm.api")
    }, add = TRUE)
    seen <- list()
    assignInNamespace(".models_get", function(url, headers, timeout) {
        seen[[length(seen) + 1L]] <<- list(url = url, headers = headers)
        list(data = list(list(id = "catalog-model")),
             models = list(list(name = "catalog-model")))
    }, "llm.api")
    endpoints <- c(openai = "https://api.openai.com/v1/models",
                   anthropic = "https://api.anthropic.com/v1/models?limit=1000",
                   moonshot = "https://api.moonshot.ai/v1/models",
                   ollama = "http://localhost:11434/api/tags",
                   openai_compatible = "https://session-gateway.invalid/v1/models")
    s <- corteza::new_session(provider = "openai", system = "fixture",
        model_map = list(cloud = "fixture"), base_url = "https://session-gateway.invalid/v1")
    cfg <- list(base_url = "https://config-gateway.invalid/v1")
    for (provider in names(endpoints)) {
        s$provider <- provider
        menu <- corteza:::.model_menu_entries(s, cfg)
        expect_true("catalog-model" %in% vapply(menu, function(x) x$model, character(1)))
        expect_equal(tail(seen, 1)[[1]]$url, endpoints[[provider]])
    }
    s$base_url <- NULL
    corteza:::.model_menu_entries(s, cfg)
    expect_equal(tail(seen, 1)[[1]]$url, "https://config-gateway.invalid/v1/models")
    expect_equal(length(seen), 6L)
})

# A room command is dispatched without an LLM turn, and its receipt is
# recorded in the room transcript. No Matrix connection is opened.
local({
    if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api needed")
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        assignInNamespace(name, fn, package)
    }
    on.exit({
        for (x in rev(saved)) assignInNamespace(x$name, x$fn, x$package)
    }, add = TRUE)
    cfg <- list(user_id = "fixture-bot", operators = "fixture-human",
                reasoning_effort = "medium")
    chat <- chat.api::chat_loopback()
    sessions <- new.env(parent = emptyenv())
    s <- corteza::new_session(channel = "matrix", provider = "openai",
        model_map = list(cloud = "gpt-6-astra"), system = "fixture")
    s$config <- list()
    s$room_id <- "fixture-room"
    s$cwd <- tempdir()
    s$seen_event_ids <- character()
    assign("fixture-room", s, sessions)
    incoming <- chat.api::chat_message(id = "effort-command", channel = "fixture-room",
        sender = "fixture-human", body = "/effort high", ts = Sys.time())
    patch("bot_load_config", function(...) cfg)
    patch("bot_chat_client", function(...) chat)
    patch("chat_whoami", function(...) chat.api::chat_identity("fixture-bot"), "chat.api")
    patch("chat_poll", function(...) list(first_run = FALSE, messages = list(incoming),
        reactions = list(), invites = list()), "chat.api")
    patch("bot_should_respond", function(...) TRUE)
    patch("bot_run_turn_in_cwd", function(...) stop("slash command reached LLM"))
    replies <- character()
    patch("bot_reply_send", function(chat, room, text, ...) {
        replies <<- c(replies, text)
        paste0("receipt-", length(replies))
    })
    expect_equal(corteza::bot_poll(sessions = sessions, timeout = 0), 1L)
    expect_equal(s$reasoning_effort, "high")
    expect_equal(length(replies), 1L)
    expect_true(grepl("high (session override)", replies[1], fixed = TRUE))
    expect_equal(length(s$transcript), 2L)
    expect_equal(corteza::bot_poll(sessions = sessions, timeout = 0), 0L)
    expect_equal(length(replies), 1L)
    incoming$id <- "effort-default-command"
    incoming$body <- "/effort default"
    expect_equal(corteza::bot_poll(sessions = sessions, timeout = 0), 1L)
    expect_equal(s$reasoning_effort, "medium")
    expect_equal(length(replies), 2L)
    expect_true(grepl("medium (configuration)", replies[2], fixed = TRUE))
})
