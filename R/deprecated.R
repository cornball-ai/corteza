#' @title Deprecated matrix_* names
#' @description The chat loop's exported functions were named
#'   \code{matrix_*} when Matrix was the only transport corteza could
#'   speak and the only one it knew how to speak to. It reaches every
#'   transport through the chat.api contract now and makes no Matrix call
#'   of its own, so the prefix named a coupling that no longer exists.
#'
#'   These wrappers keep the old names working. They warn once per
#'   session, forward everything unchanged, and are scheduled for removal
#'   at 1.0.0. Nothing about behaviour differs -- calling
#'   \code{matrix_send()} and \code{bot_send()} runs the same code.

#' Deprecated: use bot_configure()
#' @inheritParams bot_configure
#' @return See \code{\link{bot_configure}}.
#' @seealso \code{\link{bot_configure}}
#' @examples
#' \dontrun{
#' matrix_configure(server, user, password, room)  # use bot_configure()
#' }
#' @export
matrix_configure <- function(server, user, password, room, model = NULL,
                             provider = "anthropic", tools_filter = NULL,
                             auto_approve_asks = FALSE, bots = NULL,
                             models = NULL,
                             model_badge = c("never", "non_default", "always"),
                             display_name = NULL, fallback = NULL) {
    .Deprecated("bot_configure")
    bot_configure(server = server, user = user, password = password,
                  room = room, model = model, provider = provider,
                  tools_filter = tools_filter,
                  auto_approve_asks = auto_approve_asks, bots = bots,
                  models = models, model_badge = model_badge,
                  display_name = display_name, fallback = fallback)
}

#' Deprecated: use bot_send()
#' @inheritParams bot_send
#' @return See \code{\link{bot_send}}.
#' @seealso \code{\link{bot_send}}
#' @examples
#' \dontrun{
#' matrix_send("hello")  # use bot_send()
#' }
#' @export
matrix_send <- function(text, room_id = NULL, msgtype = "m.text",
                        markdown = FALSE) {
    .Deprecated("bot_send")
    bot_send(text = text, room_id = room_id, msgtype = msgtype,
             markdown = markdown)
}

#' Deprecated: use bot_poll()
#' @inheritParams bot_poll
#' @return See \code{\link{bot_poll}}.
#' @seealso \code{\link{bot_poll}}
#' @examples
#' \dontrun{
#' matrix_poll()  # use bot_poll()
#' }
#' @export
matrix_poll <- function(system = NULL, model = NULL, provider = NULL,
                        tools_filter = NULL, timeout = 0L, sessions = NULL) {
    .Deprecated("bot_poll")
    bot_poll(system = system, model = model, provider = provider,
             tools_filter = tools_filter, timeout = timeout,
             sessions = sessions)
}

#' Deprecated: use bot_run()
#' @inheritParams bot_run
#' @return See \code{\link{bot_run}}.
#' @seealso \code{\link{bot_run}}
#' @examples
#' \dontrun{
#' matrix_run()  # use bot_run()
#' }
#' @export
matrix_run <- function(timeout = 30000L, system = NULL, model = NULL,
                       provider = NULL, tools_filter = NULL) {
    .Deprecated("bot_run")
    bot_run(timeout = timeout, system = system, model = model,
            provider = provider, tools_filter = tools_filter)
}

#' Deprecated: use bot_run_init()
#' @inheritParams bot_run_init
#' @return See \code{\link{bot_run_init}}.
#' @seealso \code{\link{bot_run_init}}
#' @examples
#' \dontrun{
#' matrix_run_init()  # use bot_run_init()
#' }
#' @export
matrix_run_init <- function(system = NULL, model = NULL, provider = NULL,
                            tools_filter = NULL) {
    .Deprecated("bot_run_init")
    bot_run_init(system = system, model = model, provider = provider,
                 tools_filter = tools_filter)
}

#' Deprecated: use bot_run_step()
#' @inheritParams bot_run_step
#' @return See \code{\link{bot_run_step}}.
#' @seealso \code{\link{bot_run_step}}
#' @examples
#' \dontrun{
#' matrix_run_step(state)  # use bot_run_step()
#' }
#' @export
matrix_run_step <- function(state, timeout = 30000L) {
    .Deprecated("bot_run_step")
    bot_run_step(state = state, timeout = timeout)
}

#' Deprecated: use bot_archive_all()
#' @inheritParams bot_archive_all
#' @return See \code{\link{bot_archive_all}}.
#' @seealso \code{\link{bot_archive_all}}
#' @examples
#' \dontrun{
#' matrix_archive_all(sessions)  # use bot_archive_all()
#' }
#' @export
matrix_archive_all <- function(sessions, chat = NULL) {
    .Deprecated("bot_archive_all")
    bot_archive_all(sessions = sessions, chat = chat)
}

#' Deprecated: use bot_request_flush()
#' @return See \code{\link{bot_request_flush}}.
#' @seealso \code{\link{bot_request_flush}}
#' @examples
#' \dontrun{
#' matrix_request_flush()  # use bot_request_flush()
#' }
#' @export
matrix_request_flush <- function() {
    .Deprecated("bot_request_flush")
    bot_request_flush()
}
