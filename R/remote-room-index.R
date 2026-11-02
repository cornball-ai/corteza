# Only the courier writes bindings. Index their paths once per bot and data
# root, then update the index on each successful write. Reading a known room's
# file keeps readiness current without parsing all bindings on ordinary chat.
.remote_room_indexes <- new.env(parent = emptyenv())

.remote_room_index_key <- function(owner) {
    digest::digest(list(normalizePath(path.expand(.remote_root()), mustWork = FALSE), owner))
}

.remote_room_index <- function(owner) {
    key <- .remote_room_index_key(owner)
    if (exists(key, .remote_room_indexes, inherits = FALSE)) {
        return(.remote_room_indexes[[key]])
    }
    index <- new.env(parent = emptyenv())
    paths <- list.files(file.path(.remote_root(), "rooms"),
                        pattern = "[.]json$", full.names = TRUE)
    for (path in paths) {
        binding <- .remote_read_file(path)
        if (identical(binding$owner, owner) && is.character(binding$room) &&
            length(binding$room) == 1L && !is.na(binding$room) && nzchar(binding$room)) {
            index[[binding$room]] <- path
        }
    }
    .remote_room_indexes[[key]] <- index
    index
}

.remote_room_write <- function(path, binding) {
    .remote_write(path, binding)
    key <- .remote_room_index_key(binding$owner)
    if (!is.null(binding$room) && exists(key, .remote_room_indexes, inherits = FALSE)) {
        index <- .remote_room_indexes[[key]]
        index[[binding$room]] <- path
    }
    invisible(NULL)
}
