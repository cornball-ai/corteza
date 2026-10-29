# Set creation permissions before opening the file, including under a permissive
# inherited umask. Existing files are narrowed before more private bytes append.
.private_record_write <- function(path, text, append = TRUE) {
    previous <- Sys.umask("0077")
    on.exit(Sys.umask(previous), add = TRUE)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (file.exists(path) && !Sys.chmod(path, mode = "0600")) {
        stop("Could not restrict record permissions: ", path, call. = FALSE)
    }
    cat(text, "\n", file = path, append = append)
    invisible(path)
}

.trace_preview <- function(x, limit) {
    if (is.list(x)) return(lapply(x, .trace_preview, limit = limit))
    if (!is.character(x)) return(x)
    long <- !is.na(x) & nchar(x) > limit
    x[long] <- paste0(substr(x[long], 1L, limit - 3L), "...")
    x
}
