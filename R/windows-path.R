# Use Windows' extended path form for file operations on deep absolute paths.
# Keep ordinary paths in records. Reserve space for lock and staging suffixes.
.windows_io_path <- function(path) {
    if (.Platform$OS.type != "windows" || !length(path)) return(path)
    long <- which(nchar(path) >= 240L)
    if (!length(long)) return(path)
    normalized <- chartr("\\", "/", path[long])
    # An extended path treats dot components literally. Keep the usual
    # Windows interpretation of a relative path or a path containing them.
    clean <- !grepl("(^|/)[.]{1,2}(/|$)", normalized)
    drive <- clean & grepl("^[A-Za-z]:/", normalized)
    unc <- clean & startsWith(normalized, "//") & !startsWith(normalized, "//?/") &
        !startsWith(normalized, "//./")
    normalized[drive] <- paste0("//?/", normalized[drive], recycle0 = TRUE)
    normalized[unc] <- paste0("//?/UNC/", substring(normalized[unc], 3L), recycle0 = TRUE)
    path[long] <- chartr("/", "\\", normalized)
    path
}
