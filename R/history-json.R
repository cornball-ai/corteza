# Journal and vault histories can contain llm.api's provider-neutral image
# parts. JSON cannot encode their S3 classes. Keep a versioned wrapper around
# tagged values so an old plain record is never decoded as tagged content.
.history_json_wrapped <- function(x) {
    is.list(x) && identical(names(x), c(".corteza_history", "data")) &&
        identical(x[[".corteza_history"]], "multimodal-v1")
}

.history_json_encode <- function(x) {
    tagged <- FALSE
    encode <- function(value) {
        if (!is.list(value)) {
            return(value)
        }
        kind <- class(value)
        if (length(kind) == 1L &&
            kind %in% c("llm_content", "llm_text", "llm_image")) {
            tagged <<- TRUE
            return(list(.corteza_type = kind,
                        value = lapply(unclass(value), encode)))
        }
        if (is.object(value)) {
            return(value)
        }
        # Tool inputs may themselves use the reserved tag. Escape such lists
        # so reading them cannot turn ordinary data into a classed object.
        if (".corteza_type" %in% names(value)) {
            tagged <<- TRUE
            return(list(.corteza_type = "list", value = lapply(value, encode)))
        }
        lapply(value, encode)
    }
    data <- encode(x)
    if (tagged || .history_json_wrapped(x)) {
        list(.corteza_history = "multimodal-v1", data = data)
    } else {
        x
    }
}

.history_json_decode <- function(x) {
    if (!.history_json_wrapped(x)) {
        return(x)
    }
    decode <- function(value) {
        if (!is.list(value)) {
            return(value)
        }
        if (identical(names(value), c(".corteza_type", "value"))) {
            kind <- value[[".corteza_type"]]
            if (is.character(kind) && length(kind) == 1L &&
                kind %in% c("list", "llm_content", "llm_text", "llm_image")) {
                data <- lapply(value$value, decode)
                return(if (kind == "list") data else structure(data, class = kind))
            }
        }
        lapply(value, decode)
    }
    decode(x$data)
}

.history_json <- function(x) {
    jsonlite::toJSON(.history_json_encode(x), auto_unbox = TRUE, null = "null",
                     na = "null", digits = NA)
}
