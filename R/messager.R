messager <- function(..., v = TRUE) {
    msg <- paste(...)
    if (v) {
        message(msg)
    }
}
