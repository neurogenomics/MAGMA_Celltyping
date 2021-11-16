stopper <- function(..., v = TRUE) {
    msg <- paste(...)
    if (v) {
        stop(msg)
    }
}
