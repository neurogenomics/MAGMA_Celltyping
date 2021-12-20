messager <- function(..., v = TRUE, parallel = FALSE) {
    if(parallel){
        if(v) try({message_parallel(...)})
    } else {
        msg <- paste(...)
        if (v) try({message(msg)})
    }
}
