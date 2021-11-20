messager <- function(..., v = TRUE, parallel = FALSE) {
    if(parallel){
        if(v) message_parallel(...) 
    } else {
        msg <- paste(...)
        if (v) message(msg)
    }
}
