#' Print messages 
#' 
#' Conditionally print messages.
#'  Allows developers to easily control verbosity of functions, 
#'  and meet Bioconductor requirements that dictate the message 
#'  must first be stored to a variable before passing to \link[base]{message}. 
#'  
#' 
#' @param v Whether to print messages or not.
#' @param parallel Whether to enable message print when wrapped 
#' in parallelised functions.
#' 
#' @keywords internal 
messager <- function(..., v = TRUE, parallel = FALSE) {
    if(parallel){
        if(v) try({message_parallel(...)})
    } else {
        msg <- paste(...)
        if (v) try({message(msg)})
    }
}
