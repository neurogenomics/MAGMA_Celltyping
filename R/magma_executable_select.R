#' Select a MAGMA executable 
#' 
#' When more than one MAGMA executable is installed, 
#' selects one of them to use. 
#' 
#' @param magma_x MAGMA executable. 
#' @param return_all Whether to return paths to all MAGMA executables.
#' @param verbose Print messages. 
#' 
#' @keywords internal
magma_executable_select <- function(magma_x,
                                    return_all = FALSE,
                                    verbose = TRUE){ 
    if(length(magma_x)>1){
        messager(length(magma_x),"MAGMA versions found.",
                 v=verbose)
        if(return_all){
            messager("Returning paths to all MAGMA versions.",
                     v=verbose)
        } else {
            magma_x <- magma_x[1]
            messager("Returning path to only one MAGMA version:",
                     paste0("'",basename(dirname(magma_x)),"'"),
                     v=verbose) 
        }
    } 
    return(magma_x)
}