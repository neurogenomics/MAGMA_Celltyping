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