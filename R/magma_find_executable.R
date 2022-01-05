magma_find_executable <- function(destpath, 
                                  return_all=TRUE){
    files <- list.files(path = destpath,
                        pattern = "^magma$|^magma.exe$",
                        recursive = TRUE,
                        ignore.case = TRUE,
                        full.names = TRUE)
    if(length(files)==0) message("Warning: no MAGMA executable found.")
    if(return_all==FALSE){
        if(length(files)>1) {
            message("Warning: >1 MAGMA executable found. Only using the first.")
            files <- files[1]
        }
    } 
    return(files)
}
