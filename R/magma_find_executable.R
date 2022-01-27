#' Find MAGMA executable
#' 
#' Find any existing MAGMA executables in a given directory. 
#' 
#' @param destpath Directory to search for MAGMA executables in. 
#' @param return_all Whether to return paths to all MAGMA executables.
#' 
#' @keywords internal 
magma_find_executable <- function(destpath, 
                                  return_all = TRUE){
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
