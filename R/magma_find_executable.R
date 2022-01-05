magma_find_executable <- function(destpath){
    files <- list.files(path = destpath,
                        pattern = "^magma$|^magma.exe$",
                        recursive = TRUE,
                        ignore.case = TRUE,
                        full.names = TRUE)
    if(length(files)==0) message("Warning: no MAGMA executable found.")
    if(length(files)>1) {
        message("Warning: >1 MAGMA executable found. ONly using the first.")
        files <- files[1]
    }
    return(files)
}
