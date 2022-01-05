magma_installed_version <- function(magma_x = magma_executable(),
                                    verbose = TRUE) {
    if(length(magma_x)==0) {
        messager("MAGMA is not installed.", v = verbose)
        return(NULL)
    }
    if(length(magma_x)>1) messager(length(magma_x),
                                   "MAGMA versions installed.",
                                   v=verbose)
    versions <- lapply(magma_x, function(x){
        if (magma_installed(magma_x = x,
                            verbose = FALSE)) {
            
            x <- fix_path(x)
            check_magma <- system(as.character(paste(x,"--version")),
                                  intern = TRUE)
            version <- strsplit(check_magma, " ")[[1]][3]
            messager("Installed MAGMA version:", version, v = verbose)
        } else {
            messager("MAGMA is not installed.", v = verbose)
            version <- NULL
        }
        return(version)
    }) 
    versions <- unlist(versions) 
    names(versions) <- magma_x
    return(versions)
}
