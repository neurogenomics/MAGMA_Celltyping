magma_installed_version <- function(magma_x = magma_executable(),
                                    verbose = TRUE) {
    if(length(magma_x)==0) {
        messager("MAGMA is not installed.", v = verbose)
        return(NULL)
    }
    if (magma_installed(magma_x = magma_x,
                        verbose = FALSE)) {
        
        if(get_os()=="Windows") magma_x <- normalizePath(magma_x)
        check_magma <- system(as.character(paste(magma_x,"--version")),
                              intern = TRUE)
        version <- strsplit(check_magma, " ")[[1]][3]
        messager("Installed MAGMA version:", version, v = verbose)
    } else {
        messager("MAGMA is not installed.", v = verbose)
        version <- NULL
    }
    return(version)
}
