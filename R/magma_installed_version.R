#' MAGMA installed version
#' 
#' List which version(s) of MAGMA is currently installed. 
#' 
#' @param magma_x MAGMA executable. 
#' @param as_package_version Return as the object class 
#' \link[base]{package_version}. 
#' NOTE, this will convert any character suffixes into numbers:
#' e.g. \code{"v1.06a"} --> \code{"v1.06.1"}, or 
#' \code{"v1.06b"} --> \code{"v1.06.2"}
#' @param verbose Print messages. 
#' 
#' @keywords internal
magma_installed_version <- function(magma_x = magma_executable(),
                                    as_package_version = FALSE,
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
    #### Return ####
    if(isTRUE(as_package_version)){
        versions <- as_package_version(version = versions)
    } 
    return(versions)
}
