#' MAGMA check
#' 
#' Check that MAGMA is installed at the beginning of functions where MAGMA is 
#' required. 
#' @param version MAGMA version.
#' @param return_version Return the MAGMA version being used.
#' @inheritParams magma_install 
#' 
#' @keywords internal
magma_check <- function(version = NULL,
                        upgrade = FALSE,
                        return_version = FALSE,
                        verbose = TRUE){ 
    # devoptera::args2vars(magma_check)
    
    #### Check if it is indeed working #####
    magma_x <- magma_install(desired_version = version,
                             upgrade = upgrade,
                             verbose = verbose)
    #### After installing, check that MAGMA works ####
    ## If not, throw an error
    if(!magma_installed(magma_x = magma_x,
                        verbose = FALSE)){
        stopper("MAGMA must be installed to use this function.")
    }
    if(isTRUE(return_version)){
        version <- magma_installed_version(magma_x = magma_x,
                                           as_package_version = TRUE,
                                           verbose = FALSE)
        return(version)
    }
}
