magma_check <- function(version = NULL,
                        verbose = TRUE){
    #### Get the path to any existing MAGMA installations ####
    magma_x <- magma_executable(version = version)
    #### Check if it is indeed working #####
    ## If not, proceed to install MAGMA
    if(!magma_installed(magma_x = magma_x,
                        verbose = FALSE)) {
        magma_install(verbose = verbose) 
        magma_x <- magma_executable(version = version)
    }
    #### After installing, check that MAGMA works ####
    ## If not, throw an error
    if(!magma_installed(magma_x = magma_x,
                        verbose = FALSE)){
        stopper("MAGMA must be installed to use this function.")
    } else {
        ## If so, report the version and proceed.
        version <- magma_links_versions(links = magma_x,
                                        version = version)
        magma_x <- magma_executable(version = version)
        messager("MAGMA is installed in:",
                 dirname(magma_x),
                 v=verbose)
    }
}
