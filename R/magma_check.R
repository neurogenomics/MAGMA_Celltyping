magma_check <- function(version = NULL,
                        upgrade = FALSE,
                        verbose = TRUE){ 
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
}
