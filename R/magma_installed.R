magma_installed <- function(magma_x = magma_executable(),
                            verbose = TRUE) {
    #### Method 1 ###
    if(length(magma_x)==0){
        installed <- FALSE
    } else {
        installed <- system(as.character(magma_x),
                            ignore.stdout = TRUE) == 1
    }
    if (installed) {
        messager("Congratulations, MAGMA is installed!", v = verbose)
    } else {
        messager("MAGMA does not appear to be installed.", v=verbose)
    }
    return(installed)
}
