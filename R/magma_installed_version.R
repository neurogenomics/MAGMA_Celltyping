magma_installed_version <- function(verbose = TRUE) {
    if (magma_installed(verbose = verbose)) {
        check_magma <- system("magma --version", intern = TRUE)
        version <- strsplit(check_magma, " ")[[1]][3]
        messager("Current MAGMA version:", version, v = verbose)
    } else {
        messager("MAGMA is not installed.", v = verbose)
        version <- NULL
    }
    return(version)
}
