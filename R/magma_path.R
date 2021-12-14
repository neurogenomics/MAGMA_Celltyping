magma_path <- function() {
    
    if (magma_installed()) {
        path <- magma_executable(verbose = TRUE)
        # path <- system("which magma", intern = TRUE)
    } else {
        path <- NULL
    }
    return(path)
}
