magma_path <- function() {
    if (magma_installed()) {
        path <- system("which magma", intern = TRUE)
    } else {
        path <- NULL
    }
    return(path)
}
