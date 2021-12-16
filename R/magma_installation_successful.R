magma_installation_successful <- function(desired_version) {
    
    #### Standardize version ####
    desired_version <- paste0("v",gsub("v","",desired_version))
    magma_x <- magma_executable(version = desired_version, 
                                verbose = FALSE)
    if (!magma_installed(magma_x = magma_x,
                         verbose = FALSE)) {
        magma_installation_info()
        success <- FALSE
    } else {
        success <- TRUE
        current_version <- magma_installed_version(magma_x = magma_x,
                                                   verbose = TRUE)
        success <- (!is.null(current_version)) && 
                   (current_version == desired_version) 
    }
    return(success)
}
