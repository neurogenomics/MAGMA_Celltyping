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
        current_version <- magma_installed_version(magma_x = magma_x,
                                                   verbose = TRUE)
        success <- current_version != desired_version
        if (success) {
            messager(
                "WARNING: Old version of MAGMA",
                "(", current_version, ") still detected.",
                "Installation or symlink creation",
                "may not have been completely successful."
            )
        }
    }
    return(success)
}
