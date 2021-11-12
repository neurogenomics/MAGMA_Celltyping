magma_installation_successful <- function(desired_version) {
    if (!magma_installed()) {
        magma_installation_info()
        success <- FALSE
    } else {
        current_version <- magma_installed_version()
        success <- current_version != desired_version
        if (success) {
            messager(
                "WARNING: Old version of MAGMA",
                "(", current_version, ") still detected.",
                "Installation or symlink creation",
                "may not have been completely successful."
            )
        } else {
            messager("MAGMA installation successful")
        }
    }
    return(success)
}
