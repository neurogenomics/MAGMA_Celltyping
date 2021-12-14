magma_installed <- function(magma_x = magma_executable(),
                            verbose = TRUE) {
    #### Method 1 ###
    installed <- system(as.character(magma_x[1]),
                        ignore.stdout = TRUE) == 1
    # #### Method 2 ####
    # try({
    #     check_magma <- system("magma", intern = TRUE)
    # })
    # installed <- check_magma[1] == paste(
    #     "No arguments specified.",
    #     "Please consult manual for usage instructions.")) {
    #     message("MAGMA already installed.")
    if (installed) {
        messager("Congratulations, MAGMA is installed!", v = verbose)
    } else {
        messager("MAGMA does not appear to be installed.", v=verbose)
    }
    return(installed)
}
