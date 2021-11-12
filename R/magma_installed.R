magma_installed <- function(verbose=TRUE) {
    #### Method 1 ###
    installed <- system(sprintf("magma"), ignore.stdout = TRUE) == 1

    # #### Method 2 ####
    # try({
    #     check_magma <- system("magma", intern = TRUE)
    # })
    # installed <- check_magma[1] == paste(
    #     "No arguments specified.",
    #     "Please consult manual for usage instructions.")) {
    #     message("MAGMA already installed.") 
    if(installed){
        messager("Congratulations, MAGMA is installed!", v=verbose)
    }else {
        messager("MAGMA does not appear to be installed.")
    }
    return(installed)
}