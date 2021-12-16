#' Install the MAGMA command line tool
#'
#' Checks whether MAGMA is currently installed, and if not, 
#' tries to automatically install it.
#' 
#' @param dest_dir Folder in which to install MAGMA.
#' @param desired_version Desired version of MAGMA.
#' @param upgrade If MAGMA is already installed and is not the latest version, 
#' should it be upgraded to the latest version? (Default: \code{FALSE}). 
#' @param verbose Print messages.
#' @source \href{https://ctg.cncr.nl/software/magma}{MAGMA website}
#' @source \href{https://github.com/NathanSkene/MAGMA_Celltyping}{
#' MAGMA.celltyping documentation}
#' 
#' @return Path to MAGMA executable.
#'
#' @export
#' @importFrom utils unzip
#' @examples 
#' magma <- MAGMA.Celltyping::magma_install()
magma_install <- function(dest_dir = find_install_dir(),
                          desired_version = "latest",
                          upgrade = FALSE,
                          verbose = TRUE) { 
    #### Standardise desired_version ####
    if(is.null(desired_version)) desired_version <- "latest"
    desired_version <- tolower(desired_version)[1]
    #### Get info on latest version ####
    latest_url <- magma_links(latest_only = TRUE,
                              verbose = FALSE)
    latest_version <- magma_links_versions(links = latest_url,
                                           verbose = FALSE)
    #### Standardize the desired version ####
    if(is.null(desired_version) || tolower(desired_version)[1]=="latest"){
        magma_url <- latest_url
        desired_version <- latest_version
    } else {
        magma_url <- magma_links(latest_only = FALSE,
                                 version = desired_version,
                                 verbose = verbose)
        desired_version <- magma_links_versions(links = magma_url,
                                                verbose = verbose)
    } 
    #### Check whether ANY version of MAGMA is installed #### 
    magma_x_list <- magma_executable(return_all = TRUE,
                                     verbose = FALSE)
    current_versions <- magma_installed_version(magma_x = magma_x_list,
                                                verbose = verbose)
    is_installed <- any(!is.null(current_versions))

    #### If not, proceed to install the desired version ####
    if ((!is_installed) | upgrade) { 
        ## Upgrade message
        if ((!desired_version %in% current_versions) & upgrade) {
            messager("A different version of MAGMA is available.",
                     "Installing MAGMA:", desired_version,
                     v = verbose
            )
        } else {
            ### Regular message 
            messager("Installing MAGMA:", desired_version, v = verbose) 
        } 
        destpath <- magma_download_binary(
            magma_url = magma_url,
            dest_dir = dest_dir,
            verbose = verbose
        ) 
        dest_magma <- file.path(destpath, "magma")
        #### Change magma file permissions ####
        try({system(paste0("chmod u=rx,go=rx ", dest_magma))})
        try({Sys.chmod(dest_magma, "777", use_umask = FALSE)})  
    } else{
        messager("Skipping MAGMA installation.",v=verbose)
    }
    dest_magma <- magma_check_version_match(
        desired_version = desired_version,
        verbose = verbose
    )
    return(dest_magma)
}
