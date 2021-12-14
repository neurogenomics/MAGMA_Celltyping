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
#' @importFrom R.utils createLink
#' @examples 
#' ## MAGMA.Celltyping::magma_install()
magma_install <- function(dest_dir = NULL,
                          desired_version = "1.08b",
                          upgrade = FALSE,
                          verbose = TRUE) {
    
    magma_x = magma_executable(version = desired_version,
                               verbose = TRUE)
    current_version <- magma_installed_version(magma_x = magma_x,
                                               verbose = verbose)
    is_installed <- !is.null(current_version)
    #### Get info on latest version ####
    latest_url <- magma_links(latest_only = TRUE,
                              verbose = FALSE)
    latest_version <- magma_links_versions(links = latest_url,
                                           verbose = FALSE)
    #### Standardize the desired version ####
    if(tolower(desired_version)[1]=="latest"){
        magma_url <- latest_url
        desired_version <- latest_version
    } else {
        magma_url <- magma_links(latest_only = FALSE,
                                 version = desired_version,
                                 verbose = verbose)
        desired_version <- magma_links_versions(links = magma_url,
                                                verbose = verbose)
    } 
    #### Check whether the desired version is already installed #### 
    if (is_installed) {
        if ((current_version != desired_version) & upgrade) {
            messager("A different version of MAGMA is available.",
                "Upgrading from", current_version, "==>", desired_version,
                v = verbose
            )
        } else {
            if (current_version != desired_version) {
                messager("A different version of MAGMA",
                    paste0("(", current_version, ")"),"is installed.",
                    "Latest MAGMA:",latest_version,
                    v = verbose
                )
            } else if(current_version == latest_version){
                messager("The latest version of MAGMA",
                    paste0("(", latest_version, ")"),"is installed.",
                    v = verbose
                )
            }
            upgrade <- FALSE 
            path <- magma_path()
            return(path)
        }
    }
    
    if ((!is_installed) | upgrade) {
        if(is.null(dest_dir)) dest_dir <- find_install_dir(verbose = verbose) 
            messager("Installing MAGMA:", desired_version, v = verbose) 
            destpath <- magma_download_binary(
                magma_url = magma_url,
                dest_dir = dest_dir,
                verbose = verbose
            ) 
            dest_magma <- file.path(destpath, "magma")
            #### Change magma file permissions ####
            try({system(paste0("chmod u=rx,go=rx ", dest_magma))})
            try({Sys.chmod(dest_magma, "777", use_umask = FALSE)})
            #### Create a symlink to the actually magma executable #### 
            # symlink <- magma_create_symlink(dest_magma = dest_magma,
            #                                 upgrade = upgrade,
            #                                 verbose = verbose) 
        ##### Check that installation was successful ####
        success <- magma_installation_successful(
            desired_version = desired_version)
        if(success){
            messager("MAGMA path:", dest_magma, v = verbose)
        }else {
            dest_magma <- NULL
        }
        return(dest_magma)
    }
}
