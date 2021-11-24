#' Install the MAGMA command line tool
#'
#' Checks whether MAGMA is currently installed, and if not, 
#' tries to automatically install it.
#' 
#' @param dest_dir Folder in which to install MAGMA.
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
magma_install <- function(dest_dir = NULL,
                          upgrade = FALSE,
                          verbose = TRUE) {
    
    version <- magma_installed_version(verbose = FALSE)
    magma_url <- magma_links(latest_only = TRUE,
                             verbose = verbose)
    latest_version <- magma_links_versions(links = magma_url)
    is_installed <- magma_installed(verbose = FALSE)

    if (is_installed) {
        if ((version != latest_version) & upgrade) {
            messager("A newer version of MAGMA is available.",
                "\nUpgrading from", version, "==>", latest_version,
                v = verbose
            )
        } else {
            if (version != latest_version) {
                messager("An older version of MAGMA",
                    paste0("(", version, ")"),"is installed.",
                    "\nLatest MAGMA:",latest_version,
                    v = verbose
                )
            } else {
                messager("The latest version of MAGMA",
                    paste0("(", version, ")"),"is installed.",
                    v = verbose
                )
            }
            upgrade <- FALSE
            version <- latest_version
            path <- magma_path()
            return(path)
        }
    }
    
    if ((!is_installed) | upgrade) {
        if(is.null(dest_dir)) dest_dir <- find_install_dir(verbose = verbose)
        try({ 
            messager("Installing MAGMA:", version, v = verbose)
            messager("Downloading MAGMA executable.", v = verbose)
            destpath <- magma_download_binary(
                magma_url = magma_url,
                dest_dir = dest_dir
            ) 
            dest_magma <- file.path(destpath, "magma")
            #### Change magma file permissions ####
            try({system(paste0("chmod u=rx,go=rx ", dest_magma))})
            try({Sys.chmod(dest_magma, "777", use_umask = FALSE)})
            #### Create a symlink to the actually magma executable #### 
            symlink <- magma_create_symlink(dest_magma = dest_magma,
                                            upgrade = upgrade,
                                            verbose = verbose)
        })
        ##### Check that installation was successful ####
        success <- magma_installation_successful(desired_version = version)
        if(success){
            messager("MAGMA path:", dest_magma, v = verbose)
        }else {
            dest_magma <- NULL
        }
        return(dest_magma)
    }
}
