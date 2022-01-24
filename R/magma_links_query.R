#' Query MAGMA links
#' 
#' Gather links to MAGMA executables stored in the official MAGMA archives.
#' 
#' @param latest_only Only return the latest version of MAGMA.
#' 
#' @keywords internal  
magma_links_query <- function(latest_only){
    #### Latest MAGMA files are in the Parent Directory ####
    latest_files <- list_remote_files(
        "https://ctg.cncr.nl/software/MAGMA/prog/",
        pattern = "*.zip"
    )
    files <- latest_files
    
    if (latest_only) {
        #### They keep the original version of MAGMA
        # in the Parent Directory as well ####
        files <- files[!grepl("magma_v1\\.0\\.zip|magma_v1\\.0", names(files))]
    }
    if((!latest_only) | length(files)==0) {
        #### Archived versions are in the 'archive' subdirectory ####
        archive_files <- list_remote_files(
            "https://ctg.cncr.nl/software/MAGMA/prog/archive/",
            pattern = "*.zip"
        )
        files <- c(archive_files,files) 
    }
    return(files)
}
