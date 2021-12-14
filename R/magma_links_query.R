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
        files <- files[!grepl("magma_v1.0.zip|magma_v1.0_", files)]
    } else {
        #### Archived versions are in the 'archive' subdirectory ####
        archive_files <- list_remote_files(
            "https://ctg.cncr.nl/software/MAGMA/prog/archive/",
            pattern = "*.zip"
        )
        files <- c(files, archive_files)
    }
    return(files)
}