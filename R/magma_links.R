magma_links <- function(latest_only = TRUE,
                        os_suffix = NULL,
                        version = NULL,
                        verbose = TRUE) {

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
    #### Get the latest version number #####
    version <- magma_links_versions(links = files,
                                    version = version,
                                    return_all = TRUE)
    #### Filter by OS ####
    if (is.null(os_suffix)) { 
        # messager("Filtering magma files by OS.",v=verbose)
        suffix <- paste0(version,magma_os_suffix(),".zip")
        files <- files[endsWith(files,suffix)]
    }
    #### Filter by MAGMA version ####
    if (!is.null(version) && (!latest_only)) {
        # messager("Filtering magma files by version.",v=verbose)
        files <- files[grepl(paste0(version, "_"), names(files))]
    }
    #### Remove duplicates ####
    files <- files[!duplicated(names(files))]
    return(files)
}
