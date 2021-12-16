magma_links <- function(latest_only = TRUE,
                        os_suffix = NULL,
                        version = NULL,
                        use_local = TRUE,
                        verbose = TRUE) {
    #### Try to search archives ####
    ## If this fails, use a stored backup of the URLs
    if(use_local){
        files <- MAGMA.Celltyping::magma_links_stored
    } else {
        files <- tryCatch(expr = {
            links <- magma_links_query(latest_only = latest_only)
            links <- links[!is.na(links)]
            if(length(links)>0) links else MAGMA.Celltyping::magma_links_stored
        }, error = function(e) MAGMA.Celltyping::magma_links_stored)
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
