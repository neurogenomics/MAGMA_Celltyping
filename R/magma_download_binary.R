#' Download a MAGMA binary 
#' 
#' Download a MAGMA binary executable file. 
#' 
#' @param magma_url URL path to MAGMA executable.
#' @param dest_dir Local directory to store the MAGMA executable in.
#' @param verbose Print messages. 
#' 
#' @keywords internal 
magma_download_binary <- function(magma_url,
                                  dest_dir = find_install_dir(),
                                  verbose = TRUE) {
    #### Download the appropriate executable ####
    destfile <- file.path(dest_dir, basename(magma_url))
    destfile <- fix_path(destfile) 
    messager("Downloading MAGMA executable ==>",dest_dir, v = verbose)
    # Create the decompressed file name in advance
    destpath <- gsub("\\.zip$", "", destfile)
    options(timeout = 5 * 60)
    download.file(magma_url,
                  destfile = destfile
    )
    #### Adjust permissions to allow for file manipulation #### 
    set_permissions(path = destfile,
                    verbose = verbose)
    #### Uzip file into folder ####
    try({
        utils::unzip(
            zipfile = destfile,
            junkpaths = TRUE,
            exdir = destpath,
            overwrite = TRUE
        )
    })
    # if(file.exists(destfile)){
    #     try({file.remove(destfile,
    #                      showWarnings = FALSE)})
    # }
    return(destpath)
}
