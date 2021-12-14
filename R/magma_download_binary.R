magma_download_binary <- function(magma_url,
                                  dest_dir = "/usr/local/bin",
                                  verbose = TRUE) {
    #### Download the appropriate executable ####
    destfile <- file.path(dest_dir, basename(magma_url))
    messager("Downloading MAGMA executable ==>",dest_dir, v = verbose)
    # Create the decompressed file name in advance
    destpath <- gsub(".zip", "", destfile)
    options(timeout = 5 * 60)
    download.file(magma_url,
        destfile = destfile
    )
    
    utils::unzip(
        zipfile = destfile,
        junkpaths = TRUE,
        exdir = gsub(".zip", "", destfile),
        overwrite = TRUE
    )
    # if(file.exists(destfile)){
    #     try({file.remove(destfile,
    #                      showWarnings = FALSE)})
    # }
    return(destpath)
}
